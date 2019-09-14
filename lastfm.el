;; -*- lexical-binding: t -*-

(require 'request)
(require 'cl-lib)
(require 'elquery)
(require 's)

;;;; Configuration and setup
(defconst lastfm--url "http://ws.audioscrobbler.com/2.0/"
  "The URL for the last.fm API version 2 used to make all the
  method calls.")

(defconst lastfm--config-file
  (let ((f (concat (xdg-config-home) "/.lastfmrc")))
    (or (file-exists-p f)
        (with-temp-file f
          (insert "(CONFIG
  :API-KEY \"\"
  :SHARED-SECRET \"\"
  :USERNAME \"\")")))
    f)
  "User config file holding the last.fm api-key, shared-secret,
username and the session key. If the file does not exist when the
package is loaded, build it with empty values.")

;; The values of these configs are taken from the user config file.
(defvar lastfm--api-key)
(defvar lastfm--shared-secret)
(defvar lastfm--username)
(defvar lastfm--sk)

(defun lastfm--read-config-file ()
  "Return the config file contents as a Lisp object"
  (with-temp-buffer
    (insert-file-contents lastfm--config-file)
    (read (buffer-string))))

(defun lastfm--set-config-parameters ()
  "Read the config file and set the config parameters used
throught the package."
  (let ((config (cl-rest (lastfm--read-config-file))))
    (cl-mapcar (lambda (key value)
                 (setf (pcase key
                         (:API-KEY       lastfm--api-key)
                         (:SHARED-SECRET lastfm--shared-secret)
                         (:USERNAME      lastfm--username)
                         (:SK            lastfm--sk))
                       value))
               (cl-remove-if #'stringp config)
               (cl-remove-if #'symbolp config))))

(lastfm--set-config-parameters)         ;set params on start-up.

(defun lastfm-generate-session-key ()
  "Get an authorization token from last.fm and then ask the user
to grant persmission to his last.fm account. If granted, then ask
for the session key (sk) and append the sk value to the config's
file list of values."
  (let ((token (cl-first (lastfm-auth-gettoken))))
    ;; Ask the user to allow access.
    (browse-url (concat "http://www.last.fm/api/auth/?api_key="
                        lastfm--api-key
                        "&token=" token))
    (when (yes-or-no-p "Did you grant the application persmission 
to access your Last.fm account? ")
      ;; If permission granted, get the sk and update the config file.
      (let* ((sk (cl-first (lastfm-auth-getsession token)))
             (config (lastfm--read-config-file))
             (config-with-sk (append config (list :SK sk))))
        (with-temp-file lastfm--config-file
          (insert (prin1-to-string config-with-sk))))
      (lastfm--set-config-parameters)   ;set params on config file update.
      )))


;;;; Methods list, and functions for it's manipulation
(defconst lastfm--methods-pretty
  '((album
     (getinfo :no-auth (artist album) "track > name"))
    
    (artist
     (getinfo      :no-auth (artist)       "bio summary")
     (getsimilar   :no-auth (artist limit) "artist name")
     (gettoptags   :no-auth (artist)       "tag name")
     (gettopalbums :no-auth (artist limit) "album > name")
     (gettoptracks :no-auth (artist limit) "track > name")
     (search       :no-auth (artist limit) "artist name"))
    
    ;; Auth (only need to be called once, to get the session key (sk))
    (auth
     (gettoken   :sk ()      "token")
     (getsession :sk (token) "session key"))
    
    (tag
     (getinfo       :no-auth  (tag)       "summary")
     (gettoptracks  :no-auth  (tag limit) "artist > name, track > name")
     (gettopartists :no-auth  (tag limit) "artist name"))
    
    (track
     (love     :auth (artist track)           "lfm")
     (unlove   :auth (artist track)           "lfm")
     (scrobble :auth (artist track timestamp) "lfm"))
    
    (user
     (getlovedtracks :no-auth  (user limit)    "artist > name, track > name" )))
  "List of all the supported lastfm methods. A one liner
like (artist-getinfo ...) or (track-love ...) is more easier to
parse, but this is easier for the eyes. The latter, the
one-liner, is generated from this list and is the one actually
used for all the processing and generation of the user API. ")

(defconst lastfm--methods
  (let ((res nil))
    (mapcar
     (lambda (group)
       (mapcar
        (lambda (method)
          (push (cons (make-symbol
                       (concat (symbol-name (cl-first group)) "-"
                               (symbol-name (cl-first method))))
                      (cl-rest method))
                res))
        (cl-rest group)))
     lastfm--methods-pretty)
    (reverse res))
  "Generated list of one-liner lastfm methods from the pretty
list of methods. Each entry in this list is a complete lastm
method specification. It can be, and it is, used to generate the
API for this library.")

(defun lastfm--method-name (method)
  (cl-first method))

(defun lastfm--method-str (method)
  "The method name, as a string that can be used in a lastfm
request."
  (s-replace "-" "." (symbol-name (lastfm--method-name method))))

(defun lastfm--auth-p (method)
  "Does this method require authentication?"
  (eql (cl-second method) :auth))

(defun lastfm--sk-p (method)
  "Is this a method used for requesting the session key?"
  (eql (cl-second method) :sk))

(defun lastfm--method-params (method)
  "Parameters requested for succesfully calling this method."
  (cl-third method))

(defun lastfm--query-str (method)
  "XML query string for extracting the relevant data from the
lastfm response."
  (cl-fourth method))

(defun lastfm--multi-query-p (query)
  "CSS selectors with ',' allow retrieving multiple tags in the same request"
  (s-contains "," query))

(defun lastfm--group-params-for-signing (params)
  "The signing procedure for authentication needs all the
parameters and values lumped together in one big string without
equal or ampersand symbols between them."
  (let ((res ""))
    (mapcar (lambda (s)
              (setf res (concat res (car s) (cdr s))))
            params)
    (concat res lastfm--shared-secret)))

(defun lastfm--build-params (method values)
  "Build the parameter/value list to be used by request :params."
  (let ((result
         `(;; The api key and method is needed for all calls.
           ("api_key" . ,lastfm--api-key)
           ("method" . ,(lastfm--method-str method))
           ;;Pair the user supplied values with the  method parameters.
           ,@(cl-mapcar (lambda (param value)
                          (cons (symbol-name param) value))
                        (lastfm--method-params method)
                        values))))
    ;; Session Key(SK) parameter is needed for all auth services, but not for
    ;; the services used to obtain the SK.
    (when (lastfm--auth-p method)
      (push `("sk" . ,lastfm--sk) result))
    ;; If signing is needed, it should be added as the last parameter.
    (when (or (lastfm--auth-p method)
              (lastfm--sk-p method))
      ;; Params need to be in alphabetical order before signing.
      (setq result (cl-sort result #'string-lessp
                            :key #'cl-first))
      (add-to-list 'result
                   `("api_sig" . ,(md5 (lastfm--group-params-for-signing result)))
                   t))
    result))

(cl-defun lastfm--request (method &rest values)
  (let ((resp ""))
    (request lastfm--url
             :params  (lastfm--build-params method values)
             :parser  'buffer-string
             :type    "POST"
             :sync    t
             :success (cl-function
                       (lambda (&key data &allow-other-keys)
                         (setq resp data))))
    resp))

(defun lastfm--parser (response method)
  (mapcar #'elquery-text
          (elquery-$ (lastfm--query-str method)
                     (elquery-read-string response))))

(defun lastfm--build-function (method)
  (let* ((name-str (symbol-name (lastfm--method-name method)))
         (fn-name (intern (concat "lastfm-" name-str)))
         (params (lastfm--method-params method)))
    `(defun ,fn-name ,params
       (lastfm--parser
        (lastfm--request ',method
                         ,@params)
        ',method))))

(defmacro lastfm--build-api ()
  `(progn
     ,@(mapcar (lambda (method)
                 (lastfm--build-function method))
               lastfm--methods)))

(lastfm--build-api)

(provide 'lastfm)
