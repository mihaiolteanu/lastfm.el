;; -*- lexical-binding: t -*-

(require 'request)
(require 'cl-lib)
(require 'elquery)
(require 's)

(defconst lastfm--url         "http://ws.audioscrobbler.com/2.0/")
(defconst lastfm--config-file (concat (xdg-config-home) "/.lastfmrc"))
(defvar   lastfm--api-key)
(defvar   lastfm--shared-secret)
(defvar   lastfm--username)
(defvar   lastfm--sk)

(defconst lastfm--methods  
  '(;; Album
    (album-getinfo       :no-auth  (artist album)  "track > name"                )
    ;; Artist
    (artist-getinfo      :no-auth  (artist)        "bio summary"                 )
    (artist-getsimilar   :no-auth  (artist limit)  "artist name"                 )
    (artist-gettoptags   :no-auth  (artist)        "tag name"                    )
    (artist-gettopalbums :no-auth  (artist limit)  "album > name"                )
    (artist-gettoptracks :no-auth  (artist limit)  "track > name"                )
    (artist-search       :no-auth  (artist limit)  "artist name"                 )
    ;; Auth (only need to be called once, to get the session key (sk))
    (auth-gettoken       :sk       ()              "token"                       )
    (auth-getsession     :sk       (token)         "session key"                 )
    ;; Tag
    (tag-getinfo         :no-auth  (tag)           "summary"                     )
    (tag-gettoptracks    :no-auth  (tag limit)     "artist > name, track > name" )
    (tag-gettopartists   :no-auth  (tag limit)     "artist name"                 )
    ;; Track
    (track-love          :auth     (artist track)  "lfm"                         )
    (track-unlove        :auth     (artist track)  "lfm"                         )
    (track-scrobble      :auth     (artist track timestamp)  "lfm"               )
    ;; User
    (user-getlovedtracks :no-auth  (user limit)    "artist > name, track > name" ))
  "List of all the supported lastfm methods from which the API is generated")

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

(defun lastfm--read-config-file ()
  "Return the useful bits of the config file."
  (with-temp-buffer
    (insert-file-contents lastfm--config-file)
    ;; car is the function name used in cl, but we can't use that here since no
    ;; namespaces are available; skipping it.
    (cl-rest (read (buffer-string)))))

(defun lastfm--set-user-config ()
  "Set the api-key, shared-secret, etc from the config file"
  (let ((config (lastfm--read-config-file)))
    (cl-mapcar (lambda (key value)
                 (setf (pcase key
                         (:API-KEY lastfm--api-key)
                         (:SHARED-SECRET lastfm--shared-secret)
                         (:USERNAME lastfm--username)
                         (:SK lastfm--sk))
                       value))
               (cl-remove-if #'stringp config)
               (cl-remove-if #'symbolp config))))

(lastfm--set-user-config)

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
