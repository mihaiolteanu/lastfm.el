;; -*- lexical-binding: t -*-

(require 'request)
(require 'cl-lib)
(require 'elquery)

(defconst lastfm--url "http://ws.audioscrobbler.com/2.0/")
(defconst lastfm--config-file (concat (xdg-config-home) "/.lastfmrc"))
(defvar   lastfm--api-key)
(defvar   lastfm--shared-secret)
(defvar   lastfm--username)
(defvar   lastfm--sk)

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

(cl-defun lastfm--request (method data)
  (let ((resp ""))
    (request lastfm--url
             :params `(("method" . ,method)                   
                       ("limit" . 3)
                       ("api_key" . ,lastfm--api-key)
                       ,@data)
             :parser 'buffer-string
             :sync    t
             :success (cl-function
                       (lambda (&key data &allow-other-keys)
                         (setq resp data))))
    resp))

(defun lastfm--parser (xml query)
  (mapcar #'elquery-text
          (elquery-$ query (elquery-read-string xml))))

(defun lastfm-artist-getsimilar (artist)
  (lastfm--parser
   (lastfm--request "artist.getsimilar"
                    `(("artist" . ,artist)))
   "artist > name"))

(defun lastfm-artist-gettoptracks (artist)
  (lastfm--parser
   (lastfm--request "artist.gettoptracks"
                    `(("artist" . ,artist)))
   "track > name"))

;; (lastfm-artist-getsimilar "anathema")
;; (lastfm-artist-gettoptracks "anathema")

(provide 'lastfm)
