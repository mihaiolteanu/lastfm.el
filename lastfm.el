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
     (addTags
      "Tag an album using a list of user supplied tags."
      :yes (artist album tags) () ("lfm"))
     
     (getInfo
      "Get the metadata and tracklist for an album on Last.fm using the album name."
      :no  (artist album) () ("track > name"))
     
     (getTags
      "Get the tags applied by an individual user to an album on Last.fm."
      :yes (artist album) () ("tag name"))
     
     (getTopTags
      "Get the top tags for an album on Last.fm, ordered by popularity."
      :no (artist album) () ("tag name"))
     
     (removeTag
      "Remove a user's tag from an album."
      :yes (artist album tag) () ("lfm"))
     
     (search
      "Search for an album by name. Returns album matches sorted by relevance."
      :no (album) ((limit 10)) ("album artist")))
    

    (artist
     (addTags
      "Tag an artist with one or more user supplied tags."
      :yes (artist tags) () ("lfm"))
     
     (getCorrection
      "Check whether the artist has a correction to a canonical artist."
      :no (artist) () ("artist name"))
     
     (getInfo
      "Get the metadata for an artist. Includes biography, max 300 characters."
      :no (artist) () ("bio summary"))
     
     (getSimilar
      "Get all the artists similar to this artist."
      :no (artist) ((limit lastfm--similar-limit)
                    (user lastfm--username))
      ("artist name"))

     (getTags
      "Get the tags applied by an individual user to an artist on Last.fm."
      :yes (artist) () ("tag name"))
     
     (getTopAlbums
      "Get the top albums for an artist, ordered by popularity."
      :no (artist) ((limit 50)) ("album > name"))
     
     (getTopTags
      "Get the top tags for an artist, ordered by popularity."
      :no (artist) () ("tag name"))
     
     (getTopTracks
      "Get the top tracks by an artist, ordered by popularity."
      :no (artist) ((limit 50) (page 1))
      ("artist > name" "track > name" "track > playcount"))
     
     (removeTag
      "emove a user's tag from an artist."
      :yes (artist tag) () ("lfm"))
     
     (search
      "Search for an artist by name. Returns artist matches sorted by relevance."
      :no (artist) ((limit 30)) ("artist name")))
    

    (auth
     (getToken
      "Fetch a session key for a user (3rd step in the auth process)."
      :sk () () ("token"))
     
     (getSession
      "Fetch an unathorized request token (2nd step of the auth process)."
      :sk (token) () ("session key")))
    

    (chart
     (getTopArtists
      "Get the top artists chart."
      :no () ((limit 50)) ("name"))
     
     (getTopTags
      "Get the top tags chart."
      :no () ((limit 50)) ("name"))
     
     (getTopTracks
      "Get the top tracks chart."
      :no () ((limit 50)) ("artist > name" "track > name" "track > listeners")))


    (geo
     (getTopArtists
      "Get the most popular artists on Last.fm by country."
      :no (country) ((limit 50) (page 1)) ("artist name"))
     
     (getTopTracks
      "Get the most popular tracks on Last.fm last week by country."
      :no (country) ((limit 50) (page 1)) ("artist > name" "track > name")))


    (library 
     (getArtists
      "A list of all the artists in a user's library."
      :no () ((user lastfm--username) (limit 50) (page 1)) ("artist name")))


    (tag
     (getInfo
      "Get the metadata for a tag"
      :no (tag) () ("summary"))
     
     (getSimilar
      "Search for tags similar to this one, based on listening data."
      :no (tag) () ("tag name")) ;Doesn't return anything
     
     (getTopAlbums
      "Get the top albums tagged by this tag, ordered by tag count."
      :no (tag) ((limit 50) (page 1)) ("artist > name" "album > name"))
     
     (getTopArtists
      "Get the top artists tagged by this tag, ordered by tag count."
      :no (tag) ((limit 50) (page 1)) ("artist name"))
     
     (getTopTags
      "Fetches the top global tags on Last.fm, sorted by number of times used."
      :no () () ("name"))
     
     (getTopTracks
      "Get the top tracks tagged by this tag, ordered by tag count."
      :no (tag) ((limit 50) (page 1)) ("artist > name" "track > name")))
    

    (track
     (addTags
      "Tag an album using a list of user supplied tags."
      :yes (artist track tags) () ("lfm"))
     
     (getCorrection
      "Check whether the supplied track has a correction to a canonical track."
      :no (artist track) () ("artist > name" "track > name"))
     
     (getInfo
      "Get the track metadata."
      :no (artist track) () ("album title"))
     
     (getSimilar
      "Get similar tracks to this one, based on listening data."
      :no (artist track) ((limit 10))
      ("artist > name" "track > name")) ;; Method doesn't return anything from lastfm
     
     (getTags
      "Get the tags applied by an individual user to a track."
      :yes (artist track) () ("name"))
     
     (getTopTags
      "Get the top tags for this track, ordered by tag count."
      :no (artist track) () ("name"))
     
     (love
      "Love a track for a user profile."
      :yes (artist track) () ("lfm"))
     
     (removeTag
      "Remove a user's tag from a track."
      :yes (artist track tag) () ("lfm"))
     
     (scrobble
      "Add a track to the user listened tracks."
      :yes (artist track timestamp) () ("lfm"))
     
     (search
      "Search for a track by track name. Returned matches are sorted by relevance."
      :no (track) ((artist nil) (limit 30) (page 1)) ("track > artist" "track > name"))
     
     (unlove
      "UnLove a track for a user profile."
      :yes (artist track) () ("lfm"))
     
     (updateNowPlaying
      "Notify Last.fm that a user has started listening to a track."
      :yes (artist track)
      ((album nil) (tracknumber nil) (context nil) (duration nil)
       (albumartist nil)) ("lfm")))
    

    (user
     (getfriends
      "Get a list of the user's friends on Last.fm."
      :no (user) ((recenttracks nil) (limit 50) (page 1)) ("name"))
     
     (getInfo
      "Get information about a user profile."
      :no () ((user lastfm--username))
      ("name" "realname" "country" "age" "gender" "subscriber" "playcount"))
     
     (getLovedTracks
      "Get the last LIMIT number of tracks loved by a user."
      :no  () ((user lastfm--username) (limit 50) (page 1))
      ("artist > name" "track > name"))
     
     (getPersonalTags
      "Get the user's personal tags"
      :no (tag taggingtype)
      ((user lastfm--username) (limit 50) (page 1)) ("name"))
     
     (getRecentTracks
      "Get a list of the recent tracks listened to by this user."
      :no () ((user lastfm--username) (limit nil) (page nil)
              (from nil) (to nil) (extended 0))
      ("track > artist" "track > name"))
     
     (getTopAlbums
      "Get the top albums listened to by a user"
      :no () ((user lastfm--username) (period nil)
              (limit nil) (page nil))
      ("artist > name" "album > name"))
     
     (getTopArtists
      "Get the top artists listened to by a user."
      :no () ((user lastfm--username) (period nil)
              (limit nil) (page nil))
      ("artist name"))
     
     (getTopTags
      "Get the top tags used by this user."
      :no () ((user lastfm--username) (limit nil)) ("tag name"))
     
     (getTopTracks
      "Get the top tracks listened to by a user. "
      :no () ((user lastfm--username) (period nil)
              (limit nil) (page nil))
      ("artist > name" "track > name"))
     
     (getWeeklyAlbumChart
      "Get an album chart for a user profile, for a given date range."
      :no () ((user lastfm--username) (from nil) (to nil))
      ("album > artist" "album > name"))
     
     (getWeeklyArtistChart
      "Get an artist chart for a user profile, for a given date range."
      :no () ((user lastfm--username) (from nil) (to nil))
      ("album > name" "artist > playcount"))
     
     (getWeeklyTrackChart
      "Get a track chart for a user profile, for a given date range."
      :no () ((user lastfm--username) (from nil) (to nil))
      ("track > artist" "track > name"))))
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
method specification. It is used to generate the API for this
library.")

(defun lastfm--method-name (method)
  (make-symbol
   (downcase (symbol-name (cl-first method)))))

(defun lastfm--method-request-string (method)
  "The method name, as a string that can be used in a lastfm
request."
  (downcase
   (s-replace "-" "."
              (symbol-name (lastfm--method-name method)))))

(defun lastfm--method-url (method)
  "Return the Last.fm documentation url for this method."
  (concat "https://www.last.fm/api/show/"
          (s-replace "-" "." (symbol-name (lastfm--method-name method)))))

(defun lastfm--doc-string (method)
  (cl-second method))

(defun lastfm--auth-p (method)
  "Does this method require authentication?"
  (eql (cl-third method) :yes))

(defun lastfm--sk-p (method)
  "Is this a method used for requesting the session key?"
  (eql (cl-third method) :sk))

(defun lastfm--memoizable-p (method)
  "Returns t if a request to Last.fm would return the same data
everytime."
  (not (or (lastfm--auth-p method)
           (lastfm--sk-p method))))

(defun lastfm--method-params (method)
  "Minimum required parameters for succesfully calling this method."
  (cl-fourth method))

(defun lastfm--method-keyword-params (method)
  (cl-fifth method))

(defun lastfm--all-method-params (method)
  "A list of all the method parameters, required plus keyword."
  (append (lastfm--method-params method)
          (mapcar #'car (lastfm--method-keyword-params method))))

(defun lastfm--query-strings (method)
  "CSS selectors to parse the last.fm response with."
  (cl-sixth method))

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
           ("method" . ,(lastfm--method-request-string method))
           ;; Pair the user supplied values with the method parameters.  If no
           ;; value supplied for a given param, do not include it in the request.
           ,@(cl-remove-if #'null
              (cl-mapcar (lambda (param value)
                           (when value
                             (cons (symbol-name param) value)))
                         (lastfm--all-method-params method)
                         values)))))
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
             :params   (lastfm--build-params method values)
             :parser   'buffer-string
             :type     "POST"
             :sync     t
             :complete (cl-function
                        (lambda (&key data &allow-other-keys)
                          (setq resp data))))
    resp))

(defun lastfm--key-from-query-str (query-string)
  "Use the query string to build a key usable in alists."
  (declare (string method-name))
  (make-symbol
   (s-replace " " ""                    ;remove all extra spaces
              ;; Some queries contain '>' others only ' '. Replace both of them
              ;; with '-'.
              (if (s-contains-p ">" query-string)
                  (s-replace ">" "-" query-string)
                (s-replace " " "-" query-string)))))

(defun lastfm--parse-response (response method)
  (let* ((raw-response (elquery-read-string response))
         ;; Only one error expected, if any.
         (error-str (elquery-text
                     (cl-first (elquery-$ "error" raw-response)))))
    (if error-str
        (error error-str)
      (let ((query-strings (lastfm--query-strings method)))
        (cl-labels
            ((helper (queries)
                     (if (null queries)
                         '()
                       ;; Use the same raw response to extract a different text
                       ;; object each time, according to the current query
                       ;; string. Build an alist from the query string and the
                       ;; extracted text object.
                       (cons (--map (cons (lastfm--key-from-query-str (car queries))
                                          (elquery-text it))
                                    (elquery-$ (car queries) raw-response))
                             (helper (cdr queries))))))
          (let ((result (helper query-strings)))            
            (reverse
             ;; The cons from the helper method above groups all the text
             ;; objects from the first query string together, followed by all
             ;; the text objects from the next query string grouped together and
             ;; so on until all the query strings are exhausted. If the query
             ;; string would look like '("artist" "song") then we would have
             ;; '((artist1 artist2) (song1 song2)) as a result from the helper
             ;; method, but we want '((artist1 songs1) (artist2 song2)) instead.
             (if (= (length query-strings) 2)
                 ;; Workaround for -zip returning a cons cell instead of a list
                 ;; when two lists are provided to it.
                 (-zip-with #'list (cl-first result) (cl-second result))
               (apply #'-zip (helper query-strings))))))))))

(defun lastfm--build-function (method)
  "Use the method data to build a complete user function."
  (let* ((name-str (symbol-name (lastfm--method-name method)))
         (fn-name (intern (concat "lastfm-" name-str)))
         (params (lastfm--method-params method))
         (key-params (lastfm--method-keyword-params method))
         (fn `(cl-defun ,fn-name ,(if key-params ;Name and parameters
                                      `(,@params &key ,@key-params)
                                    `,@params)
                ;; Documentation string.
                ,(concat (lastfm--doc-string method)
                         "\n \n"
                         "See the official Last.fm page for full documentation at"
                         "\nURL `"
                         (lastfm--method-url method)
                         "'")
                ;; Body.
                (lastfm--parse-response
                 (lastfm--request ',method
                                  ,@(if key-params
                                        `(,@params ,@(mapcar #'car key-params))
                                      `,params))
                 ',method))))
    ;; Memoize if needed and not already memoized (when reloading the library,
    ;; for example), return the function definition as is, otherwise.
    (if (lastfm--memoizable-p method)
        `(condition-case nil
             (memoize ,fn)
           ,fn)
      fn)))

(defmacro lastfm--build-api ()
  `(progn
     ,@(--map (lastfm--build-function it)
              lastfm--methods)))

(lastfm--build-api)

(provide 'lastfm)
