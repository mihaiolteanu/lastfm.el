;;; lastfm.el --- Last.fm API for Emacs Lisp -*- lexical-binding: t -*-

;; Copyright (C) 2019 Free Software Foundation, Inc.

;; Author: Mihai Olteanu <mihai_olteanu@fastmail.fm>
;; Version: 1.0
;; Package-Requires: ((emacs "26.1") (request "0.3.0") (memoize "1.1") (elquery "0.1.0") (s "1.12.0"))
;; Keywords: multimedia, api
;; URL: https://github.com/mihaiolteanu/lastfm.el/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; lastfm.el provides a complete interface to the Last.fm API as defined by URL
;; `https://www.last.fm/api/'.  An API account, obtainable for free from
;; Last.fm, is needed to use the majority of provided services.  A one-time
;; authentication process is needed to access the rest of the methods.

;; Example usage to get the top three Duran Duran songs of all time:

;; (lastfm-artist-gettoptracks "duran duran" :limit 3)
;; => (((track-name . "Ordinary World")
;;      (playcount . "2299087")
;;      (listeners . "435772"))
;;     ((track-name . "Hungry Like the Wolf")
;;      (playcount . "1912264")
;;      (listeners . "427961"))
;;     ((track-name . "Come Undone")
;;      (playcount . "1743751")
;;      (listeners . "290926")))
;;
;; See the package URL for complete documentation and installation instructions.

;;; Code:

(require 'request)
(require 'cl-lib)
(require 'memoize)
(require 'elquery)
(require 'xdg)
(require 's)

(defgroup lastfm ()
  "Customize Last.fm API."
  :group 'music)

(defcustom lastfm-enable-doc-generation 'nil
  "If t, generate markdown documentation at load time.
Only used for development purposes."
  :type 'boolean
  :group 'lastfm)

;;;; Configuration and setup
(defconst lastfm--url "http://ws.audioscrobbler.com/2.0/"
  "The URL for the last.fm API version 2.")

(defconst lastfm--config-file
  (let ((f (concat (xdg-config-home) "/.lastfmrc")))
    (or (file-exists-p f)
        (with-temp-file f
          (insert "(CONFIG
  :API-KEY \"\"
  :SHARED-SECRET \"\"
  :USERNAME \"\")")))
    f)
  "The user config file for this library.
The file is generated when the library is loaded the first time,
if it doesn't already exists.")

;; The values of these configs are taken from the user config file.
(defvar lastfm--api-key)
(defvar lastfm--shared-secret)
(defvar lastfm--username)
(defvar lastfm--sk)

(defun lastfm--read-config-file ()
  "Return the config file contents as a Lisp object."
  (with-temp-buffer
    (insert-file-contents lastfm--config-file)
    (read (buffer-string))))

(defun lastfm--set-config-parameters ()
  "Initialize the variables used by all Last.fm requests."
  (let ((config (cl-rest (lastfm--read-config-file))))
    (cl-mapcar (lambda (key value)
                 (pcase key
                   (:API-KEY       (setq lastfm--api-key value))
                   (:SHARED-SECRET (setq lastfm--shared-secret value))
                   (:USERNAME      (setq lastfm--username value))
                   (:SK            (setq lastfm--sk value))))
               (cl-remove-if #'stringp config)
               (cl-remove-if #'symbolp config))))

(lastfm--set-config-parameters)         ;set params on start-up.

(defun lastfm-generate-session-key ()
  "Generate a session key and save it in the .lastfmrc file.
The user needs to grant Last.fm access to his/her application.
Both lastfm-auth-gettoken and lastfm-auth-getsession methods used
here are generated based on the lastfm--methods lists so they
don't appear as being defined anywhere."
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
      ;; Reload the file after the sk update.
      (lastfm--set-config-parameters))))

(defconst lastfm--methods-pretty
  '((album
     (addTags
      "Tag an album using a list of user supplied tags."
      :yes (artist album tags) () ("lfm"))
     
     (getInfo
      "Get the metadata and tracklist for an album on Last.fm using the album name."
      :no (artist album) () ("track > name"))
     
     (getTags
      "Get the tags applied by an individual user to an album on Last.fm."
      :yes (artist album) () ("tag name" "tag count"))
     
     (getTopTags
      "Get the top tags for an album on Last.fm, ordered by popularity."
      :no (artist album) () ("tag name" "tag count"))
     
     (removeTag
      "Remove a user's tag from an album."
      :yes (artist album tag) () ("lfm"))
     
     (search
      "Search for an album by name. Returns album matches sorted by relevance."
      :no (album) ((limit 10)) ("album artist" "album name")))
    

    (artist
     (addTags
      "Tag an artist with one or more user supplied tags."
      :yes (artist tags) () ("lfm"))
     
     (getCorrection
      "Check whether the artist has a correction to a canonical artist."
      :no (artist) () ("artist name"))
     
     (getInfo
      "Get the metadata for an artist. Includes biography, max 300 characters."
      :no (artist) () ("bio summary" "listeners" "playcount"))
     
     (getSimilar
      "Get all the artists similar to this artist."
      :no (artist) ((limit 10)
                    (user lastfm--username))
      ("artist name"))

     (getTags
      "Get the tags applied by an individual user to an artist on Last.fm."
      :yes (artist) () ("tag name"))
     
     (getTopAlbums
      "Get the top albums for an artist, ordered by popularity."
      :no (artist) ((limit 10)) ("album > name"))
     
     (getTopTags
      "Get the top tags for an artist, ordered by popularity."
      :no (artist) () ("tag name"))
     
     (getTopTracks
      "Get the top tracks by an artist, ordered by popularity."
      :no (artist) ((limit 10) (page 1))
      ("track > name" "playcount" "listeners"))
     
     (removeTag
      "Remove a user's tag from an artist."
      :yes (artist tag) () ("lfm"))
     
     (search
      "Search for an artist by name. Returns artist matches sorted by relevance."
      :no (artist) ((limit 10)) ("artist name")))
    

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
      :no () ((limit 10)) ("artist name" "playcount" "listeners"))
     
     (getTopTags
      "Get the top tags chart."
      :no () ((limit 10)) ("tag-name"))
     
     (getTopTracks
      "Get the top tracks chart."
      :no () ((limit 10)) ("artist > name" "track > name" "playcount" "listeners")))


    (geo
     (getTopArtists
      "Get the most popular artists on Last.fm by country."
      :no (country) ((limit 10) (page 1)) ("artist name" "playcount"))
     
     (getTopTracks
      "Get the most popular tracks on Last.fm last week by country."
      :no (country) ((limit 10) (page 1)) ("artist > name" "track > name" "playcount")))


    (library
     (getArtists
      "A list of all the artists in a user's library."
      :no () ((user lastfm--username) (limit 50) (page 1))
      ("artist name" "playcount" "tagcount")))


    (tag
     (getInfo
      "Get the metadata for a tag"
      :no (tag) () ("tag summary"))
     
     (getSimilar
      "Search for tags similar to this one, based on listening data."
      :no (tag) () ("tag name")) ;Doesn't return anything
     
     (getTopAlbums
      "Get the top albums tagged by this tag, ordered by tag count."
      :no (tag) ((limit 10) (page 1)) ("artist > name" "album > name"))
     
     (getTopArtists
      "Get the top artists tagged by this tag, ordered by tag count."
      :no (tag) ((limit 10) (page 1)) ("artist name"))
     
     (getTopTags
      "Fetches the top global tags on Last.fm, sorted by number of times used."
      :no () () ("tag name"))
     
     (getTopTracks
      "Get the top tracks tagged by this tag, ordered by tag count."
      :no (tag) ((limit 10) (page 1)) ("artist > name" "track > name")))
    

    (track
     (addTags
      "Tag an album using a list of user supplied tags."
      :yes (artist track tags) () ("lfm"))
     
     (getCorrection
      "Check whether the supplied track has a correction to a canonical track."
      :no (artist track) () ("artist > name" "track > name"))
     
     (getInfo
      "Get the track metadata."
      :no (artist track) () ("album title" "tag name" "playcount" "listeners"))
     
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
      :no (track) ((artist nil) (limit 10) (page 1)) ("track > artist" "track > name"))
     
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
      :no (user) ((recenttracks nil) (limit 10) (page 1))
      ("name" "realname" "country" "age" "gender" "subscriber" "playcount"))
     
     (getInfo
      "Get information about a user profile."
      :no () ((user lastfm--username))
      ("name" "realname" "country" "age" "gender" "subscriber" "playcount"))
     
     (getLovedTracks
      "Get the last LIMIT number of tracks loved by a user."
      :no  () ((user lastfm--username) (limit 10) (page 1))
      ("artist > name" "track > name"))
     
     (getPersonalTags
      "Get the user's personal tags"
      :no (tag taggingtype)
      ((user lastfm--username) (limit 10) (page 1)) ("artist name"))
     
     (getRecentTracks
      "Get a list of the recent tracks listened to by this user."
      :no () ((user lastfm--username) (limit 10) (page 1)
              (from nil) (to nil) (extended 0))
      ("track > artist" "track > name" "date"))
     
     (getTopAlbums
      "Get the top albums listened to by a user"
      :no () ((user lastfm--username) (period nil)
              (limit nil) (page nil))
      ("artist > name" "album > name" "playcount"))
     
     (getTopArtists
      "Get the top artists listened to by a user."
      :no () ((user lastfm--username) (period nil)
              (limit nil) (page nil))
      ("artist name" "playcount"))
     
     (getTopTags
      "Get the top tags used by this user."
      :no () ((user lastfm--username) (limit 10)) ("tag name"))
     
     (getTopTracks
      "Get the top tracks listened to by a user. "
      :no () ((user lastfm--username) (period nil)
              (limit nil) (page nil))
      ("artist > name" "track > name" "playcount"))
     
     (getWeeklyAlbumChart
      "Get an album chart for a user profile, for a given date range."
      :no () ((user lastfm--username) (from nil) (to nil))
      ("album > artist" "album > name" "playcount"))
     
     (getWeeklyArtistChart
      "Get an artist chart for a user profile, for a given date range."
      :no () ((user lastfm--username) (from nil) (to nil))
      ("artist > name" "playcount"))
     
     (getWeeklyTrackChart
      "Get a track chart for a user profile, for a given date range."
      :no () ((user lastfm--username) (from nil) (to nil))
      ("track > artist" "track > name" "playcount"))))
  "A user friendly list of all the supported lastfm methods.
A one liner like (artist-getinfo ...) or (track-love ...) is more
easier to parse, but this is easier for the eyes.  The latter, the
one-liner, is generated from this list and is the one actually
used for all the processing and generation of the user API.")

(defconst lastfm--methods
  (let (res)
  (dolist (group lastfm--methods-pretty)
    (dolist (method (cdr group))
      (push (cons (make-symbol
                   (concat (symbol-name (cl-first group)) "-"
                           (symbol-name (cl-first method))))
                  (cl-rest method))
            res)))
  (reverse res))
  "A computer friendly list of all lastfm methods.
This is a generated list of one-liner lastfm methods from the
user friendly list of methods.  Each entry in this list is a
complete lastm method specification.  It is used to generate the
complete API and documentation for this library.")

(defun lastfm--raw-method-name (method)
  "The unprocessed (raw) METHOD's name."
  (cl-first method))

(defun lastfm--method-name (method)
  "Return the METHOD's name used for defining the function name."
  (make-symbol
   (downcase (symbol-name (lastfm--raw-method-name method)))))

(defun lastfm--method-request-string (method)
  "Return the METHOD's name in the format requested by Last.fm.
This string is used as a parameter in the Last.fm request."
  (downcase
   (s-replace "-" "."
              (symbol-name (lastfm--method-name method)))))

(defun lastfm--method-url (method)
  "Return the METHOD's Last.fm documentation url."
  (concat "https://www.last.fm/api/show/"
          (s-replace "-" "."
                     (symbol-name (lastfm--raw-method-name method)))))

(defun lastfm--doc-string (method)
  "Return the METHOD's documentation string."
  (cl-second method))

(defun lastfm--auth-p (method)
  "Return t if this METHOD requires authentication."
  (eql (cl-third method) :yes))

(defun lastfm--sk-p (method)
  "Return t if this METHOD is used for requesting the session key."
  (eql (cl-third method) :sk))

(defun lastfm--memoizable-p (method)
  "Return t if the METHOD can be safely memoized."
  (not (or (lastfm--auth-p method)
           (lastfm--sk-p method))))

(defun lastfm--method-params (method)
  "Return a list of required METHOD's parameters."
  (cl-fourth method))

(defun lastfm--method-keyword-params (method)
  "Return a list of METHOD's key parameters."
  (cl-fifth method))

(defun lastfm--all-method-params (method)
  "Return a list of all the METHOD parameters."
  (append (lastfm--method-params method)
          (mapcar #'car (lastfm--method-keyword-params method))))

(defun lastfm--query-strings (method)
  "Return the METHOD's CSS selectors as a list of strings."
  (cl-sixth method))

(defun lastfm--group-params-for-signing (params)
  "Return all the PARAMS in one string.
The signing procedure for authentication needs all the parameters
and values lumped together in one big string without equal or
ampersand symbols between them."
  (let ((res ""))
    (mapc (lambda (s)
            (setf res (concat res (car s) (cdr s))))
          params)
    (concat res lastfm--shared-secret)))

(defun lastfm--build-params (method values)
  "Pair the METHOD's parameters with the supplied VALUES.
If the method is an authentication method, an api_sig is also
added with the corresponding signature."
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
      ;; Sign and append the signature.
      (alet (lastfm--group-params-for-signing result)
        (setq result (append result (list `("api_sig" . ,(md5 it)))))))
    result))

(cl-defun lastfm--request (method &rest values)
  "Make a Last.fm request and return the raw response.
Pair up the METHODS's parameters with the given values VALUES."
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
  "Use the QUERY-STRING to build a key usable in alists."
  (make-symbol
   (s-replace " " ""                    ;remove all extra spaces
              ;; Some queries contain '>' others only ' '. Replace both of them
              ;; with '-'.
              (if (s-contains-p ">" query-string)
                  (s-replace ">" "-" query-string)
                (s-replace " " "-" query-string)))))

(defun lastfm--parse-response (response method)
  "Extract the relevant data from the RESPONSE.
The METHOD holds the CSS selector strings."
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
  "Use the METHOD data to build a complete user function."
  (let* ((fn-name (intern (concat "lastfm-"
                                  (symbol-name (lastfm--method-name method)))))
         (params (lastfm--method-params method))
         (key-params (lastfm--method-keyword-params method))
         (signature `(,fn-name ,(if key-params ;Name and parameters
                                    `(,@params &key ,@key-params)
                                  `,@params)))
         (doc-string (concat (lastfm--doc-string method)
                             "\n \n"
                             "See the official Last.fm page for full documentation at"
                             "\nURL `"
                             (lastfm--method-url method)
                             "'")))

    ;; Generate markdown documentation for this method, if needed.
    (when lastfm-enable-doc-generation
      (insert
       (format "**[%s](%s)** %s\n\n    %s\n    => %s\n"
               (cl-first signature)        ;Function name.
               (lastfm--method-url method) ;Last.fm official doc for this method.
               (cadr signature)            ;Function parameters.
               (lastfm--doc-string method)
               ;; Return elements.
               (mapcar #'lastfm--key-from-query-str
                       (lastfm--query-strings method)))))

    ;; Build the function (this will be part of the API).
    `(progn
       (cl-defun ,@signature
           ,doc-string
         ;; Body.
         (lastfm--parse-response
          (lastfm--request ',method
                           ,@(if key-params
                                 `(,@params ,@(mapcar #'car key-params))
                               `,params))
          ',method))
       ;; Memoize the methods that return the same thing every time.
       ,(if (lastfm--memoizable-p method)
            `(condition-case nil
                 (memoize #',fn-name)
               ;; Memoizing a function a second time returns an error. Do
               ;; nothing in that case.
               (user-error nil))))))

(defun lastfm--local-file-path (name)
  "Return the NAME's absolute path."
  (concat (file-name-directory load-file-name)
          ;; For dev, (add-to-list 'load-path "~/.emacs.d/lisp/lastfm"), or
          ;; wherever the lastfm.el is located, so that (require 'lastfm) works
          ;; and the documentation is generated in the right folder.
          name))

(defmacro lastfm--build-api ()
  "Generate all the API functions and their documentation."
  (when lastfm-enable-doc-generation
    (with-temp-file (lastfm--local-file-path "README_api.md")
      `(progn
         ,@(--map (lastfm--build-function it)
                  lastfm--methods)))))

(defun lastfm--build-api-and-documentation ()
  "Build the API and the documentation md file for it."
  (lastfm--build-api)
  ;; Merge the generated API documentation with the handwritten one.
  (when lastfm-enable-doc-generation
    (with-temp-file (lastfm--local-file-path "README.md")
      (insert-file-contents (lastfm--local-file-path "README_api.md"))
      (insert-file-contents (lastfm--local-file-path "README_overview.md")))))

(lastfm--build-api-and-documentation)

(provide 'lastfm)

;;; lastfm.el ends here
