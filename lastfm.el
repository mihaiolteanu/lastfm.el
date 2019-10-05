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

;;;; API methods definition.
(defun lastfm--api-fn-name (method-name)
  "Turn the METHOD-NAME into an API function name.
Example: artist.addTags -> lastfm-artist-add-tags"
  (intern
   (concat "lastfm-"
           (--reduce (concat acc "-" it)
                     (--map (downcase it)
                            (s-split-words (symbol-name method-name)))))))

(defmacro lastfm--defmethod (name params docstring auth query-strings)
  (declare (indent defun))
  (let ((fn-name (lastfm--api-fn-name name))
        (required-params (cl-remove-if #'consp params))
        (keyword-params (cl-remove-if #'atom params))
        (all-params (--map (if (consp it) (car it) it) params)))
    `(progn
       (cl-defun ,fn-name
           ,(if keyword-params
                `(,@required-params &key ,@keyword-params)
              `(,@required-params))
         ,docstring
         (lastfm--parse-response
          (lastfm--request ,(symbol-name name)
                           ,auth ',all-params ,@all-params)
          ',query-strings))
       (when (eq ,auth :no)
         (memoize #',fn-name)))))

(lastfm--defmethod album.getInfo (artist album)
  "Get the metadata and tracklist for an album on Last.fm using the album name."
  :no ("track > name"))

(lastfm--defmethod album.getTags (artist album)
  "Get the tags applied by an individual user to an album on Last.fm."
  :yes ("tag name" "tag count"))

(lastfm--defmethod album.getTopTags (artist album)
  "Get the top tags for an album on Last.fm, ordered by popularity."
  :no ("tag name" "tag count"))

(lastfm--defmethod album.removeTag (artist album tag)
  "Remove a user's tag from an album."
  :yes ("lfm"))

(lastfm--defmethod album.search (album (limit 10))
  "Search for an album by name. Returns album matches sorted by relevance."
  :no ("album artist" "album name"))

(lastfm--defmethod artist.addTags (artist tags)
  "Tag an artist with one or more user supplied tags."
  :yes ("lfm"))

(lastfm--defmethod artist.getCorrection (artist)
  "Check whether the artist has a correction to a canonical artist."
  :no ("artist name"))

(lastfm--defmethod artist.getInfo (artist)
  "Get the metadata for an artist. Includes biography, max 300 characters."
  :no ("bio summary" "listeners" "playcount"))

(lastfm--defmethod artist.getSimilar (artist (limit 10) (user lastfm--username))
  "Get all the artists similar to this artist."
  :no ("artist name"))

(lastfm--defmethod artist.getTags (artist)
  "Get the tags applied by an individual user to an artist on Last.fm."
  :yes ("tag name"))

(lastfm--defmethod artist.getTopAlbums (artist (limit 10))
  "Get the top albums for an artist, ordered by popularity."
  :no ("album > name"))

(lastfm--defmethod artist.getTopTags (artist)
  "Get the top tags for an artist, ordered by popularity."
  :no ("tag name"))

(lastfm--defmethod artist.getTopTracks (artist (limit 10) (page 1))
  "Get the top tracks by an artist, ordered by popularity."
  :no ("track > name" "playcount" "listeners"))

(lastfm--defmethod artist.removeTag (artist tag)
  "Remove a user's tag from an artist."
  :yes ("lfm"))

(lastfm--defmethod artist.search (artist (limit 10))
  "Search for an artist by name. Returns artist matches sorted by relevance."
  :no ("artist name"))

(lastfm--defmethod auth.getToken ()
  "Fetch a session key for a user (3rd step in the auth process)."
  :sk ("token"))

(lastfm--defmethod auth.getSession (token)
  "Fetch an unathorized request token (2nd step of the auth process)."
  :sk ("session key"))

(lastfm--defmethod chart.getTopArtists ((limit 10))
  "Get the top artists chart."
  :no ("artist name" "playcount" "listeners"))

(lastfm--defmethod chart.getTopTags ((limit 10))
  "Get the top tags chart."
  :no ("tag-name"))

(lastfm--defmethod chart.getTopTracks ((limit 10))
  "Get the top tracks chart."
  :no ("artist > name" "track > name" "playcount" "listeners"))

(lastfm--defmethod geo.getTopArtists (country (limit 10) (page 1))
  "Get the most popular artists on Last.fm by country."
  :no ("artist name" "playcount"))

(lastfm--defmethod geo.getTopTracks (country (limit 10) (page 1))
  "Get the most popular tracks on Last.fm last week by country."
  :no ("artist > name" "track > name" "playcount"))

(lastfm--defmethod library.getArtists ((user lastfm--username) (limit 50) (page 1))
  "A list of all the artists in a user's library."
  :no ("artist name" "playcount" "tagcount"))

(lastfm--defmethod tag.getInfo (tag)
  "Get the metadata for a TAG."
  :no ("tag summary"))

(lastfm--defmethod tag.getSimilar (tag)
  "Search for tags similar to this one, based on listening data."
  :no ("tag name"))

(lastfm--defmethod tag.getTopAlbums (tag (limit 10) (page 1))
  "Get the top albums tagged by this tag, ordered by tag count."
  :no ("artist > name" "album > name"))

(lastfm--defmethod tag.getTopArtists (tag (limit 10) (page 1))
  "Get the top artists tagged by this tag, ordered by tag count."
  :no ("artist name"))

(lastfm--defmethod tag.getTopTags ()
  "Fetches the top global tags on Last.fm, sorted by number of times used."
  :no ("tag name"))

(lastfm--defmethod tag.getTopTracks (tag (limit 10) (page 1))
  "Get the top tracks tagged by this tag, ordered by tag count."
  :no ("artist > name" "track > name"))

(lastfm--defmethod track.addTags (artist track tags)
  "Tag an album using a list of user supplied tags."
  :yes ("lfm"))

(lastfm--defmethod track.getCorrection (artist track)
  "Check whether the supplied track has a correction to a canonical track."
  :no ("artist > name" "track > name"))

(lastfm--defmethod track.getInfo (artist track)
  "Get the track metadata."
  :no ("album title" "tag name" "playcount" "listeners"))

(lastfm--defmethod track.getSimilar (artist track (limit 10))
  "Get similar tracks to this one, based on listening data."
  :no ("artist > name" "track > name"))

(lastfm--defmethod track.getTags (artist track)
  "Get the tags applied by an individual user to a track."
  :yes ("name"))

(lastfm--defmethod track.getTopTags (artist track)
  "Get the top tags for this track, ordered by tag count."
  :no ("name"))

(lastfm--defmethod track.love (artist track)
  "Love a track for a user profile."
  :yes ("lfm"))

(lastfm--defmethod track.removeTag (artist track tag)
  "Remove a user's tag from a track."
  :yes ("lfm"))

(lastfm--defmethod track.scrobble (artist track timestamp)
  "Add a track to the user listened tracks."
  :yes ("lfm"))

(lastfm--defmethod track.search (track (artist nil) (limit 10) (page 1))
  "Search for a track by track name. Returned matches are sorted by relevance."
  :no ("track > artist" "track > name"))

(lastfm--defmethod track.updateNowPlaying
  (artist track (album nil) (tracknumber nil) (context nil)
          (duration nil) (albumartist nil))
  "Notify Last.fm that a user has started listening to a track."
  :yes ("lfm"))

(lastfm--defmethod user.getfriends (user (limit 10) (page 1))
  "Get a list of the user's friends on Last.fm."
  :no ("name" "realname" "country" "age" "gender" "subscriber" "playcount"))

(lastfm--defmethod user.getInfo ((user lastfm--username))
  "Get information about a USER profile."
  :no ("name" "realname" "country" "age" "gender" "subscriber" "playcount"))

(lastfm--defmethod user.getLovedTracks ((user lastfm--username) (limit 10) (page 1))
  "Get the last LIMIT number of tracks loved by a USER."
  :no ("artist > name" "track > name"))

(lastfm--defmethod user.getPersonalTags
  (tag taggingtype (user lastfm--username) (limit 10) (page 1))
  "Get the user's personal TAGs."
  :no ("artist name"))

(lastfm--defmethod user.getRecentTracks
  ((user lastfm--username) (limit 10) (page 1) (from nil) (to nil) (extended 0))
  "Get a list of the recent tracks listened to by this user."
  :no ("track > artist" "track > name" "date"))

(lastfm--defmethod user.getTopAlbums
  ((user lastfm--username) (period nil) (limit nil) (page nil))
  "Get the top albums listened to by a user"
  :no ("artist > name" "album > name" "playcount"))

(lastfm--defmethod user.getTopArtists
  ((user lastfm--username) (period nil) (limit nil) (page nil))
  "Get the top artists listened to by a user."
  :no ("artist name" "playcount"))

(lastfm--defmethod user.getTopTags
  ((user lastfm--username) (limit 10))
  "Get the top tags used by this user."
  :no ("tag name"))

(lastfm--defmethod user.getTopTracks
  ((user lastfm--username) (period nil) (limit nil) (page nil))
  "Get the top tracks listened to by a user. "
  :no ("artist > name" "track > name" "playcount"))

(lastfm--defmethod user.getWeeklyAlbumChart
  ((user lastfm--username) (from nil) (to nil))
  "Get an album chart for a user profile, for a given date range."
  :no ("album > artist" "album > name" "playcount"))

(lastfm--defmethod user.getWeeklyArtistChart
  ((user lastfm--username) (from nil) (to nil))
  "Get an artist chart for a user profile, for a given date range."
  :no ("artist > name" "playcount"))

(lastfm--defmethod user.getWeeklyTrackChart
  ((user lastfm--username) (from nil) (to nil))
  "Get a track chart for a user profile, for a given date range."
  :no ("track > artist" "track > name" "playcount"))

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

(defun lastfm--build-params (method-name auth params values)
  (let ((result
         `(;; The api key and method is needed for all calls.
           ("api_key" . ,lastfm--api-key)
           ("method" . ,method-name)
           ;; Pair the user supplied values with the method parameters.  If no
           ;; value supplied for a given param, do not include it in the request.
           ,@(cl-remove-if #'null
                           (cl-mapcar (lambda (param value)
                                        (when value
                                          (cons (symbol-name param) value)))
                                      params
                                      values)))))
    (when (eq auth :sk)
      (push `("sk" . ,lastfm--sk) result))
    (when (or (eq auth :sk)
              (eq auth :yes))
      ;; Params need to be in alphabetical order before signing.
      (setq result (cl-sort result #'string-lessp
                            :key #'cl-first))
      ;; Sign and append the signature.
      (alet (lastfm--group-params-for-signing result)
        (setq result (append result (list `("api_sig" . ,(md5 it)))))))
    result))

(defun lastfm--request (method-name auth params &rest values)
  (let (resp)
    (request lastfm--url
             :params (lastfm--build-params method-name auth params values)
             :parser 'buffer-string
             :type   "POST"
             :sync   t
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

(defun lastfm--parse-response (response query-strings)
  "Extract the relevant data from the RESPONSE.
The METHOD holds the CSS selector strings."
  (let* ((raw-response (elquery-read-string response))
         ;; Only one error expected, if any.
         (error-str (elquery-text
                     (cl-first (elquery-$ "error" raw-response)))))
    (if error-str
        (error error-str)
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
               (apply #'-zip (helper query-strings)))))))))

(provide 'lastfm)

;; (add-to-list 'load-path "~/.emacs.d/lisp/lastfm")
;; (progn
;;   (unload-feature 'lastfm)
;;   (require 'lastm))

;;; lastfm.el ends here

