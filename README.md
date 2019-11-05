# Overview
lastfm.el provides a complete interface to the Last.fm API as defined by [https://www.last.fm/api/](https://www.last.fm/api/).

An API account, obtainable for free from Last.fm, is needed to use the majority
of provided services. A one-time authentication process is needed to access the
rest of the methods.

Example usage to get the top tracks tagged as "rock" from last.fm, based on
user preferences,

```common-lisp
(lastfm-tag-get-top-tracks "rock" :limit 3)
=> (("Nirvana" "Smells Like Teen Spirit")
    ("The Killers" "Mr Brightside")
    ("Oasis" "Wonderwall"))
```
Or add a track to your list of loved songs,

```common-lisp
(lastfm-track-love "anathema" "springfield")
=> ("")
```

See the API below for the complete functionality.

# Install
Follow the instructions on
[https://www.last.fm/api/](https://www.last.fm/api/'.) to obtain an API key and
a Shared Secret.

lastfm.el keeps a config file, `.lastfmrc` in your `xdg-config-home` folder
(usually `~/.config`). All the info needed to make calls to last.fm are kept here.

On the first load, if the file does not exist, it is created,

```common-lisp
;; .lastfmrc
(CONFIG
 :API-KEY ""
 :SHARED-SECRET ""
 :USERNAME "")
```

Fill up the API-KEY and the SHARED-SECRET received from Last.fm, as well as your
Last.fm USERNAME. At this point, all the services that don't need authentication
work. For the extra services (love song, tag artist, etc.) the Secret Key (SK)
is also needed.

To obtain the SK, eval `(lastfm-generate-session-key)` in Emacs. Your browser
will load the Last.fm page that will beg you to "Yes, Allow Access" to your
application. Emacs will wait nicely until you grant that access, with a "Did you
grant the application persmission to access your Last.fm account?"
question. Grant access to Last.fm, make sure Lastm.fm displays the "Application
Authenticated" message and then answer yes to Emacs. At this point, the SK is
added to your lastfmrc file and all the services are now working. Save the file
and use it for all eternity, never having to repeat this process ever again.

# A Few Details
Most of the functions are memoized. Calling the same function with the same
arguments won't make a call to Last.fm, except on the first call. This speeds
things up a bit.

Most of the functions return a list with multiple entries. Each entry is again a
list of conses. The `car` of this cons is the attribute returned and the `cdr`
is the value. In the documentation string this appears as `=> (track-name
playcount listeners)`, for example, which means the function will return a list,
where each element is a list of conses with `track-name`, `playcount` and
`listeners` as the `car`s. See the "duran duran" example in the overview
section.

# Complete API
**lastfm-album-get-info** (artist album)

    Get the metadata and tracklist for an album on Last.fm using the album name.
    => (track-name)

**lastfm-album-get-tags** (artist album)

    Get the tags applied by an individual user to an album on Last.fm.
    => (tag-name tag-count)

**lastfm-album-get-top-tags** (artist album)

    Get the top tags for an album on Last.fm, ordered by popularity.
    => (tag-name tag-count)

**lastfm-album-remove-tag** (artist album tag)

    Remove a user's tag from an album.
    => (lfm)

**lastfm-album-search** (album &key (limit 10))

    Search for an album by name. Returns album matches sorted by relevance.
    => (album-artist album-name)

**lastfm-artist-add-tags** (artist tags)

    Tag an artist with one or more user supplied tags.
    => (lfm)

**lastfm-artist-get-correction** (artist)

    Check whether the artist has a correction to a canonical artist.
    => (artist-name)

**lastfm-artist-get-info** (artist)

    Get the metadata for an artist. Includes biography, max 300 characters.
    => (bio-summary listeners playcount)

**lastfm-artist-get-similar** (artist &key (limit 10) (user lastfm--username))

    Get all the artists similar to this artist.
    => (artist-name)

**lastfm-artist-get-tags** (artist)

    Get the tags applied by an individual user to an artist on Last.fm.
    => (tag-name)

**lastfm-artist-get-top-albums** (artist &key (limit 10))

    Get the top albums for an artist, ordered by popularity.
    => (album-name)

**lastfm-artist-get-top-tags** (artist)

    Get the top tags for an artist, ordered by popularity.
    => (tag-name)

**lastfm-artist-get-top-tracks** (artist &key (limit 10) (page 1))

    Get the top tracks by an artist, ordered by popularity.
    => (track-name playcount listeners)

**lastfm-artist-remove-tag** (artist tag)

    Remove a user's tag from an artist.
    => (lfm)

**lastfm-artist-search** (artist &key (limit 10))

    Search for an artist by name. Returns artist matches sorted by relevance.
    => (artist-name)

**lastfm-auth-get-token** nil

    Fetch a session key for a user (3rd step in the auth process).
    => (token)

**lastfm-auth-get-session** (token)

    Fetch an unathorized request token (2nd step of the auth process).
    => (session-key)

**lastfm-chart-get-top-artists** (&key (limit 10))

    Get the top artists chart.
    => (artist-name playcount listeners)

**lastfm-chart-get-top-tags** (&key (limit 10))

    Get the top tags chart.
    => (tag-name)

**lastfm-chart-get-top-tracks** (&key (limit 10))

    Get the top tracks chart.
    => (artist-name track-name playcount listeners)

**lastfm-geo-get-top-artists** (country &key (limit 10) (page 1))

    Get the most popular artists on Last.fm by country.
    => (artist-name playcount)

**lastfm-geo-get-top-tracks** (country &key (limit 10) (page 1))

    Get the most popular tracks on Last.fm last week by country.
    => (artist-name track-name playcount)

**lastfm-library-get-artists** (&key (user lastfm--username) (limit 50) (page 1))

    A list of all the artists in a user's library.
    => (artist-name playcount tagcount)

**lastfm-tag-get-info** (tag)

    Get the metadata for a TAG.
    => (tag-summary)

**lastfm-tag-get-similar** (tag)

    Search for tags similar to this one, based on listening data.
    => (tag-name)

**lastfm-tag-get-top-albums** (tag &key (limit 10) (page 1))

    Get the top albums tagged by this tag, ordered by tag count.
    => (artist-name album-name)

**lastfm-tag-get-top-artists** (tag &key (limit 10) (page 1))

    Get the top artists tagged by this tag, ordered by tag count.
    => (artist-name)

**lastfm-tag-get-top-tags** nil

    Fetches the top global tags on Last.fm, sorted by number of times used.
    => (tag-name)

**lastfm-tag-get-top-tracks** (tag &key (limit 10) (page 1))

    Get the top tracks tagged by this tag, ordered by tag count.
    => (artist-name track-name)

**lastfm-track-add-tags** (artist track tags)

    Tag an album using a list of user supplied tags.
    => (lfm)

**lastfm-track-get-correction** (artist track)

    Check whether the supplied track has a correction to a canonical track.
    => (artist-name track-name)

**lastfm-track-get-info** (artist track)

    Get the track metadata.
    => (album-title tag-name playcount listeners)

**lastfm-track-get-similar** (artist track &key (limit 10))

    Get similar tracks to this one, based on listening data.
    => (artist-name track-name)

**lastfm-track-get-tags** (artist track)

    Get the tags applied by an individual user to a track.
    => (name)

**lastfm-track-get-top-tags** (artist track)

    Get the top tags for this track, ordered by tag count.
    => (name)

**lastfm-track-love** (artist track)

    Love a track for a user profile.
    => (lfm)

**lastfm-track-remove-tag** (artist track tag)

    Remove a user's tag from a track.
    => (lfm)

**lastfm-track-scrobble** (artist track timestamp)

    Add a track to the user listened tracks.
    => (lfm)

**lastfm-track-search** (track &key (artist nil) (limit 10) (page 1))

    Search for a track by track name. Returned matches are sorted by relevance.
    => (track-artist track-name)

**lastfm-track-unlove** (artist track)

    UnLove a track for a user profile.
    => (lfm)

**lastfm-track-update-now-playing** (artist track &key (album nil) (tracknumber nil) (context nil) (duration nil) (albumartist nil))

    Notify Last.fm that a user has started listening to a track.
    => (lfm)

**lastfm-user-getfriends** (user &key (limit 10) (page 1))

    Get a list of the user's friends on Last.fm.
    => (name realname country age gender subscriber playcount)

**lastfm-user-get-info** (&key (user lastfm--username))

    Get information about a USER profile.
    => (name realname country age gender subscriber playcount)

**lastfm-user-get-loved-tracks** (&key (user lastfm--username) (limit 10) (page 1))

    Get the last LIMIT number of tracks loved by a USER.
    => (artist-name track-name)

**lastfm-user-get-personal-tags** (tag taggingtype &key (user lastfm--username) (limit 10) (page 1))

    Get the user's personal TAGs.
    => (artist-name)

**lastfm-user-get-recent-tracks** (&key (user lastfm--username) (limit 10) (page 1) (from nil) (to nil) (extended 0))

    Get a list of the recent tracks listened to by this user.
    => (track-artist track-name date)

**lastfm-user-get-top-albums** (&key (user lastfm--username) (period nil) (limit nil) (page nil))

    Get the top albums listened to by a user
    => (artist-name album-name playcount)

**lastfm-user-get-top-artists** (&key (user lastfm--username) (period nil) (limit nil) (page nil))

    Get the top artists listened to by a user.
    => (artist-name playcount)

**lastfm-user-get-top-tags** (&key (user lastfm--username) (limit 10))

    Get the top tags used by this user.
    => (tag-name)

**lastfm-user-get-top-tracks** (&key (user lastfm--username) (period nil) (limit nil) (page nil))

    Get the top tracks listened to by a user. 
    => (artist-name track-name playcount)

**lastfm-user-get-weekly-album-chart** (&key (user lastfm--username) (from nil) (to nil))

    Get an album chart for a user profile, for a given date range.
    => (album-artist album-name playcount)

**lastfm-user-get-weekly-artist-chart** (&key (user lastfm--username) (from nil) (to nil))

    Get an artist chart for a user profile, for a given date range.
    => (artist-name playcount)

**lastfm-user-get-weekly-track-chart** (&key (user lastfm--username) (from nil) (to nil))

    Get a track chart for a user profile, for a given date range.
    => (track-artist track-name playcount)

