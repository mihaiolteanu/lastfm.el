# Overview
lastfm.el provides a complete interface to the Last.fm API as defined by [https://www.last.fm/api/](https://www.last.fm/api/).

An API account, obtainable for free from Last.fm, is needed to use the majority
of provided services. A one-time authentication process is needed to access the
rest of the methods.

Example usage to get the top three Duran Duran songs of all time:

```common-lisp
(lastfm-artist-gettoptracks "duran duran" :limit 3)
=> (((track-name . "Ordinary World")
     (playcount . "2299087")
     (listeners . "435772"))
    ((track-name . "Hungry Like the Wolf")
     (playcount . "1912264")
     (listeners . "427961"))
    ((track-name . "Come Undone")
     (playcount . "1743751")
     (listeners . "290926")))
```

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
Last.fm username. At this point, all the services that don't need authentication
work. For the extra services (love song, tag artist, etc.) the Secret Key (SK)
is also needed.

To obtain the SK, eval `(lastfm-generate-session-key)` in Emacs. Your browser
will then load the `http://www.last.fm/api/auth/?api_key=API-KEY` page and wait
for your permission to allow access to your account. Emacs will wait nicely
until you grant that access, with a "Did you grant the application persmission
to access your Last.fm account?" question. Confirm whatever the Last.fm page
asks you, then answer yes to Emacs. At this point, the SK is added to your
lastfmrc file and all the services are now working. Save the file and use it for
all eternity, never having to repeat this process ever again.

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
**[lastfm-album-addtags](https://www.last.fm/api/show/album.addTags)** (artist album tags)

    Tag an album using a list of user supplied tags.
    => (lfm)
**[lastfm-album-getinfo](https://www.last.fm/api/show/album.getInfo)** (artist album)

    Get the metadata and tracklist for an album on Last.fm using the album name.
    => (track-name)
**[lastfm-album-gettags](https://www.last.fm/api/show/album.getTags)** (artist album)

    Get the tags applied by an individual user to an album on Last.fm.
    => (tag-name tag-count)
**[lastfm-album-gettoptags](https://www.last.fm/api/show/album.getTopTags)** (artist album)

    Get the top tags for an album on Last.fm, ordered by popularity.
    => (tag-name tag-count)
**[lastfm-album-removetag](https://www.last.fm/api/show/album.removeTag)** (artist album tag)

    Remove a user's tag from an album.
    => (lfm)
**[lastfm-album-search](https://www.last.fm/api/show/album.search)** (album &key (limit 10))

    Search for an album by name. Returns album matches sorted by relevance.
    => (album-artist album-name)
**[lastfm-artist-addtags](https://www.last.fm/api/show/artist.addTags)** (artist tags)

    Tag an artist with one or more user supplied tags.
    => (lfm)
**[lastfm-artist-getcorrection](https://www.last.fm/api/show/artist.getCorrection)** (artist)

    Check whether the artist has a correction to a canonical artist.
    => (artist-name)
**[lastfm-artist-getinfo](https://www.last.fm/api/show/artist.getInfo)** (artist)

    Get the metadata for an artist. Includes biography, max 300 characters.
    => (bio-summary listeners playcount)
**[lastfm-artist-getsimilar](https://www.last.fm/api/show/artist.getSimilar)** (artist &key (limit lastfm--similar-limit) (user lastfm--username))

    Get all the artists similar to this artist.
    => (artist-name)
**[lastfm-artist-gettags](https://www.last.fm/api/show/artist.getTags)** (artist)

    Get the tags applied by an individual user to an artist on Last.fm.
    => (tag-name)
**[lastfm-artist-gettopalbums](https://www.last.fm/api/show/artist.getTopAlbums)** (artist &key (limit 10))

    Get the top albums for an artist, ordered by popularity.
    => (album-name)
**[lastfm-artist-gettoptags](https://www.last.fm/api/show/artist.getTopTags)** (artist)

    Get the top tags for an artist, ordered by popularity.
    => (tag-name)
**[lastfm-artist-gettoptracks](https://www.last.fm/api/show/artist.getTopTracks)** (artist &key (limit 10) (page 1))

    Get the top tracks by an artist, ordered by popularity.
    => (track-name playcount listeners)
**[lastfm-artist-removetag](https://www.last.fm/api/show/artist.removeTag)** (artist tag)

    Remove a user's tag from an artist.
    => (lfm)
**[lastfm-artist-search](https://www.last.fm/api/show/artist.search)** (artist &key (limit 10))

    Search for an artist by name. Returns artist matches sorted by relevance.
    => (artist-name)
**[lastfm-auth-gettoken](https://www.last.fm/api/show/auth.getToken)** nil

    Fetch a session key for a user (3rd step in the auth process).
    => (token)
**[lastfm-auth-getsession](https://www.last.fm/api/show/auth.getSession)** (token)

    Fetch an unathorized request token (2nd step of the auth process).
    => (session-key)
**[lastfm-chart-gettopartists](https://www.last.fm/api/show/chart.getTopArtists)** (&key (limit 10))

    Get the top artists chart.
    => (artist-name playcount listeners)
**[lastfm-chart-gettoptags](https://www.last.fm/api/show/chart.getTopTags)** (&key (limit 10))

    Get the top tags chart.
    => (tag-name)
**[lastfm-chart-gettoptracks](https://www.last.fm/api/show/chart.getTopTracks)** (&key (limit 10))

    Get the top tracks chart.
    => (artist-name track-name playcount listeners)
**[lastfm-geo-gettopartists](https://www.last.fm/api/show/geo.getTopArtists)** (country &key (limit 10) (page 1))

    Get the most popular artists on Last.fm by country.
    => (artist-name playcount)
**[lastfm-geo-gettoptracks](https://www.last.fm/api/show/geo.getTopTracks)** (country &key (limit 10) (page 1))

    Get the most popular tracks on Last.fm last week by country.
    => (artist-name track-name playcount)
**[lastfm-library-getartists](https://www.last.fm/api/show/library.getArtists)** (&key (user lastfm--username) (limit 50) (page 1))

    A list of all the artists in a user's library.
    => (artist-name playcount tagcount)
**[lastfm-tag-getinfo](https://www.last.fm/api/show/tag.getInfo)** (tag)

    Get the metadata for a tag
    => (tag-summary)
**[lastfm-tag-getsimilar](https://www.last.fm/api/show/tag.getSimilar)** (tag)

    Search for tags similar to this one, based on listening data.
    => (tag-name)
**[lastfm-tag-gettopalbums](https://www.last.fm/api/show/tag.getTopAlbums)** (tag &key (limit 10) (page 1))

    Get the top albums tagged by this tag, ordered by tag count.
    => (artist-name album-name)
**[lastfm-tag-gettopartists](https://www.last.fm/api/show/tag.getTopArtists)** (tag &key (limit 10) (page 1))

    Get the top artists tagged by this tag, ordered by tag count.
    => (artist-name)
**[lastfm-tag-gettoptags](https://www.last.fm/api/show/tag.getTopTags)** nil

    Fetches the top global tags on Last.fm, sorted by number of times used.
    => (tag-name)
**[lastfm-tag-gettoptracks](https://www.last.fm/api/show/tag.getTopTracks)** (tag &key (limit 10) (page 1))

    Get the top tracks tagged by this tag, ordered by tag count.
    => (artist-name track-name)
**[lastfm-track-addtags](https://www.last.fm/api/show/track.addTags)** (artist track tags)

    Tag an album using a list of user supplied tags.
    => (lfm)
**[lastfm-track-getcorrection](https://www.last.fm/api/show/track.getCorrection)** (artist track)

    Check whether the supplied track has a correction to a canonical track.
    => (artist-name track-name)
**[lastfm-track-getinfo](https://www.last.fm/api/show/track.getInfo)** (artist track)

    Get the track metadata.
    => (album-title tag-name playcount listeners)
**[lastfm-track-getsimilar](https://www.last.fm/api/show/track.getSimilar)** (artist track &key (limit 10))

    Get similar tracks to this one, based on listening data.
    => (artist-name track-name)
**[lastfm-track-gettags](https://www.last.fm/api/show/track.getTags)** (artist track)

    Get the tags applied by an individual user to a track.
    => (name)
**[lastfm-track-gettoptags](https://www.last.fm/api/show/track.getTopTags)** (artist track)

    Get the top tags for this track, ordered by tag count.
    => (name)
**[lastfm-track-love](https://www.last.fm/api/show/track.love)** (artist track)

    Love a track for a user profile.
    => (lfm)
**[lastfm-track-removetag](https://www.last.fm/api/show/track.removeTag)** (artist track tag)

    Remove a user's tag from a track.
    => (lfm)
**[lastfm-track-scrobble](https://www.last.fm/api/show/track.scrobble)** (artist track timestamp)

    Add a track to the user listened tracks.
    => (lfm)
**[lastfm-track-search](https://www.last.fm/api/show/track.search)** (track &key (artist nil) (limit 10) (page 1))

    Search for a track by track name. Returned matches are sorted by relevance.
    => (track-artist track-name)
**[lastfm-track-unlove](https://www.last.fm/api/show/track.unlove)** (artist track)

    UnLove a track for a user profile.
    => (lfm)
**[lastfm-track-updatenowplaying](https://www.last.fm/api/show/track.updateNowPlaying)** (artist track &key (album nil) (tracknumber nil) (context nil) (duration nil) (albumartist nil))

    Notify Last.fm that a user has started listening to a track.
    => (lfm)
**[lastfm-user-getfriends](https://www.last.fm/api/show/user.getfriends)** (user &key (recenttracks nil) (limit 10) (page 1))

    Get a list of the user's friends on Last.fm.
    => (name realname country age gender subscriber playcount)
**[lastfm-user-getinfo](https://www.last.fm/api/show/user.getInfo)** (&key (user lastfm--username))

    Get information about a user profile.
    => (name realname country age gender subscriber playcount)
**[lastfm-user-getlovedtracks](https://www.last.fm/api/show/user.getLovedTracks)** (&key (user lastfm--username) (limit 10) (page 1))

    Get the last LIMIT number of tracks loved by a user.
    => (artist-name track-name)
**[lastfm-user-getpersonaltags](https://www.last.fm/api/show/user.getPersonalTags)** (tag taggingtype &key (user lastfm--username) (limit 10) (page 1))

    Get the user's personal tags
    => (artist-name)
**[lastfm-user-getrecenttracks](https://www.last.fm/api/show/user.getRecentTracks)** (&key (user lastfm--username) (limit 10) (page 1) (from nil) (to nil) (extended 0))

    Get a list of the recent tracks listened to by this user.
    => (track-artist track-name date)
**[lastfm-user-gettopalbums](https://www.last.fm/api/show/user.getTopAlbums)** (&key (user lastfm--username) (period nil) (limit nil) (page nil))

    Get the top albums listened to by a user
    => (artist-name album-name playcount)
**[lastfm-user-gettopartists](https://www.last.fm/api/show/user.getTopArtists)** (&key (user lastfm--username) (period nil) (limit nil) (page nil))

    Get the top artists listened to by a user.
    => (artist-name playcount)
**[lastfm-user-gettoptags](https://www.last.fm/api/show/user.getTopTags)** (&key (user lastfm--username) (limit 10))

    Get the top tags used by this user.
    => (tag-name)
**[lastfm-user-gettoptracks](https://www.last.fm/api/show/user.getTopTracks)** (&key (user lastfm--username) (period nil) (limit nil) (page nil))

    Get the top tracks listened to by a user. 
    => (artist-name track-name playcount)
**[lastfm-user-getweeklyalbumchart](https://www.last.fm/api/show/user.getWeeklyAlbumChart)** (&key (user lastfm--username) (from nil) (to nil))

    Get an album chart for a user profile, for a given date range.
    => (album-artist album-name playcount)
**[lastfm-user-getweeklyartistchart](https://www.last.fm/api/show/user.getWeeklyArtistChart)** (&key (user lastfm--username) (from nil) (to nil))

    Get an artist chart for a user profile, for a given date range.
    => (artist-name playcount)
**[lastfm-user-getweeklytrackchart](https://www.last.fm/api/show/user.getWeeklyTrackChart)** (&key (user lastfm--username) (from nil) (to nil))

    Get a track chart for a user profile, for a given date range.
    => (track-artist track-name playcount)
