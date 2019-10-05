# Overview
lastfm.el provides a complete interface to the Last.fm API as defined by [https://www.last.fm/api/](https://www.last.fm/api/).

An API account, obtainable for free from Last.fm, is needed to use the majority
of provided services. A one-time authentication process is needed to access the
rest of the methods.

Example usage to get the top tracks tagged as "rock" from last.fm, based on
user preferences,

```common-lisp
(lastfm-tag-get-top-tracks "rock" :limit 3)

=> (((artist-name . "Nirvana")     (track-name . "Smells Like Teen Spirit"))
    ((artist-name . "The Killers") (track-name . "Mr Brightside"))
    ((artist-name . "Oasis")       (track-name . "Wonderwall")))
```
or to add a track to your list of loved songs,

```common-lisp
(lastfm-track-love "anathema" "springfield")
=> (((lfm . "")))
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
