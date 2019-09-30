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
