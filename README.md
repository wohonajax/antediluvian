# antediluvian

Torrent client in Common Lisp

## Getting started

First, you'll need the "magnet" library, which isn't on quicklisp yet.
To get it, do:
```
git clone https://codeberg.org/wohonajax/cl-magnet.git ~/quicklisp/local-projects/cl-magnet
```

Next, you'll need to clone this repository:
```
git clone https://codeberg.org/wohonajax/antediluvian.git ~/quicklisp/local-projects/antediluvian
```

Some dependencies aren't on the quicklisp dist. You'll need to add ultralisp like so:
```
(ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)
```

After that, simply quickload it:
```
(ql:quickload "antediluvian")
```

## Usage

```
(antediluvian:start "/path/to/file.torrent")
```

Note that this software isn't ready for primetime quite yet. In order to free
up sockets and shut down threads, you'll need to run
```
(antediluvian::cleanup)
```