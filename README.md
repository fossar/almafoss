álmafoss
===========

This is an alternative front end for [selfoss](https://github.com/SSilence/selfoss) rss reader written in [Elm](http://elm-lang.org/).

Installation
------------

1. Extract the [archive](https://github.com/fossar/almafoss/releases) to public directory of web server of your choice.

Set-up
------

1. Update `host` field in the `index.html` to match your selfoss installation.
2. If the selfoss installation is on different host, enable [remote access](https://developer.mozilla.org/en-US/docs/Web/HTTP/Access_control_CORS#Access-Control-Allow-Origin) to selfoss by adding `Header set Access-Control-Allow-Origin "https://almafoss-host.example.org"` to selfoss’s `.htaccess` file.

Development
-----------

Install [Elm Platform](http://guide.elm-lang.org/install.html), [elm-github-install](https://github.com/gdotdesign/elm-github-install) and [elm-live](https://github.com/tomekwi/elm-live). Set-up almafoss as described above. Then you can issue `make watch` to start a testing server.
