# jbo.el

GNU Emacs plugin to the Lojban dictionary [jbo](http://github.com/dag/jbo)

## Install Requirements

* jbo

    $ git clone http://github.com/dag/jbo.git ~/jbo && cd jbo
    $ sudo ./setup.py develop
    $ jbo index en.xml

* A recent version of GNU Emacs

## How to install

* Add the following lines to .emacs

    (add-to-list 'load-path "path/to/directory/with/jbo.el")
    (require 'jbo)

## How to use

* M-x jbo-lookup
