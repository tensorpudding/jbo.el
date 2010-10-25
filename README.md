# jbo.el

GNU Emacs plugin to the Lojban dictionary [jbo](http://github.com/dag/jbo)

## Install Requirements

* jbo:

    $ git clone http://github.com/dag/jbo.git ~/jbo && cd jbo

    $ sudo ./setup.py develop

    $ jbo index en.xml

* A recent version of GNU Emacs

## How to install

* Add the following lines to .emacs:

    (add-to-list 'load-path "path/to/directory/with/jbo.el")

    (require 'jbo)

## How to use

* Try doing <tt>M-x jbo-lookup</tt> to enter a Lojban valsi.

* You can add this other line to .emacs:

    (global-set-key (kbd "C-c M-l") 'jbo-lookup-at-point)
    
  Now you can lookup words by hovering the cursor over them, and pressing that key combination.
