;;; jbo.el --- jbovlaste integration for Emacs using jbo
;;;            Requires jbo, available at http://github.com/dag/jbo
;;;            Available for Unix-like systems only at this time
;;
;; Copyright (C) 2010
;;
;; Author:     2010 Michael Moorman
;; Created:    October 2010
;; Version:    0.0.1 (October 2010)
;; Keywords:   Lojban dictionary language emacs jbo
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;; Installing:
;; Install jbo using git:
;;
;;    $ git clone http://github.com/dag/jbo.git ~/jbo && cd jbo
;;    $ sudo ./setup.py develop
;;    $ jbo index
;;
;; Add the following lines to your ~/.emacs file:
;;
;;    (add-to-list 'load-path "<path to directory containing jbo.el>")
;;    (require 'jbo)
;;
;; TODO:
;;   * Add support for indexing automatically through emacs
;;   * Add filter support
;;
;;
;; History:
;;   * 2010 October - First 0.0.1 release, with support for basic lookups,
;;     based on the git version of jbo.
;;
;;----------------------------------------------------------------------------
;; Code:

(provide 'jbo)

;;; Front-end functions

(defun jbo-lookup (name)
  "Prompt user for a valsi to look up, then returns the definition in the minibuffer *jbo*."
  (interactive "sValsi to look up: ")
  (when (or (not (stringp name))
	    (string-equal "" name))
    (error "Need to specify a word."))
  (jbo-lookup-definition name))

; might need reworking to handle hyphens and other things which English words 
; have, but Lojban does not consider a word.
(defun jbo-lookup-at-point ()
  "Lookup valsi under point."
  (interactive)
  (jbo-lookup-definition (current-word)))

(defun jbo-apropos-word (word)
  "Prompt user for word, then filter."
  (interactive "sFilter by word: ")
  (when (or (not (stringp word))
	    (string-equal "" name))
    (error "Need to specify a word."))
  (jbo-lookup-matches))

(defun jbo-lookup-definition (valsi)
  (let ((entry (jbo-get-definition valsi)))
    (if (not (string= "" entry))
	(let ((parsed-text (jbo-prettifier entry)))
	  (jbo-get-buffer-create)
	  (jbo-display-definition (ring-insert jbo-history parsed-text))))))

(defun jbo-get-buffer-create ()
  "If *jbo* buffer exists, pop to it, otherwise initialize it then pop to it."
  (let ((jbuff (get-buffer "*jbo*")))
    (if (not jbuff)
	(progn (pop-to-buffer (get-buffer-create "*jbo*"))
	       (jbo-dict-mode))
      (pop-to-buffer jbuff))))

(defun jbo-previous-definition ()
  "Return to the previous definition in the history. Wraps around."
  (interactive)
  (jbo-get-buffer-create)
  (ring-insert-at-beginning jbo-history (ring-remove jbo-history 0))
  (jbo-display-definition (ring-ref jbo-history 0)))

(defun jbo-next-definition ()
  "Return to next definition in the history. Wraps around."
  (interactive)
  (jbo-get-buffer-create)
  (ring-insert jbo-history (ring-remove jbo-history))
  (jbo-display-definition (ring-ref jbo-history 0)))

(defun jbo-display-definition (definition)
  "Puts definition in *jbo* and resizes to fit."
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert definition)
  (fit-window-to-buffer)
  (setq buffer-read-only t))

;;; The prettifier!
(defun jbo-prettifier (entry)
  "Prettifies the jbo dictionary entry by replicating the original bold formatting and by inserting cross-references."
  (let* ((index 0)
	 (word-regex "^[a-zA-Z']+")
	 (link-regex "{[a-zA-Z']+}")
	 (text (replace-regexp-in-string word-regex 'jbo-boldifier entry)))
    (replace-regexp-in-string link-regex 'jbo-linkifier text)))

(defun jbo-boldifier (word)
  "Add bold text properties to word"
  (propertize word 'face 'bold))

(defun jbo-linkifier (word)
  "Add link properties to links, so that you can cross-reference them."
  (propertize (substring word 1 -1)
	      'face 'link
	      'help-echo "RET: follow link"
	      'keymap jbo-link-keymap))

(defvar jbo-link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'jbo-lookup-at-point)
    map)
  "Link keymap")

;;; jbo querying functions

(defun jbo-get-definition (valsi)
  "Query the jbo database for the definition of VALSI. If the definition exists return it, otherwise yield an error."
  (let* ((command-string (concat "jbo define \"" valsi "\""))
	(result (shell-command-to-string* command-string)))
    (if (string-prefix-p "error:" result)
	(error (concat valsi ": not found"))
      result)))

(defun jbo-filter-word (word)
  "Query the jbo database, and filter entries which contain WORD, returning a list of corresponding valsi."
  (let* ((command-string (concat "jbo filter \"" word "\""))
	 (result (shell-command-to-string* command-string)))
    (if (string= "" result)
	(error (concat word ": no matches"))
      result)))

(defun shell-command-to-string* (command-string)
  "Cut off that damn newline at the end"
  (substring (shell-command-to-string command-string) 0 -1))

;;; jbo-dict-mode information

(defvar jbo-dict-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "q") 'delete-window)
      (define-key map (kbd "b") 'jbo-previous-definition)
      (define-key map (kbd "f") 'jbo-next-definition)
      (define-key map (kbd "l") 'jbo-lookup)
      map)
    "Keymap for jbo-dict-mode")

(defvar jbo-max-history 100
  "Maximum number of dictionary entries to remember")

(defun jbo-dict-mode ()
  "Major mode for viewing dictionary definitions from jbovlaste, the official Lojban dictionary. 
Commands:
\\{jbo-dict-mode-map}
"
  (kill-all-local-variables)
  (use-local-map jbo-dict-mode-map)
  (setq mode-name "jbo")
  (setq major-mode 'jbo-dict-mode)
  (setq buffer-read-only t)
  (make-local-variable 'jbo-history)
  (setq jbo-history (make-ring jbo-max-history))
  (run-hooks 'jbo-mode-hook))
