;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; This is where you install packages, by declaring them with the `package!'
;; macro, then running 'doom refresh' on the command line. You'll need to
;; restart Emacs for your changes to take effect! Or at least, run M-x
;; `doom/reload'.
;;
;; WARNING: Don't disable core packages listed in ~/.emacs.d/core/packages.el.
;; Doom requires these, and disabling them may have terrible side effects.
;;
;; Here are a couple examples:

;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))


; INTERACTION STUFF
(package! evil-surround)
(package! buffer-move)
(package! reformatter)
; Shows keyboard macros or latest interactive commands as emacs lisp.
(package! elmacro)
; A GNU Emacs library to ensure environment variables inside Emacs look the same as in the user's shell.
(package! exec-path-from-shell)
; use sane regexp (PCRE) in emacs
(package! pcre2el)

; UI STUFF

(package! darktooth-theme)
(package! powerline)
(package! nlinum-relative)
(package! visual-regexp-steroids)
; extends Vline Mode https://www.emacswiki.org/emacs/VlineMode
(package! col-highlight)
;; (column-highlight-mode 0) ; 1 = highlighting, 0 = off
; TODO re column-highlight change the color
; list-faces-display
; shows you a col-highlight face that controls the color of the highlight column
; edit that face to change what it looks like.
(package! rainbow-mode)
(package! xterm-color)
(package! emojify)
; Pins the most recent method signature to the top of the window
; https://github.com/alphapapa/topsy.el#readme
(package! topsy
  :recipe (:host github :repo "alphapapa/topsy.el"))

; LANGUAGES

; HTML, JavaScript, CSS
(package! handlebars-mode)
(package! htmlize)
(package! multi-web-mode)
(package! polymode)
(package! sass-mode)
(package! web-mode)

; (package! rjsx-mode) # you prolly have this installed.
; type M-x rjs and you should see it autocompleting
;




; -- protobuffers
(package! protobuf-mode)

; -- Raku
(package! raku-mode)

; -- Ruby
(package! rspec-mode)
(package! ruby-hash-syntax)
(package! ruby-tools)

; -- Schemes
(package! geiser-chicken)
(package! racket-mode)
(package! scheme-complete)
(package! scribble-mode) ; racket documentation is "scribble"

; -- SQL
(package! sql-indent)

; -- Yaml
(package! yaml-mode)

; CURRENTLY UNUSED LANGUAGES
; -- Elixir
; -- Schemes
; (package! geiser-chicken)
;; (package! jinja2-mode) ; Jinja2 templates (python and chicken scheme)
; -- HTML & Friends
; (package! haml-mode)
; -- Lua
; (package! lua-mode)
; -- Janet
; (package! janet-mode)
; -- Schemes
; (package! fennel-mode)
; (package! geiser-chicken)
; geiser-chicken requires the following chicken libs to be installed
; chicken-install apropos chicken-doc srfi-18 srfi-1
; also installation of latest docs
; cd (csi -R chicken.platform -p '(chicken-home)')
; curl https://3e8.org/pub/chicken-doc/chicken-doc-repo-5.tgz | sudo tar zx




; -- Org-Mode
; pretty bullets
(package! org-bullets)
(package! ox-clip)
(package! denote)
(package! plantuml-mode)
(package! flycheck-plantuml)
(package! org-attach-screenshot)
(package! org-wc)
(package! toc-org) ; table of contents generator


; UTILITIES

; interactive align
(package! ialign)
; folding based on indentation
(package! yafolding)

; APP Support
(package! ox-hugo) ; hugo file format for org mode
;; (package! ox-slack)
(package! ox-slack
  :recipe (:host github :repo "masukomi/ox-slack" :branch "community"))
(package! ox-bb)
(package! ox-leanpub)
; ripgrep (rg)
(package! rg)
(package! sqlite3)
(package! mastodon)
(package! private-comments-mode)
; (package! example
;   :recipe (:host github :repo "masukomi/private-comments-mode"))
