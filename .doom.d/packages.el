;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)


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
(package! avy)

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

; -- protobuffers
(package! protobuf-mode)

; -- Raku
(package! raku-mode)

; -- Ruby
(package! rspec-mode)
(package! ruby-hash-syntax)
(package! ruby-tools) ; https://github.com/rejeep/ruby-tools.el

; -- Schemes
; (package! geiser-chicken) # FORBIDDEN - EVIL BREAKS EVERYTHING
(package! racket-mode)
(package! scheme-complete)
(package! scribble-mode) ; racket documentation is "scribble"

; -- SQL
(package! sql-indent)

; -- Yaml
(package! yaml-mode)


; -- Org-Mode
; pretty bullets
(package! org-bullets)
; in config: (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
; alternative org-superstar adds fancyness to unordered lists
; (package! org-superstar)
; in config: (add-hook 'org-mode-hook (lambda () (org-superstart-mode 1)))
(package! evil-org)
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
  :recipe (:host github :repo "masukomi/ox-slack" :branch "main"))
(package! ox-bb)
(package! ox-leanpub)
; ripgrep (rg)
(package! rg)
(package! sqlite3)
; (package! mastodon)
(package! private-comments-mode)
; (package! example
;   :recipe (:host github :repo "masukomi/private-comments-mode"))
