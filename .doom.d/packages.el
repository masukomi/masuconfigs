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


(package! buffer-move)
(package! darktooth-theme)
(package! powerline)
(package! moe-theme)
(package! ialign)
(package! nlinum-relative)
(package! ruby-hash-syntax)
(package! ruby-tools)
(package! visual-regexp-steroids)
; extends Vline Mode https://www.emacswiki.org/emacs/VlineMode
(package! col-highlight)
;; (column-highlight-mode 0) ; 1 = highlighting, 0 = off
; TODO re column-highlight change the color
; list-faces-display
; shows you a col-highlight face that controls the color of the highlight column
; edit that face to change what it looks like.

(package! sql-indent)
(package! web-mode)
(package! rainbow-mode)
(package! polymode)
(package! org-bullets)
; (package! alchemist) ; elixir support
(package! ox-hugo) ; hugo file format for org mode

; USE THIS FOR LSP vvv
; (package! lsp-mode)

;(package! haml-mode)
(package! sass-mode)
(package! reformatter)
(package! evil-surround)
(package! yaml-mode)
; (package! editorconfig-mode)
; geiser-chicken requires the following chicken libs to be installed
; chicken-install apropos chicken-doc srfi-18 srfi-1
; also installation of latest docs
; cd (csi -R chicken.platform -p '(chicken-home)')
; curl https://3e8.org/pub/chicken-doc/chicken-doc-repo-5.tgz | sudo tar zx
(package! geiser-chicken)
(package! scheme-complete)
;; (package! rubocop)
(package! rspec-mode)
(package! protobuf-mode)
(package! ox-clip)
(package! ox-slack)
(package! ox-leanpub)
(package! elmacro)
;; (unpin! org-roam) ; Danger Will Robinson!
;; (package! org-roam)
;; (package! org-roam-ui) ; this likes danger
(package! denote)

;; (package! jinja2-mode) ; Jinja2 templates (python and chicken scheme)
(package! handlebars-mode)
(package! multi-web-mode)

(package! plantuml-mode)
(package! flycheck-plantuml)
(package! racket-mode)
(package! scribble-mode) ; racket documentation is "scribble"
(package! exec-path-from-shell)
(package! sqlite3)
(package! htmlize)
(package! org-attach-screenshot)
(package! org-wc)
;; (package! undo-tree)

; (package! private-comments-mode)
; (package! example
;   :recipe (:host github :repo "masukomi/private-comments-mode"))
;; (package! textmate)
(package! mastodon)
(package! emojify)
(package! janet-mode)
;; (package! lua-mode)
;; (package! fennel-mode)
(package! xterm-color)
(package! ialign)
(package! pcre2el)
(package! raku-mode)
