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
; (package! avy) it's installed by default so no need for it here
;                check out the gs prefix for many doom specific avy jump things
(package! evil-surround)
(package! expand-region)
(package! buffer-move)
(package! reformatter)
; Shows keyboard macros or latest interactive commands as emacs lisp.
(package! elmacro)
; A GNU Emacs library to ensure environment variables inside Emacs look the same as in the user's shell.
(package! exec-path-from-shell)
; use sane regexp (PCRE) in emacs
(package! pcre2el)

; UI STUFF

; treemacs-icons-dired gives icons by each file in dired
(package! treemacs-icons-dired)

; extends Vline Mode https://www.emacswiki.org/emacs/VlineMode
(package! col-highlight)
;; (column-highlight-mode 0) ; 1 = highlighting, 0 = off
; TODO re column-highlight change the color
; list-faces-display
; shows you a col-highlight face that controls the color of the highlight column
; edit that face to change what it looks like.

(package! darktooth-theme)
;; (package! emojify)
;; (package! minimap)
(package! nlinum-relative)
; extends org-mode's clickable link support to other modes
; the built-in version is goto-address mode which works well,
; but doesn't support as many link types
(package! orglink)
(package! powerline)
; Pins the most recent method signature to the top of the window
; https://github.com/alphapapa/topsy.el#readme
(package! topsy
  :recipe (:host github :repo "alphapapa/topsy.el"))
(package! rainbow-mode)
;; (package! visual-regexp-steroids)
(package! xterm-color)
; (package! git-gutter)
;; (package! git-gutter-fringe)


; LANGUAGES
; -- Crystal
(package! crystal-mode)

; -- Docker
(package! dockerfile-mode)

; -- Elisp
(package! elisp-autofmt)

; -- Fennel
; https://github.com/emacsmirror/fennel-mode#readme
(package! fennel-mode)
; LSP language server for fennel
(package! fennel-ls
  :recipe (:host sourcehut :repo "xerool/fennel-ls"))

; -- Fish shell
(package! fish-mode)

; -- HTML, JavaScript, CSS
(package! handlebars-mode)
(package! htmlize)
(package! multi-web-mode)
(package! polymode)
(package! sass-mode)
(package! web-mode)
(package! typescript-mode)
(package! tt-mode) ; Perl Template Toolkit & Raku Template6
(package! json-reformat)

(package! erb-mode
  :recipe (:host github :repo "qdzhang/erb-mode"))

; -- Language Server Protocol (lsp) stuff
(package! lsp-mode) ; https://emacs-lsp.github.io/lsp-mode/
(package! lsp-ui) ; https://emacs-lsp.github.io/lsp-ui/

; -- Lua
(package! lua-mode)

; -- protobuffers
(package! protobuf-mode)

; -- Raku
(package! raku-mode)
(package! ob-raku)

; -- Ruby
(package! rubocop :disable t) ; arg fuck off rubocop!
(package! rspec-mode)
;; (package! ruby-hash-syntax)
;; (package! ruby-tools) ; https://github.com/rejeep/ruby-tools.el

; -- Rust
(package! rust-mode)

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
; newer version of org mode broke searching in folder sections
; Please revisit this ticket and see if it's changed
; https://github.com/doomemacs/doomemacs/issues/6478
; last revisited by me on Aug 29, 2023.
; last update to ticket was Jan 27, 2023
;
(package! org-mode) ; :pin "971eb6885ec996c923e955730df3bafbdc244e54")

; pretty bullets
;; (package! org-bullets)
; in config: (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
; alternative org-superstar adds fancyness to unordered lists
; (package! org-superstar)
; in config: (add-hook 'org-mode-hook (lambda () (org-superstart-mode 1)))

; Make invisible parts of Org elements appear visible. Usually emphasis markers.
; See https://github.com/awth13/org-appear for docs.
(package! org-appear
  :recipe (:host github
           :repo "awth13/org-appear"))
(package! evil-org)
(package! org-hide-drawers)
(package! org-modern)
(package! ox-clip)
(package! ox-gfm)
; ox-pandoc has a lot of configuration stuff
; https://github.com/kawabata/ox-pandoc
(package! ox-pandoc)
(package! denote)
(package! plantuml-mode)
; (package! flycheck-plantuml)
(package! org-attach-screenshot)
(package! org-wc)
(package! wc-mode)
(package! toc-org) ; table of contents generator

; UTILITIES

; dtrt-indent: An Emacs minor mode that guesses the indentation offset
; originally used for creating source code files and transparently adjusts the
; corresponding settings in Emacs, making it more convenient to edit foreign
; files.
(package! dtrt-indent)
; interactive align
(package! ialign)
; folding based on indentation
(package! yafolding)
; tailing files only better
; usage: M-x itail RET /file/to/tail
; customization: M-x customize RET (search for itail)
;; key        binding
;; ---        -------
;; C-c -      itail-remove-last-filter
;; C-c c      itail-clear
;; C-c f      itail-toggle-filter
;; C-c g      itail-add-grep
;; C-c h      itail-highlight
;; C-c r      itail-remove-all-filters
;; C-c s      itail-show-filters
;; C-c u      itail-unhighlight
;; C-c C-k    itail-reload
;; C-c C-c    itail-kill
(package! itail)

; APP Support
(package! ox-hugo) ; hugo file format for org mode
;; (package! ox-slack)
(package! ox-slack
  :recipe (:host github :repo "masukomi/ox-slack" :branch "main"))
(package! ox-bb)
;; (package! flycheck-vale)
;; (package! ox-leanpub)
;; ox-leanpub commit dd21c99705bc3863c43e05c9d24ab33646789711
;; from 2023 has broken support for specifying a directory
;; mine vvv fixes that
 ;; (package! ox-leanpub
 ;;   :recipe (:host gitlab :repo "masukomi/ox-leanpub" :branch "output_dir_bugs"))
(package! ox-leanpub
  :recipe (:local-repo "~/workspace/ox-leanpub"))

; ripgrep (rg)
;; (package! rg)
;; (package! sqlite3)

;; what normal people would use...
;; (package! private-comments-mode)
;; when I'm working on it...
;; (package! private-comments-mode
;;   :recipe (:local-repo "~/workspace/private-comments-mode"
;;            ;; the following is to avoid having to run 'doom sync' every time you
;;            ;; change the package.
;;            :build (:not compile)
;;                        ))

(package! explain-pause-mode
  :recipe (:host github :repo "lastquestion/explain-pause-mode"))


; (package! example
;   :recipe (:host github :repo "masukomi/private-comments-mode"))

;
;; (package! copilot
;;   :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

;; TEMPORARY (hopefully) Bullshittery
;; via https://emacs.stackexchange.com/a/75836/30947
;; (package! transient
;;       :pin "c2bdf7e12c530eb85476d3aef317eb2941ab9440"
;;       :recipe (:host github :repo "magit/transient"))

;; (package! with-editor
;;           :pin "bbc60f68ac190f02da8a100b6fb67cf1c27c53ab"
;;           :recipe (:host github :repo "magit/with-editor"))
