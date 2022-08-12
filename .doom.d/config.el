;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!

;;---------------------------------
;; General Config
(add-to-list 'load-path "~/.doom.d/custom")

;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Kay Rhodes"
      user-mail-address "masukomi@masukomi.org"
      org-export-default-language "en-US")

; automatically sync buffer with changes in the filesystem
; Auto Revert will not revert a buffer if it has unsaved changes, or if its file
; on disk is deleted or renamed.
; When a buffer is auto-reverted, a message is generated. This can be suppressed
; by setting auto-revert-verbose to nil.
; https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Revert.html#Auto-Revert
(global-auto-revert-mode t)

; stop asking if i really want to quit,
; and more importantly those horrible messages.
(setq confirm-kill-emacs nil) ; 'y-or-n-p

;;;;;;;;;;;;;;;;
; stop cluttering my directories with foo.txt~ files
(setq backup-directory-alist '(("." . "~/.emacs-tmp")))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;---- alternately
;; disable auto-save and auto-backup
; (setq auto-save-default nil)
; (setq make-backup-files nil)
; neotree enable auto-refresh
(setq neo-autorefresh t)

(setq auto-completion-delay 1)

; https://melpa.org/#/exec-path-from-shell
(exec-path-from-shell-initialize)
;;---------------------------------
;; Configuring DOOM itself

; image courtesy of eccentric-j in this GitHub issue
; https://github.com/doomemacs/doomemacs/issues/3382
(setq fancy-splash-image "~/.doom.d/images/doom_icon_256x256.png")

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.




;;---------------------------------
;; UI CONFIG
; kinda-sorta forcing initial window size because it seems incapable
; of remembering
(setq default-frame-alist '((left . 106) (width . 106) (fullscreen . fullheight)))
; main monitor is currently 106 x 60 (characters)

;;;;;;;;;;;;;;;;
;; Toggle Window Split by JeffDWork
;; found here: https://www.emacswiki.org/emacs/ToggleWindowSplit
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(define-key ctl-x-4-map "t" 'toggle-window-split)

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; test
; (setq doom-font (font-spec :family "monospace" :size 20)
;       doom-variable-pitch-font (font-spec :family "sans"))
(setq doom-font (font-spec :family "JetBrains Mono Medium" :size 20)
      doom-variable-pitch-font (font-spec :family "Monaco")
      doom-big-font (font-spec :family "JetBrains Mono Medium"))
;; RESET FONT TO DEFAULT `doom-font` with a call to
;; doom/reset-font-size

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function.
;; (setq doom-theme 'darktooth)
(setq doom-theme 'doom-gruvbox)


;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)
(display-line-numbers-mode)
(nlinum-relative-on)
(nlinum-relative-setup-evil)               ;; setup for evil
(add-hook 'prog-mode-hook 'nlinum-relative-mode)
(setq nlinum-relative-redisplay-delay 0)   ;; delay
(setq nlinum-relative-current-symbol "->") ;; or "" for display current line number
(setq nlinum-relative-offset 0)            ;; 1 if you want 0, 2, 3...

; buffer-move stuff
; https://github.com/lukhas/buffer-move
;; (global-set-key (kbd "<C-S-up>")     'buf-move-up)
;; (global-set-key (kbd "<C-S-down>")   'buf-move-down)
;; (global-set-key (kbd "<C-S-left>")   'buf-move-left)
;; (global-set-key (kbd "<C-S-right>")  'buf-move-right)

; $ should go to the end of the actual line not
; the visual line
(setq evil-respect-visual-line-mode nil)
(global-visual-line-mode t)

;; disable the graphical toolbar
(tool-bar-mode -1)
(setq whitespace-display-mappings
  '((space-mark   ?\    [?\xB7]     [?.])  ; space
    (space-mark   ?\xA0 [?\xA4]     [?_])  ; hard space
    (newline-mark ?\n   [?\xB6 ?\n] [?$ ?\n])  ; end-of-line
    (tab-mark   ?\t   [?\xBB ?\t] [?\\ ?\t]))); tab

(setq tab-width 4)
; indentation guides
; https://github.com/DarthFennec/highlight-indent-guides
(setq highlight-indent-guides-method 'fill)

;; paren mode (highlight all code in the current parens)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)
(set-face-attribute 'show-paren-match-expression nil
		    :background "peru"
		    :foreground "beige")
(show-paren-mode 1)

; Ctrl + =/+ contracts or expands visual selection
(map! :nv "C-=" #'er/contract-region
      :nv "C-+" #'er/expand-region)

; (defun halve-other-window-height ()
;   "Expand current window to use half of the other window's lines."
;   (interactive)
;   (enlarge-window (/ (window-height (next-window)) 2)))
;
; (global-set-key (kbd "C-c v") 'halve-other-window-height)

(setq display-buffer-base-action '(display-buffer-in-tab))
(setq projectile-track-known-projects-automatically nil)
;; ;------------
;; ; customize the mode-line (think airline in vim)
;; (set-face-attribute 'mode-line nil
;;  :background "#6c6f31" ; actually gets used as the foreground
;;  :foreground "#314d6f" ; actually....the background
;;  :box nil
;;  :overline nil
;;  :underline nil
;;  )

;; (set-face-attribute 'mode-line-inactive nil
;;                     :background "#4872a4"
;;                     :foreground "#1a283a"
;;                     :box nil    ; could do something like '(:line-width 8 :color "#565063")
;;                     :overline nil
;;                     :underline nil)
(setq doom-modeline-height 25)

;------------
;; (straight-use-package 'rainbow-mode)
; auto-highlights css colors like #ffffff
; https://elpa.gnu.org/packages/rainbow-mode.html
; haven't figured out how to do vvvv with straight-use-package
; or if one even should
(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode ))

; topsy: keeps function signatures pinned to the top as you scroll
; https://github.com/alphapapa/topsy.el#readme
(add-hook 'prog-mode-hook #'topsy-mode)


;; ---- Doom Word Wrap
;; see https://github.com/hlissner/doom-emacs/blob/3614109c7a0cdd5bc474f095beebe9c126ae8f01/modules/editor/word-wrap/README.org
;
;; to enable word-wrap in specific languages C/C++/ObjC/Java
;;(add-hook 'c-mode-common-hook #'+word-wrap-mode)
;
;; to enable it globally
; (+global-word-wrap-mode +1)
;; to disable global word-wrap in a specific mode (emacs-lisp-mode in this case)
;; (add-to-list '+word-wrap-disabled-modes 'emacs-lisp-mode)

; convince projectile to create new files
; not just find existing ones.
; found here: https://www.reddit.com/r/emacs/comments/3m8i5r/helmprojectile_quickly_findcreate_new_file_in/
(with-eval-after-load 'helm-projectile
  (defvar helm-source-file-not-found
    (helm-build-dummy-source
        "Create file"
      :action (lambda (cand) (find-file cand))))


  (add-to-list 'helm-projectile-sources-list helm-source-file-not-found t))


; TODO: make this toggleable
; tab mode
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(defun masu-insert-tab-char ()
  "Insert a tab char. (ASCII 9, \t)"
  (interactive)
  (insert "\t"))

;; (global-set-key (kbd "TAB") 'masu-insert-tab-char)
(add-hook 'prog-mode-hook
          (lambda ()
            (unless (derived-mode-p 'ruby-mode)
              (add-hook 'after-init-hook
		(lambda () (local-set-key (kbd "TAB") #'masu-insert-tab-char))
			))))


;;---------------------------------
;; UTILITY CONFIG
;; ialign (interatvie alignment)
(global-set-key (kbd "C-x l") #'ialign)

;; dired
;; found here:: https://wilkesley.org/~ian/xah/emacs/emacs_dired_tips.html
(defun masu-dired-mode-setup ()
	"hook for 'dired-mode'"
	(dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'masu-dired-mode-setup)

;;---------------------------------
;; ORG Configs

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/.config/org/")
(setq org-hide-emphasis-markers t)
;(straight-use-package 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
; prevent it from interpreting _ as subscript and ^ as superscript
; and thus generating <sub> and <super> tags when exporting to markdown
(setq org-export-with-sub-superscripts nil)

; syntax highlighting within org blocks
(setq org-src-fontify-natively t)
; if that is too slow you can call
;
; org-src-fontify-buffer
; org-src-fontify-block
;
; TODO KEYWORDS
(setq org-todo-keywords
	'((sequence "TODO(t)" "INPROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")
	(sequence "[ ](T)" "|" "[X](D)")
	(sequence "|" "OKAY(o)" "YES(y)" "NO(n)"))
      org-todo-keyword-faces
      '(
	("TODO"		:foreground "#7c7c75" :weight normal :underline t)
	("WAITING"	:foreground "#9f7efe" :weight normal :underline t)
	("INPROGRESS"	:foreground "#0098dd" :weight normal :underline t)
	("DONE"		:foreground "#50a14f" :weight normal :underline t)
	("CANCELLED"	:foreground "#ff6480" :weight normal :underline t)

	)
      org-agenda-files (list "~/Documents/notes/")

      )


; EMOJIFY things
(add-hook 'after-init-hook #'global-emojify-mode)
; found here: https://github.com/bgutter/dotemacs/blob/master/my-init.org
(defun my:emojify-inhibit-fix-org-drawers (text beg end)
  "Since org-mode now uses lower-case :begin:, :end:, etc tags, some of them are
now being rendered as Emojis. Filter this case out."
  (and (equal major-mode 'org-mode) (member (downcase text) '(":begin:" ":end:"))))

; made by me
(defun my:emojify-inhibit-no-inline-escape-emoji (text beg end)
  "disable creation of emojis starting with = or ~ in org-mode"
  (and (equal major-mode 'org-mode)
       (or))
  (string-prefix-p "=" (downcase te))
  (string-prefix-p "~" (downcase te)))



(with-eval-after-load "emojify"
	(add-to-list 'emojify-inhibit-functions 'my:emojify-inhibit-fix-org-drawers)
	(add-to-list 'emojify-inhibit-functions 'my:emojify-inhibit-no-inline-escape-emoji)
)

(with-eval-after-load 'ox
	(require 'ox-hugo)
	(require 'ox-clip)
	(require 'ox-md)
	(require 'ox-publish)


  )
(with-eval-after-load 'org
	;; org-hugo blogging things
	(setq time-stamp-active t
		time-stamp-start "#\\+lastmod:[ \t]*"
		time-stamp-end "$"
		time-stamp-format "%04Y-%02m-%02d")
	(add-hook 'before-save-hook 'time-stamp nil)
	(add-to-list
		'org-src-lang-modes '("plantuml" . plantuml))
  )

;; New link type for Org-Hugo internal links
(with-eval-after-load 'ox-hugo
	(org-link-set-parameters "hugo"
		:complete (lambda ()
			(concat "{{% ref "(file-name-nondirectory (read-file-name "File: "))" %}}"))))
;; end org-hugo

; sticking this under org because it's the only place i use PlantUML
;; Enable plantuml-mode for PlantUML files
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
; WARNING: previewing of files may result in info being sent to
; plantuml.com. if execution mode is "server".
; You can customize plantuml-default-exec-mode
; or run plantuml-set-exec-mode
; from a plantuml-mode buffer to switch modes.

; To avoid this use executable mode or install the jar
; (note) homebrew installs the jar
; the plantuml-server-url defaults to
; "https://www.plantuml.com/plantuml"
; executable should work if you have run brew install plantuml
; BUT it seems like babel wants jar. so :shrug:
(setq plantuml-set-exec-mode
      "executable"); because babel needs the jar i think

; find this via
; brew --prefix plantuml
; then find . -name "*.jar"

;; Sample jar configuration
;; (setq plantuml-jar-path "/usr/local/opt/plantuml/libexec/plantuml.jar")
(setq plantuml-jar-path "/usr/local/Cellar/plantuml/1.2022.6/libexec/plantuml.jar")
(setq plantuml-default-exec-mode 'jar)
;
; ;; Sample executable configuration
; (setq plantuml-executable-path "/path/to/your/copy/of/plantuml.bin")
; (setq plantuml-default-exec-mode 'executable))


;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)))

; for Babel support http://eschulte.github.io/babel-dev/DONE-integrate-plantuml-support.html
;; (setq org-plantuml-jar-path
;;   (expand-file-name "/usr/local/opt/plantuml/libexec/plantuml.jar"))
(setq org-plantuml-jar-path
      (expand-file-name "/usr/local/Cellar/plantuml/1.2022.6/libexec/plantuml.jar" ))

;; BEGIN DENOTE stuff
(require 'denote)

;; Remember to check the doc strings of those variables.
(setq denote-directory (expand-file-name "~/Documents/notes/"))
(setq denote-known-keywords '("daily" "todo" "project"))
(setq denote-infer-keywords t)
(setq denote-sort-keywords t)
(setq denote-file-type nil) ; Org is the default, set others here
(setq denote-prompts '(title keywords))

;; We allow multi-word keywords by default.  The author's personal
;; preference is for single-word keywords for a more rigid workflow.
(setq denote-allow-multi-word-keywords nil)

(setq denote-date-format nil) ; read doc string

;; By default, we fontify backlinks in their bespoke buffer.
(setq denote-link-fontify-backlinks t)

;; Also see `denote-link-backlinks-display-buffer-action' which is a bit
;; advanced.

;; If you use Markdown or plain text files (Org renders links as buttons
;; right away)
(add-hook 'find-file-hook #'denote-link-buttonize-buffer)

(setq denote-dired-rename-expert nil)

;; We use different ways to specify a path for demo purposes.
(setq denote-dired-directories
      (list denote-directory
            (thread-last denote-directory (expand-file-name "attachments"))
            ; (expand-file-name "~/Documents/books")
            ))

;; Generic (great if you rename files Denote-style in lots of places):
;; (add-hook 'dired-mode-hook #'denote-dired-mode)
;;
;; OR if only want it in `denote-dired-directories':
(add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

;; Here is a custom, user-level command from one of the examples we
;; showed in this manual.  We define it here and add it to a key binding
;; below.
;; (defun my-denote-journal ()
;;   "Create an entry tagged 'journal', while prompting for a title."
;;   (interactive)
;;   (denote
;;    (denote--title-prompt)
;;    '("journal")))
;;
(defun denote-journal ()
  "Create an entry tagged 'journal' with the date as its title."
  (interactive)
  (denote
   (format-time-string "%A %e %B %Y") ; format like Tuesday 14 June 2022
   '("journal"))) ; multiple keywords are a list of strings: '("one" "two")

(defun show-denote-dir ()
  "open an dired window on the default denote directory"
  (interactive) ; required to make it accessible via keybdingings
  (split-window-vertically)
  (other-window 1)
  (dired denote-directory))

;; Denote DOES NOT define any key bindings.  This is for the user to
;; decide.  For example:
(let ((map global-map))
  (define-key map (kbd "C-c n f") #'show-denote-dir)   ; custom
  (define-key map (kbd "C-c n j") #'denote-journal) ; custom
  (define-key map (kbd "C-c n n") #'denote)
  (define-key map (kbd "C-c n N") #'denote-type)
  (define-key map (kbd "C-c n d") #'denote-date)
  (define-key map (kbd "C-c n s") #'denote-subdirectory)
  ;; If you intend to use Denote with a variety of file types, it is
  ;; easier to bind the link-related commands to the `global-map', as
  ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
  ;; `markdown-mode-map', and/or `text-mode-map'.
  (define-key map (kbd "C-c n i") #'denote-link) ; "insert" mnemonic
  (define-key map (kbd "C-c n I") #'denote-link-add-links)
  (define-key map (kbd "C-c n l") #'denote-link-find-file) ; "list" links
  (define-key map (kbd "C-c n b") #'denote-link-backlinks)
  ;; Note that `denote-dired-rename-file' can work from any context, not
  ;; just Dired bufffers.  That is why we bind it here to the
  ;; `global-map'.
  (define-key map (kbd "C-c n r") #'denote-dired-rename-file)
  (define-key map (kbd "C-c n R") #'denote-dired-rename-file-and-add-front-matter))

;; Key bindings specifically for Dired.
(let ((map dired-mode-map))
  (define-key map (kbd "C-c C-d C-i") #'denote-link-dired-marked-notes)
  (define-key map (kbd "C-c C-d C-r") #'denote-dired-rename-marked-files)
  (define-key map (kbd "C-c C-d C-R") #'denote-dired-rename-marked-files-and-add-front-matters))

(with-eval-after-load 'org-capture
  (setq denote-org-capture-specifiers "%l\n%i\n%?")
  (add-to-list 'org-capture-templates
               '("n" "New note (with denote.el)" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)))


;; END DENOTE stuff







;;---------------------------------
;; EVIL Mode


; a port of Tim Pope's surround.vim
; https://github.com/emacs-evil/evil-surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))



;;---------------------------------
;; LANGUAGE SPECIFIC STUFF
;;------------- Raku
;; see raku-mode https://github.com/Raku/raku-mode
(define-auto-insert
	'("\\.rakumod\\'" . "Raku module skeleton")
	'raku-module-skeleton)
(define-auto-insert
	'("\\.raku\\'" . "Raku script skeleton")
	'raku-script-skeleton)
;;------------- HTML

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))
; vvv--- live eex
(add-to-list 'auto-mode-alist '("\\.leex\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

; because i want HTML & Javascript highlighting in the same file
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags
  '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
    (js-mode  "<script[^>]*>" "</script>")
    (css-mode "<style[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5" "erb"))
(multi-web-global-mode 1)

;;------------- CHICKEN SCHEME
; this and more at https://wiki.call-cc.org/emacs

(setq scheme-programe-name "csi -:c")
;; Indenting module body code at column 0
(defun scheme-module-indent (state indent-point normal-indent) 0)
(put 'module 'scheme-indent-function 'scheme-module-indent)

(put 'and-let* 'scheme-indent-function 1)
(put 'parameterize 'scheme-indent-function 1)
(put 'handle-exceptions 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'match 'scheme-indent-function 1)

; to enable binding.pry and byebug when using rspec-mode
; When you've hit the breakpoint, hit C-x C-q to enable inf-ruby.
(add-hook 'after-init-hook 'inf-ruby-switch-setup)

;;------------- RACKET
; racket
; auto-format on save
(defun raco-fmt ()
  "format with raco fmt"
  (when (eq major-mode 'racket-mode)
    (shell-command-to-string (format "raco fmt -i --width 80 %s" buffer-file-name))))

(add-hook 'after-save-hook #'raco-fmt)


;;------------- LUA
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;;------------- FENNEL
;; (autoload 'fennel-mode "/path/to/fennel-mode/fennel-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode))

;;------------- ELIXIR

;; Highlighting of Elixir's Inline LiveView templates
;; https://blog.evalcode.com/phoenix-liveview-inline-syntax-highlighting-for-emacs/
;; Assumes web-mode and elixir-mode are already set up
;;
(use-package polymode
  :mode ("\.ex$" . poly-elixir-web-mode)
  :config
  (define-hostmode poly-elixir-hostmode :mode 'elixir-mode)
  (define-innermode poly-liveview-expr-elixir-innermode
    :mode 'web-mode
    :head-matcher (rx line-start (* space) "~L" (= 3 (char "\"'")) line-end)
    :tail-matcher (rx line-start (* space) (= 3 (char "\"'")) line-end)
    :head-mode 'host
    :tail-mode 'host
    :allow-nested nil
    :keep-in-mode 'host
    :fallback-mode 'host)
  (define-polymode poly-elixir-web-mode
    :hostmode 'poly-elixir-hostmode
    :innermodes '(poly-liveview-expr-elixir-innermode))
  )
(setq web-mode-engines-alist '(("elixir" . "\\.ex\\'")))

; reformatter + elixir code found here:
; https://medium.com/@victor.nascimento/elixir-development-on-emacs-9f6776265e4d
(use-package reformatter
  :ensure t
  :config
  ; Adds a reformatter configuration called "+elixir-format"
  ; This uses "mix format -"
  (reformatter-define +elixir-format
    :program "mix"
    :args '("format" "-"))
  ; defines a function that looks for the .formatter.exs file used by mix format
  (defun +set-default-directory-to-mix-project-root (original-fun &rest args)
    (if-let* ((mix-project-root (and buffer-file-name
                                     (locate-dominating-file buffer-file-name
                                                             ".formatter.exs"))))
        (let ((default-directory mix-project-root))
          (apply original-fun args))
      (apply original-fun args)))
  ; adds an advice to the generated function +elxir-format-region that sets the proper root dir
  ; mix format needs to be run from the root directory otherwise it wont use the formatter configuration
  (advice-add '+elixir-format-region :around #'+set-default-directory-to-mix-project-root)
  ; Adds a hook to the major-mode that will add the generated function +elixir-format-on-save-mode
  ; So, every time we save an elixir file it will try to find a .formatter.exs and then run mix format from
  ; that file's directory
  (add-hook 'elixir-mode-hook #'+elixir-format-on-save-mode))
; NECESSARY LSP bits vvvv
(use-package lsp-mode
      :commands lsp
      :ensure t
      :diminish lsp-mode
      :hook
      (elixir-mode . lsp)
      :init
      (add-to-list 'exec-path "~/workspace/reference/elixir/elixir-ls/release"))

;------------------------------- APP SPECIFIC

;;------------- GIT
;; magit stuff
;; documentation for magit-blame-styles here
;; https://github.com/magit/magit/blob/9b48dd7e3618ac3736f66ef964ae5e1fedd54f98/lisp/magit-blame.el#L39
(setq magit-blame-styles
           '((margin
              (margin-width . 32)
              (margin-format . ("%C %a %f"))
              (margin-face . magit-blame-margin)
              (margin-body-face . magit-blame-dimmed)
              (show-message . t))))
;; the cache is always wrong after switching branches
;; use magit to switch and you can then auto-invalidate it.
(defun run-projectile-invalidate-cache (&rest _args)
  ;; We ignore the args to `magit-checkout'.
  (projectile-invalidate-cache nil))
(advice-add 'magit-checkout
            :after #'run-projectile-invalidate-cache)
(advice-add 'magit-branch-and-checkout ; This is `b c'.
            :after #'run-projectile-invalidate-cache)

;;------------- DIFF
;;
; ediff stuff
; for making M-x ediff-files and M-x ediff-current-file
; experience better
; found here: https://pragmaticemacs.wordpress.com/2015/06/13/visualise-and-copy-differences-between-files/
(require 'ediff)
;; don't start another frame
;; this is done by default in preluse
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; put windows side by side
(setq ediff-split-window-function (quote split-window-horizontally))
;;revert windows on exit - needs winner mode
(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;;------------- MASTODON
; current repo here: https://codeberg.org/martianh/mastodon.el
(use-package mastodon
  :ensure t)
(setq mastodon-instance-url "https://connectified.com"
      mastodon-active-user "masukomi")
(require 'mastodon-async)




;------------------------------- DETRITUS
; currently disabled because i can't figure out how the eff
; to make a new tab instead of a new workspace
;; (use-package centaur-tabs
;;   :ensure t
;;   :config
;;     (setq centaur-tabs-set-bar 'under
;;           centaur-tabs-style "slant"
;;           centaur-tabs-gray-out-icons 'buffer
;;           centaur-tabs-set-modified-marker t
;;           centaur-tabs-set-icons t
;;           )
;;     (centaur-tabs-headline-match)
;;     (centaur-tabs-mode t)
;;     :bind
;;     (:map evil-normal-state-map
;;       ("g t" . centaur-tabs-forward)
;;       ("g T" . centaur-tabs-backward))
;;   )
;;(define-key evil-normal-state-map (kbd "g t") 'centaur-tabs-forward)
;; (define-key evil-normal-state-map (kbd "g T") 'centaur-tabs-backward)


; tell yasnippet to expand on space too (because that's the way it worked in vim)
; blows up if run here. i think something's not loaded yet
;; (define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand)



;; (defun my-eshell-mode-faces ()
;;     ;; (face-remap-add-relative 'default '((:foreground "#BD9700")))
;;     ;; (face-remap-add-relative 'eshell-prompt '((:foreground "#BD9700" :weight bold)))
;;     (interactive)
;;     (setq buffer-face-mode-face '(:family "Courrier"))
;;     (buffer-face-mode)
;;     )

;; (add-hook 'eshell-mode-hook 'my-eshell-mode-faces)




; UNNECESSARY LSP bits vvvv
; (setq lsp-enable-folding t)
; (use-package! lsp-origami)
; (add-hook! 'lsp-after-open-hook #'lsp-origami-try-enable)







; rubocop
; https://github.com/rubocop/rubocop-emacs
; (add-hook 'ruby-mode-hook #'rubocop-mode)
; (setq rubocop-autocorrect-on-save t)
; ;(setq rubocop-autocorrect-command "rubocop -A --format emacs")
; ☝️ that code would be good but we're using rubocop-flexport which is buggy
;    and causes that to break
;; (defun rubocop-format ()
;; (defun rfmt ()
;;   "rubocop-flexport is crap that breaks rubocop-autocorrect-command"
;;   (when (eq major-mode 'ruby-mode)
;;     (shell-command-to-string (format "rubocop -A %s" buffer-file-name))))

; (add-hook 'after-save-hook #'rubocop-format)
; (add-hook 'after-save-hook #'rfmt)



(evil-ex-define-cmd "clean" 'rubocop-format)

;; ;; org-roam stuff

;; ;; (make-directory "~/Documents/org-roam")
;; (setq org-roam-directory (file-truename "~/Documents/org-roam"))
;; (org-roam-db-autosync-mode)
;; ;; (setq org-roam-database-connector 'sqlite3)

;; ; org-roam-ui stuff
;; (use-package! websocket
;;   :after org-roam)
;; (use-package! org-roam-ui
;;   :after org-roam ; or :after org
;; ;;         normally we'd recommend hooking orui after org-roam, but since
;; ;;         org-roam does not have a hookable mode anymore, you're advised to
;; ;;         pick something yourself if you don't care about startup time, use
;; ;;         :hook (after-init . org-roam-ui-mode)
;;     :config
;;     (setq org-roam-ui-sync-theme t
;;           org-roam-ui-follow t
;;           org-roam-ui-update-on-save t
;;           org-roam-ui-open-on-start t))

;; (setq org-roam-dailies-directory "daily/")

;; ;; see this link for how to create new templates (daily & otherwise)
;; ;; https://systemcrafters.net/build-a-second-brain-in-emacs/capturing-notes-efficiently/
;; (setq org-roam-dailies-capture-templates
;;       '(("d" "default" entry
;;          "* %?"
;;          :target (file+head "%<%Y-%m-%d>.org"
;;                             "#+title: %<%Y-%m-%d>\n"))
;;         ("w" "work" plain
;;          "\n\nprevious: \nnext: \n* TODO \n- [ ] get a ticket to work on\n- [ ] check PR comments \n- [ ] see if there are PRs needing review\n\n%?")
;;         :if-new (file+head "%<%Y-%m-%d>.org"
;;                            "#+title: %<%Y-%m-%d>\n")
;;         :unnarrowed t))


;; ; for searching org-roam stuff
;; ; for more on deft check out this video:
;; ; https://www.youtube.com/watch?v=mldoUx_wi10
;; (use-package deft
;;   :after org
;;   :bind
;;   ("C-c n d" . deft)
;;   :custom
;;   (deft-recursive t)
;;   (deft-use-filter-string-for-filename t)
;;   (deft-default-extension "org")
;;   (deft-directory org-roam-directory))









;














