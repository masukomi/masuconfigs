;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Kay Rhodes"
      user-mail-address "masukomi@masukomi.org"
      org-export-default-language "en-US")

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
;; `load-theme' function. These are the defaults.
;; (setq doom-theme 'doom-one)
(setq doom-theme 'darktooth)
;; (setq doom-theme 'doom-snazzy)
;; (setq doom-theme 'sanity-inc-tomorrow-night)
;; (setq doom-theme 'doom-spacegrey)

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/.config/org/")
(setq org-hide-emphasis-markers t)
;(straight-use-package 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
; prevent it from interpreting _ as subscript and ^ as superscript
; and thus generating <sub> and <super> tags when exporting to markdown
(setq org-export-with-sub-superscripts nil)


;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)
(display-line-numbers-mode)
(nlinum-relative-on)

; $ should go to the end of the actual line not
; the visual line
(setq evil-respect-visual-line-mode nil)


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

;---- added stuff
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;---- alternately
;; disable auto-save and auto-backup
; (setq auto-save-default nil)
; (setq make-backup-files nil)

; kinda-sorta forcing initial window size because it seems incapable
; of remembering
(setq default-frame-alist '((left . 106) (width . 106) (fullscreen . fullheight)))
; main monitor is currently 106 x 60 (characters)

; automatically sync buffer with changes in the filesystem
; Auto Revert will not revert a buffer if it has unsaved changes, or if its file
; on disk is deleted or renamed.
; When a buffer is auto-reverted, a message is generated. This can be suppressed
; by setting auto-revert-verbose to nil.
; https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Revert.html#Auto-Revert
(global-auto-revert-mode t)


(global-visual-line-mode t)
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


; relative line numbers
; via the linum-relative package https://github.com/coldnew/linum-relative
; (require 'linum-relative)



;; (require 'nlinum-relative)
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

;; disable the graphical toolbar
(tool-bar-mode -1)

(setq whitespace-display-mappings
  '((space-mark   ?\    [?\xB7]     [?.])	; space
    (space-mark   ?\xA0 [?\xA4]     [?_])	; hard space
    (newline-mark ?\n   [?\xB6 ?\n] [?$ ?\n])	; end-of-line
    (tab-mark   ?\t   [?\xBB ?\t] [?\\ ?\t])	; tab
    ))

; Ctrl + =/+ contracts or expands visual selection
(map! :nv "C-=" #'er/contract-region
      :nv "C-+" #'er/expand-region)


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


; stop asking if i really want to quit,
; and more importantly those horrible messages.
(setq confirm-kill-emacs nil) ; 'y-or-n-p

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

;; (defun my-eshell-mode-faces ()
;;     ;; (face-remap-add-relative 'default '((:foreground "#BD9700")))
;;     ;; (face-remap-add-relative 'eshell-prompt '((:foreground "#BD9700" :weight bold)))
;;     (interactive)
;;     (setq buffer-face-mode-face '(:family "Courrier"))
;;     (buffer-face-mode)
;;     )

;; (add-hook 'eshell-mode-hook 'my-eshell-mode-faces)

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

; neotree enable auto-refresh
(setq neo-autorefresh t)
; NECESSARY LSP bits vvvv
(use-package lsp-mode
      :commands lsp
      :ensure t
      :diminish lsp-mode
      :hook
      (elixir-mode . lsp)
      :init
      (add-to-list 'exec-path "~/workspace/reference/elixir/elixir-ls/release"))


; UNNECESSARY LSP bits vvvv
; (setq lsp-enable-folding t)
; (use-package! lsp-origami)
; (add-hook! 'lsp-after-open-hook #'lsp-origami-try-enable)

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

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

; Chicken Scheme
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



; convince projectile to create new files
; not just find existing ones.
; found here: https://www.reddit.com/r/emacs/comments/3m8i5r/helmprojectile_quickly_findcreate_new_file_in/
(with-eval-after-load 'helm-projectile
  (defvar helm-source-file-not-found
    (helm-build-dummy-source
        "Create file"
      :action (lambda (cand) (find-file cand))))


  (add-to-list 'helm-projectile-sources-list helm-source-file-not-found t))

; rubocop
; https://github.com/rubocop/rubocop-emacs
; (add-hook 'ruby-mode-hook #'rubocop-mode)
; (setq rubocop-autocorrect-on-save t)
; ;(setq rubocop-autocorrect-command "rubocop -A --format emacs")
; ☝️ that code would be good but we're using rubocop-flexport which is buggy
;    and causes that to break
;; (defun rubocop-format ()
(defun rfmt ()
  "rubocop-flexport is crap that breaks rubocop-autocorrect-command"
  (when (eq major-mode 'ruby-mode)
    (shell-command-to-string (format "rubocop -A %s" buffer-file-name))))

; (add-hook 'after-save-hook #'rubocop-format)
; (add-hook 'after-save-hook #'rfmt)

;; paren mode (highlight all code in the current parens)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)
(set-face-attribute 'show-paren-match-expression nil :background "MediumPurple2")
(show-paren-mode 1)

(require 'ox-md)
;;(require 'ox-clip) ;format org mode and copy to clipboard
(require 'ox-publish)
(with-eval-after-load 'ox
  (require 'ox-hugo)
  (require 'ox-clip))


(evil-ex-define-cmd "clean" 'rubocop-format)

;; org-roam stuff

;; (make-directory "~/Documents/org-roam")
(setq org-roam-directory (file-truename "~/Documents/org-roam"))
(org-roam-db-autosync-mode)

; org-roam-ui stuff
(use-package! websocket
  :after org-roam)
(use-package! org-roam-ui
  :after org-roam ; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since
;;         org-roam does not have a hookable mode anymore, you're advised to
;;         pick something yourself if you don't care about startup time, use
;;         :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;;
;; because i want HTML & Javascript highlighting in the same file
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags
  '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
    (js-mode  "<script[^>]*>" "</script>")
    (css-mode "<style[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5" "erb"))
(multi-web-global-mode 1)
