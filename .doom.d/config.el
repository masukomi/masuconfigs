;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

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
      doom-variable-pitch-font (font-spec :family "Monaco"))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'darktooth)
;; (setq doom-theme 'doom-snazzy)
(setq doom-theme 'doom-spacegrey)

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/.config/org/")

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)

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

;crosshairs.el
;https://www.emacswiki.org/emacs/crosshairs.el
;(cursor crosshair) requires hl-line+ which doesn't exist (anymore?)
;there is hl-line-plus though so maybe crosshairs.el could be edited to use
;that and it'd work.
 ;; (straight-use-package 'hl-line-plus)
