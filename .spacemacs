(setq-default dotspacemacs-configuration-layers '(osx))
;; (setq-default dotspacemacs-additional-packages '(tabbar))
;; Commented out because I can't figure out how to make it actually show up
;; So, no point in loading it.

(setq dotspacemacs-elpa-https nil) ; GNU Elpa installs fail without this
(setq dotspacemacs-elpa-timeout 20); ditto
(setq dotspacemacs-editing-style "vim")
; because the Emacs.app homebrew thing told me to:
; but commented out because it fucks up Spacemacs (i think)
;(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
;  (normal-top-level-add-subdirs-to-load-path))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (## lv ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline reveal-in-osx-finder restart-emacs request rainbow-delimiters popwin persp-mode pcre2el pbcopy paradox spinner osx-trash osx-dictionary org-plus-contrib org-bullets open-junk-file neotree move-text lorem-ipsum linum-relative link-hint launchctl info+ indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight dumb-jump f dash s diminish column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
