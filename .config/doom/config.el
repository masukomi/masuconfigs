;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;
;; DO NOT READ THIS. DO NOT EDIT THIS.
;; READ config.org instead!
;;
;; This is a file generated on ${date} from a literate programing source file located at
;; https://github.com/masukomi/masuconfigs/blob/master/.config/doom/config.org
;;
;; config.org HAS ALL OF THE JUICY DETAILS
;; Future me: You should make any changes there and regenerate it from Emacs org-mode
;; using org-babel-tangle (C-c C-v t)
;;

;;---------------------------------
;; General Config
(add-to-list 'load-path "~/.config/doom/custom")
(load "eruby-mode")
(load "maybe-insert-org-entity")

; automatically sync buffer with changes in the filesystem
(global-auto-revert-mode t)

(setq confirm-kill-emacs nil) ; 'y-or-n-p

;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Kay Rhodes"
      user-mail-address "masukomi@masukomi.org"
      org-export-default-language "en-US")

; eff you emacs. Stop litering my directories with backup files.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(bind-key* (kbd "C-x") 'kill-region)

; kinda-sorta forcing initial window size because it seems incapable
; of remembering
(setq default-frame-alist '(
                            (left . 106)
                            (width . 106)
                            (fullscreen . fullheight)
                            (right-divider-width . 8)
                            ))

; .dir-locals.el helper methods
(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun my-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir)
          (my-reload-dir-locals-for-current-buffer))))))

(setq fancy-splash-image "~/.config/doom/images/doom_icon_256x256.png")

; https://melpa.org/#/exec-path-from-shell
(exec-path-from-shell-initialize)

; a macOS specific tweak to allow you to use the option key to
; type special characters like
; - an accent grave á <option-e a>
; - a degree sign ° <option-shift-8>
;
; toggle it with C-c m o
; found here: https://www.reddit.com/r/emacs/comments/mpbgx7/comment/gu9opv1/
(setq mac-opt-keymap (make-sparse-keymap))

;; equivalent to C-M-x with mac-opt-chars-mode on
(define-key mac-opt-keymap (kbd "C-≈") 'execute-extended-command)

(defun mac-toggle-ns-alt-modifier ()
  (if (not mac-opt-chars-mode)
      (setq ns-alternate-modifier 'meta)
    (setq ns-alternate-modifier nil)))

(define-minor-mode mac-opt-chars-mode
  "Type characters with option as in other Mac applications."
  :global t
  :lighter " mac-opt-chars"
  :keymap mac-opt-keymap
  (mac-toggle-ns-alt-modifier))
                                        ; toggle it on and off with C-c m o
(define-key mac-opt-keymap (kbd "C-c m o") 'mac-opt-chars-mode)
(define-key global-map (kbd "C-c m o") 'mac-opt-chars-mode)

(setq projectile-indexing-method 'hybrid)
(setq projectile-git-fd-args "--ignore-file .fdignore --no-ignore -H -0 -E .git -tf --strip-cwd-prefix")

(setq projectile-track-known-projects-automatically nil)

(with-eval-after-load 'helm-projectile
  (defvar helm-source-file-not-found
    (helm-build-dummy-source
        "Create file"
      :action (lambda (cand) (find-file cand))))


  (add-to-list 'helm-projectile-sources-list helm-source-file-not-found t)
  (setq +workspaces-on-switch-project-behavior t)
  )

(defun run-projectile-invalidate-cache (&rest _args)
  ;; We ignore the args to `magit-checkout'.
  (projectile-invalidate-cache nil))
(advice-add 'magit-checkout
            :after #'run-projectile-invalidate-cache)
(advice-add 'magit-branch-and-checkout ; This is `b c'.
            :after #'run-projectile-invalidate-cache)

; delay autocomplete a little bit longer
(setq auto-completion-delay 2)

; stop mixing tabs and spaces when indenting!
(setq-default indent-tabs-mode nil)
; i like tabs to be 4 characters wide.
; the beauty of the tab character is that
; it can show as 4 chars on mine, and 2 or 20 on yours.
(setq-default tab-width 4)

;; turn on dtrt-indent, which makes it use the right indentation.
(setq dtrt-indent-global-mode t)

; indentation guides
(setq highlight-indent-guides-method 'character)

;; ialign (interatvie alignment)
(global-set-key (kbd "C-x l") #'ialign)

; don't auto-pair single quotes anywhere
(sp-pair "'" nil :actions nil)
; don't auto-pair double quotes anywhere
(sp-pair "\"" "\"" :actions nil)
; don't auto-pair escaped double quotes either
(sp-pair "\\\"" "\\\"" :actions nil)

(add-hook 'org-mode-hook #'turn-off-smartparens-mode)

; neotree should autorefresh to maintain a current
; representation of your directories.
(setq neo-autorefresh t)

(setq doom-font (font-spec :family "JetBrains Mono Medium" :size 20)
      doom-variable-pitch-font (font-spec :family "Monaco")
      doom-big-font (font-spec :family "JetBrains Mono Medium"))

(setq doom-theme 'doom-gruvbox)

(custom-set-faces!
  '(cursor :background "#AA00FF") ; doesn't seem to work
  '(cursor :foreground "#FFFFFF")
        )

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; for both, you gotta get funky.
(setq display-line-numbers-type t)
;; commence funkyness…
(display-line-numbers-mode)
(nlinum-relative-on)
(nlinum-relative-setup-evil)               ;; setup for evil
(add-hook 'prog-mode-hook 'nlinum-relative-mode)
(setq nlinum-relative-redisplay-delay 0)   ;; delay
(setq nlinum-relative-current-symbol "->") ;; or "" for display current line number
(setq nlinum-relative-offset 0)            ;; 1 if you want 0, 2, 3...

; highlight the contents of the selected parentheses
(setq show-paren-delay 0)
(setq show-paren-style 'expression)
(set-face-attribute 'show-paren-match-expression nil
		    :background "peru"
		    :foreground "beige")
(show-paren-mode 1)

; highlight CSS color codes in the color they represent
(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode ))

; enable highlighting of common comment keywords
(global-hl-todo-mode t)

; enable topsy mode when programming
(add-hook 'prog-mode-hook #'topsy-mode)

;; disable the graphical toolbar
(tool-bar-mode -1)

;; Modeline tweaks (think Airline in vim)
(setq doom-modeline-height 25)

(set-face-attribute 'mode-line nil
 :background "#6c6f31" ; actually gets used as the foreground
 :foreground "#27271a" ; actually....the background
 :box nil
 :overline nil
 :underline nil
 )

(set-face-attribute 'mode-line-inactive nil
                    :background "#31446f"
                    :foreground "#1a283a"
                    :box nil    ; could do something like '(:line-width 8 :color "#565063")
                    :overline nil
                    :underline nil)

(global-set-key (kbd "s-w")  '+workspace/kill)

; Ctrl + =/+ contracts or expands visual selection
(map! :nv "C-=" #'er/contract-region
      :nv "C-+" #'er/expand-region)

(setq display-buffer-base-action '(nil))

; from the bottom buffer swap its contents with the top
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
; and vice-versa
(global-set-key (kbd "<C-S-down>")     'buf-move-down)
; and so on…
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

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

; make the cursor go to the actual end of the line
; instead of the VISUAL end of the line. ugh.
(setq evil-respect-visual-line-mode nil)
(global-visual-line-mode t)

;; yafolding
;; https://github.com/emacsorphanage/yafolding
(defvar yafolding-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-S-return>") #'yafolding-hide-parent-element)
    (define-key map (kbd "<C-M-return>") #'yafolding-toggle-all)
    (define-key map (kbd "<C-return>") #'yafolding-toggle-element)
    map))
                                        ; we're usually not in yafolding-mode so...
(let ((map global-map))
  (define-key map (kbd "C-c f") #'yafolding-toggle-element))

(add-hook 'prog-mode-hook
          (lambda () (progn
		       (yafolding-mode)
                       ; private commments is unrelated
		       (private-comments-mode)
		       )))

(defun sg-toggle-fold ()
  "Toggle code folding according to indentation of current line."
  (interactive)
  (set-selective-display
   (if selective-display
       nil
     (save-excursion
       (back-to-indentation)
       (1+ (current-column))))))

;; avy (jumping to visible text using a char-based decision tree.)
(map!
 :nv "C-f" #'avy-goto-char
 :nv "C-s" #'avy-goto-char-2
 :nv "C-d" #'avy-goto-line
 )

;; dired: hide user permission details
(defun masu-dired-mode-setup ()
	"hook for 'dired-mode'"
	(dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'masu-dired-mode-setup)

(with-eval-after-load "private-comments-mode"
  (set-face-background 'private-comments-face "#527568")
  (set-face-foreground 'private-comments-face "#FFFFFF"))

(setq private-comments-mode-display-warnings nil)

(setq org-fold-core-style 'text-properties)
(after! evil
   (evil-select-search-module 'evil-search-module 'isearch))

(defun my/org-tab-conditional ()
  (interactive)
  (if (yas-active-snippets)
      (yas-next-field-or-maybe-expand)
    (org-cycle)))

(map! :after evil-org
      :map evil-org-mode-map
      :i "<tab>" #'my/org-tab-conditional)

  (setq org-export-with-sub-superscripts nil)

  ; KEYWORDS
  (setq
    org-todo-keywords
    '((sequence "TODO(t)" "INPROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")
    (sequence "[ ](T)" "|" "[X](D)")
    (sequence "|" "OKAY(o)" "YES(y)" "NO(n)"))

    org-todo-keyword-faces '(
    ("TODO"    :foreground "#7c7c75" :weight normal :underline t)
    ("WAITING"  :foreground "#9f7efe" :weight normal :underline t)
    ("INPROGRESS"  :foreground "#0098dd" :weight normal :underline t)
    ("DONE"    :foreground "#50a14f" :weight normal :underline t)
    ("CANCELLED"  :foreground "#ff6480" :weight normal :underline t)
    )
  )

(setq
  org-agenda-files '("~/Documents/notes/"
                     "~/.config/org/")
  ; DEBATING if ^^ and vvv should be the same directory
  ; org-directory needs to be set before org loads
  org-directory "~/.config/org/"
)

; pretty bullets in org-mode
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

; enable shift selection
(setq org-support-shift-select t)

(require 'org-mouse)

(use-package evil-org
  :after (evil org)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme '(navigation insert textobjects additional calendar))))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(defun image-url-overlays ()
  "Put image overlays on remote image urls."
  (interactive)
  (loop for image-url in (org-element-map (org-element-parse-buffer) 'link
               (lambda (link)
                 (when (string= "image-url" (org-element-property :type link))
                   link)))
    do
    (let* ((path (org-element-property :path image-url))
           (ov (make-overlay (org-element-property :begin image-url)
                 (org-element-property :end image-url)))
           (img (create-image (expand-file-name
                   (concat (md5 path)
                       "."
                       (file-name-extension
                        path))
                   temporary-file-directory))))
      (overlay-put ov 'display img)
      (overlay-put ov 'image-url t))))

(defun image-url-clear-overlays ()
  "Remove overlays on image-urls."
  (interactive)
  (require 'ov)
  (ov-clear 'image-url))

; enable table of contents generation in org-mode
(if (require 'toc-org nil t)
    (progn
      (add-hook 'org-mode-hook 'toc-org-mode))
      ;(add-hook 'markdown-mode-hook 'toc-org-mode))
      ;; enable in markdown, too
      ; disabled because it thinks markdown-mode-map is a void
      ; variable
      ;(add-hook 'markdown-mode-hook 'toc-org-mode))
      ;(define-key markdown-mode-map (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point)
  (warn "toc-org not found"))

; teach org-babel about Raku
; ⚠ note that this path is to my local clone of the ob-raku repo.
(let ((ob-raku-el "~/workspace/reference/emacs/ob-raku/ob-raku.el"))
 (when (file-exists-p ob-raku-el)
    (load-file ob-raku-el)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((shell . t)
       (raku . t))
     )
   )
)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t))
 )

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

; add wc-mode to doom modeline
(setq wc-modeline-format "words: %tw") ; simpler output than the default
(add-to-list 'global-mode-string '("" wc-buffer-stats))

(global-set-key (kbd "C-c a") 'org-agenda)

(setq
  org-default-notes-file (concat org-directory "notes.org")
  +org-capture-notes-file (concat org-directory "notes.org")
  ; use denote instead for journal stuff
  +org-capture-notes-file (concat org-directory "journal.org")
  +org-capture-todo-file (concat org-directory "todo.org")

  ; org-log-done adds a timestamp when marking a todo item as done
  org-log-done t
)

(with-eval-after-load 'ox
  (require 'ox-hugo)
  (require 'ox-gfm nil t)
  (require 'ox-md)
  (require 'ox-clip)
  (require 'ox-publish)
  (require 'ox-slack))

(with-eval-after-load 'org
  (org-link-set-parameters
   "relref"
   :complete (lambda ()
               (concat
                "relref:"
                (file-name-nondirectory (read-file-name "File: "))
                )
               )

   :export (lambda (path description backend)
             (format "[%s]({{< relref %s >}})" description path  )
        )
   )

  (org-link-set-parameters
   "ref"
   :complete (lambda ()
               (concat
                "ref:"
                (file-name-nondirectory (read-file-name "File: "))
                )
               )

   :export (lambda (path description backend)
             (format "[%s]({{< ref %s >}})" description path  )
        )
   )

)

(with-eval-after-load 'org
	;; org-hugo blogging things
	(setq time-stamp-active t
		time-stamp-start "#\\+hugo_lastmod:[ \t]*"
		time-stamp-end "$"
		time-stamp-format "%04Y-%02m-%02d"
                org-hugo-auto-set-lastmod t)
	(add-hook 'before-save-hook 'time-stamp nil)
	(add-to-list
		'org-src-lang-modes '("plantuml" . plantuml))
	(org-add-link-type
		"image-url"
		(lambda (path)
		(let ((img (expand-file-name
				(concat (md5 path) "." (file-name-extension path))
				temporary-file-directory)))
		(if (file-exists-p img)
		(find-file img)
			(url-copy-file path img)
			(find-file img)))))

  )

;; Enable plantuml-mode for PlantUML files
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

(setq plantuml-set-exec-mode "executable"); because babel needs the jar i think

;; Sample jar configuration
(setq plantuml-jar-path "/opt/homebrew/opt/plantuml/libexec/plantuml.jar")
(setq plantuml-default-exec-mode 'jar)

;; active Org-babel languages
(org-babel-do-load-languages 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)))

(setq org-plantuml-jar-path "/opt/homebrew/opt/plantuml/libexec/plantuml.jar")

(add-hook 'org-mode-hook 'org-appear-mode)

(setq
    ; Non-nil mean font-lock should hide the emphasis marker characters.
    ; e.g. / / for italics disappear
    org-hide-emphasis-markers t
)

(defun insert-entity (character)
        "Insert the org entity (if any) that corresponds to the typed character"
        (interactive "sEnter character: ")

        ; uses the code in custom/maybe-insert-org-entity.el
        (setq entity-name (modi/org-entity-get-name character))
        (if entity-name
          (progn
                (setq entity-name (concat "\\" entity-name "{}"))
                (insert entity-name)
                (message (concat "Inserted `org-entity' "
                                (propertize entity-name
                                        'face 'font-lock-function-name-face)
                                " for the symbol "
                                (propertize pressed-key
                                        'face 'font-lock-function-name-face)
                                ".")))
          (message "Unable to find the org-entity for %s"
                           character)
          )
    )

;display inline images and cache them
  (setq org-display-remote-inline-images 'cache)

(setq
  org-image-actual-width nil
  ; see auto-image-resize function below which will override this
  ; related #+STARTUP: inlineimages
)

 (defun org-image-resize (frame)
   (when (derived-mode-p 'org-mode)
       (if (< (window-total-qwidth) 80)
       (setq org-image-actual-width (window-pixel-width))
     (setq org-image-actual-width (* 80 (window-font-width))))
       (org-redisplay-inline-images)))
 (add-hook 'window-size-change-functions 'org-image-resize)

; a port of Tim Pope's surround.vim
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

; BEGIN DENOTE STUFF
(require 'denote)

(setq denote-directory (expand-file-name "~/Documents/notes/"))

(setq denote-known-keywords '("daily" "todo" "project"))
(setq denote-infer-keywords t)
(setq denote-sort-keywords t)
;; We allow multi-word keywords by default.  The author's personal
;; preference is for single-word keywords for a more rigid workflow.
(setq denote-allow-multi-word-keywords nil)

(setq denote-file-type nil) ; Org is the default, set others here
(setq denote-prompts '(title keywords))
(setq denote-date-format nil) ; read doc string

(setq denote-link-fontify-backlinks t)
(add-hook 'text-mode-hook #'denote-fontify-links-mode-maybe)
(setq denote-dired-rename-expert nil)



(setq denote-dired-directories
      (list denote-directory
            (thread-last denote-directory (expand-file-name "attachments"))
                                          (expand-file-name "~/Documents/notes")
            ))
(add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

(defun denote-dated-journal ()
  "Create an entry tagged 'journal', while prompting for a title."
  (interactive)
  (denote
   (denote--title-prompt)
   '("journal")))

(defun denote-journal ()
  "Create an entry tagged 'journal' with the date as its title."
  (interactive)
  (denote
   (format-time-string "%A %e %B %Y") ; format like Tuesday 14 June 2022
   '("journal"))) ; multiple keywords are a list of strings: '("one" "two")

(setq denote-org-capture-specifiers "%l\n%i\n%?")
(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
               '("n" "New note (with Denote)" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)))

(defun show-denote-dir ()
  "open an dired window on the default denote directory"
  (interactive) ; required to make it accessible via keybdingings
  ;; (split-window-vertically)
  ;; (other-window 1)
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
; END DENOTE STUFF

;;------------- ELIXIR
; highlight inline LiveView templates
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

;;------------- LUA
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;;------------- RACKET
(defun raco-fmt ()
  "format with raco fmt"
  (when (eq major-mode 'racket-mode)
    (shell-command-to-string (format "raco fmt -i --width 80 %s" buffer-file-name))))

(add-hook 'after-save-hook #'raco-fmt)

;;------------- Raku
(define-auto-insert
	'("\\.rakumod\\'" . "Raku module skeleton")
	'raku-module-skeleton)
(define-auto-insert
	'("\\.raku\\'" . "Raku script skeleton")
	'raku-script-skeleton)

;;------------- RUBY
(after! 'ruby-mode
  (require 'ruby-tools))
(setq-default flycheck-disabled-checkers '(ruby-rubocop ruby-reek))

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setq magit-blame-styles
           '((margin
              (margin-width . 32)
              (margin-format . ("%C %a %f"))
              (margin-face . magit-blame-margin)
              (margin-body-face . magit-blame-dimmed)
              (show-message . t))))

; ediff stuff
(require 'ediff)
;; don't start another frame
;; this is done by default in preluse
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; put windows side by side
(setq ediff-split-window-function (quote split-window-horizontally))
;;revert windows on exit - needs winner mode
(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)
