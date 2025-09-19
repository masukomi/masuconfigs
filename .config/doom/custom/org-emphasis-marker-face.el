; original code found here:
; https://www.reddit.com/r/emacs/comments/eipbvk/org_emphasis_marker_face/
; written by Matthew Newton https://www.mnewton.com/
; then merged with the latest version of the
; core org-do-emphasis-faces function as of Sept 19, 2025
; by masukomi: https://masukomi.org
(defface org-emphasis-marker '((t (:inherit shadow)))
    "Face for Org emphasis markers"
    :group 'org-faces)

(defun org-do-emphasis-faces (limit)
  "Run through the buffer and emphasize strings."
  (require 'org-macs)
  (require 'org-compat)

  (let ((quick-re (format "\\([%s]\\|^\\)\\([~=*/_+]\\)"
              (car org-emphasis-regexp-components))))
    (catch :exit
      (while (re-search-forward quick-re limit t)
    (let* ((marker (match-string 2))
           (verbatim? (member marker '("~" "="))))
      (when (save-excursion
          (goto-char (match-beginning 0))
          (and
           ;; Do not match table hlines.
           (not (and (equal marker "+")
                 (org-match-line
                  "[ \t]*\\(|[-+]+|?\\|\\+[-+]+\\+\\)[ \t]*$")))
           ;; Do not match headline stars.  Do not consider
           ;; stars of a headline as closing marker for bold
           ;; markup either.
           (not (and (equal marker "*")
                 (save-excursion
                   (forward-char)
                   (skip-chars-backward "*")
                   (looking-at-p org-outline-regexp-bol))))
           ;; Match full emphasis markup regexp.
           (looking-at (if verbatim? org-verbatim-re org-emph-re))
           ;; Do not span over paragraph boundaries.
           (not (string-match-p org-element-paragraph-separate
                    (match-string 2)))
           ;; Do not span over cells in table rows.
           (not (and (save-match-data (org-match-line "[ \t]*|"))
                 (string-match-p "|" (match-string 4))))))
        (pcase-let ((`(,_ ,face ,_) (assoc marker org-emphasis-alist))
            (m (if org-hide-emphasis-markers 4 2)))
          (font-lock-prepend-text-property
           (match-beginning m) (match-end m) 'face face)
          (when verbatim?
            (org-remove-flyspell-overlays-in
             (match-beginning 0) (match-end 0))
            (remove-text-properties (match-beginning 2) (match-end 2)
                        '(display t invisible t intangible t)))
          (add-text-properties (match-beginning 2) (match-end 2)
                   '(font-lock-multiline t org-emphasis t))

          ;; Begin new code
          ; via https://www.reddit.com/r/emacs/comments/eipbvk/org_emphasis_marker_face/
          (font-lock-prepend-text-property
           (match-beginning 3) (match-end 3) 'face 'org-emphasis-marker)
          (font-lock-prepend-text-property
           (match-end 4) (match-beginning 5) 'face 'org-emphasis-marker)
          ;; End new code


          (when (and org-hide-emphasis-markers
                 (not (org-at-comment-p)))
            (add-text-properties (match-end 4) (match-beginning 5)
                         '(invisible t))
            ;; https://orgmode.org/list/8b691a7f-6b62-d573-e5a8-80fac3dc9bc6@vodafonemail.de
            (org-rear-nonsticky-at (match-beginning 5))
            (add-text-properties (match-beginning 3) (match-end 3)
                         '(invisible t))
                    ;; FIXME: This would break current behavior with point
                    ;; being adjusted before hidden emphasis marker when
                    ;; using M-b.  A proper fix would require custom
                    ;; syntax function that will mark emphasis markers as
                    ;; word constituents where appropriate.
                    ;; https://orgmode.org/list/87edl41jf0.fsf@localhost
                    ;; (org-rear-nonsticky-at (match-end 3))
                )
          (throw :exit t))))))))
