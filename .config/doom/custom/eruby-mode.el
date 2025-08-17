;;; eruby-mode.el --- minor mode for eRuby (.erb) template files

;; Copyright (C) 2015 Peter Eisentraut

;; Author: Peter Eisentraut <peter@eisentraut.org>
;; URL: https://github.com/petere/emacs-eruby-mode
;; Version: 0
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is an Emacs package with a minor mode for editing eRuby (.erb)
;; template files.
;;
;; This package is mainly intended for editing configuration file
;; templates in Chef and Puppet and the like, where the embedding
;; files can be of a wide variety (configuration files, YAML, shell
;; scripts, whatever) and the embedded Ruby is often very simple.  You
;; edit the file using its normal major mode, and this minor mode will
;; highlight the placeholder sections.  This package will
;; automatically set that up.

;;; Code:

(defgroup eruby-mode nil
  "Mode for eRuby template files"
  :group 'languages)

(defgroup eruby-mode-faces nil
  "Faces for highlighting eRuby template files"
  :prefix "eruby-mode-"
  :group 'eruby-mode-
  :group 'faces)

(defface eruby-standard-face
  '((t (:background "gray")))
  "Face used to highlight eRuby template snippets"
  :group 'eruby-mode-faces)

(defface eruby-comment-face
  '((t (:inherit font-lock-comment-face :background "gray")))
  "Face used to highlight eRuby template snippets"
  :group 'eruby-mode-faces)

(defvar eruby-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "%" 'eruby-mode-electric-percent)
    map)
    "Keymap for eRuby mode.")

(defun eruby-mode-electric-percent ()
  "Called when % is pressed."
  (interactive)
  (if (and electric-pair-mode
           (equal (char-before) ?<))
      (progn
        (insert "% %>")
        (backward-char 3))
    (insert "%")))

(defvar eruby-mode-font-lock-keywords
  '(("<%.*?%>" . '(0 'eruby-standard-face t))
    ("<%#.*?%>" . '(0 'eruby-comment-face t))))

;;;###autoload
(define-minor-mode eruby-mode
  "Minor mode for eRuby templates"
  :lighter "ERB"
  :keymap eruby-mode-map
  (if eruby-mode
      (font-lock-add-keywords nil eruby-mode-font-lock-keywords)
    (font-lock-remove-keywords nil eruby-mode-font-lock-keywords)))

;;;###autoload
(defconst eruby-mode-file-regexp "\\.erb\\'")

;;;###autoload
(add-to-list 'auto-mode-alist `(,eruby-mode-file-regexp ignore t))

;; https://stackoverflow.com/questions/13945782/emacs-auto-minor-mode-based-on-extension
;;;###autoload
(defun eruby-mode-auto-mode ()
  "Turn on eRuby mode for appropriate file extensions."
  (when buffer-file-name
    (when (string-match eruby-mode-file-regexp buffer-file-name)
      (eruby-mode 1))))

;;;###autoload
(add-hook 'find-file-hook #'eruby-mode-auto-mode)

;;; eruby-mode.el ends here
