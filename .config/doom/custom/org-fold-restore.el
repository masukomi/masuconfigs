;;; org-fold-restore.el --- Auto restore folding state of org buffers  -*- lexical-binding: t; -*-

;; NOTE only stores state if buffer has been modified and saved

(defun org-fold-store-state ()
  (let ((state (org-fold-get-regions)))
    (with-temp-file (concat buffer-file-name ".fold")
      (prin1 state (current-buffer)))))

;; TODO move .fold files into temporary-file-directory
;; I'm thinking generate a sha256 hash of the
;; ?absolute path? to the current file
;; then save <hash>.fold in temporary-file-directory/folds/
;;
;; BETTER
;; in the following I think teh files in the git repo
;; are a backup of the one in temporary-file-directory/folds/
;; on save one in the temp directory gets modified.
;; on commit it gets copied into git.
;;
;; if it is a file in a git repo
;; add an post-commit hook that
;; - for each new / modified .org file in the commit
;;   - creates <relative path>.fold
;;   - shoves it in a fold states branch
;;   - commits it
;; for each deleted .org file in the commit
;;   - sees if <relative path>.fold exists in fold states
;;   - deletes it if it does
;; for each moved .org file in the commit
;;   - recalculates the name of the <relative path>.fold file
;;   - deletes the old one
;;   - adds the new / current one
;;
;; BETTER BETTER
;; shove the metadata in the private comments server
;; and read it back from there
(defun org-fold-restore-state ()
  (let* ((file-base buffer-file-name)
         (file-dot-fold (concat file-base ".fold")))
    (if (file-exists-p file-dot-fold)
        (progn
               (let ((state
                      (with-temp-buffer
                        (insert-file-contents-literally file-dot-fold)
                        (read (current-buffer)))))
                  (org-fold-regions state)
                  (goto-char (point-min))
                 )))))


(add-hook 'org-mode-hook 'org-fold-activate)

(defun org-fold-activate ()
  ;; call the restore function
  (org-fold-restore-state)
  ;; and add a local hook, to save the foldstates
  ;; (only is buffer was saved, see `orgfold-kill-buffer')
  (add-hook 'kill-buffer-hook 'org-fold-kill-buffer nil t))

(defun org-fold-kill-buffer ()
  ;; don't save folding info for unsaved buffers
  (unless (buffer-modified-p)
    (org-fold-store-state)))
