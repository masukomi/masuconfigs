;;; custom/maybe-insert-org-entity.el -*- lexical-binding: t; -*-
;
; Functions to easily find and insert org-entities into an org-mode document.
; for example, if you want to type *shrug* without having the asterisks be
; used to bold, you should insert the org-entitiy that corresponds to the
; asterisk character.
;
; With these functions the user does not need to remember the org entities
; for various symbols. It inserts the org entity (if available) when user
; does C-u SYMBOL; works for C-u *, C-u /, C-u =, etc.
;
; Originally written by Katushal Modi
; https://emacs.stackexchange.com/users/115/kaushal-modi
; Originally found here: https://emacs.stackexchange.com/a/16746/30947
; Distributed under the CC BY-SA 3.0 license
; https://creativecommons.org/licenses/by-sa/3.0/

(defun modi/org-entity-get-name (char)
  "Return the entity name for CHAR. For example, return \"ast\" for *."
  (let ((ll (append org-entities-user
                    org-entities))
        e name utf8)
    (catch 'break
      (while ll
        (setq e (pop ll))
        (when (not (stringp e))
          (setq utf8 (nth 6 e))
          (when (string= char utf8)
            (setq name (car e))
            (throw 'break name)))))))

(defun modi/org-insert-org-entity-maybe (&rest args)
  "When the universal prefix C-u is used before entering any character,
    insert the character's `org-entity' name if available.

    If C-u prefix is not used and if `org-entity' name is not available, the
    returned value `entity-name' will be nil."
  ;; It would be fine to use just (this-command-keys) instead of
  ;; (substring (this-command-keys) -1) below in emacs 25+.
  ;; But if the user pressed "C-u *", then
  ;;  - in emacs 24.5, (this-command-keys) would return "^U*", and
  ;;  - in emacs 25.x, (this-command-keys) would return "*".
  ;; But in both versions, (substring (this-command-keys) -1) will return
  ;; "*", which is what we want.
  ;; http://thread.gmane.org/gmane.emacs.orgmode/106974/focus=106996
  (let ((pressed-key (substring (this-command-keys) -1))
        entity-name)
    (when (and (listp args) (eq 4 (car args)))
      (setq entity-name (modi/org-entity-get-name pressed-key))
      (when entity-name
        (setq entity-name (concat "\\" entity-name "{}"))
        (insert entity-name)
        (message (concat "Inserted `org-entity' "
                         (propertize entity-name
                                     'face 'font-lock-function-name-face)
                         " for the symbol "
                         (propertize pressed-key
                                     'face 'font-lock-function-name-face)
                         "."))))
    entity-name))

;; Run `org-self-insert-command' only if `modi/org-insert-org-entity-maybe'
;; returns nil.
(advice-add 'org-self-insert-command :before-until #'modi/org-insert-org-entity-maybe)
