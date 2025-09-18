;;; ---------------------------------------------------------------------------
;;;  Fediverse “fedi:” link type
;;;
;;;  Teach org-mode how to recognize fediverse usernames like @username@domain
;;;  and create links from them when they're preceded by fedi:
;;;  Thus fedi:@me@example.com becomes a clickable link
;;;  to https://example.com/@me
;;;
;;;  Emulates org-mode's handling of mailto: links
;;;  to facilitate linking to Fediverse profiles.
;;;
;;;  - Recognizes fedi:@username@domain as link to a Fediverse profile
;;;  - Makes it a clickable link
;;;  - Links to https://domain/@username
;;; ---------------------------------------------------------------------------

;; Helper that turns `@user@domain.tld` into a real URL
(defun fedi--make-url (fedi-id)
  "Return the profile URL for a Fediverse ID in the form `@user@domain`."
  (when (string-match "@\\([^@]+\\)@\\([^@]+\\)" fedi-id)
    (let ((user   (match-string 1 fedi-id))
          (domain (match-string 2 fedi-id)))
      (format "https://%s/@%s" domain user))))

;; Function that opens the link when the user presses `C-c C-o`
(defun fedi--follow (path _)
  "Open the Fediverse profile that is stored in PATH."
  (browse-url (fedi--make-url path)))   ; `browse-url` is part of Emacs core

;; Export function – called when Org exports to HTML, LaTeX, etc.
(defun fedi--export (path _desc backend)
  "Return the exported URL for the fedi link."
  (let ((url (fedi--make-url path)))
    (cond
     ; <a href="https://example.com/@foo">@foo@example.com</a>
     ((eq backend 'html)  (format "<a href=\"%s\">%s</a>" url path))
     ((eq backend 'md)  (format "[%s](%s)" path url))
     ((eq backend 'markua)  (format "[%s](%s)" path url))
     (t url))))                                      ; plain string otherwise

;; Install the link type into Org
(after! org
  (org-link-set-parameters
   "fedi"
   :follow  #'fedi--follow          ; what to do when we follow the link
   :export  #'fedi--export))        ; how to export the link
