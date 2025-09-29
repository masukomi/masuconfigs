; a stupid hack because I'll never normally use a "verse" block
; but it's one of the few you can customize the styling for. So, I'm repurposing them.
(with-eval-after-load 'org
  (set-face-attribute 'org-verse nil
                      :foreground "#c0abfe"
                      :background "#333333"
        )
  )
