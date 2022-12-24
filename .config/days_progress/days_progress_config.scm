; copy me to ~/.config/days_progress/days_progress_config.scm
; then edit the numbers and labels to what you need
; make sure the hours are in 24 hr time E.g. 6PM is hour 18
; google for What is my UTC time zone offset?
; if you don't know what yours is

; What is the UTC offset for my current time zone?
(define my-utc-offset -4)

; What hour of the day do I consider the "start" (in my time zone)?
(define start-hour-local 9)

; What hour of the day do I consider the "end" of _my_ day
(define my-end-hour-local 17)

; What hour of the day do I consider the "end" (in my time zone)?
(define end-hour-local 21)

(define day-cutover-hour-local 4)

; When does the new "day" start? When working late, midnight
; is rarely the "end" of the "day". Pick a time that's
; later than you would ever reasonably stay up. I've chosen 4AM. Must be < start-hour-local


; labels: These are for display only. I use
; "9 EDT" and "6 PST" because I start work at 9 AM EDT
; and my coworkers _end_ their day at 6 PM PST
; I could also say "New York" and "California" if 
; that felt better.
; If you want _no_ labels to be displayed use 
; empty strings. E.g. ""

; What label do I want shown at the start of the output?
(define start-hour-label "9 EDT")
; What label do I want shown at the end of the output?
(define end-hour-label "6 PDT")

