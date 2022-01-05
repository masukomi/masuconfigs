;;; Compiled snippets and support files for `ruby-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'ruby-mode
                     '(("sterr" "STDERR.puts(\"XXX $0\")\n" "sterr" t nil nil "/Users/kayrhodes/.doom.d/snippets/ruby-mode/sterr" nil "")
                       ("rspec_d" "require 'rails_helper'\n\nRSpec.describe  ${1:ClassName} do\n\nend" "RSpec.describe ..." nil nil nil "/Users/kayrhodes/.doom.d/snippets/ruby-mode/rspec_d" nil nil)
                       ("dfile" "File.write(\"debugging.log\", \"XXX $0\\n\", mode: \"a\")\n" "debuging log file" nil nil
                        ((yas/indent-line 'auto)
                         (yas/wrap-around-region 't))
                        "/Users/kayrhodes/.doom.d/snippets/ruby-mode/dfile" nil nil)
                       ("byeb" "require 'byebug'; byebug" "byebug" nil nil nil "/Users/kayrhodes/.doom.d/snippets/ruby-mode/byebug" nil nil)))


;;; Do not edit! File generated at Fri Dec 10 10:30:12 2021
