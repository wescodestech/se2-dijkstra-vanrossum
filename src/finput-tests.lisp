(include-book "finput")
(include-book "testing" :dir :teachpacks)
(include-book "doublecheck" :dir :teachpacks)

; chrs->strings test suite
(check-expect (chrs->strings '(#\t #\e #\s #\t #\i #\d #\o #\u #\t))
                             '("t" "e" "s" "t" "i" "d" "o" "u" "t"))

(check-expect (chrs->strings '())
              nil)

(check-expect (chrs->strings '(#\return))
              '("\r"))

; chrs-matrix->strings-matrix test suite
(check-expect (chrs-matrix->strings-matrix '((#\t #\e #\s) (#\t #\i #\d) (#\o #\u #\t)))
              '(("t" "e" "s") ("t" "i" "d") ("o" "u" "t")))

(check-expect (chrs-matrix->strings-matrix '())
              nil)

(check-expect (chrs-matrix->strings-matrix '((#\a) (#\b) (#\c #\d)))
              '(("a") ("b") ("c" "d")))

; words-hints test suite
(check-expect (words-hints '((#\w #\o #\r #\d #\1) (#\h #\i #\n #\t #\1) (#\w #\o #\r #\d #\n) (#\h #\i #\n #\t #\n)))
              '(("word1" "hint1") ("wordn" "hintn")))

(check-expect (words-hints '((#\b #\l #\a #\h) (#\l #\o #\n #\g #\h #\i #\n #\t) (#\a #\b) (#\b #\l #\a #\h #\2)))
              '(("blah" "longhint") ("ab" "blah2")))

(check-expect (words-hints '())
              nil)