(in-package "ACL2")

(include-book "check_board")
(include-book "testing" :dir :teachpacks)
(include-book "doublecheck" :dir :teachpacks)

; first ensure our get_n covers all cases
(check-expect (get_n 0 '()) nil)
(check-expect (get_n 1 '()) nil)
(check-expect (get_n 0 '(9)) 9)
(check-expect (get_n 1 '(9)) nil)
(check-expect (get_n 0 '(9 3 8 2)) 9)
(check-expect (get_n 2 '(9 3 8 2)) 8)
(check-expect (get_n 3 '(9 3 8 2)) 2)
(check-expect (get_n 4 '(9 3 8 2)) nil)
(check-expect (get_n 20 '(9 3 8 2)) nil)
(check-expect (get_n -1 '(9 3 8 2)) nil)

(defproperty check-get_n :repeat 100
  (lim          :value (random-natural)
   n            :value (random-between 1 lim)
   xs           :value (random-list-of (random-between 1 3) :size n)
   check_n      :value (random-between 1 n))
  (implies (and (< check_n (length xs)))
           (let* ((output (get_n check_n xs)))
                   (or (equal output 1)
                       (equal output 2)
                       (equal output 3))
             )))

(defproperty check-get_n-outside-range :repeat 100
  (lim          :value (random-natural)
   n            :value (random-between 1 lim)
   xs           :value (random-list-of (random-between 1 3) :size n)
   check_n      :value (random-between n (+ 10 n)))
  (implies (and (> check_n (length xs)))
           (let* ((output (get_n check_n xs)))
                   (equal nil output))
             ))

; then the horizontal tests
(check-expect 
 (check-one-solution-horizontal 1 0 #\r 0 0 '(#\r #\r #\r)) #\r)
(check-expect 
 (check-one-solution-horizontal 0 4 #\r 0 4 '(#\r #\r #\r)) #\r)
(check-expect 
 (check-one-solution-horizontal 1 3 #\r 0 8 '(#\r #\r #\r)) nil)
(check-expect 
 (check-one-solution-horizontal 8 4 #\r 0 4 '(#\r #\r #\r)) nil)

(check-expect 
 (check-one-solution-horizontal 0 0 #\r 0 0 '(#\a #\r #\r)) nil)
(check-expect 
 (check-one-solution-horizontal 1 4 #\r 0 4 '(#\a #\r #\a)) #\r)
(check-expect 
 (check-one-solution-horizontal 5 4 #\r 4 4 '(#\a #\r #\a)) #\r)

; vertical tests
(check-expect 
 (check-one-solution-vertical 0 1 #\r 0 0 '(#\r #\r #\r)) #\r)
(check-expect 
 (check-one-solution-vertical 4 4 #\r 4 4 '(#\r #\r #\r)) #\r)
(check-expect 
 (check-one-solution-vertical 1 3 #\r 0 3 '(#\r #\r #\r)) nil)
(check-expect 
 (check-one-solution-vertical 4 4 #\r 4 18 '(#\r #\r #\r)) nil)

(check-expect 
 (check-one-solution-vertical 0 0 #\r 0 0 '(#\a #\r #\r)) nil)
(check-expect 
 (check-one-solution-vertical 4 1 #\r 4 0 '(#\a #\r #\a)) #\r)
(check-expect 
 (check-one-solution-vertical 4 5 #\r 4 4 '(#\a #\r #\a)) #\r)

; reversing up and down test
(check-expect (check-one-solution 0 5 #\o '("wordses" (0 0) (0 7))) nil)
(check-expect (check-one-solution 0 1 #\o '("wordses" (0 0) (0 7))) #\o)
(check-expect (check-one-solution 0 1 #\o '("wordses" (0 7) (0 0))) nil)
(check-expect (check-one-solution 0 5 #\o '("wordses" (0 7) (0 0))) #\o)

; reversing left and right
(check-expect (check-one-solution 5 2 #\o '("wordses" (0 2) (9 2))) nil)
(check-expect (check-one-solution 1 2 #\o '("wordses" (0 2) (9 2))) #\o)
(check-expect (check-one-solution 1 2 #\o '("wordses" (9 2) (0 2))) nil)
(check-expect (check-one-solution 5 2 #\o '("wordses" (9 2) (0 2))) #\o)

; final big test
(check-expect (check-solution 5 2 #\o '(("wordses" (9 2) (0 2)))) #\o)
(check-expect (check-solution 5 2 #\o '(("zzzzzzz" (9 2) (0 2))
                                        ("wordses" (9 2) (0 2))
                                        ("aaaaaaa" (9 2) (0 2)))) #\o)
(check-expect (check-solution 5 2 #\o '(("zzzzzzz" (9 2) (0 2))
                                        ("bbbbbbb" (9 2) (0 2))
                                        ("wordses" (9 2) (0 2)))) #\o)
(check-expect (check-solution 5 2 #\o '(("wordses" (9 2) (0 2))
                                        ("bbbbbbb" (9 2) (0 2))
                                        ("ccccccc" (9 2) (0 2)))) #\o)
(check-expect (check-solution 5 2 #\o '(("qqqqqqq" (9 2) (0 2))
                                        ("bbbbbbb" (9 2) (0 2))
                                        ("ccccccc" (9 2) (0 2)))) nil)

(check-expect (check-solution-entry 5 2 "o" '(("zzzzzzz" (9 2) (0 2))
                                        ("wordses" (9 2) (0 2))
                                        ("aaaaaaa" (9 2) (0 2)))) #\o)

(check-expect (check-one-solution 12 10 #\. '("your" (7 10) (11 10))) nil)

(check-expect (check-solution-entry 12 10 "." 
 '(("Put" (5 2) (2 2)) 
   ("your" (7 10) (11 10)) 
   ("words" (10 6) (10 1)) 
   ("here" (5 3) (5 7)))) nil)