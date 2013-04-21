;This file contains unit tests for Hill Climbing Solver.

(include-book "hillclimbingsolver")
(include-book "testing" :dir :teachpacks)
(include-book "doublecheck" :dir :teachpacks)

;char-concat-count tests
(check-expect (char-concat-count 
               (list (list "c" "a" "t")
                     (list "d" "o" "g")
                     (list "t" "u" "r" "t" "l" "e")
                     ( list "s" "n" "a" "k" "e")))
             (list(list "c" "cat" 3) 
                  (list "d" "dog" 3) 
                  (list "t" "turtle" 6)
                  (list"s" "snake" 5))) 

(check-expect (char-concat-count (list nil))
                            (list (list nil "" 0)))

;match-left-to-right
(check-expect (match-left-to-right 0 2 (list "a" "b" "c" "d" "e") (list "cd" 2)) 
             (list 0 2 "right" 1))

(check-expect (match-left-to-right 0 2 nil (list "cd" 2)) 
             nil)

;match-right-to-left
(check-expect (match-right-to-left 0 2 (reverse (list "a" "b" "c" "d" "e")) (list "cb" 2)) 
             (list 0 2 "left" 1))

(check-expect (match-right-to-left 0 3 (list "s" "a" "s" "d" "g") (list "cd" 2)) 
             nil)

;match-up-to-down
(check-expect (match-up-to-down 0 1 (transpose-hill(list (list "s" "c" "g" "j" ) (list "s" "d" "y" "u" )) 4 0) (list "cd" 2))
              (list 0 1 "down" 1))

(check-expect (match-up-to-down 0 2 (transpose-hill(list (list "s" "c" "g" "j" ) (list "s" "d" "y" "u" )) 4 0) (list "gy" 2))
              (list 0 2 "down" 1))

;match-down-to-up
(check-expect (match-down-to-up 1 2 (transpose-hill
                                            (nthrdc-hill 
                                              (- (len (nthcdr (- (+ 1 1) 
                                                       (cadr (list "gc" 2))) 
                                                              (list (list "a" "b" "c" "d") (list "e" "f" "g" "h"))))
                                                                       (cadr (list "gc" 2)))
                                                                    (nthcdr (- (+ 1 1) (cadr (list "gc" 2)))
                                                                            (list (list "a" "b" "c" "d") (list "e" "f" "g" "h"))))
                                                            (len (car (list (list "a" "b" "c" "d")
                                                                            (list "e" "f" "g" "h"))))
                                                            0)
                                             (list "gc" 2)) 
              (list 1 2 "up" 1)
              )

;localize
(check-expect (car(localize 0 2  (list (list "a" "b" "a" "d") 
                                   (list "e" "f" "g" "h"))
                        (list (list "ag" 2)
                              (list "ac" 2))))
              (list 0 2 "down" 1)
              )

(check-expect (car (localize 1 2  (list (list "a" "b" "g" "d") 
                                   (list "e" "f" "a" "h"))
                        (list (list "ag" 2)
                              (list "ac" 2))))
              (list 1 2 "up" 1)
              )

(check-expect (car (localize 1 1  (list (list "a" "b" "g" "d") 
                                   (list "e" "f" "a" "h"))
                        (list (list "ag" 2)
                              (list "fa" 2))))
              (list 1 1 "right" 1)
              )

(check-expect (car (localize 0 3  (list (list "a" "b" "g" "d") 
                                   (list "e" "f" "a" "h"))
                        (list (list "dg" 2)
                              (list "fa" 2))))
              (list 0 3 "left" 1)
              )

;char-match
(check-expect (char-match "h" (list (list "g" "grtd" 4)(list "h" "hyd" 3)(list "k" "kjg" 3)(list "h" "hhu" 3)))
              (list (list "hyd" 3) (list "hhu" 3)))

(check-expect (char-match "z" (list (list "g" "grtd" 4)(list "h" "hyd" 3)(list "k" "kjg" 3)(list "h" "hhu" 3)))
              nil)

;hill-climbing-solver (encompasses search-and-localize and row-char-search, along with everything else)
(check-expect (hill-climbing-solver(list(list "w" "b" "y" "i" "g" "g" "d" "a" "m") ;example game board
                                        (list "t" "a" "c" "d" "i" "q" "p" "u" "r") 
                                        (list "p" "o" "t" "o" "c" "f" "d" "f" "o") 
                                        (list "d" "o" "g" "t" "s" "c" "a" "c" "w")
                                        (list "r" "p" "p" "t" "w" "w" "r" "g" "o")
                                        (list "e" "p" "t" "o" "g" "o" "x" "z" "a")
                                        (list "w" "f" "t" "r" "q" "r" "w" "d" "b")
                                        (list "e" "d" "t" "r" "i" "r" "t" "f" "p")
                                        (list "h" "d" "t" "a" "r" "a" "t" "f" "p")
                                        (list "e" "w" "t" "p" "a" "p" "r" "o" "t")
                                        (list "i" "i" "u" "u" "q" "s" "o" "k" "j")
                                        (list "a" "u" "d" "d" "f" "j" "h" "w" "q")
                                        (list "j" "d" "f" "a" "g" "l" "f" "g" "d")
                                        (list "g" "d" "w" "o" "c" "n" "o" "o" "r")
                                        (list "d" "s" "j" "s" "k" "m" "x" "h" "x")
                                        (list "p" "i" "g" "q" "q" "a" "d" "c" "z"))
                    
                    (list (list "c" "a" "t"); example word list 
                          (list "d" "o" "g") 
                          (list "p" "i" "g")
                          (list "r" "a" "t") ;rat appears twice
                          (list "p" "a" "r" "r" "o" "t") 
                          (list "s" "p" "a" "r" "r" "o" "w") 
                          (list "w" "o" "r" "m") 
                          (list "f" "o" "x") 
                          (list "h" "o" "g")
                          (list "c" "o" "w")
                          )
                    )
(list (list 1 2 "left" 2)
 (list 3 0 "right" 2)
 (list 3 8 "up" 3)
 (list 8 4 "right" 2)
 (list 8 4 "left" 2)
 (list 9 3 "up" 5)
 (list 10 5 "up" 6)
 (list 12 6 "down" 2)
 (list 13 4 "left" 2)
 (list 14 7 "up" 2)
 (list 15 0 "right" 2)
))

(check-expect (hill-climbing-solver(list(list "r" "i" "g" "h" "t" "f" "e" "l" "l") ;example game board
                                        (list "k" "a" "c" "d" "i" "q" "k" "u" "r") 
                                        (list "p" "o" "t" "o" "c" "f" "d" "f" "o") 
                                        (list "d" "o" "g" "t" "s" "t" "a" "c" "w")
                                        (list "r" "p" "p" "t" "w" "w" "r" "g" "o")
                                        (list "e" "p" "t" "o" "g" "o" "x" "z" "a")
                                        (list "w" "f" "t" "r" "q" "r" "w" "d" "b")
                                        (list "e" "d" "t" "r" "i" "r" "t" "f" "p")
                                        (list "h" "d" "t" "a" "r" "a" "t" "f" "p")
                                        (list "e" "l" "k" "f" "e" "l" "r" "o" "t")
                                        (list "i" "i" "u" "j" "q" "s" "o" "k" "j")
                                        (list "a" "u" "d" "d" "f" "j" "h" "w" "q")
                                        (list "d" "t" "e" "s" "t" "l" "f" "g" "d")
                                        (list "o" "d" "w" "o" "c" "n" "o" "o" "r")
                                        (list "w" "s" "j" "s" "k" "m" "x" "h" "p")
                                        (list "n" "i" "g" "q" "l" "j" "f" "e" "u"))
                    
                    (list (list "l" "e" "f" "t") 
                          (list "r" "i" "g" "h" "t")
                          (list "u" "p")
                          (list "d" "o" "w" "n")
                          (list "t" "e" "s" "t")
                           (list "d" "o" "g")
                           (list "c" "a" "t")
                          )
                    )
(list(list 0 0 "right" 4)
 (list 0 7 "left" 3)
 (list 3 0 "right" 2)
 (list 3 7 "left" 2)
 (list 12 0 "down" 3)
 (list 12 1 "right" 3)
 (list 15 8 "up" 1))
)

;properties

;char-concat-count always returns a list with the same number of elements passed in.
(defproperty char-concat-count-always-gives-list :repeat 2000
  (xs :value (random-list-of (random-list-of (random-string))))
    (implies (consp xs)
             (= (len xs) (len (char-concat-count xs)))))

;clean-results-hill will always return a list with less than or equal the number of elements than its original.
(defproperty clean-results-always-cleans :repeat 2000
  (xs :value (random-list-of (random-element-of (list nil nil "test" nil nil "test" nil "test"))))
  (implies (consp xs)
           (>= (len xs) (len (clean-results-hill xs)))))

      

