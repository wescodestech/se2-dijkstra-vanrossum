;This file contains unit tests for Brute Force solver.
;Authored by Cezar Delucca


(include-book "bruteforcesolver")
(include-book "testing" :dir :teachpacks)
(include-book "doublecheck" :dir :teachpacks)

;concat-count tests
(check-expect (concat-count (list (list "d" "o" "g")(list "r" "a" "t")(list "s" "q" "u" "i" "r" "r" "e" "l")(list "s" "n" "a" "k" "e")))
                            (list (list "dog" 3) (list "rat" 3) (list "squirrel" 8) (list "snake" 5)))

(check-expect (concat-count (list nil))
                            (list (list "" 0)))

;nthrdc tests
(check-expect (nthrdc 3 (list "a" "b" "c" "d" "e" "f" "g")) (list "a" "b" "c" "d"))

(check-expect (nthrdc 300 (list "a" "b" "c" "d" "e" "f" "g")) nil)

(check-expect (nthrdc 3 nil)  nil)

;linear-search tests
;Note: only testing right direction since going left, up, down require other functions.
(check-expect (linear-search (list "rat" 3) (list (list "r" "p" "p" "r" "a" "t" "r" "g" "o")  
                                                  (list "e" "p" "t" "o" "g" "r" "a" "z" "a")) "right" -1)
              (list 0 3 "right" 2))

(check-expect (linear-search (list "rat" 3) (list(list "e" "p" "t" "o" "g" "r" "a" "z" "a")
                                                  (list "r" "p" "p" "r" "a" "t" "r" "g" "o")) "right" -1)
              (list 1 3 "right" 2))

;tests for search-left-to-right

(check-expect (search-left-to-right (list (list "w" "b" "y" "i" "g" "g" "d" "a" "w") 
                                          (list "m" "l" "i" "d" "i" "q" "p" "u" "o") 
                                          (list "p" "o" "t" "a" "c" "f" "d" "f" "r") 
                                          (list "d" "o" "g" "t" "s" "c" "a" "c" "m")
                                          (list "r" "p" "p" "t" "w" "w" "r" "g" "o")
                                          (list "e" "p" "t" "o" "g" "o" "x" "z" "a")
                                          (list "w" "f" "t" "r" "q" "r" "w" "d" "b")
                                          (list "e" "d" "t" "r" "i" "r" "t" "f" "p")
                                          (list "h" "d" "g" "a" "r" "a" "t" "f" "p")
                                          (list "e" "w" "t" "p" "a" "p" "r" "o" "t")
                                          (list "i" "i" "u" "u" "q" "s" "o" "k" "n")
                                          (list "a" "u" "q" "s" "y" "f" "p" "s" "p")
                                          (list "e" "f" "d" "a" "q" "z" "j" "d" "c") )
                                    (list (list "rat" 3) (list "dog" 3)))
              (list (list 8 4 "right" 2)
                    (list 3 0 "right" 2)
                    )
              );resulting vectors

(check-expect (search-left-to-right (list (list "w" "b" "y" "i" "g" "g" "d" "a" "w") 
                                          (list "m" "l" "i" "d" "i" "q" "p" "u" "o") 
                                          (list "p" "o" "t" "a" "c" "f" "d" "f" "r") 
                                          (list "d" "o" "g" "t" "s" "c" "a" "c" "m")
                                          (list "r" "p" "p" "t" "w" "w" "r" "g" "o")
                                          (list "e" "p" "t" "o" "g" "o" "x" "z" "a")
                                          (list "w" "f" "t" "r" "q" "r" "w" "d" "b")
                                          (list "e" "d" "t" "r" "i" "r" "t" "f" "p")
                                          (list "h" "d" "g" "a" "r" "a" "t" "f" "p")
                                          (list "e" "w" "t" "p" "a" "p" "r" "o" "t")
                                          (list "i" "i" "u" "u" "q" "s" "o" "k" "n")
                                          (list "a" "u" "q" "s" "y" "f" "p" "s" "p")
                                          (list "e" "f" "d" "a" "q" "z" "j" "d" "c") )
                                   nil)
             nil
              )

;reverse-matrix test
(check-expect (reverse-matrix (list (list "w" "b" "y" "i" "g" "g" "d" "a" "w") 
                                          (list "m" "l" "i" "d" "i" "q" "p" "u" "o") 
                                          (list "p" "o" "t" "a" "c" "f" "d" "f" "r") 
                                          (list "d" "o" "g" "t" "s" "c" "a" "c" "m")
                                          (list "r" "p" "p" "t" "w" "w" "r" "g" "o")
                                          (list "e" "p" "t" "o" "g" "o" "x" "z" "a")
                                          (list "w" "f" "t" "r" "q" "r" "w" "d" "b")
                                          (list "e" "d" "t" "r" "i" "r" "t" "f" "p")
                                          (list "h" "d" "g" "a" "r" "a" "t" "f" "p")
                                          (list "e" "w" "t" "p" "a" "p" "r" "o" "t")
                                          (list "i" "i" "u" "u" "q" "s" "o" "k" "n")
                                          (list "a" "u" "q" "s" "y" "f" "p" "s" "p")
                                          (list "e" "f" "d" "a" "q" "z" "j" "d" "c") ))
              
              (list (list "w" "a" "d" "g" "g" "i" "y" "b" "w")
                    (list "o" "u" "p" "q" "i" "d" "i" "l" "m")
                    (list "r" "f" "d" "f" "c" "a" "t" "o" "p")
                    (list "m" "c" "a" "c" "s" "t" "g" "o" "d")
                    (list "o" "g" "r" "w" "w" "t" "p" "p" "r")
                    (list "a" "z" "x" "o" "g" "o" "t" "p" "e")
                    (list "b" "d" "w" "r" "q" "r" "t" "f" "w")
                    (list "p" "f" "t" "r" "i" "r" "t" "d" "e")
                    (list "p" "f" "t" "a" "r" "a" "g" "d" "h")
                    (list "t" "o" "r" "p" "a" "p" "t" "w" "e")
                    (list "n" "k" "o" "s" "q" "u" "u" "i" "i")
                    (list "p" "s" "p" "f" "y" "s" "q" "u" "a")
                    (list "c" "d" "j" "z" "q" "a" "d" "f" "e"))
              )

;search-right-to-left test

(check-expect (search-right-to-left (list (list "w" "b" "y" "i" "g" "g" "d" "a" "w") 
                                          (list "m" "l" "i" "d" "i" "q" "p" "u" "o") 
                                          (list "p" "o" "t" "a" "c" "f" "d" "f" "r") 
                                          (list "d" "k" "g" "t" "s" "c" "a" "c" "m")
                                          (list "r" "p" "p" "t" "w" "w" "r" "g" "o")
                                          (list "e" "p" "t" "o" "g" "o" "x" "z" "a")
                                          (list "w" "f" "t" "r" "q" "r" "w" "d" "b")
                                          (list "e" "d" "t" "r" "i" "r" "t" "f" "p")
                                          (list "h" "d" "t" "a" "r" "a" "o" "f" "p")
                                          (list "e" "w" "t" "p" "a" "p" "r" "o" "t")
                                          (list "i" "i" "u" "u" "q" "g" "o" "d" "n")
                                          (list "a" "u" "q" "s" "y" "f" "p" "s" "p")
                                          (list "e" "f" "d" "a" "q" "z" "j" "d" "c") )
                                     
                                    (list (list "rat" 3) (list "dog" 3)))
              
              (list (list 8 4 "left" 2)
                    (list 10 7 "left" 2)
                    )
              )
              

(check-expect (search-right-to-left (list (list "w" "b" "y" "i" "g" "g" "d" "a" "w") 
                                          (list "m" "l" "i" "d" "i" "q" "p" "u" "o") 
                                          (list "p" "o" "t" "a" "c" "f" "d" "f" "r") 
                                          (list "d" "k" "g" "t" "s" "c" "a" "c" "m")
                                          (list "r" "p" "p" "t" "w" "w" "r" "g" "o")
                                          (list "e" "p" "t" "o" "g" "o" "x" "z" "a")
                                          (list "w" "f" "t" "r" "q" "r" "w" "d" "b")
                                          (list "e" "d" "t" "r" "i" "r" "t" "f" "p")
                                          (list "h" "d" "t" "a" "r" "a" "o" "f" "p")
                                          (list "e" "w" "t" "p" "a" "p" "r" "o" "t")
                                          (list "i" "i" "u" "u" "q" "g" "o" "d" "n")
                                          (list "a" "u" "q" "s" "y" "f" "p" "s" "p")
                                          (list "e" "f" "d" "a" "q" "z" "j" "d" "c") )
                                     
                                    nil)
              
              nil
              )


(check-expect (transpose (list            (list "w" "b" "y" "i" "g" "g" "d" "a" "w") 
                                          (list "m" "l" "i" "d" "i" "q" "p" "u" "o") 
                                          (list "p" "o" "t" "a" "c" "f" "d" "f" "r") 
                                          (list "d" "k" "g" "t" "s" "c" "a" "c" "m")
                                          (list "r" "p" "p" "t" "w" "w" "r" "g" "o")
                                          (list "e" "p" "t" "o" "g" "o" "x" "z" "a")
                                          (list "w" "f" "t" "r" "q" "r" "w" "d" "b")
                                          (list "e" "d" "t" "r" "i" "r" "t" "f" "p")
                                          (list "h" "d" "t" "a" "r" "a" "o" "f" "p")
                                          (list "e" "w" "t" "p" "a" "p" "r" "o" "t")
                                          (list "i" "i" "u" "u" "q" "g" "o" "d" "n")
                                          (list "a" "u" "q" "s" "y" "f" "p" "s" "p")
                                          (list "e" "f" "d" "a" "q" "z" "j" "d" "c") )
           9 0)
              
              
(list (list "w" "m" "p" "d" "r" "e" "w" "e" "h" "e" "i" "a" "e")
      (list "b" "l" "o" "k" "p" "p" "f" "d" "d" "w" "i" "u" "f")
      (list "y" "i" "t" "g" "p" "t" "t" "t" "t" "t" "u" "q" "d")
      (list "i" "d" "a" "t" "t" "o" "r" "r" "a" "p" "u" "s" "a")
      (list "g" "i" "c" "s" "w" "g" "q" "i" "r" "a" "q" "y" "q")
      (list "g" "q" "f" "c" "w" "o" "r" "r" "a" "p" "g" "f" "z")
      (list "d" "p" "d" "a" "r" "x" "w" "t" "o" "r" "o" "p" "j")
      (list "a" "u" "f" "c" "g" "z" "d" "f" "f" "o" "d" "s" "d")
      (list "w" "o" "r" "m" "o" "a" "b" "p" "p" "t" "n" "p" "c"))

)

;search-up-to-down (requires transpose)

(check-expect (search-up-to-down (transpose(list (list "w" "b" "y" "i" "g" "g" "d" "a" "w") 
                                   (list "m" "l" "i" "d" "i" "q" "p" "u" "o") 
                                   (list "p" "o" "t" "a" "c" "f" "d" "f" "r") 
                                   (list "d" "k" "g" "t" "s" "c" "a" "c" "m")
                                   (list "r" "p" "p" "t" "w" "w" "r" "g" "o")
                                   (list "e" "p" "t" "o" "g" "o" "x" "z" "a")
                                   (list "w" "f" "t" "r" "q" "r" "w" "d" "b")
                                   (list "e" "m" "t" "r" "i" "r" "t" "f" "p")
                                   (list "h" "o" "t" "a" "r" "a" "o" "o" "p")
                                   (list "e" "u" "t" "p" "a" "p" "r" "x" "t")
                                   (list "i" "s" "u" "u" "q" "g" "o" "d" "n")
                                   (list "a" "e" "q" "s" "y" "f" "p" "s" "p")
                                   (list "e" "f" "d" "a" "q" "z" "j" "d" "c")) 9 0)
                   
                   (list (list "mouse" 5) (list "fox" 3)))
              
              (list (list 7 1 "down" 4) (list 7 7 "down" 2))
              )

(check-expect (search-up-to-down (transpose(list (list "w" "b" "y" "i" "g" "g" "d" "a" "w") 
                                   (list "m" "l" "i" "d" "i" "q" "p" "u" "o") 
                                   (list "p" "o" "t" "a" "c" "f" "d" "f" "r") 
                                   (list "d" "k" "g" "t" "s" "c" "a" "c" "m")
                                   (list "r" "p" "p" "t" "w" "w" "r" "g" "o")
                                   (list "e" "p" "t" "o" "g" "o" "x" "z" "a")
                                   (list "w" "f" "t" "r" "q" "r" "w" "d" "b")
                                   (list "e" "m" "t" "r" "i" "r" "t" "f" "p")
                                   (list "h" "o" "t" "a" "r" "a" "o" "o" "p")
                                   (list "e" "u" "t" "p" "a" "p" "r" "x" "t")
                                   (list "i" "s" "u" "u" "q" "g" "o" "d" "n")
                                   (list "a" "e" "q" "s" "y" "f" "p" "s" "p")
                                   (list "e" "f" "d" "a" "q" "z" "j" "d" "c")) 9 0)
                   
                   nil)
              
              nil
              )


;search-down-to-up

(check-expect (search-down-to-up (transpose(list (list "w" "b" "y" "i" "g" "g" "d" "a" "w") 
                                   (list "m" "l" "i" "d" "i" "q" "p" "u" "o") 
                                   (list "p" "o" "t" "a" "c" "f" "e" "f" "r") 
                                   (list "d" "k" "d" "t" "s" "c" "l" "c" "m")
                                   (list "r" "p" "r" "t" "w" "w" "t" "g" "o")
                                   (list "e" "p" "i" "o" "g" "o" "r" "z" "a")
                                   (list "w" "f" "b" "r" "q" "r" "u" "d" "b")
                                   (list "e" "m" "t" "r" "i" "r" "t" "f" "p")
                                   (list "h" "o" "t" "a" "r" "a" "o" "o" "p")
                                   (list "e" "u" "t" "p" "a" "p" "r" "x" "t")
                                   (list "i" "s" "u" "u" "q" "g" "o" "d" "n")
                                   (list "a" "e" "q" "s" "y" "f" "p" "s" "p")
                                   (list "e" "f" "d" "a" "q" "z" "j" "d" "c")) 9 0)
                   
                   (list (list "bird" 4) (list "turtle" 6)))
              
              (list (list 6 2 "up" 3) (list 7 6 "up" 5))
              )

(check-expect (search-down-to-up (transpose(list (list "w" "b" "y" "i" "g" "g" "d" "a" "w") 
                                   (list "m" "l" "i" "d" "i" "q" "p" "u" "o") 
                                   (list "p" "o" "t" "a" "c" "f" "e" "f" "r") 
                                   (list "d" "k" "d" "t" "s" "c" "l" "c" "m")
                                   (list "r" "p" "r" "t" "w" "w" "t" "g" "o")
                                   (list "e" "p" "i" "o" "g" "o" "r" "z" "a")
                                   (list "w" "f" "b" "r" "q" "r" "u" "d" "b")
                                   (list "e" "m" "t" "r" "i" "r" "t" "f" "p")
                                   (list "h" "o" "t" "a" "r" "a" "o" "o" "p")
                                   (list "e" "u" "t" "p" "a" "p" "r" "x" "t")
                                   (list "i" "s" "u" "u" "q" "g" "o" "d" "n")
                                   (list "a" "e" "q" "s" "y" "f" "p" "s" "p")
                                   (list "e" "f" "d" "a" "q" "z" "j" "d" "c")) 9 0)
                   
                   nil)
              
              nil
              )

;clean-results

(check-expect (clean-results (list nil nil nil nil (list 0 0 "dir" 5) nil nil nil))
                             (list(list 0 0 "dir" 5)))

(check-expect (clean-results (list nil nil nil nil nil nil nil nil))
                             nil)


;everything together


(check-expect (brute-force-solver (list (list "w" "b" "y" "i" "g" "g" "d" "a" "w") ;example game board
                          (list "m" "l" "i" "d" "i" "q" "p" "u" "o") 
                          (list "p" "o" "t" "a" "c" "f" "d" "f" "r") 
                          (list "d" "o" "g" "t" "s" "c" "a" "c" "m")
                          (list "r" "p" "p" "t" "w" "w" "r" "g" "o")
                          (list "e" "p" "t" "o" "g" "o" "x" "z" "a")
                          (list "w" "f" "t" "r" "q" "r" "w" "d" "b")
                          (list "e" "d" "t" "r" "i" "r" "t" "f" "p")
                          (list "h" "d" "t" "a" "r" "a" "t" "f" "p")
                          (list "e" "w" "t" "p" "a" "p" "r" "o" "t")
                          (list "i" "i" "u" "u" "q" "s" "o" "k" "n")
                          (list "a" "u" "q" "s" "y" "f" "p" "s" "p")
                          (list "e" "f" "d" "a" "q" "z" "j" "d" "c"))
                    
                    (list (list "c" "a" "t"); example word list 
                          (list "d" "o" "g") 
                          (list "p" "i" "g")
                          (list "r" "a" "t")
                          (list "p" "a" "r" "r" "o" "t")
                          (list "s" "p" "a" "r" "r" "o" "w")
                          (list "w" "o" "r" "m")
                          )
                    )
              
              (list(list 3 0 "right" 2) (list 8 4 "right" 2) (list 2 4 "left" 2) 
                   (list 8 4 "left" 2) (list 0 8 "down" 3) (list 9 3 "up" 5) (list 10 5 "up" 6))
              )

(check-expect (brute-force-solver (list (list "w" "b" "y" "i" "g" "g" "d" "a" "w") ;example game board
                          (list "m" "l" "i" "d" "i" "q" "p" "u" "o") 
                          (list "p" "o" "t" "a" "c" "f" "d" "f" "r") 
                          (list "d" "o" "g" "t" "s" "c" "a" "c" "m")
                          (list "r" "p" "p" "t" "w" "w" "r" "g" "o")
                          (list "e" "p" "t" "o" "g" "o" "x" "z" "a")
                          (list "w" "f" "t" "r" "q" "r" "w" "d" "b")
                          (list "e" "d" "t" "r" "i" "r" "t" "f" "p")
                          (list "h" "d" "t" "a" "r" "a" "t" "f" "p")
                          (list "e" "w" "t" "p" "a" "p" "r" "o" "t")
                          (list "i" "i" "u" "u" "q" "s" "o" "k" "n")
                          (list "a" "u" "q" "s" "y" "f" "p" "s" "p")
                          (list "e" "f" "d" "a" "q" "z" "j" "d" "c"))
                    
                    nil
                    )
              
              nil
              )

;properties

;concat-count always returns a list with the same number of elements passed in.

(defproperty concat-count-always-gives-list :repeat 2000
  (xs :value (random-list-of (random-list-of (random-string))))
    (implies (consp xs)
             (= (len xs) (len (concat-count xs)))))

;reverse always gives the same number of rows afterwards
(defproperty reverse-always-generates-grid :repeat 2000
  (xs :value (random-list-of (random-list-of (random-string))))
    (implies (consp xs)
             (= (len xs) (len (reverse xs)))))

;tranpose always gives the same number of rows afterwards
(defproperty tranpose-always-generates-grid :repeat 2000
  (xs :value (random-list-of (random-list-of (random-string) :size 10):size 10))
    (implies (consp xs)
             (= (len xs) (len (transpose xs (len (car xs)) 0)))))

;The length of a list after nthrdc will always be less than or equal the origin length
(defproperty nthrdc-less-than-equal-orig-length :repeat 2000
  (xs :value (random-list-of (random-string))
    pos :value (random-between 0 (len xs)))
    (implies (consp xs)
             (>= (len xs) (len (nthrdc pos xs)))))









  




