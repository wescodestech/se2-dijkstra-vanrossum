(include-book "foutput")
(include-book "testing" :dir :teachpacks)
(include-book "doublecheck" :dir :teachpacks)

; foutput test suites

(check-expect (add-commas 2 '(#\a #\b #\c))
              "a,b,c")

(check-expect (add-dict-elements '(1 2 "right" 3))
              "x:1,y:2,direction:right,numberofspaces:3")

(check-expect (add-dict-elements '(3 2 "left" 1))
              "x:3,y:2,direction:left,numberofspaces:1")

(check-expect (add-brackets 2 '((#\1 #\2 #\3) (#\4 #\5 #\6) (#\7 #\8 #\9)))
              "[1,2,3],[4,5,6],[7,8,9]")

(check-expect (add-braces '((1 2 "right" 3) (3 2 "left" 1) (8 12 "up" 7)))
              "{x:1,y:2,direction:right,numberofspaces:3},{x:3,y:2,direction:left,numberofspaces:1},{x:8,y:12,direction:up,numberofspaces:7}")