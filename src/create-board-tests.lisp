; Team Van Rossum
; create-board-test.lisp
;
; Testing purposes

(include-book "create-board")
(include-book "testing" :dir :teachpacks)


(check-expect (largest-elem "" '()) 0)
(check-expect (largest-elem "blue" '("cats" "dogs" "babies" "ants" "black" "white" "orange")) 6)
(check-expect (largest-elem "hi" '("hello" "this" "is" "longest")) 7)

(check-expect (gen-diff '()) 0)
(check-expect (gen-diff '("frak") )8)
(check-expect (gen-diff '("hello" "this" "is" "longest")) 14)
(check-expect (gen-diff '("cats" "dogs" "babies" "ants" "black" "white" "orange")) 12)


(check-expect (mtx-row 0) nil)
(check-expect (mtx-row 3) '(#\. #\. #\.))
(check-expect (mtx-row 5) '(#\. #\. #\. #\. #\.))

(check-expect (mtx 0 0) nil)
(check-expect (mtx 3 3) '((#\. #\. #\.)
			  (#\. #\. #\.)
			  (#\. #\. #\.)))
(check-expect (mtx 5 5)  '((#\. #\. #\. #\. #\.)
 (#\. #\. #\. #\. #\.)
 (#\. #\. #\. #\. #\.)
 (#\. #\. #\. #\. #\.)
 (#\. #\. #\. #\. #\.)))


(check-expect (string-brd '()) nil)
(check-expect (string-brd '((#\e #\n #\b #\c #\d #\r #\t #\b)
			    (#\b #\e #\t #\q #\n #\e #\i #\s)
			    (#\n #\i #\f #\w #\i #\q #\n #\d)
			    (#\y #\q #\w #\y #\m #\w #\s #\o)
			    (#\e #\d #\e #\r #\g #\y #\t #\g)
			    (#\w #\z #\h #\i #\n #\d #\a #\s)
			    (#\e #\c #\m #\h #\g #\m #\c #\b)
			    (#\w #\n #\x #\f #\m #\r #\q #\a)))
	      '(("e" "n" "b" "c" "d" "r" "t" "b")
		("b" "e" "t" "q" "n" "e" "i" "s")
		("n" "i" "f" "w" "i" "q" "n" "d")
		("y" "q" "w" "y" "m" "w" "s" "o")
		("e" "d" "e" "r" "g" "y" "t" "g")
		("w" "z" "h" "i" "n" "d" "a" "s")
		("e" "c" "m" "h" "g" "m" "c" "b")
		("w" "n" "x" "f" "m" "r" "q" "a")))
