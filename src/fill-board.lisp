; Team Van Rossum
; Fills the game board appropriately depending
; on whether it is a crossword or a word srch

;1 random number and keep modifying it.

(in-package "ACL2")

(include-book "rand" :dir :teachpacks)
; Returns random letter
(defun rand-let (num)
  (cond (( = num 0) #\a)
	((= num 1) #\b)
	((= num 2) #\c)
	((= num 3) #\d)
	((= num 4) #\e)
	((= num 5) #\f)
	((= num 6) #\g)
	((= num 7) #\h)
	((= num 8) #\i)
	((= num 9) #\j)
	((= num 10) #\k)
	((= num 11) #\l)
	((= num 12) #\m)
	((= num 13) #\n)
	((= num 14) #\o)
	((= num 15) #\p)
	((= num 16) #\q)
	((= num 17) #\r)
	((= num 18) #\s)
	((= num 19) #\t)
	((= num 20) #\u)
	((= num 21) #\v)
	((= num 22) #\w)
	((= num 23) #\x)
	((= num 24) #\y)
	((= num 25) #\z)
))


; Fills each row with a random letter
; depending on the correct selection
(defun row-fill (row seed)
  (if (endp row) '()
  (if (char-equal #\. (car row))
      (cons (rand-let (rand 25 seed)) 
            (row-fill (cdr row) (+ seed 42)))
      (cons (car row) (row-fill (cdr row) (+ seed 31))))))

(defun fill-brd (wdsrch seed)
  (if (endp wdsrch) '()
      (cons (row-fill (car wdsrch) seed)
            (fill-brd (cdr wdsrch) (+ seed 61)))))