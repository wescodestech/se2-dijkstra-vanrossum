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






;NOT IMPLEMENTED FULLY YET------------------------------------------------------

;; (defun read-rands (f-in state)
;;     (mv-let (input-as-string error-open state)
;;             ;convert file to string
;;             (file->string f-in state)
;;             (if error-open
;;                 (mv error-open state)
;;                 (mv-let (error-close state)
;;                         ;do output
;;                         (string-list->file  
;;                          (string-append 
;;                           (subseq f-in 0 (position-equal #\. f-in)) "") 
;;                           (get-output input-as-string)
;;                                            state)
;;                        (if error-close
;;                          (mv error-close state)
;;                          (mv (string-append "input file: "
;;                                  (string-append f-in
;;                           (string-append ", output file: " (string-append f-in "withLRcoeffs"))))
;;                              state))))))





;; ;Function will place letters in empty
;; ;spaces if we are making a wordsearch
;; (defun fill-board (brd)
;;   (let ((rands (read-rands "rands.txt" true)))
;;     (row-fill rands brd)))


;; ; Just puts asterisk in empty places
;; (defun xrow-fill (row)
;;   (if (equal (length (car  row)) 0)
;;       (cons "*" (cdr row))
;;       (cons (car row) (cdr row))))


;End Fill-board