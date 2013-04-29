; Team Van Rossum
; create-board.lisp
;
; creates the game board
;

(in-package "ACL2")

(include-book "list-utilities" :dir :teachpacks)
(include-book "placement")
(include-book "fill-board")

;Generate Board for word-search
(defun wdsrch-brd (words)
  (let* ((seed1 1111)
         (board-struct (plc-wdsrch words '(((#\. #\. #\. #\. #\. #\.) (#\. #\. #\. #\. #\. #\.) (#\. #\. #\. #\. #\. #\.) (#\. #\. #\. #\. #\. #\.) (#\. #\. #\. #\. #\. #\.) (#\. #\. #\. #\. #\. #\.))) seed1))
         (board (car board-struct))
         (solutions (cadr board-struct))
         (masked (fill-brd board seed1)))
    (list masked solutions)))

; Initial call to create appropriate board
(defun create-board (words game)
   (cond ((= game 1) (wdsrch-brd words))
	 ((= game 2) nil)))


; Helper to convert board to string
(defun string-brd-helper (row)
  (if (endp row) '()
      (cons (chrs->str (list (car row))) 
            (string-brd-helper (cdr row)))))

; Convert from characters to strings
(defun string-brd (brd)
  (if (endp brd) '()
      (cons (string-brd-helper (car brd))
            (string-brd (cdr brd)))))