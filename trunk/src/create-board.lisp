; Team Van Rossum
; create-board.lisp
;
; creates the game board
;

(in-package "ACL2")

(include-book "placement")
(include-book "fill-board")



; Helper function to get largest element returns larger
; of two words
(defun largest-elem-helper (word1 word2)
  (if (>= (length word1) (length word2))
      word1
  word2))

; Finds the largest element
; of a list of words to decide
; the correct matrix size
(defun largest-elem (word words)
  (if (endp words) (length word)
      (largest-elem 
       (largest-elem-helper word (car words)) 
      (cdr words))))

; Generates difficulty given
; Selected difficulty returns 
; an integer for board size
(defun gen-diff (words)
   (let ((n (largest-elem (car words) (cdr words))))
    (+ 3(+ n n))))


; To construct rows for the matrix
(defun mtx-row (n)
  (if (= n 0) nil
      (cons #\. (mtx-row (- n 1))))
  )

;Create our matrix-board
(defun mtx (m n)
    (if (equal n 0) nil
      (cons (mtx-row m) (mtx m (- n 1)))))

; Extrac tthe coords from the mess
;@param mess of boards words and coordinates
;@return word and respective coordinates
(defun extrct-coords (brds)
  (if (endp brds) '()
      (cons (caar brds) (extrct-coords (cdr brds)))))
; Extrac tthe coords from the mess
;@param mess of boards words and coordinates
;@return the boards as they were places
(defun extrct-brds (brds)
  (if (endp brds) '()
      (cons (cdar brds) (extrct-brds (cdr brds)))))

; since we are returning the coords of words
; as well as the boards we need to split this
; shiznit up to have both
;Utilize two helper methods to do this
(defun split-coords-brds (brds)
  (let* ((coords (extrct-coords brds))
         (brds (extrct-brds brds)))
    (list coords brds)))


;Generate Board for word-search
(defun wdsrch-brd (words)
  (let* ((n (gen-diff words))
         (brd (mtx n n))
        (seeds 23)
        (brds-coords (plc-wdsrch words brd seeds)) ;board and words with coords
        (clean-brds-coords (split-coords-brds brds-coords)); split brd and coords
        (wds-coords (car clean-brds-coords))
        (brds (cadr clean-brds-coords))
        (wdsrch (car (last (last brds))));sorry
       (filld-srch (fill-brd wdsrch seeds)))
   
    (list filld-srch wds-coords)))


;Generate Board for xword
;(defun xwrd-brd (words)
;  (let* ((brd (mtx (len words) (len words)))
;        (xwrd (plc-words brd words)))        
;        xwrd))


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

; End create-board.lisp