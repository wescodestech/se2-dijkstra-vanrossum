; fits.lisp
;
; This file contains all the necessary functions to determine if the value
; that is to be placed in the crossword puzzle will fit into the location
; of the random coordinate.
;
; @Author Team Van Rossum
;
; Adapted by Team Dijkstra on April 23, 2013 Matthew Crist, Wesley Howell 
; and Adam Ghodratnama
;
; fits.lisp
;
; Finds all of the appropriate places for the words
; to appear in the board when being built
;
; CHANGE LOG:
; ------------------------------------------------------------------------
; 2013-23-04 - Matthew A. Crist - Documented the functions with their 
;              purpose, expected input structure and expected output
;              structure.

(in-package "ACL2")

;--------------------------------------------------------
;-----------------Start Find Placements------------------
;--------------------------------------------------------

; (get-end n nums)
; Acquires the length of the row (which is a reference for the row coord)
; n    - The increment to determine the row width
; nums - The placement numbers in each row for free columns
; 
; returns - the number of elements in this list
(defun get-end (n nums)
  (if (endp nums) n
      (if (= (car nums) n)
          (get-end (+ n 1) (cdr nums))
          (-  n 1))))

; (start-end n nums)
; Gets the start and end coordinates for the row
; This returns a list of consec coordinates for row
; n    - the incremental value to determine the starting point
; nums - the row that contains the unused coordinates available for 
;        placement.
(defun start-end (n nums)
  (if (endp nums) '()
      (if (= n (car nums))
          (let ((start (car nums)) ; get the starting point of row
                (end (get-end (+ n 1) (cdr nums)) ;get the end pt
                     ))
                (cons (list start end) ;add starting and ending pts
                      (start-end end  (nthcdr (+ 1 end) nums))))
          (start-end (+ n 1) nums))))

; Locates the start coordinate in a row and the coord that is available
; to the end of the list.
; nums - The matrix of available placements in the form of:
;        ( (0 1 2 3 4 5 6 7)
;          (0 1 2 3 4 5 6 7)
;          (0 4 5 6 7)
;          (0 1 2 3 4 5 6 7)
;          (0 1 2 3 4 5 6 7)
;          (0 1 2 3 4 5 6 7)
;          (0 1 2 3 4 5 6 7)
;          (0 1 2 3 4 5 6 7) )
; This would result in a format
; (((0 8)) 
;  ((0 8)) 
;  ((0 0) (4 8))  <- Notice that 1-3 are used
;  ((0 8)) 
;  ((0 8)) 
;  ((0 8)) 
;  ((0 8)) 
;  ((0 8)))
(defun do-start-end (nums)
  (if (endp nums) 
      '()
      (cons (start-end 0 (car nums)) 
            (do-start-end (cdr nums)))))

#|----------------------------------------------------------------------|#
#| These are supposed to be in column major form.                       |#
#|----------------------------------------------------------------------|#

; now we will have the form for the coordinates
;((0,3) (0,6))
(defun mtx-form-horiz-helper (n coord)
  (list (list n (car coord)) (list n (cadr coord))))

; (mtx-form-horiz n coords)
; Converts each row into a set of proper coordinates (x y) that is a 
; list of the range of coordinates that will be available for 
; placement.
; n - the column index (x)
; coords - 
(defun mtx-form-horiz (n coords)
  (if (endp coords) '()
      (cons (mtx-form-horiz-helper n (car coords)) 
            (mtx-form-horiz n (cdr coords)))))

; (coords-horiz n nums)
; Acquires the coordinates that are available for word placement.
; n    - the start index for the x coordinate
; nums - the column numbers that are available for placement.
(defun coords-horiz (n nums)
  (if (endp nums) 
      '()
      (if (equal nil (car nums)) ;nothing open here
          (coords-horiz (+ n 1) (cdr nums))
      (let* ((vects (start-end 0 (car nums)))
             (tx (mtx-form-horiz n vects)))
        (cons tx (coords-horiz (+ n 1) (cdr nums)))))))



; Checks the row and finds all the indices that are blank
(defun ckrow (n row)
  (if (endp row) '()
    (if (char-equal (car row) #\.)
	(cons n (ckrow (+ n 1) (cdr row) ))
      (ckrow (+ n 1) (cdr row))
  )))

; (open-brd-coords n brd)
; Gets all open coords for board.
; n - 
(defun open-brd-coords (n brd)
  (if (endp brd) 
      '()
      (cons (ckrow 0 (car brd))
            ( open-brd-coords (+ n 1) (cdr brd)))))

;-----------------------------------------------------------
;------------------Find fits Horiz--------------------------
;-----------------------------------------------------------

; Filter list down to coordinates that fit
; COLLISION DETECTION OCCURS HERE
(defun fitsp (word coords)
    (if ( <= (length word) (- (cadr (cadr coords)) (cadr (car coords))))
	 t
         nil)) 
		 
; Find our fit horizontally
(defun fits (word coords)
  (if (endp coords) '()
      (if (fitsp word (car coords))
          (cons (car coords) (fits word (cdr coords)))
          (fits word (cdr coords)))))

; Find all the areas that the word will fit given coordinates
(defun do-fits (word coords)
  (if (endp coords) '()
      (cons (fits word (car coords)) 
            (do-fits word (cdr coords)))))    
    
;----------------------------------------------------End do fits

; ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
;                                                                            COLLISION DETECTION OCCURS HERE



;-----------------------------------------------------------
;------------------Find fits Vert---------------------------
;-----------------------------------------------------------
; Filter list down to coordinates that fit
(defun fitsp-vert (word coords)
	(if ( <= (len word) (- (caadr coords) (caar coords)))
		t
		nil)) 


(defun fits-vert (word coords)
  (if (endp coords) '()
      (if (fitsp-vert word (car coords))
          (cons (car coords) (fits-vert word (cdr coords)))
          (fits-vert word (cdr coords)))))

; Find all the areas that the word will fit given co
(defun do-fits-vert (word coords)
  (if (endp coords) '()
      (cons (fits-vert word (car coords)) 
            (do-fits-vert word (cdr coords)))))    
    
;-----------------------------------------------------

;----------------------------------------------------------------
;-------------Convert open spots to vert coords------------------
;----------------------------------------------------------------


; now we will have the form for the coordinates
;((0,3) (0,6))
(defun mtx-form-vert-helper (n coord)
  (list (list (car coord) n) (list (cadr coord) n)))

; Have our spots in proper coordinates for placement  
(defun mtx-form-vert (n coords)
  (if (endp coords) '()
      (cons (mtx-form-vert-helper n (car coords)) 
            (mtx-form-vert n (cdr coords)))))

; Predicate if number is same for number wea re looking
(defun indxp (col num)
  (if (= col num)
      t
      nil))

(defun openv-helper (col row nums)
  (if (endp nums) '()
      (if (member-equal col (car nums))
          (cons row (openv-helper col (+ 1 row) (cdr nums)))
      (openv-helper col (+ 1 row) (cdr nums)))))

; Find all of the open spots for vertical placement
(defun openv (col nums)
  (if (= col (len nums)) '()
      (cons (openv-helper col 0 nums) (openv (+ 1 col) nums))))

; Gets the Vertical coordinates for placement
(defun coords-vert (n nums)
  (if (endp nums) '()
;      (if (equal nil (car nums))
 ;         (coords-vert (+ n 1) (cdr nums))
      (let* ((vects (start-end 0 (car nums)))
             (tx (mtx-form-vert n vects)))
        (cons tx (coords-vert (+ n 1) (cdr nums))))))
                 
;-------------------------------------------------------End Vert Conv



;---------------------------------------------------------
;---------- Unused ---------------------------------------
;---------------------------------------------------------



; Helper method handles the row colm conversion
;(defun open-vert-helper (col row nums lrow)
  ;(if (= row lrow) '()
      ;(if (member-equal col (car nums))
      ;    (cons row (open-vert-helper col (+ 1 row) (cdr nums) lrow))
     ;     (open-vert-helper col (+ 1 row) (cdr nums) lrow))))

; gets all the open spots for vertical placement
;(defun open-vert (n nums)
 ; (if (= n (len nums))  '()
  ;    (let* ((col n)
    ;         (vcords (open-vert-helper col 0 nums (len nums))))
   ;     (cons vcords (open-vert (+ n 1) nums)))))
; End fits.lisp