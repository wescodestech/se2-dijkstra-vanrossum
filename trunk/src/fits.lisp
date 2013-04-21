; @Author Team Van Rossum
;
; fits.lisp
;
; Finds all of the appropriate places for the words
; to appear in the board when being built

(in-package "ACL2")

;--------------------------------------------------------
;-----------------Start Find Placements------------------
;--------------------------------------------------------

; Gets the end coordinate for the row
(defun get-end (n nums)
  (if (endp nums) n
      (if (= (car nums) n)
          (get-end (+ n 1) (cdr nums))
          (-  n 1))))


; Gets the start and end coordinates for the row
; This returns a list of consec coordinates for row
(defun start-end (n nums)
  (if (endp nums) '()
      (if (= n (car nums))
          (let ((start (car nums)) ; get the starting point of row
                (end (get-end (+ n 1) (cdr nums)) ;get the end pt
                     ))
                (cons (list start end) ;add starting and ending pts
                      (start-end end  (nthcdr (+ 1 end) nums))))
          (start-end (+ n 1) nums))))

; Init the start end coords
(defun do-start-end (nums)
  (if (endp nums) '()
      (cons (start-end 0 (car nums)) 
            (do-start-end (cdr nums)))))


; now we will have the form for the coordinates
;((0,3) (0,6))
(defun mtx-form-horiz-helper (n coord)
  (list (list n (car coord)) (list n (cadr coord))))

; Have our spots in proper coordinates for placement  
(defun mtx-form-horiz (n coords)
  (if (endp coords) '()
      (cons (mtx-form-horiz-helper n (car coords)) 
            (mtx-form-horiz n (cdr coords)))))

; Horizontal coordinates
(defun coords-horiz (n nums)
  (if (endp nums) '()
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


; Gets all open coords for board
(defun open-brd-coords (n brd)
  (if (endp brd) '()
    (cons (ckrow 0 (car brd) ) 
	  ( open-brd-coords (+ n 1) (cdr brd)))))

;-----------------------------------------------------------
;------------------Find fits Horiz--------------------------
;-----------------------------------------------------------

; Filter list down to coordinates that fit
(defun fitsp (word coords)
    (if ( <= (len word) 
             (- (cadr (cadr coords)) 
                    (cadr (car coords))))
	 t
         nil)) 

; Find our fit horizontally
(defun fits (word coords)
  (if (endp coords) '()
      (if (fitsp word (car coords))
          (cons (car coords) (fits word (cdr coords)))
          (fits word (cdr coords)))))

; Find all the areas that the word will fit given co
(defun do-fits (word coords)
  (if (endp coords) '()
      (cons (fits word (car coords)) 
            (do-fits word (cdr coords)))))    
    
;----------------------------------------------------End do fits


;-----------------------------------------------------------
;------------------Find fits Vert---------------------------
;-----------------------------------------------------------
; Filter list down to coordinates that fit
(defun fitsp-vert (word coords)
    (if ( <= (len word) 
             (- (caadr coords) 
                    (caar coords)))
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