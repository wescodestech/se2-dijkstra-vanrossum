; @Author Van Rossum
;
; placement.lisp 
;
; places the words within the word search
;

(in-package "ACL2")

(include-book "rand" :dir :teachpacks)
(include-book "io-utilities" :dir :teachpacks)
(include-book "list-utilities" :dir :teachpacks)

(include-book "fits")

;----------------------------------------------------------------
;---------X,Y placement-----------------------------------------
;---------------------------------------------------------------

;Get specified row of the board
(defun get-row (brd n)
  (nth n brd))

; Returns the column for replacement
(defun get-column (brd  col)
  (if (endp brd) '()
  (cons (nth col (car brd)) 
        (get-column (cdr brd) col))))


;Updates the board by putting the modified row
; into its right place making a new board
(defun update-row (brd brd-length row row-num n)
  (if (= n brd-length) nil ;finish
  (if (= n row-num)
      (cons  row 
	     (update-row 
	      (cdr brd) brd-length row row-num (+ 1 n)))
      (cons (car brd) 
            (update-row (cdr brd) brd-length 
                        row  
                        row-num   
                      (+ 1 n)))) ))

; Helper method for placing at spec indx
(defun plc-indx-helper (n y row char)
  (if (endp row) '()
    (if (= n y )
	(cons char (plc-indx-helper (+ 1 n) y (cdr row) char))
      (cons (car row) (plc-indx-helper (+ 1 n) y (cdr row) char)))))



; Places character at specified index
(defun plc-indx (brd x y char)
  (let* ((row (get-row brd x))
	 (new-row (plc-indx-helper 0 y row char)))
     (update-row brd (len brd) new-row x 0)))



; Replaces column
(defun col-rep (brd chrs y col)
  (if (endp chrs) brd
      (let* ((new-brd (plc-indx brd y col (car chrs))))
        (col-rep new-brd (cdr chrs) (+ 1 y) col))))


;Replace row
(defun row-rep (brd chrs y col)
  (if (endp chrs) brd
      (let* ((new-brd (plc-indx brd y col (car chrs))))
        (row-rep new-brd (cdr chrs) y (+ 1 col) ))))




;----------------------------------------------End Board Modifications


; Picks a random starting point for horizontal placement
(defun rand-start-horiz (start range word row-num seed)
  (let* ((new-start (+ start (rand range seed)))
              (new-end (+ new-start (len word))))
        (list (list row-num new-start) (list row-num new-end)) ))

; Picks a random starting point for vertical placement
(defun rand-start-vert (start range word row-num seed)
  (let*  ((new-start (+ start (rand (- range 1) seed)))
             (new-end (+ new-start (len word))))
        (list (list new-start row-num) (list new-end row-num))))

;word len = 5 
; start 0 
; end 10
; diff 10
; new-start = rand( diff - lenword start anywhere between 0, 5  
(defun rand-start (coords word seed type)
  (if (< type 2)
      (let* ((start (cadar coords))
              (end (cadadr coords))
              (row-num (caar coords))
              (diff (- end start))
              (range (- diff (len word))))
        (if (<= range 2)
            coords
            (rand-start-horiz start range word row-num (+ 19 seed)))) ;ugly function              
      (let* ((start (caar coords))
             (row-num (cadar coords))
             (end (caadr coords))
             (diff (- end start))
             (range (- diff (len word))))
     (if (<= range 2)
            coords
            (rand-start-vert start range word row-num (+ 12 seed))))))
         

; Randomly picks a coord from coords list for placement
(defun rand-coord (seed coords)
  (nth (rand (len coords) seed) coords))

; Function just gathers all needed info
; returns coordinate for where to place
(defun fit-coords (type word brd seed)
  (if (< type 2) ; horizontal placement
      (let* ((opn (open-brd-coords 0 brd)) ;opn-brd_>final-coords->wd-fits->plc-cord
             (mf (coords-horiz 0 opn))
             (wd-fits (do-fits word mf))
             (rcords (rand-coord seed (remove nil wd-fits)))
             (ret-cords (rand-start (car rcords) word seed type)))
       ret-cords)
      (let* ((opn (open-brd-coords 0 brd)) ; else vertical placement
             
             (mf (coords-vert 0 (openv 0  opn)))
             (wd-fits (do-fits-vert word mf))
            (rcords (rand-coord seed (remove nil wd-fits)))
        (ret-cords (rand-start (car rcords) word seed type)))
      ret-cords)))




;-------------------------------------------------------------------
;---------Place these words in the Board----------------------------
;-------------------------------------------------------------------

; Place this word vertically dog
#| Old code no longer in use
(defun plc-vert (brd word coords)
  (let* ((col-num (cadar coords))
         (y1 (caar coords))
         (new-brd (col-rep brd word y1 col-num )))
    new-brd)) |#


;Find a place letters horizontally
;across the game-board this will do it
#| Old code no longer in use.
(defun plc-horiz (brd word coords)
   (let* ((row-num (caar coords))
        (y1 (cadar coords)) ; y1 value for placement
        (new-brd (row-rep brd word row-num y1)))
    new-brd)) ; return new board     |#

#|======================================================================|#
#| START OF DIJKSTRA CODE                                               |#
#|======================================================================|#

; (verify-placement word brd col-num row-num direction)
; Verifies the placement of the word.  If the word does not meet any of 
; the conditions (such as equivalent intercepts or board boundaries) 
; then the function returns nil and the board must consider another 
; placement coordinate or placement format.
(defun verify-placement (word brd col-num row-num direction)
   ; We have checked all possible permutations of the word and our col-num
   ; and row-nums are within the boundaries of the board.
  (if (not (natp col-num))
      nil
   (if (and (equal nil (car word))
            (> (length brd) row-num)
            (> (length (car brd)) col-num)
            (or (< 0 col-num) (equal col-num 0))
            (or (< 0 row-num) (equal row-num 0)))
       t
       (if (or (> row-num (length brd)) (> col-num (length (car brd))))
           nil
           (let* ((row-tuple (nth row-num brd))
                  (col-value (nth col-num row-tuple)))
             (if (or (equal col-value (car word)) (equal col-value #\.))
                 (if (equal direction "right-down")
                     (verify-placement (cdr word) brd (+ col-num 1) (+ row-num 1) direction)
                     (if (equal direction "right-up")
                         (verify-placement (cdr word) brd (+ col-num 1) (- row-num 1) direction)
                         (if (equal direction "left-down")
                             (verify-placement (cdr word) brd (- col-num 1) (+ row-num 1) direction)
                             (if (equal direction "left-up")
                                 (verify-placement (cdr word) brd (- col-num 1) (- row-num 1) direction)
                                 (if (equal direction "down")
                                     (verify-placement (cdr word) brd col-num (+ row-num 1) direction)
                                     (if (equal direction "up")
                                         (verify-placement (cdr word) brd col-num (- row-num 1) direction)
                                         (if (equal direction "left")
                                             (verify-placement (cdr word) brd (- col-num 1) row-num direction)
                                             (if (equal direction "right")
                                                 (verify-placement (cdr word) brd (+ col-num 1) row-num direction)
                                                 nil)))))))) ; Unknown placement format
                 nil)))))) ; Item does not meet the conditions for placement

(defun get-rows-after (brd row-num)
  (if (equal row-num 0)
     (cdr brd)
     (get-rows-after (cdr brd) (- row-num 1))))

(defun get-rows-before (brd row-num)
  (if (equal row-num 0)
      '()
      (append (list (list (car brd))) (get-rows-before (cdr brd) (- row-num 1)))))

(defun get-row-at (brd row-num)
  (if (equal row-num 0)
      (car brd)
      (get-row-at (cdr brd) (- row-num 1))))

(defun get-cols-after (row col-num)
  (if (equal col-num 0)
      (cdr row)
      (get-cols-after (cdr row) (- col-num 1))))

(defun get-cols-before (row col-num)
  (if (equal col-num 0)
      nil
      (cons (car row) (get-cols-before (cdr row) (- col-num 1)))))

(defun replace-characters (word brd col-num row-num direction)
  (if (verify-placement word brd col-num row-num direction)
      ; Place word onto the board
      (if (equal nil (car word))
          brd ; We have no more letters to place
          (let* ((front (get-rows-before brd row-num))
                 (back (get-rows-after brd row-num))
                 (change (get-row-at brd row-num))
                 (fcol (get-cols-before change col-num))
                 (bcol (get-cols-after change col-num))
                 (nrow (append fcol (cons (car word) bcol)))
                 (nbrd (append front (cons nrow back))))
            (if (equal direction "right")
                (replace-characters (cdr word) nbrd 
                   (+ col-num 1) row-num direction)
                (if (equal direction "left")
                    (replace-characters (cdr word) nbrd 
                       (- col-num 1) row-num direction)
                    (if (equal direction "up")
                        (replace-characters (cdr word) nbrd 
                           col-num (- row-num 1) direction)
                        (if (equal direction "down")
                            (replace-characters (cdr word) nbrd 
                               col-num (+ row-num 1) direction)
                            (if (equal direction "right-down")
                                (replace-characters (cdr word) nbrd 
                                   (+ col-num 1) (+ row-num 1) direction)
                                (if (equal direction "right-up")
                                    (replace-characters (cdr word) nbrd (+ col-num 1) (- row-num 1) direction)
                                    (if (equal direction "left-down")
                                        (replace-characters (cdr word) nbrd (- col-num 1) (+ row-num 1) direction)
                                        (if (equal direction "left-up")
                                            (replace-characters (cdr word) nbrd (- col-num 1) (- row-num 1) direction)
                                            nil)))))))))) ; Unknown placement
                                       
      ; Find another placement method
      ; Return the original board for now
      brd))

; (plc-rd brd word coord)
; Places a word into the board at the specified coordinate
; brd - the word board
; word - the word to be placed on the board
; coord - the x/y coordinate for the word placement
(defun plc-rd (brd word coord)
  (let* ((row-num (caar coord))
         (col-num (cadar coord))
         (new-brd (replace-characters word brd col-num row-num "right-down")))
    new-brd))

; (plc-ld brd word coord)
; Places a word in the board at the specified coordinate in the left-down
; format.
; brd - the current word board.
; word - the word that is to be placed into the word board.
; coord - the coordinate in which the word will be placed.
(defun plc-ld (brd word coord)
  (let* ((row-num (caar coord))
         (col-num (cadar coord))
         (new-brd (replace-characters word brd col-num row-num "left-down")))
    new-brd))

; (plc-ru brd word coord)
; Places a word in the board at the specified coordinate in the right-up
; format.
; brd - the current word board.
; word - the word that is to be placed into the board.
; coord - the location the word will be placed.
(defun plc-ru (brd word coord)
  (let* ((row-num (car coord))
         (col-num (cadr coord))
         (new-brd (replace-characters word brd col-num row-num "right-up")))
    new-brd))

; (plc-lu brd word coord)
; Places a word in the board at the specified coordinate in the left-up
; format.
; brd - the current word board.
; word - the word that is to be placed into the board.
; coord - the location the word will be placed.
(defun plc-lu (brd word coord)
  (let* ((row-num (car coord))
         (col-num (cadr coord))
         (new-brd (replace-characters word brd col-num row-num "left-up")))
    new-brd))

(defun plc-r (brd word coord)
  (let* ((row-num (car coord))
         (col-num (cadr coord))
         (new-brd (replace-characters word brd col-num row-num "right")))
    new-brd))

(defun plc-l (brd word coord)
  (let* ((row-num (car coord))
         (col-num (cadr coord))
         (new-brd (replace-characters word brd col-num row-num "left")))
    new-brd))

(defun plc-u (brd word coord)
  (let* ((row-num (car coord))
         (col-num (cadr coord))
         (new-brd (replace-characters word brd col-num row-num "up")))
    new-brd))

(defun plc-d (brd word coord)
  (let* ((row-num (car coord))
         (col-num (cadr coord))
         (new-brd (replace-characters word brd col-num row-num "down")))
    new-brd))
	
; (place brd word type coord)
; 
 ;Place word on the board according to random num generator
(defun place (brd word type coord)
  (cond ((= type 0) (plc-r brd word coord)) 
	((= type 1) (plc-l brd word coord))
        ((= type 2) (plc-d brd word coord))
        ((= type 3) (plc-u brd word coord))
	((= type 4) (plc-rd brd word coord))
	((= type 5) (plc-ld brd word coord))
	((= type 6) (plc-ru brd word coord))
	((= type 7) (plc-lu brd word coord))))

; (get-end-coords start-coord word-length orientation)
; Acquires the endpoint coordinate of the word based on its placement on the board.
; start-coord - the x,y tuple (col, row) where the word is started
; word-length - the length of the word that was placed
; orientation - the integer representation for orientation - see below for int values
(defun get-end-coords (start-coord word-length orientation)
        ; Right placement - shift x, keep y
  (cond ((= orientation 0) (list (+ (car start-coord) (- word-length 1)) (cadr start-coord)))
        ; Left placement - shift x, keep y
        ((= orientation 1) (list (- (car start-coord) (- word-length 1)) (cadr start-coord)))
        ; Downward placement - shift y, keep x
        ((= orientation 2) (list (car start-coord) (+ (cadr start-coord) (- word-length 1))))
        ; Upward placement - shift y, keep x
        ((= orientation 3) (list (car start-coord) (- (cadr start-coord) (- word-length 1))))
        ; Right downward placement
        ((= orientation 4) (list (+ (car start-coord) (- word-length 1)) (+ (cadr start-coord) (- word-length 1))))
        ; Left downward placement
        ((= orientation 5) (list (- (car start-coord) (- word-length 1)) (+ (cadr start-coord) (- word-length 1))))
        ; Right upward placement
        ((= orientation 6) (list (+ (car start-coord) (- word-length 1)) (- (cadr start-coord) (- word-length 1))))
        ; Left upward placement
        ((= orientation 7) (list (- (car start-coord) (- word-length 1)) (- (cadr start-coord) (- word-length 1))))))

 ;Main workhorse of this module places word search
(defun plc-wdsrch (words brd seed)
  (if (endp words)
      brd
      (let* ((word (str->chrs (car words)))
             (letter-board (car brd))
             (solutions    (cadr brd))
             (type (rand 8 seed))
             (x-coord (rand 12 seed))
             (y-coord (rand 12 seed))
             (start-coord (list x-coord y-coord))
             (new-board (place letter-board word type start-coord))
             (new-word-solution (list (car words) start-coord (get-end-coords start-coord (length word) type)))
             (new-solutions (append solutions (list new-word-solution)))
             (new-brd (cons new-board new-solutions)))
        new-brd)))








; Old unused code
#|
(defun plc-wdsrch (words brd seed)
  (if (endp words) 
      '()
      (let* ((word (str->chrs (car words))) ;cnvrt str chrs
             ;(type (rand 4 seed)) ;get the type we are placing
             (type (rand 8 seed))
             (coords (fit-coords type word brd (+ seed 57)))
             (new-brd (place brd word type coords)));our new updated board
       
        (if (or (= type 1) (= type 3) (= type 6) (= type 7))
            (cons (cons (cons (car words) (reverse coords)) new-brd) (plc-wdsrch (cdr words) new-brd (+ 99 seed)))
            (cons (cons (cons (car words) coords) new-brd) (plc-wdsrch (cdr words) new-brd (+ 39 seed)))))))
|#
;-------------------------------------------------------End Placement

;-------------------------------------------------------Unused

;; Changing values of a row puts the
;; correct spot utilizing coordinates
;(defun row-rep (chrs y1 y2 cnt row)
;  (if  (=  cnt (len row)) 
;       (if (null chrs) '()
;           (list (car chrs)));we reached end of row
;  (if  (and (not (endp chrs)) (and (>= cnt y1) (< cnt y2)))
;      (cons (car chrs) ; where we put characters into board
;            (row-rep (cdr chrs) y1 y2 (+ 1 cnt) row)) 
;    (cons (nth cnt  row) ; kee going until we are in range
;         (row-rep chrs y1 y2 (+ 1 cnt) row)
;  
;))))

;(defun replace-col (brd col col-num)
;  (if (endp col) brd
;      (cons (row-rep col col-num (+ 1 col-num) 0 (car brd))
;            (replace-col (cdr brd) (cdr col) col-num))))

;End placement.lisp