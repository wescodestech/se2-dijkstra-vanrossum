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
#|
; Place this word vertically dog
(defun plc-vert (brd word coords)
  (let* ((col-num (cadar coords))
         (y1 (caar coords))
         (new-brd (col-rep brd word y1 col-num )))
    new-brd))

;Find a place letters horizontally
;across the game-board this will do it
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
; word - the list of characters that represent the word to be placed on 
;        the board.
; brd - the matrix of letters that represent the board (list of lists of 
;       characters).
; col-num - the column number for placement.
; row-num - the row number for placement.
; direction - the direction in which the word is to be placed.
(defun verify-placement (word brd col-num row-num direction)
  (if (and (natp col-num)
           (natp row-num))
      (if (and (> (length brd) row-num)
               (> (length (car brd)) col-num)
               (or (< 0 col-num) (equal col-num 0))
               (or (< 0 row-num) (equal row-num 0)))
          (if (endp word)
              t ; There are no more letters to verify and the meet the conditions
              (let* ((row    (nth row-num brd))
                     (column (nth col-num row)))
                (if (or (equal column (car word))
                        (equal column #\.))
                    (cond ((equal direction "right-down")
                           (verify-placement (cdr word) brd (+ col-num 1) (+ row-num 1) direction))
                          ((equal direction "right-up")
                           (verify-placement (cdr word) brd (+ col-num 1) (- row-num 1) direction))
                          ((equal direction "left-down")
                           (verify-placement (cdr word) brd (- col-num 1) (+ row-num 1) direction))
                          ((equal direction "left-up")
                           (verify-placement (cdr word) brd (- col-num 1) (- row-num 1) direction))
                          ((equal direction "down")
                           (verify-placement (cdr word) brd col-num (+ row-num 1) direction))
                          ((equal direction "up")
                           (verify-placement (cdr word) brd col-num (- row-num 1) direction))
                          ((equal direction "left")
                           (verify-placement (cdr word) brd (- col-num 1) row-num direction))
                          ((equal direction "right")
                           (verify-placement (cdr word) brd (+ col-num 1) row-num direction)))
                    nil))) ; Word cannot interst (collision issue)
          nil) ; Word is outside of the bounds of the board
      nil)) ; Row and column do not exist - negative values
                              
; (get-rows-after brd row-num)
; Acquires all rows after a given row-num.
; brd - the board matrix (list of lists) that contain the row information.
; row-num - the number of the row after which you wish to acquire rows.
(defun get-rows-after (brd row-num)
  (if (equal row-num 0)
     (cdr brd)
     (get-rows-after (cdr brd) (- row-num 1))))

; (get-rows-before brd row-num)
; Acquires all rows before a given row-num.
; brd - the board matrix (list of lists) that contain the row information.
; row-num - the number of the row before which you wish to acquire rows.
(defun get-rows-before (brd row-num)
  (if (equal row-num 0)
      '()
      (append (list (car brd)) (get-rows-before (cdr brd) (- row-num 1)))))

; (get-row-at brd row-num)
; Acquires the row at a given row-num.
; brd - the board matrix (list of lists) that contain the row information.
; row-num - the row in which you wish to acquire from the matrix.
; ** It is important to note that this returns the row tuple and not a 
;    list of lists like the previous boundary acquisition functions.
(defun get-row-at (brd row-num)
  (if (equal row-num 0)
      (car brd)
      (get-row-at (cdr brd) (- row-num 1))))

; (get-cols-after row col-num)
; Acquires the columns that occur after a given col-num.
; row - the list of columns in a row.
; col-num - the column after which you wish to acquire the columns.
(defun get-cols-after (row col-num)
  (if (equal col-num 0)
      (cdr row)
      (get-cols-after (cdr row) (- col-num 1))))

; (get-cols-before row col-num)
; Acquires the columns that occur before a given col-num.
; row - the list of columns in a row.
; col-num - he column before which you wish to acquire the columns.
(defun get-cols-before (row col-num)
  (if (equal col-num 0)
      nil
      (cons (car row) (get-cols-before (cdr row) (- col-num 1)))))

; (replace-characters word brd col-num row-num direction)
; Replaces a character at the given row and column.  The direction determines
; the next replacement coordinate until all the letters in the word have been
; placed onto the board.
; word - the list of characters that will be placed on the board.
; brd  - the list of lists of characters that currently make up the board.
; col-num - the column index that is to be replaced.
; row-num - the row index that is to be replaced.
; direction - the direction that the characters will be replaced (next index).
(defun replace-characters (word brd col-num row-num direction)
  (if (endp word)
      brd ; We have no more letters to place
      (let* ((front  (get-rows-before brd row-num))
             (back   (get-rows-after brd row-num))
             (change (get-row-at brd row-num))
             (fcol   (get-cols-before change col-num))
             (bcol   (get-cols-after change col-num))
             (nrow   (append fcol (cons (car word) bcol)))
             (nbrd   (append front (cons nrow back))))
        (cond ((equal direction "right")      (replace-characters (cdr word) nbrd (+ col-num 1) row-num       direction))
              ((equal direction "left")       (replace-characters (cdr word) nbrd (- col-num 1) row-num       direction))
              ((equal direction "up")         (replace-characters (cdr word) nbrd col-num       (- row-num 1) direction))
              ((equal direction "down")       (replace-characters (cdr word) nbrd col-num       (+ row-num 1) direction))
              ((equal direction "right-down") (replace-characters (cdr word) nbrd (+ col-num 1) (+ row-num 1) direction))
              ((equal direction "right-up")   (replace-characters (cdr word) nbrd (+ col-num 1) (- row-num 1) direction))
              ((equal direction "left-down")  (replace-characters (cdr word) nbrd (- col-num 1) (+ row-num 1) direction))
              ((equal direction "left-up")    (replace-characters (cdr word) nbrd (- col-num 1) (- row-num 1) direction))))))
	
; (place brd word type coord)
; Place word on the board at the given coordinate in the direction 
; specified.  We first do a dry run with verify-placement and if 
; that function returns true, we are able to place our word onto the 
; board without conflict.  If we are not able to place it on the board, 
; we return the original board.  ** This is intended to be replaced by
; randomly selecting another place for the word on the board.
; brd   - the board in which to place the word.
; word  - the word that will be placed into the board.
; type  - the orientation of placement for the word.
; coord - the coordinate (col row) to place the word.
(defun place (brd word type coord)
  (let* ((column (car coord))
         (row    (cadr coord)))
        ; Right placement
    (cond ((= type 0) 
           (if (verify-placement word brd column row "right")
               (replace-characters word brd column row "right")
               brd))
          
          ; Left placement
          ((= type 1) 
           (if (verify-placement word brd column row "left")
               (replace-characters word brd column row "left")
               brd))
          
          ; Down placement
          ((= type 2) 
           (if (verify-placement word brd column row "down")
               (replace-characters word brd column row "down")
               brd))
          
          ; Up placement
          ((= type 3)
           (if (verify-placement word brd column row "up")
               (replace-characters word brd column row "up")
               brd))
          
          ; Right-Down placement
	  ((= type 4) 
           (if (verify-placement word brd column row "right-down")
               (replace-characters word brd column row "right-down")
               brd))
          
          ; Left-Down placement
	  ((= type 5)
           (if (verify-placement word brd column row "left-down")
               (replace-characters word brd column row "left-down")
               brd))
          
          ; Right-Up placement
	  ((= type 6) 
           (if (verify-placement word brd column row "right-up")
               (replace-characters word brd column row "right-up")
               brd))
          
          ; Left-Up placement
	  ((= type 7) 
           (if (verify-placement word brd column row "left-up")
               (replace-characters word brd column row "left-up")
               brd)))))

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

; Main workhorse of this module places word search

(defun plc-wdsrch (words brd seed)
  (if (endp words)
      '()
      (let* ((word (str->chrs (car words)))
             (letter-board (car brd))
             (solutions    (cdr brd))
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
             (new-brd (place (car brd) word type coords)));our new updated board
       
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