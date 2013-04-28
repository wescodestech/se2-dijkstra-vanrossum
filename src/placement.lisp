; @Author Van Rossum
;
; placement.lisp 
;
; places the words within the word search
;

(in-package "ACL2")

(include-book "rand" :dir :teachpacks)

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
           (if (> (- (length (cadr brd)) 1) (+ column (- (length word) 1)))
               (let* ((overflow (- (+ column (length word)) (length (cadr brd))))
                      (new-col  (- column overflow)))
                 (if (verify-placement word brd new-col row "right")
                     (replace-characters word brd new-col row "right")
                     brd))
               (if (verify-placement word brd column row "right")
                   (replace-characters word brd column row "right")
                   brd)))
          
          ; Left placement
          ((= type 1) 
           (if (> 0 (- column (- (length word) 1)))
               (let* ((overflow (abs (- column (length word))))
                      (new-col  (+ column (- overflow 1))))
                 (if (verify-placement word brd new-col row "left")
                     (replace-characters word brd new-col row "left")
                     brd))
               (if (verify-placement word brd column row "left")
                   (replace-characters word brd column row "left")
                   brd)))
          
          ; Down placement
          ((= type 2)
           (if (> (+ row (- (length word) 1)) (- (length brd) 1))
               (let* ((overflow (- (+ row (length word)) (length brd)))
                      (new-row  (- column (- overflow 1))))
                 (if (verify-placement word brd column new-row "down")
                     (replace-characters word brd column new-row "down")
                     brd))
               (if (verify-placement word brd column row "down")
                   (replace-characters word brd column row "down")
                   brd)))
          
          ; Up placement
          ((= type 3)
           (if (> 0 (- row (- (length word) 1)))
               (let* ((overflow (abs (- row (- (length word) 1))))
                      (new-row (+ row overflow)))
                 (if (verify-placement word brd column new-row "up")
                     (replace-characters word brd column new-row "up")
                     brd))
               (if (verify-placement word brd column row "up")
                   (replace-characters word brd column row "up")
                   brd)))
          
          ; Right-Down placement
	  ((= type 4)
           (let* ((actual-row (if (> (+ row (length word)) (- (length brd) 1))
                                  (- row (- (+ row (length word)) (- (length brd) 1)))
                                  row))
                  (actual-col (if (> (+ column (length word)) (- (length (car brd)) 1))
                                  (- column (- (+ column (length word)) (- (length (car brd)) 1)))
                                  column)))
             (if (verify-placement word brd actual-col actual-row "right-down")
                     (replace-characters word brd actual-col actual-row "right-down")
                     brd)))
          
          ; Left-Down placement
	  ((= type 5)
           (let* ((actual-row (if (> (+ row (length word)) (- (length brd) 1))
                                  (- row (- (+ row (length word)) (- (length brd) 1)))
                                  row))
                  (actual-col (if (> 0 (- column (length word)))
                                  (+ column (abs (- column (length word))))
                                  column)))
             (if (verify-placement word brd actual-col actual-row "left-down")
                     (replace-characters word brd actual-col actual-row "left-down")
                     brd)))
          
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

; (is-in-matrix col row bounds)
; This function is used to determine if the point is within the bounds of
; the matrix.
; col - the x coordinate.
; row - the y coordinate.
; bounds - the length of the list of columns of the matrix which should 
;          be equivalent to the length of the rows in the matrix.
(defun is-in-matrix (col row bounds)
  (if (and (or (> col 0) (= col 0))
           (< col bounds)
           (or (> row 0) (= row 0))
           (< row bounds))
      t
      nil))

; (generate-new-coord bounds seed)
; Generates a new coordinate based on the bounds and the seed that is
; fed to the function.
; bounds - the integer values that determines the limit of the row and 
;          column.  Since the matrix is a perfect square, this is an int
;          value to represent both axis'.
; seed - the seed that will be used to determine the next random value 
;        for placement.
(defun generate-new-coord (bounds seed)
  (let* ((seed1 (next-seed seed))
         (col (rand bounds seed1))
         (seed2 (next-seed seed1))
         (row (rand bounds seed2)))
    (list col row)))

; (fit-left coords length bounds)
; Attempts to make this word fit with a left orientation.
; coords - the tuple (col row) of the placement of a word.
; length - the length of the word that is to be placed.
; bounds - the bound value for the board.  ** Board is a square so bound
;          is a single int that represents the x and y bounds.
(defun fit-left (coords length bounds)
  (let* ((col (car coords))
         (row (cadr coords)))
    (if (is-in-matrix col row bounds)
        ; end-point is equivalent to the overflow in this case
        (let* ((end-point (- col (- length 1))))
          (if (< end-point 0)
              (let* ((new-col (+ (abs end-point) col)))
                (list new-col row))
              (list col row)))
        ; Use the col as a seed value
        (fit-left (generate-new-coord bounds (abs col)) length bounds))))

; (fit-right coords length bounds)
; Attempts to make this word fit with a right orientation.
; coords - the tuple (col row) of the placement of a word.
; length - the length of the word that is to be placed.
; bounds - the bound value for the board.  ** Board is a square so bound
;          is a single int that represents the x and y bounds.
(defun fit-right (coords length bounds)
  (let* ((col (car coords))
         (row (cadr coords)))
    (if (is-in-matrix col row bounds)
        (let* ((end-point (+ col (- length 1))))
          (if (> end-point (- bounds 1))
              (let* ((overflow (- (abs end-point) (- length 1)))
                     (new-col (- col (- overflow 1))))
                (list new-col row))
              (list col row)))
        ; Use the col as a seed value
        (fit-right (generate-new-coord bounds (abs col)) length bounds))))

; (fit-up coords length bounds)
; Attempts to make this word fit with a up orientation.
; coords - the tuple (col row) of the placement of a word.
; length - the length of the word that is to be placed.
; bounds - the bound value for the board.  ** Board is a square so bound
;          is a single int that represents the x and y bounds.
(defun fit-up (coords length bounds)
  (let* ((col (car coords))
         (row (cadr coords)))
    (if (is-in-matrix col row bounds)
        ; end-point is equivalent to the overflow in this case
        (let* ((end-point (- row (- length 1))))
          (if (< end-point 0)
              (let* ((new-row (+ (abs end-point) row)))
                (list col new-row))
              (list col row)))
        ; Use the row as a seed value
        (fit-left (generate-new-coord bounds (abs row)) length bounds))))

; (fit-down coords length bounds)
; Attempts to make this word fit with a right orientation.
; coords - the tuple (col row) of the placement of a word.
; length - the length of the word that is to be placed.
; bounds - the bound value for the board.  ** Board is a square so bound
;          is a single int that represents the x and y bounds.
(defun fit-down (coords length bounds)
  (let* ((col (car coords))
         (row (cadr coords)))
    (if (is-in-matrix col row bounds)
        (let* ((end-point (+ row (- length 1))))
          (if (> end-point (- bounds 1))
              (let* ((overflow (- (abs end-point) (- length 1)))
                     (new-row (- row (- overflow 1))))
                (list col new-row))
              (list col row)))
        ; Use the row as a seed value
        (fit-right (generate-new-coord bounds (abs row)) length bounds))))

; (fit-left-down coords length bounds)
; Attempts to make this word fit with a left down diagonal orientation.
; coords - the tuple (col row) of the placement of a word.
; length - the length of the word that is to be placed.
; bounds - the bound value for the board.  ** Board is a square so bound
;          is a single int that represents the x and y bounds.
(defun fit-left-down (coords length bounds)
  (fit-down (fit-left coords length bounds) length bounds))

; (fit-right-down coords length bounds)
; Attempts to make this word fit with a right down diagonal orientation.
; coords - the tuple (col row) of the placement of a word.
; length - the length of the word that is to be placed.
; bounds - the bound value for the board.  ** Board is a square so bound
;          is a single int that represents the x and y bounds.
(defun fit-right-down (coords length bounds)
  (fit-down (fit-right coords length bounds) length bounds))

; (fit-right-up coords length bounds)
; Attempts to make this word fit with a right up diagonal orientation.
; coords - the tuple (col row) of the placement of a word.
; length - the length of the word that is to be placed.
; bounds - the bound value for the board.  ** Board is a square so bound
;          is a single int that represents the x and y bounds.
(defun fit-right-up (coords length bounds)
  (fit-up (fit-right coords length bounds) length bounds))

; (fit-left-up coords length bounds)
; Attempts to make this word fit with a left up diagonal orientation.
; coords - the tuple (col row) of the placement of a word.
; length - the length of the word that is to be placed.
; bounds - the bound value for the board.  ** Board is a square so bound
;          is a single int that represents the x and y bounds.
(defun fit-left-up (coords length bounds)
  (fit-up (fit-left coords length bounds) length bounds))

; (collision graph start-point end-point)
; Detects if there is a collision on the graph with the current word being
; placed into the matrix.
; ** If the collision is of like characters, the collision is ignored since
;    this would allow for word intersection.
; graph - the matrix that contains all the characters currently placed on 
;         the matrix.
; start-point - the starting point of a word being checked against the matrix.
; end-point - the ending point of a word being checked against the matrix.
; word - the word that is being placed into the matrix.
(defun collision (graph start-point end-point word)
  (let* ((x1 (car start-point))
         (x2 (car end-point))
         (y1 (cadr start-point))
         (y2 (cadr end-point))
         (row (nth y1 graph))
         (col (nth x1 row)))
    (if (or (equal col #\.) (equal col (car word)))
        (cond ((= x1 x2)
               (cond ((= y1 y2) nil)
                     ; Right position collision detection
                     ((< y1 y2) (collision graph (list x1 (+ y1 1)) end-point (cdr word)))
                     ; Left position collision detection
                     ((> y1 y2) (collision graph (list x1 (- y1 1)) end-point (cdr word)))))
              ((< x1 x2)
                     ; Down position collision detection
               (cond ((= y1 y2) (collision graph (list (+ x1 1) y1) end-point (cdr word)))
                     ; Down-Right position collision detection
                     ((< y1 y2) (collision graph (list (+ x1 1) (+ y1 1)) end-point (cdr word)))
                     ; Down-Left position collision detection
                     ((> y1 y2) (collision graph (list (+ x1 1) (- y1 1)) end-point (cdr word)))))
              ((> x1 x2)
                     ; Up position collision detection
               (cond ((= y1 y2) (collision graph (list (- x1 1) y1) end-point (cdr word)))
                     ; Up-Right collision detection
                     ((< y1 y2) (collision graph (list (- x1 1) (+ y1 1)) end-point (cdr word)))
                     ; Up-Left collision detection
                     ((> y1 y2) (collision graph (list (- x1 1) (- y1 1)) end-point (cdr word))))))
        t)))

; (fit-to-board word matrix orientation seed attempts)
; This function will determine the correct course of action for adding 
; a word to the board.  It will attempt to find a suitable start location
; and if it cannot find a suitable location after 10 tries, it will 
; expand the playing board to accomidate more play tiles for adding
; the word.
; word - the word that will be added to the matrix.
; matrix - the current playing board that will have the word added.
; orientation - the placement orientation of the word.
; seed - the next seed to generate a random number.
; attempts - the incrementer to check attempts of adding word to board.
(defun fit-to-board (word matrix orientation seed attempts)
  (if (> attempts 10)
      nil
      (let* ((bounds (length matrix))
             (length (length word)))
        (cond ((equal orientation "right") 
               (let* ((seed1 (next-seed seed))
                      (row (rand bounds seed1))
                      (seed2 (next-seed seed1))
                      (col (rand bounds seed2))
                      (coord (list col row))
                      (start-coord (fit-right coord length bounds))
                      (end-coord (list (+ (car start-coord) (- length 1)) (cadr start-coord))))
                 (if (collision matrix start-coord end-coord word)
                     (fit-to-board word matrix orientation seed2 (+ attempts 1))
                     start-coord)))
              ((equal orientation "left")
               (let* ((seed1 (next-seed seed))
                      (row (rand bounds seed1))
                      (seed2 (next-seed seed1))
                      (col (rand bounds seed2))
                      (coord (list col row))
                      (start-coord (fit-left coord length bounds))
                      (end-coord (list (- (car start-coord) (- length 1)) (cadr start-coord))))
                 (if (collision matrix start-coord end-coord word)
                     (fit-to-board word matrix orientation seed2 (+ attempts 1))
                     start-coord)))
              ((equal orientation "up")
               (let* ((seed1 (next-seed seed))
                      (row (rand bounds seed1))
                      (seed2 (next-seed seed1))
                      (col (rand bounds seed2))
                      (coord (list col row))
                      (start-coord (fit-up coord length bounds))
                      (end-coord (list (car start-coord) (- (cadr start-coord) (- length 1)))))
                 (if (collision matrix start-coord end-coord word)
                     (fit-to-board word matrix orientation seed2 (+ attempts 1))
                     start-coord)))
              ((equal orientation "down")
               (let* ((seed1 (next-seed seed))
                      (row (rand bounds seed1))
                      (seed2 (next-seed seed1))
                      (col (rand bounds seed2))
                      (coord (list col row))
                      (start-coord (fit-down coord length bounds))
                      (end-coord (list (car start-coord) (+ (cadr start-coord) (- length 1)))))
                 (if (collision matrix start-coord end-coord word)
                     (fit-to-board word matrix orientation seed2 (+ attempts 1))
                     start-coord)))
              ((equal orientation "right-down")
               (let* ((seed1 (next-seed seed))
                      (row (rand bounds seed1))
                      (seed2 (next-seed seed1))
                      (col (rand bounds seed2))
                      (coord (list col row))
                      (start-coord (fit-right-down coord length bounds))
                      (end-coord (list (+ (car start-coord) (- length 1)) (+ (cadr start-coord) (- length 1)))))
                 (if (collision matrix start-coord end-coord word)
                     (fit-to-board word matrix orientation seed2 (+ attempts 1))
                     start-coord)))
              ((equal orientation "right-up")
               (let* ((seed1 (next-seed seed))
                      (row (rand bounds seed1))
                      (seed2 (next-seed seed1))
                      (col (rand bounds seed2))
                      (coord (list col row))
                      (start-coord (fit-right-up coord length bounds))
                      (end-coord (list (+ (car start-coord) (- length 1)) (- (cadr start-coord) (- length 1)))))
                 (if (collision matrix start-coord end-coord word)
                     (fit-to-board word matrix orientation seed2 (+ attempts 1))
                     start-coord)))
              ((equal orientation "left-down")
               (let* ((seed1 (next-seed seed))
                      (row (rand bounds seed1))
                      (seed2 (next-seed seed1))
                      (col (rand bounds seed2))
                      (coord (list col row))
                      (start-coord (fit-left-down coord length bounds))
                      (end-coord (list (- (car start-coord) (- length 1)) (+ (cadr start-coord) (- length 1)))))
                 (if (collision matrix start-coord end-coord word)
                     (fit-to-board word matrix orientation seed2 (+ attempts 1))
                     start-coord)))
              ((equal orientation "left-up")
               (let* ((seed1 (next-seed seed))
                      (row (rand bounds seed1))
                      (seed2 (next-seed seed1))
                      (col (rand bounds seed2))
                      (coord (list col row))
                      (start-coord (fit-left-up coord length bounds))
                      (end-coord (list (- (car start-coord) (- length 1)) (- (cadr start-coord) (- length 1)))))
                 (if (collision matrix start-coord end-coord word)
                     (fit-to-board word matrix orientation seed2 (+ attempts 1))
                     start-coord)))))))

; (append-columns board)
; This will append an extra column to the end of each row.
; board the current playing surface to be expanded.
(defun append-columns (board)
  (if (endp board)
      '()
      (append (list (append (car board) '(#\.))) (append-columns (cdr board)))))

; (append-row board)
; This will append a row to the board.
; board - the current playing surface to be expanded.
(defun append-row (board)
  (append board (list (make-list (length (car board)) :initial-element #\.))))

; (expand-board board)
; This function will add an additional column and row to the playing matrix
; causing the playing surface to expand to accept more words to add to the
; crossword puzzle.
; board - the current playing surface to be expanded.
(defun expand-board (board)
  (append-columns (append-row board)))

; (plc-wdsrch words brd seed)
; Entry point for the placement module. 
; words - a list of words as strings '("word1" "word2" ... "wordn")
; brd - a game board with solutions (list (list (list #\. #\. #\.)
;                                               (list #\. #\. #\.)
;                                               (list #\. #\. #\.)) 
;                                               '())
; seed - an integer for the random  numbers
(defun plc-wdsrch (words brd seed)
  (if (endp words)
      brd
      (let* ((word (coerce (car words) 'list))
             (letter-board (car brd))
             (solutions    (cdr brd))
             (seed1 (next-seed seed))
             (type (rand 8 seed1))
             (seed2 (next-seed seed1))
             (orientation (cond ((= type 0) "right")
                                ((= type 1) "left")
                                ((= type 2) "down")
                                ((= type 3) "up")
                                ((= type 4) "right-down")
                                ((= type 5) "left-down")
                                ((= type 6) "right-up")
                                ((= type 7) "left-up")))
             (start-coord (fit-to-board word letter-board seed2 orientation 0)))
        (if start-coord
            (let* ((new-board (place letter-board word type start-coord))
                   (new-word-solution (list (car words) start-coord (get-end-coords start-coord (length word) type)))
                   (new-solutions (append solutions (list new-word-solution)))
                   (new-brd (cons new-board new-solutions)))
              (plc-wdsrch (cdr words) new-brd (next-seed seed2)))
            ; We don't have any place to put this word!  Expand the matrix.
            nil))))