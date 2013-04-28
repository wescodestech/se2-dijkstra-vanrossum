(in-package "ACL2")

(include-book "rand" :dir :teachpacks)

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

; We should attempt 10 times before expanding the matrix
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