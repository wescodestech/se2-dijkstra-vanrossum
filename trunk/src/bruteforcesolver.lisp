(in-package "ACL2")

;This file contains the brute force functions for solving a wordsearch puzzle. 
;
;Example format for a 4x4 matrix is as follows
; Each entry in the list of lists represents a row. 
;
; (("a" "d" "f" "g")("e" "w" "r" "c")("u" "j" "h" "g")( "b" "n" "j" "k"))
;
; The list of solution will be passed in as:
;(("c" "a" "t")("d" "o" "g") ...)
;
;Authored by Cezar Delucca

;concat-count-helper (strList)
;This function is a helper function for concatenating a list of strings into a 
;single string
;strList = the string list
(defun concat-helper (strList)
  (if (endp strList)
      ""
      (concatenate 'string (car strList) 
                   (concat-helper (cdr strList))
                   )))

;Concat-Count (solutions)
;This function takes in a list of character string representations of
;the solutions and concats into a single string and stores its length 
; for the solver to utilize in string matching.
;solutions = the list of chracter strings
;
;Example
;(concat-count (list (list "c" "a" "t")(list "d" "o" "g")(list "t" "u" "r" "t" "l" "e")( list "s" "n" "a" "k" "e")))
; returns -> (("cat" 3) ("dog" 3) ("turtle" 6) ("snake" 5))
(defun concat-count (solutions)
  (if (endp solutions)
      nil
      (cons (list (concat-helper (car solutions))
                  (len (car solutions)))(concat-count (cdr solutions)))
            ))

; nthrdc (xs)
; This function is similar to nthcdr except we are 
; going backwards and removing the nth elements
; from the end of the list.
; n = the number of elements to delete
; xs = the list
(defun nthrdc (n xs)
 (reverse (nthcdr n (reverse xs)))
  )
  
;linear-row-search-helper (row sol)
;This function does the string comparison for each row
; and returns a vector if a solution is found.
;row = the current row
;sol = the current solution to find within the row
;leftOrRight = the direction of search
;rowNum = the current row number
;rowLen = the length of the total row to determine col num
(defun linear-search-helper (row sol direction rowNum colIndex rowSize)
  (if (< (len row) (cadr sol))
      nil ;the length of the current row is
          ;smaller than the solution, hence its not on this row.
      (if (equal (concat-helper (nthrdc (- (len row)(cadr sol)) row ))
                              (car sol);if the sub-row matches the solution
                              )
                              ;found solution
                              ;resulting vector (starting coords, direction, num of spaces from starting coords)
                              (cond ((equal direction "right")(list rowNum colIndex  direction (- (cadr sol) 1)));right coords)
                                    ((equal direction "left")(list rowNum (- rowSize colIndex)  direction (- (cadr sol) 1)));left coords
                                    ((equal direction "up")(list (- rowSize colIndex) rowNum  direction (- (cadr sol) 1)) )
                                    ((equal direction "down")(list colIndex rowNum direction (- (cadr sol) 1)))
                                    )
                              ;else keep searching within the current row, but the next col
          (linear-search-helper (cdr row) sol direction rowNum (+ colIndex 1) rowSize)
          )))
   

;linear-row-search (sol rows)
;This function takes in a solution and
; a list of rows and does extensive string matching
;within the bounds of each row.
;sol = the target solution
;rows = the matrix in row major order
;leftOrRight = the direction of search

;Note: This is used for searching left to right 
; and right to left only. 
(defun linear-search (sol rows direction rowNum)
  (if (endp rows)
      nil
      (let* ((vect (linear-search-helper (car rows) sol direction
                                             (+ rowNum 1)  0 (- (len (car rows)) 1))))
        (if (not (equal vect nil))
            vect
            (linear-search sol (cdr rows) direction (+ rowNum 1))
   ))))

;search-left-to-right (matrix solList)
;This function searches the matrix from left to right
;in order to string match the solutions.
;If solutions are found, then it returns a list of vectors.
;matrix = the game board
;solList = the concatenated list of string characters along with their word sizes.
(defun search-left-to-right (matrix solList)
(if (endp solList) ; no more solutions to check for
    nil
    (cons (linear-search (car solList) matrix "right" -1);start indexing the row
          (search-left-to-right matrix (cdr solList)
                                ))))

;reverse-matrix (matrix)
; This function takes in a matrix
;in row order form and reverses it.
;matrix = the matrix to be reversed
(defun reverse-matrix (matrix)
  (if (endp matrix)
      nil
      (cons (reverse (car matrix))(reverse-matrix (cdr matrix)))
   ))

;search-right-to-left (matrix solList)
;This function searches the matrix from right to left
;in order to string match the solutions.
;If solutions are found, then it returns a list of vectors.
;matrix = the game board
;solList = the concatenated list of string characters along with their word sizes.
(defun search-right-to-left (matrix solList)
(if (endp solList) ; no more solutions to check for
    nil
    (cons (linear-search (car solList)
                             (reverse-matrix matrix) "left" -1);start indexing the row
          (search-right-to-left matrix (cdr solList)
                                ))))

;transpose-helper (matrix index)
;Thie function acts as a helper for transpose by
;iterating through the whole matrix and creating a column
;out of the given row index.
;matrix = the matrix to be flipped
;index = the row index
(defun transpose-helper (matrix index)
  (if (endp matrix)
      nil
      (cons (car (nthcdr index (car matrix)))
            (transpose-helper (cdr matrix) index))
      ))

;transpose (matrix)
;This function takes in a matrix
;and returns its transpose (flipping to the right)
;matrix = the matrix to flip
;rowLength = the size of every row in the matrix
;Note: To be used for searching up to down and down to up
(defun transpose (matrix rowLength index)
  (if (= rowLength index)
      nil
      (cons (transpose-helper matrix index)
            (transpose matrix rowLength (+ index 1)))
      ))

;search-up-to-down (matrix solList)
;This function searches the matrix from up to down
;in order to string match the solutions.
;If solutions are found, then it returns a list of vectors.
;tmatrix = the transposed matrix
;solList = the concatenated list of string characters along with their word sizes.
(defun search-up-to-down (tmatrix solList)
(if (endp solList) ; no more solutions to check for
    nil
    (cons (linear-search (car solList) tmatrix "down" -1);start indexing the col
          (search-up-to-down tmatrix (cdr solList)
                                ))))

;search-down-to-up (matrix solList)
;This function searches the matrix from down to up
;in order to string match the solutions.
;If solutions are found, then it returns a list of vectors.
;tmatrix = the transposed matrix
;solList = the concatenated list of string characters along with their word sizes.
(defun search-down-to-up (tmatrix solList)
(if (endp solList) ; no more solutions to check for
    nil
    (cons (linear-search (car solList)
                             (reverse-matrix tmatrix) "up" -1);start indexing the row
          (search-down-to-up tmatrix (cdr solList)
                                ))))

;clean-results (results)
;This function takes the list of vectors
;and removes all of the nil entries
;that each search direction returns if 
;the word was not found. This allows the
;results to be human readable and easy to work with
;for modularity.
;results = the list of vectors
(defun clean-results (results)
  (if (endp results)
      nil
      (if (equal (car results) nil)
          (clean-results (cdr results))
          (cons (car results) (clean-results (cdr results)))
          )))
   

;brute-force-solver (matrix solutions)
; This function utilizes a brute force algorithm
; to search, match, and return locations within the grid
; with the pass-in solutions. This is the only function which should be called externally. 
; matrix = the populated grid
; solutions = a list of words that we are searching for
;
;**Note: we are excluding diagonals due to complexity**
;
; The result is a list of vectors of the form (x y direction number-of-spaces). 
;Each x and y is zero indexed and the number of spaces is starting from that coordinate
;and going in the direction specified.
(defun brute-force-solver (matrix solutions)
  (let* ((solList (concat-count solutions));concatenate sols with their sizes
         (searchLeftToRight(search-left-to-right matrix solList))
         (searchRightToLeft(search-right-to-left matrix solList))
         (searchUpToDown (search-up-to-down (transpose matrix (len (car matrix)) 0) solList));invert the matrix and use implemented logic
         (searchDownToUp (search-down-to-up (transpose matrix (len (car matrix)) 0) solList)))
        (clean-results(concatenate 'list searchLeftToRight searchRightToLeft
                     searchUpToDown searchDownToUp));return a list of all vectors found. 
   ))

;TESTING...this is how we use this solver.
;(brute-force-solver(list  (list "w" "r" "y" "i" "g" "g" "d" "a" "m") ;example game board
;                          (list "t" "a" "c" "g" "i" "q" "p" "u" "r") 
;                          (list "p" "t" "t" "o" "c" "f" "d" "f" "o") 
;                          (list "d" "o" "g" "t" "s" "c" "a" "c" "w")
;                          (list "r" "p" "p" "t" "w" "w" "r" "g" "o")
;                          (list "e" "p" "t" "o" "g" "o" "x" "z" "a")
;                          (list "w" "f" "t" "r" "q" "r" "w" "d" "b")
;                          (list "e" "d" "t" "r" "i" "r" "t" "f" "p")
;                          (list "h" "d" "t" "a" "r" "a" "t" "f" "p")
;                          (list "e" "w" "t" "p" "a" "p" "r" "o" "t")
;                          (list "i" "i" "u" "u" "q" "s" "o" "k" "j")
;                          (list "a" "u" "d" "d" "f" "j" "h" "w" "q")
;                          (list "j" "d" "f" "a" "g" "l" "f" "g" "d")
;                          (list "p" "d" "w" "o" "c" "n" "o" "o" "r")
;                          (list "u" "s" "j" "s" "k" "m" "x" "h" "x")
;                          (list "p" "i" "g" "p" "p" "p" "p" "p" "p"))
;                    
;                    (list (list "c" "a" "t"); example word list (some appear multiple times for testing purposes)
;                          (list "d" "o" "g") 
;                          (list "p" "i" "g")
;                          (list "r" "a" "t") 
;                          (list "p" "u" "p") 
;                          (list "p" "a" "r" "r" "o" "t") 
;                          (list "s" "p" "a" "r" "r" "o" "w") 
;                          (list "w" "o" "r" "m") 
;                          (list "f" "o" "x") 
;                          (list "h" "o" "g") 
;                          (list "c" "o" "w") 
;                          )
;                    )



