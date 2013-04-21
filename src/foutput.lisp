(in-package "ACL2")

(include-book "create-board")
(include-book "bruteforcesolver")
(include-book "hillclimbingsolver")
(include-book "io-utilities" :dir :teachpacks)

(set-compile-fns t)
(set-state-ok t)

; (mux xs ys)
; This function is taken directly from lecture 2 for SEI.
; (mux (x1 x2 x3...) (y1 y2 y3...)) = (x1 y1 x2 y2 x3 y3...)
; xs = first list of elements
; ys = second list of elements
(defun mux (xs ys)
  (if (endp xs)
      ys
      (if (endp ys)
          xs
          (append (list (car xs) (car ys))
                  (mux (cdr xs) (cdr ys))))))

; (add-commas n xs)
; This function takes in a number and list of characters and returns
; a string of the characters in the list with commas inserted
; between each character.
; n = number of rows in grid - 1
; xs = row in grid
(defun add-commas (n xs)
  (let* ((ys (replicate n #\,))
         (xsys (mux xs ys)))
    (chrs->str xsys)))

; (add-dict-elements xs)
; This function takes in a list similar to
; (1 2 "right" 3) and returns a string of the form
; "x:1,y:2,direction:right,number-of-spaces:3"
; xs = list representing single word in solution
(defun add-dict-elements (xs)
  (let* ((x (rat->str (car xs) 0))
         (y (rat->str (cadr xs) 0))
         (dir (caddr xs))
         (nos (rat->str (cadddr xs) 0)))
    (concatenate 'string "x:" x ",y:" y ",direction:" dir ",numberofspaces:" nos)))

; (add-brackets n xss)
; This function takes in a list of lists of characters and returns
; a string of the form "[x00,x01,x02],[x10,x11,x12],[x20,x21,x22]".
; n = number of rows in grid - 1
; xss = game grid from create-board function
(defun add-brackets (n xss)
  (if (consp (cdr xss)) ;not last row in grid
      (let* ((f (add-commas n (car xss)))
             (first (concatenate 'string "[" f "],"))
             (rest (add-brackets n (cdr xss))))
        (concatenate 'string first rest))
      (concatenate 'string "[" (add-commas n (car xss)) "]")))

; (add-braces xss)
; This function takes in a list of lists representing the solution
; of a wordsearch. Inner lists are of the form
; (x y "direction" number-of-spaces), and the function returns a
; string of the form "{x:x1,y:y1,direction:dir1,number-of-spaces:n1},
; ...{x:xN,y:yN,direction:dirN,number-of-spaces:nN}
; xss = list of lists representing solution of wordsearch
(defun add-braces (xss)
  (if (consp (cdr xss))
      (let* ((f (add-dict-elements (car xss)))
             (first (concatenate 'string "{" f "},"))
             (rest (add-braces (cdr xss))))
        (concatenate 'string first rest))
      (concatenate 'string "{" (add-dict-elements (car xss)) "}")))

; (file->json gametype filename state)
; This function takes in a type of game, a file, and a state and
; returns a string that is formatted like JSON output.
; See create-words-list for more documentation on input.
; gt = type of game to create lists for
; fn = string representation of file to be read
; state = file->str state

;The way to call the function is as follows:
;(file->json 1 "C:/pathname/filename.txt" t)
;The file should be formatted as follows:
;word1
;word2
;word3
;...
;wordN

(defun create-board->json (gt words)
  (let* ((board (create-board words gt))
         (xss (car board))
         (soln (second board))
         (n (- (len xss) 1))
         (json-xss (concatenate 'string "[" (add-brackets n xss) "]")))
    (list json-xss soln)))

; (solver->json matrix solutions solvertype)
; This function takes a matrix representing a wordsearch table,
; a list of lists of characters representing words to find in
; said wordsearch table, and a number representing which type
; of solver to use, and returns a string resembling JSON
; formatting of the data as a dictionary.
; solvertype 1: brute force solver method
; solvertype 2: hill climber solver mehtod
; matrix = wordsearch table
; solutions = words list
; solvertype = which solver to use
(defun solver->json (matrix solutions solvertype)
  (if (= solvertype 1) ;use brute force method
      (concatenate 'string "[" (add-braces (brute-force-solver matrix solutions)) "]")
      (if (= solvertype 2) ;use hill climber method
          (concatenate 'string "[" (add-braces (hill-climbing-solver matrix solutions)) "]")
          nil))) ;else you're using a bogus solvertype, get out