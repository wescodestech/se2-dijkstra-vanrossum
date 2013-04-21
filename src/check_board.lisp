(in-package "ACL2")

(include-book "list-utilities" :dir :teachpacks)

; returns a single nth item out of a list
; n is a number, zero indexed
; list is a list
(defun get_n (n list)
  (if (< n (length list))
  (first (last (first (break-at-nth (+ n 1) list))))
  nil))

(defun make_nil_char (char)
  (if (characterp char)
      char
      #\~))

; x and y are the start position of letter, zero indexed
; letter is the char we are checking, 
; sol_x and sol_y is the start position of the word
; chrs_list is the word split into a list of chars
(defun check-one-solution-horizontal (x y letter sol_x sol_y chrs_list)
  (if (equal y sol_y)
      (if (char-equal (make_nil_char (get_n (- x sol_x) chrs_list)) letter)
          letter
          nil)
      nil))

; same as above
(defun check-one-solution-vertical (x y letter sol_x sol_y chrs_list)
  (if (equal x sol_x)
      (if (char-equal (make_nil_char (get_n (- y sol_y) chrs_list)) letter)
          letter
          nil)
      nil))

; x and y are the start position of letter, zero indexed
; letter is the char we are checking
; solution is in the form ("word" (sol_x sol_y) (sol_x_end sol_y_end))
; and all sol_# values are numbers
(defun check-one-solution (x y letter solution)
  (let* ((sol_x (first (second solution)))
         (sol_y (second (second solution)))
         (sol_x_end (first (third solution)))
         (sol_y_end (second (third solution)))
         (chrs_list (str->chrs (first solution)))
         (rev_chrs_list (reverse chrs_list)))
    (if (equal sol_x sol_x_end)
        (if (< sol_y sol_y_end)
            (check-one-solution-vertical x y letter sol_x sol_y chrs_list); vertical
            (check-one-solution-vertical x y letter sol_x_end sol_y_end rev_chrs_list)); vertical
        (if (< sol_x sol_x_end)    
            (check-one-solution-horizontal x y letter sol_x sol_y chrs_list) ; horizontal
            (check-one-solution-horizontal x y letter sol_x_end sol_y_end rev_chrs_list)) ; horizontal
        )))

; x and y are the start position of the letter, zero indexed
; letter is a char here
; solutions is a list of solution
; will return the char if it is correct, nil if it is incorrect
(defun check-solution (x y letter solutions)
  (if (consp solutions)
      (let* ((output (check-one-solution x y letter (car solutions))))
        (if (characterp output)
            output
            (check-solution x y letter (cdr solutions))))
      nil
  ))
      
; letter is the string we are checking (make it length of 1)
; this is the method to call
(defun check-solution-entry (x y letter solutions)
  (check-solution x y (first (str->chrs letter)) solutions))