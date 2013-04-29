(in-package "ACL2")

(defun along-axis (check-x check-y next-x next-y direction duration)
  (if (and (= check-x next-x) (= check-y next-y))
      t
      (if (> duration 0)
          (cond ((equal direction "left")
                 (along-axis check-x check-y (- next-x 1) next-y direction (- duration 1)))
                ((equal direction "right")
                 (along-axis check-x check-y (+ next-x 1) next-y direction (- duration 1)))
                ((equal direction "up")
                 (along-axis check-x check-y next-x (- next-y 1) direction (- duration 1)))
                ((equal direction "down")
                 (along-axis check-x check-y next-x (+ next-y 1) direction (- duration 1)))
                ((equal direction "left-up")
                 (along-axis check-x check-y (- next-x 1) (- next-y 1) direction (- duration 1)))
                ((equal direction "left-down")
                 (along-axis check-x check-y (- next-x 1) (+ next-y 1) direction (- duration 1)))
                ((equal direction "right-up")
                 (along-axis check-x check-y (+ next-x 1) (- next-y 1) direction (- duration 1)))
                ((equal direction "right-down")
                 (along-axis check-x check-y (+ next-x 1) (+ next-y 1) direction (- duration 1))))
          nil))) ; Not located in this solution term

; letter is the string we are checking (make it length of 1)
; this is the method to call
; The letter is insignificant - Matt
(defun check-solution-entry (x y letter solutions)
  (if (endp solutions)
      nil ; It is not a part of any solution
      (let* ((next-try (car solutions))
             (word (coerce (car next-try) 'list))
             (start (cadr next-try))
             (start-x (car start))
             (start-y (cadr start))
             (end (caddr next-try))
             (end-x (car end))
             (end-y (cadr end))
             (placement-type
              ;; left-down, left-up or left
              (cond ((> start-x end-x)
                     (cond ((> start-y end-y) "left-up")
                           ((< start-y end-y) "left-down")
                           ((= start-y end-y) "left")))
                    ;; right-down, right-up or right
                    ((< start-x end-x)
                     (cond ((> start-y end-y) "right-up")
                           ((< start-y end-y) "right-down")
                           ((= start-y end-y) "right")))
                    ;; up, down or singlet
                    ((= start-x end-x)
                     (cond ((> start-y end-y) "up")
                           ((< start-y end-y) "down")
                           ((= start-y end-y) "singlet")))))
             (found (along-axis x y start-x start-y placement-type (- (length word) 1))))
        (if found
            t
            (check-solution-entry x y letter (cdr solutions))))))