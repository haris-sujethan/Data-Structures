(defun arith-eval (expr)
  "EXPR is a list of symbols that may include:
square brackets, arithmetic operations, and numbers."

  (let ((ops ())
        (vals ())
        (op nil)
        (val nil)
        (brac ())
        (fact 1)
        (x 0)
        (y 0))
    
    (dolist (item expr)
      (case item
        ([ (push item brac)) 
        ((+ - * / ^ SDIV MAXF FACT) (push item ops))
        (] (setf op (pop ops) val (pop vals)) (push item brac)
         (case op
           
           (+ (setf val (+ val (pop vals))))
           (- (setf val (- (pop vals)  val)))
           (* (setf val (* val (pop vals))))
           (^ (setf val (expt (pop vals) val)))
           (/ (setf val (/ (pop vals) val)))

           (SDIV (setf x (pop vals)) (setf y (pop vals)) (setf val (/ (- y x) val)))
           (MAXF (setf val (max val (pop vals) (pop vals))))
           (FACT (setf val (dotimes (i (+ val 1) fact)
                             (if (= i 0) fact (setf fact (* fact i)))))))
       
         (push val vals))
        (otherwise (push item vals))))
    (setf num (mod (length brac) 2))
    (if (= num 1) (print "Error")
        (pop vals))))
