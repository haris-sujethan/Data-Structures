(Defun a-sum (x y)
  (setq sum 0)
  (loop :for i :from x :to y
        :do (setq sum (+ sum i)))
  (print sum))

(defun sum-odd (x y)
  (setq sum 0)
  (loop :for i :from x :to y
        :do (setq z (mod i 2))
        (if (= z 0)
            :do (setq sum (+ sum i))))
        (print sum))

(defun my-function (f) (funcall f 1))

(defun sigma (f n p)
  (setq sum 0)
  (loop for i :from n :to p
        :do (setq sum (+ sum (funcall f i))))
  (print sum))
