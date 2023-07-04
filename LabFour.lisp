(defstruct movie
  title  director year type)

(defparameter *size* 3)

(defvar *db*)

(setf *db* (make-array *size*  :initial-element nil))

(defvar *db-list*)

(setf *db-list*  nil)  

(defun quicksort (vec comp)
  (when (> (length vec) 1)
    (let ((pivot-i 0)
          (pivot (aref vec (1- (length vec)))))
      (dotimes (i (1- (length vec)))
        (when (funcall comp (aref vec i) pivot)
          (rotatef (aref vec i)
                   (aref vec pivot-i))
          (incf pivot-i)))
      (rotatef (aref vec (1- (length vec)))
               (aref vec pivot-i))
      (quicksort (rtl:slice vec 0 pivot-i) comp)
      (quicksort (rtl:slice vec (1+ pivot-i)) comp)))
  vec)

(defun add-movie (m)
  "Adds a movie to the DB and returns true"
  (dotimes (i *size*)
    (when (null (aref *db* i))
      (setf (aref *db* i) m)
      (return *db*))))

(defun sort-title ()
  (quicksort *db* 'string<=))

(defun sort-year ()
  (quicksort *db* '<))

(defun add-movie-list (m)
  "Adds a movie to the end of *db-list* and returns the list"

  (loop for x in *db-list*
        :do (if (equalp (movie-title x) m)
                (return nil)))
  (setf *db-list* (append *db-list* (list m))))
  
  
      

(defun in-db-list? (title)
  (dolist (elem *db-list*)
    (when (equal (movie-title elem) title)
      (return *db-list*))))

(defun from-year (year)
  (setq year-list '())
  (loop for x in *db-list*
        :do (if (equalp (movie-year x) year)
                (setq year-list (append year-list (list x)))))
        (print year-list))
