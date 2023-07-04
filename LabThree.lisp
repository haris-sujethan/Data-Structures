(defstruct movie
  title  director year type)

(defparameter *size* 3) 

(defvar *db*)

(setf *db* (make-array *size*  :initial-element nil))

(defun add-movie (m)
  "Adds a movie to *db* and returns *db*"
  (dotimes (i *size*)
    (if (>= i *size*)
        (return nil))
    (when (not (null (aref *db* i)))
      (if (equalp (aref *db* i) m)
          (return nil)))
    (when (null (aref *db* i))
      (setf (aref *db* i) m)
      (return *db*))))
  

(defun in-db? (title)
  "Returns *db* if movie title is in the database; otherwise returns NIL"

  (dotimes (i *size*)
    (when (and (typep (aref *db* i) 'movie)
               (equal (movie-title (aref *db* i)) title))
      (return *db*))))

(defun delete-movie (title)
  (dotimes (i *size*)
    (when (and (typep (aref *db* i) 'movie)
               (equal (movie-title (aref *db* i)) title))
      (delete (aref *db* i) *db*)
      (setf (aref *db* i) nil)
      (return *db*))))


(defun replace-movie (m nm)
  (dotimes (i *size*)
    (when (and (typep (aref *db* i) 'movie)
               (equal (movie-title (aref *db* i)) nm))
      (return nil)))
  (dotimes (i *size*)
    (when (and (typep (aref *db* i) 'movie)
               (equal (movie-title (aref *db* i)) m))
      (setf (aref *db* i) nm)
      (return t))))

(defun num-movies ()
  (setq sum 0)
  (dotimes (i *size*)
    (when (not (null (aref *db* i)))
      (print "Hello")
      (setq sum (+ sum 1))))
  (print sum))
