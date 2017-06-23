;;; This file is for testing various solutions to the assignment before
;;; Finalizing and writing supplied/dksort.cpp

(in-package :algo.bonus-project)

(defun plot-belief-functions ()
  (draw
   (plot2d (list (line #'identity :title "Subject" :sampling '(:low 0 :high 100))
                 (line #'slightly-off-function :title "Slightly off" :sampling '(:low 0 :high 100))
                 (line #'delusional-function :title "Delusional" :sampling '(:low 0 :high 100)))
           :x-title "Competence"
           :y-title "Belief"
           :x-range '(0 . 100)
           :y-range '(0 . 100)
           :legend (legend :location '(:left :top)))))

(defun plot-array (subjects)
  (draw (loop :for (nil . comp) :across subjects
              :for i :from 0
              :collect (cons i comp))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun make-agreement-matrix (subjects)
  (let* ((n (length subjects))
         (agrees (make-array (list n n) :element-type '(member 0 > <) :initial-element '0)))
    (loop :for i :from 0 :below n :do
      (loop :for j :from 0 :below n
            :do (when (and (subject> (aref subjects i) (aref subjects j))
                           (subject<= (aref subjects j) (aref subjects i)))
                  (setf (aref agrees i j) '>)
                  (setf (aref agrees j i) '<))))
    agrees))

(defun make-greater-matrix (subjects)
  (let* ((n (length subjects))
         (greater (make-array (list n n) :element-type 'bit :initial-element '0)))
    (loop :for i :from 0 :below n :do
      (loop :for j :from 0 :below n
            :do (when (subject> (aref subjects i) (aref subjects j))
                  (setf (aref greater i j) '1))))
    greater))

(defun make-less-matrix (subjects)
  (let* ((n (length subjects))
         (less (make-array (list n n) :element-type 'bit :initial-element '0)))
    (loop :for i :from 0 :below n :do
      (loop :for j :from 0 :below n
            :do (when (subject<= (aref subjects j) (aref subjects i))
                  (setf (aref less i j) '1))))
    less))

(defun directed-reachable (adjacency-matrix root-index target-index)
  (let ((visited (make-array (array-dimension adjacency-matrix 0) :element-type 'bit :initial-element 0)))
    (labels ((helper (my-root)
               (cond ((= my-root target-index) t)
                     (t (setf (aref visited my-root) 1)
                        (loop :for v :from 0 :below (array-dimension adjacency-matrix 0) :do
                          (if (and (= 1 (aref adjacency-matrix my-root v))
                                   (= 0 (aref visited v)))
                              (let ((foundp (helper v)))
                                (if foundp
                                    (return foundp)))))))))
      (helper root-index))))

(defun make-less-reachable-matrix (subjects)
  (let* ((less-matrix (make-less-matrix subjects))
         (less-reachable (make-array (array-dimensions less-matrix) :element-type 'bit :initial-element 0)))
    (loop :for i :from 0 :below (length subjects) :do
      (loop :for j :from 0 :below (length subjects) :do
        (setf (aref less-reachable i j) (if (directed-reachable less-matrix i j) 1 0))))
    less-reachable))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun sort-by-superiority-count (subjects)
  (let* ((n (length subjects))
         (counts (make-array n)))
    (loop :for i :from 0 :below n
          :do (setf (aref counts i) (cons 0 (aref subjects i)))
              (loop :for j :from 0 :below n
                    :unless (= i j)
                      :do (incf (car (aref counts i))
                              (if (subject<= (aref subjects j) (aref subjects i)) 1 0))))
    (coerce (loop :for (nil . subject) :across (sort counts #'< :key #'car)
                  :collect subject)
            'vector)))

(defun sort-extremes (subjects)
  (let* ((n (length subjects))
         (high-bound (ceiling (/ (* *high-competence* n) 100)))
         (low-bound (floor (/ (* *low-competence* n) 100)))
         (high-count (make-array (- n high-bound) :element-type 'integer :initial-element 0))
         (low-count (make-array (1+ low-bound) :element-type 'integer :initial-element 0)))

    (loop :for i :from 0 :to low-bound :do
      (setf (aref low-count i)
            (cons (loop :for j :from (1+ low-bound) :below high-bound
                        :counting (subject> (aref subjects i) (aref subjects j)))
                  (aref subjects i))))

    (loop :for i :from high-bound :below n :do
      (setf (aref high-count (- i high-bound))
            (cons (loop :for j :from (1+ low-bound) :below high-bound
                        :counting (subject> (aref subjects i) (aref subjects j)))
                  (aref subjects i))))

    (concatenate 'vector
                 (loop :for (nil . subject) :across (sort low-count #'> :key #'car)
                       :collect subject)
                 (subseq subjects (1+ low-bound) high-bound)
                 (loop :for (nil . subject) :across (sort high-count #'< :key #'car)
                       :collect subject))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun best-solution-so-far (subjects)
  (sort-extremes (sort-by-superiority-count subjects)))
