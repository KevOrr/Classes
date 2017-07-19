;;; This file is for testing various solutions to the assignment before
;;; Finalizing and writing supplied/dksort.cpp

(in-package :algo.bonus-project)

(defun plot-belief-functions ()
  (draw
   (plot2d (list (line #'identity :title "Subject" :sampling '(:low 0 :high 100 :nsamples 5000))
                 (line #'slightly-off-function :title "Slightly off" :sampling '(:low 0 :high 100 :nsamples 5000))
                 (line #'delusional-function :title "Delusional" :sampling '(:low 0 :high 100 :nsamples 5000)))
           :x-title "Competence"
           :y-title "Belief"
           :x-range '(0 . 100)
           :y-range '(0 . 100)
           :x-tics (tics :sampling 10)
           :y-tics (tics :sampling 10)
           :legend (legend :location '(:left :top)))))

(defun plot-array (subjects)
  (draw (plot2d (list (line (loop :for (nil . comp) :across subjects
                             :for i :from 0
                             :collect (cons i comp))
                       :point-type "7"
                       :point-size "0.4"))
                :legend (legend :show nil)
                :x-title "Position in array"
                :y-title "Competence"))
  subjects)

(defun cheat-sort (subjects)
  (let ((subjects (make-array (array-dimensions subjects) :initial-contents subjects)))
    (sort subjects #'< :key #'cdr)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun sort-by-superiority-count (subjects)
  (let* ((n (length subjects))
         (counts (make-array n))
         (high-bound (ceiling (* *high-competence* n) 100)))
    (loop :for i :from 0 :below n
          :do (setf (aref counts i) (list 0 0 (aref subjects i)))
              (loop :for j :from 0 :below n
                    :unless (= i j)
                      :do (if (subject<= (aref subjects j) (aref subjects i))
                              (incf (first (aref counts i))))
                          (if (subject> (aref subjects i) (aref subjects j))
                              (incf (second (aref counts i))))))

    (setf counts (sort counts #'< :key #'first))
    (rotatef (aref counts (1- high-bound))
             (aref counts (iter:iter (iter:for i :from 0 :below n)
                            (iter:finding i :maximizing (second (aref counts i))))))
    (map 'vector #'third counts)))

(defun sort-extremes (subjects)
  (let* ((n (length subjects))
         (high-bound (ceiling (* *high-competence* n) 100))
         (low-bound (floor (1+ (/ (* *low-competence* n) 100))))
         (high-count (make-array (- n high-bound) :element-type 'integer :initial-element 0))
         (low-count (make-array (1+ low-bound) :element-type 'integer :initial-element 0)))

    (loop :for i :from 0 :to low-bound :do
      (setf (aref low-count i)
            (cons (loop :for j :from (1+ low-bound) :below high-bound
                        :counting (subject> (aref subjects i) (aref subjects j)))
                  (aref subjects i))))

    (loop :for i :from high-bound :below n :do
      (setf (aref high-count (- i high-bound))
            (cons (loop :for j :from (1+ low-bound) :below n
                        :counting (subject> (aref subjects i) (aref subjects j)))
                  (aref subjects i))))

    (concatenate 'vector
                 (map 'vector #'cdr (stable-sort low-count #'> :key #'car))
                 (subseq subjects (1+ low-bound) high-bound)
                 (map 'vector #'cdr (stable-sort high-count #'< :key #'car)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun best-solution-so-far (subjects)
  (sort-extremes (sort-by-superiority-count subjects)))
