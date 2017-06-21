(in-package :algo.bonus-project)

(defun make-agreement-matrix (subjects)
  (let* ((n (length subjects))
         (agrees (make-array (list n n) :element-type '(integer 0 2) :initial-element 0)))
    (loop :for i :from 0 :below n :do
      (loop :for j :from 0 :below n
            :do (when (and (subject> (aref subjects i) (aref subjects j))
                           (subject<= (aref subjects j) (aref subjects i)))
                  (setf (aref agrees i j) 2)
                  (setf (aref agrees j i) 1))))
    agrees))
