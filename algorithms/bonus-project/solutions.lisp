;;; This file is for testing various solutions to the assignment before
;;; Finalizing and writing supplied/dksort.cpp

(in-package :algo.bonus-project)

(defun make-agreement-matrix (subjects)
  (let* ((n (length subjects))
         (agrees (make-array (list n n) :element-type '(member _ > <) :initial-element '_)))
    (loop :for i :from 0 :below n :do
      (loop :for j :from 0 :below n
            :do (when (and (subject> (aref subjects i) (aref subjects j))
                           (subject<= (aref subjects j) (aref subjects i)))
                  (setf (aref agrees i j) '>)
                  (setf (aref agrees j i) '<))))
    agrees))

(defun make-greater-matrix (subjects)
  (let* ((n (length subjects))
         (greater (make-array (list n n) :element-type '(member _ %) :initial-element '_)))
    (loop :for i :from 0 :below n :do
      (loop :for j :from 0 :below n
            :do (when (subject> (aref subjects i) (aref subjects j))
                  (setf (aref greater i j) '%))))
    greater))

(defun make-less-matrix (subjects)
  (let* ((n (length subjects))
         (less (make-array (list n n) :element-type '(member _ %) :initial-element '_)))
    (loop :for i :from 0 :below n :do
      (loop :for j :from 0 :below n
            :do (when (subject<= (aref subjects j) (aref subjects i))
                  (setf (aref less i j) '%))))
    less))
