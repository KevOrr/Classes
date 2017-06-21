(in-package :algo.bonus-project)

(defparameter *low-competence* 25)
(defparameter *high-competence* 90)

(defun get-belief (subject)
  (ecase (car subject)
    (:subject
     (cdr subject))
    (:slightly-off
     (let ((competence (cdr subject)))
       (cond ((< competence *low-competence*)
              (+ (* 1.01 *low-competence*) (* -0.01 competence) 1.5))
             ((>= competence *high-competence*)
              (- competence 1.5))
             (t competence))))
    (:delusional
     (let ((competence (cdr subject)))
       (cond ((< competence *low-competence*)
              (+ (* 3 *low-competence*) (* -2 competence) 5.5))
             ((>= competence *high-competence*)
              (+ (* competence 0.5) *low-competence* -0.25))
             (t competence))))))

(defun subject<= (subject &rest more-subjects)
  (loop :for a :in (cons subject more-subjects)
        :for b :in more-subjects
        :if (> (cdr a) (get-belief b))
          :return nil
        :finally (return t)))

(defun subject> (subject &rest more-subjects)
  (loop :for a :in (cons subject more-subjects)
        :for b :in more-subjects
        :if (<= (cdr a) (get-belief b))
          :return nil
        :finally (return t)))

(defun shuffle (subjects)
  (declare (vector subjects))
  (let ((subjects (make-array (array-dimensions subjects) :initial-contents subjects))
        (n (length subjects)))
    (loop :for i :from 0 :below (1- n)
          ;; This is not uniform. Correct implementation should be
          ;; `(+ (random (- n i)) i)'. The last element has (n-1)/n probability
          ;; of staying put after the shuffle. Take advantage?
          :for rand-index := (random (- n i))
          :do (rotatef (aref subjects i) (aref subjects rand-index)))
    subjects))

;; Horner's method. (eval-polynomial '(1 2 3) x) -> {1 + 2x + 3x^2}
;; (eval-polynomial '(1 2 3) x) -> (+ 1 (* x (+ 2 (* x 3))))
(defun eval-polynomial (coefficients var)
  (reduce (lambda (old new) (+ new (* var old)))
          coefficients))

(defun normcdf (x)
  (let* ((a1 0.254829592)
         (a2 -0.284496736)
         (a3 1.421413741)
         (a4 -1.453152027)
         (a5 1.061405429)
         (p 0.3275911)
         (x (/ x (sqrt 2.0)))
         (tee (/ 1.0 (1+ (* p x))))
         (y (1- (* (eval-polynomial (list a5 a4 a3 a2 a1 0) tee)
                   (exp (- (* x x)))))))
    (* 0.5 (1+ y))))

(defun score (subjects)
  (declare (vector subjects))
  (let* ((n (length subjects))
         (count
           (iter:iter top
             (iter:for i :from 0 :below n)
             (iter:iter
               (iter:for j :from (1+ i) :below n)
               (iter:in top (iter:counting (>= (cdr (aref subjects i))
                                               (cdr (aref subjects j)))))))))
    (normcdf (/ (* count 2.0) n (1+ n)))
    ))

(defun make-subjects (type &optional (n 100))
  (shuffle
   (coerce (loop :for i :from 0 :below n
                 :collect (cons type (/ (* 100.0 i) n)))
           'vector)))

(defun test-solution (sort-fn
                      &optional
                        (subject-types '(:subject :slightly-off :delusional))
                        (repeat 5)
                        (n 100))
  (iter:iter top
    (iter:for type :in subject-types)
    (iter:iter
      (iter:repeat repeat)
      (let* ((subjects (make-subjects type n))
             (start (get-internal-run-time))
             (results (funcall sort-fn subjects))
             (delta (- (get-internal-run-time) start))
             (score (score results)))
        (iter:sum delta :into timesum)
        (iter:sum score :into scoresum))
      (iter:finally
       (iter:in
        top
        (iter:collect (list* type
                             (float (/ scoresum repeat))
                             (float (/ timesum internal-time-units-per-second repeat)))))))))
