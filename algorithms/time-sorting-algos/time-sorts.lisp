(in-package :algo.time-sorts)

(defparameter *si-prefixes* '((1 . nil)
                             (1000 . "thousands")
                             (1000000 . "millions")
                             (1000000000 . "billions")))

(defparameter *sorting-algorithms* '(("selection" "s")
                                     ("insertion" "i")
                                     ("mergesort" "m")
                                     ("quicksort" "q")))

(defparameter *input-types* '(("increasing" "i")
                              ("decreasing" "d")
                              ("constant" "c")
                              ("random" "r")))

;; steps for test array size per power of 10. E.g. '(1 2 5) will
;; test arrays of size 1, 2, 5, 10, 20, 50, 100, 200, 500, etc.
(defparameter *plotting-multipliers* '(1 2 4 6 8))
(defparameter *testing-multipliers* '(1))

(defun make-sizes (max-size multipliers)
  (append (loop :for power-of-ten := 1 :then (* 10 power-of-ten)
                :while (<= power-of-ten max-size)
                :nconc (loop :for multiplier :in multipliers
                             :for size := (* power-of-ten multiplier)
                             :while (<= size max-size)
                             :collect size))
          (list max-size)))

(defun test-once (size algo input-type &optional max-time)
  (declare ((integer 1 1000000000) size))
  (assert (<= size (expt 10 9)))

  (ignore-errors
   (let* ((command (list "sorting-algos/sorting"
                         (write-to-string size)
                         algo
                         input-type))
          (timeout-command (if max-time
                               (append (list "timeout" (write-to-string (float (/ max-time 1000)))) command)
                               command))
          (output (uiop:run-program timeout-command :output :string))
          (last-line (car (last (cl-ppcre:split "\\n" output)))))
     ;; (print timeout-command)
     ;; (print output)
     (values
      (parse-integer
       (aref (nth-value 1 (cl-ppcre:scan-to-strings
                           "Median time:\\s* (\\d+) ms"
                           last-line))
             0))))))

(defun time-all (max-size &key
                            (size-multipliers *testing-multipliers*)
                            (sorting-algorithms *sorting-algorithms*)
                            (input-types *input-types*)
                            max-time
                            verbose)
  (declare ((integer 1 1000000000) max-size))
  (assert (<= max-size (expt 10 9)))

  ;;(uiop:run-program '("make" "clean") :output t)
  ;;(uiop:run-program '("make") :output t)

  (let ((ht (make-hash-table :test #'equal)))
    (iter:iter
      (iter:with choices := (alexandria:map-product #'list
                                                    sorting-algorithms
                                                    input-types))
      (iter:with sizes := (make-sizes max-size size-multipliers))
      (iter:with num-tests := (* (length sizes) (length choices)))
      (iter:with test-number := 0)
      (iter:with num-types := (length choices))
      (iter:with type-number := 0)
      (iter:for ((algo-name algo-choice) (input-type input-type-choice)) :in choices)
      (if verbose (format t "~&Starting ~W" (list algo-name input-type)))
      (iter:iter
        (iter:with next-test-number := (+ test-number (length sizes)))
        (iter:for size :in sizes)
        (iter:for time := (test-once size algo-choice input-type-choice max-time))
        (iter:while time)
        (if verbose (format t "~&~A/~A (~A/~A)" (incf test-number) num-tests type-number num-types))
        (setf (gethash (list algo-name input-type) ht)
              (cons (cons size time) (gethash (list algo-name input-type) ht nil)))
        (iter:finally
         (decf num-tests (- next-test-number test-number))
         (incf type-number)
         (when verbose
           (unless time
             (format t "~&Skipping ~W after ~A elements timed out ~A ms"
                     (list algo-name input-type) size max-time))
           (format t "~&~A/~A (~A/~A)" test-number num-tests type-number num-types)))))

    (loop :for key :being :the :hash-key :in ht :using (hash-value times)
          :collect (list key (nreverse times)))))

(defun get-stats (times)
  (loop :for (params time-list) :in times
        :for min-valid := (iter:iter
                            (iter:for time :in time-list)
                            (when (>= (cdr time) 20)
                              (iter:finding time :minimizing (cdr time))))
        :for max-valid := (iter:iter
                            (iter:for time :in time-list)
                            (when (< (cdr time) (* 10 60 1000))
                              (iter:finding time :maximizing (cdr time))))
        :collect (list params min-valid max-valid)))

(defun get-magnitude (number &optional (prefix-names-alist *si-prefixes*))
  (loop :with largest := (caar prefix-names-alist)
        :for candidate :in prefix-names-alist
        :if (> (car candidate) number)
          :return largest
        :do (setf largest (car candidate))
        :finally (return largest)))

(defun group-by (key list &optional (eq-pred #'equal))
  (let ((ht (make-hash-table :test eq-pred))
        (order nil))
    (loop :for item :in list
          :for group := (funcall key item)
          :do (push item (gethash group ht))
              (unless (position group order :test eq-pred)
                (push group order)))
    (loop :for group :in (nreverse order)
          :collect (cons group (nreverse (gethash group ht))))))

(defun cl-ana-plot (times)
  (ensure-directories-exist #p"output/")
  (loop :for (algo . experiments) :in (group-by #'caar times) :do
    (print (format nil "output/~A.png" algo))
    (draw (page (loop :for experiment :in experiments
                      :for ((nil input-type) data-alist) := experiment
                      :for min := (reduce #'min data-alist :key #'car)
                      :for max := (reduce #'max data-alist :key #'car)
                      :for magnitude := (get-magnitude (1- max))
                      :for prefix := (cdr (assoc magnitude *si-prefixes*))
                      :for x-title := (if (= 1 magnitude)
                                          "Array size"
                                          (format nil "Array size (~A)" prefix))
                      :for scaled-times := (mapcar (lambda (trial)
                                                     (cons (floor (car trial) magnitude) (cdr trial)))
                                                   data-alist)
                      :collect (plot2d (list (line scaled-times
                                                   :style "linespoints"))
                                       :title input-type
                                       :title-offset '(1 . -1)
                                       :x-title x-title
                                       :x-title-offset '(3 . 0.5)
                                       :y-title "Median sort time (ms)"
                                       :y-title-offset '(1 . 1.75)
                                       :legend (legend :show nil)
                                       :x-tics (tics :sampling (floor (1+ (- max min)) (* magnitude 5)))
                                       :x-mtics nil))
                ;; :shown-title (format nil "~A" algo)
                :layout '(2 . 2)
                :scale '(0.95 . 0.95)
                :terminal (png-term :size '(640 . 480))
                :output (format nil "output/~A.png" algo)))))

(defun glue-tables (&rest tables)
  ;; Make sure each table has same number of rows
  (assert (every (lambda (n) (= n (length (car tables)))) (mapcar #'length tables)))
  (apply #'mapcar #'list
         (apply #'append (loop :for table :in tables
                               :collect (apply #'mapcar #'list table)))))

(defun make-stats-table (stats &optional latex-headers)
  (append
   (if latex-headers
       '(("Algorithm" "Input Type" "$n_{min}$" "$t_{min}$" "$n_{max}$" "$t_{max}$"))
       '(("Algorithm" "Input Type" "nmin" "tmin" "nmax" "tmax")))
   (loop :for experiment :in stats
         :collect (destructuring-bind ((algo input-type) (nmin . tmin) (nmax . tmax)) experiment
                    (list algo input-type nmin tmin nmax tmax)))))

(defun make-analysis-table (stats &optional latex-headers)
  (append (if latex-headers
              '(("Algorithm" "Input Type" "$t_{max}/t_{min}$" "$n_{max}/n_{min}$"
                 "$\\frac{n_{max} \\lg n_{max}}{n_{min} \\lg n_{min}}$" "$n_{max}^2/n_{min}^2$"))
              '(("Algorithm" "Input Type" "t1/t0" "n1/n0" "(n1 lg n1)/(n0 lg n0)" "n1^2/n0^2")))
          (loop :for ((algo input-type) (nmin . tmin) (nmax . tmax)) :in stats
                :collect (list algo
                               input-type
                               (float (/ tmax tmin))
                               (float (/ nmax nmin))
                               ;; (a lg a)/(b lg b) = a/b lg_b a
                               ;; This helps avoid floating point presentation errors
                               (float (* (/ nmax nmin) (log nmax nmin)))
                               (float (/ (expt nmax 2) (expt nmin 2)))))))

(defun write-times-csv (times path &optional (if-exists :supersede))
  (ensure-directories-exist path)
  (with-open-file (stream path :direction :output :if-exists if-exists)
    (write-csv (make-stats-table times) :stream stream)))

(defun write-object (object path)
  (with-open-file (stream path :direction :output :if-exists :supersede)
    (prin1 object stream)))

(defun read-object (path)
  (with-open-file (stream path :direction :input)
    (read stream)))
