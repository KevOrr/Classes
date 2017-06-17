(in-package :time-sorts)

(defparameter *sorting-algorithms* '(
                                     ("selection sort" "s")
                                     ("insertion sort" "i")
                                     ("merge sort" "m")
                                     ("quicksort" "q")
                                     ))

(defparameter *input-types* '(
                              ("increasing" "i")
                              ;; ("decreasing" "d")
                              ;;("constant" "c")
                              ;;("random" "r")
                              ))

;; steps for test array size per power of 10. E.g. '(1 2 5) will
;; test arrays of size 1, 2, 5, 10, 20, 50, 100, 200, 500, etc.
(defparameter *array-size-multipliers* '(1 2 4 6 8))

(defun make-sizes (max-size multipliers)
  (loop :for power-of-ten := 1 :then (* 10 power-of-ten)
        :while (<= power-of-ten max-size)
        :nconc (loop :for multiplier :in multipliers
                     :for size := (* power-of-ten multiplier)
                     :while (<= size max-size)
                     :collect size)))

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
     (print timeout-command)
     ;; (print output)
     (nth-value
      1
      (parse-integer
       (aref (nth-value 1 (cl-ppcre:scan-to-strings
                           "Median time:\\s* (\\d+) ms"
                           last-line))
             0))))))

(defun time-all (max-size size-multipliers sorting-algorithms input-types &optional max-time)
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
      (iter:with num-skipped := 0)
      (iter:with test-number := 0)
      (iter:for ((algo-name algo-choice) (input-type input-type-choice)) :in choices)
      (iter:iter
        (iter:with next-test-number := (+ test-number (length sizes)))
        (iter:for size :in sizes)
        (iter:for time := (test-once size algo-choice input-type-choice max-time))
        (iter:while time)
        (format t "~&~A/~A" (incf test-number) (- num-tests num-skipped))
        (setf (gethash (list algo-name input-type) ht)
              (cons (cons size time) (gethash (list algo-name input-type) ht nil)))
        (iter:finally
         (format t "~&(incf ~A (- ~A ~A)) -> ~A" num-skipped next-test-number test-number
                 (incf num-skipped (- next-test-number test-number))))))
    (loop :for key :being :the :hash-key :in ht :using (hash-value times)
          :collect (list key (nreverse times)))))

(defun get-stats (times)
  (loop :for (params time-info) :in times
        :for min-valid := (reduce #'min
                                  (remove-if-not (lambda (one-test)
                                                   (>= (cdr one-test) 20))
                                                 time-info)
                                  :key #'car)
        :for max-valid := (reduce #'max
                                  (remove-if-not (lambda (one-test)
                                                   (< (cdr one-test) (* 10 60 1000)))
                                                 time-info)
                                  :key #'car)
        :collect (list params min-valid max-valid)))

(defun cl-ana-plot-times (&key (max-size (expt 10 9))
                            (max-time (* 31 60 1000))
                            (size-multipliers *array-size-multipliers*)
                            (sorting-algorithms *sorting-algorithms*)
                            (input-types *input-types*))
  (declare ((integer 1 1000000000) max-size))
  (assert (<= max-size (expt 10 9)))

  ;;(uiop:run-program '("make" "clean") :output t)
  ;;(uiop:run-program '("make") :output t)

  (let* ((times (time-all max-size size-multipliers sorting-algorithms input-types max-time))
         (plots (loop :for time-info :in times
                      :for ((algo input-type) time) := time-info
                      :collect (plot2d (list (line time :title (format nil "~A, ~A" algo input-type)))))))
    (draw (page plots
                :shown-title "Time efficiency for 4 sorting algorithms using 4 input types"
                :layout '(4 . 4)
                :scale '(1 . 1)))
    times))
