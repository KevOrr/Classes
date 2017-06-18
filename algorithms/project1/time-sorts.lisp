(in-package :time-sorts)

(defparameter *sorting-algorithms* '(
                                     ("selection" "s")
                                     ("insertion" "i")
                                     ("mergesort" "m")
                                     ("quicksort" "q")
                                     ))

(defparameter *input-types* '(
                              ("increasing" "i")
                              ("decreasing" "d")
                              ("constant" "c")
                              ("random" "r")
                              ))

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

(defun cl-ana-plot (&key (max-size (expt 10 9))
                      (max-time (* 31 60 1000))
                      (size-multipliers *testing-multipliers*)
                      (sorting-algorithms *sorting-algorithms*)
                      (input-types *input-types*))
  (declare ((integer 1 1000000000) max-size))
  (assert (<= max-size (expt 10 9)))

  ;;(uiop:run-program '("make" "clean") :output t)
  ;;(uiop:run-program '("make") :output t)

  (let ((times (time-all max-size
                         :size-multipliers size-multipliers
                         :sorting-algorithms sorting-algorithms
                         :input-types input-types
                         :max-time max-time)))
    (ensure-directories-exist #p"output/")
    (loop :for time-info :in times
          :for ((algo input-type) time) := time-info
          :do (print (format nil "output/~A-~A.png" algo input-type))
              (draw (page (list (plot2d (list (line time
                                                    :style "linespoints"))
                                        :x-title "Array size"
                                        :y-title "Median sort time (ms)"
                                        :legend (legend :show nil)))
                          :shown-title (format nil "~A, ~A" algo input-type)
                          :terminal (png-term)
                          :output (format nil "output/~A-~A.png" algo input-type))))
    times))
