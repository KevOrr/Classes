(in-package :time-sorts)

(defparameter *sorting-algorithms* '(
                                     ;; ("selection sort" "s")
                                     ("insertion sort" "i")
                                     ;;("merge sort" "m")
                                     ;;("quicksort" "q")
                                     ))

(defparameter *input-types* '(
                              ("increasing" "i")
                              ("decreasing" "d")
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

(defun time-all (max-size size-multipliers sorting-algorithms input-types)
  (let ((ht (make-hash-table :test #'equal)))
    (loop :with choices := (alexandria:map-product #'list
                                                   (make-sizes max-size size-multipliers)
                                                   sorting-algorithms
                                                   input-types)
          :for (size (algo-name algo-choice) (input-type input-type-choice)) :in choices
          :for test-number :from 1
          :for num-tests := (length choices)
          :for command := (list "sorting-algos/sorting"
                                (write-to-string size)
                                algo-choice
                                input-type-choice)
          :for output := (uiop:run-program command :output :string)
          :for last-line := (car (last (cl-ppcre:split "\\n" output)))
          :for time := (parse-integer (aref (nth-value 1 (cl-ppcre:scan-to-strings
                                                          "Median time:\\s* (\\d+) ms"
                                                          last-line))
                                            0))
          :do (format t "~&~A/~A" test-number num-tests)
              ;; (print command)
              ;; (print output)
              (setf (gethash (list algo-name input-type) ht)
                    (cons (cons size time) (gethash (list algo-name input-type) ht nil))))
    (loop :for key :being :the :hash-key :in ht :using (hash-value times)
          :collect (list key (nreverse times)))))

(defun cl-ana-plot-times (&key (max-size (expt 10 9))
                            (size-multipliers *array-size-multipliers*)
                            (sorting-algorithms *sorting-algorithms*)
                            (input-types *input-types*))
  (declare ((integer 1 1000000000) max-size))
  (assert (<= max-size (expt 10 9)))

  ;;(uiop:run-program '("make" "clean") :output t)
  ;;(uiop:run-program '("make") :output t)

  (let* ((times (time-all max-size size-multipliers sorting-algorithms input-types))
         (plots (loop :for time-info :in times
                      :for ((algo input-type) time) := time-info
                      :collect (line time :title (format nil "~A, ~A" algo input-type)))))
    (draw (plot2d plots))
    times))
