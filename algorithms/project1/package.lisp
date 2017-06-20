(defpackage #:time-sorts
  (:use #:cl #:cl-ana.plotting #:cl-csv)
  (:export #:*si-prefixes*
           #:*sorting-algorithms*
           #:*input-types*
           #:*plotting-multipliers*
           #:*testing-multipliers*

           #:test-once
           #:time-all
           #:cl-ana-plot
           #:make-stats-table
           #:make-analysis-table
           #:write-times-csv
           #:write-object
           #:read-object))
