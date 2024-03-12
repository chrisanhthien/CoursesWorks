(defun compress (list)
  (cond
   ((null list) nil)
   ((null (tail list)) list)
   ((eql (head list) (head (tail list)))
    (compress (tail list)))
   (t (cons (head list) (compress (tail list))))
   )
  )