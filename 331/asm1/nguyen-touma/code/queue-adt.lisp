
(defun push (stack element)
  (cons element stack))

(defun pop (stack)
  (if (null stack)
      (error "Stack is empty")
      (values (car stack) (cdr stack))))

(defun stack-empty? (stack)
  (null stack))

(defun enqueue (queue element)
  (setf (symbol-value 's1) (push (symbol-value 's1) element)))

(defun dequeue (queue)
  (if (stack-empty? (symbol-value 's2))
      (progn
        (while (not (stack-empty? (symbol-value 's1)))
          (multiple-value-bind (element remaining-stack) (pop (symbol-value 's1))
            (setf (symbol-value 's2) (push (symbol-value 's2) element))))
        (if (stack-empty? (symbol-value 's2))
            (error "Queue is empty")))
      nil)
  (multiple-value-bind (element remaining-stack) (pop (symbol-value 's2))
    (setf (symbol-value 's2) remaining-stack)
    element))

(setq s1 nil) ; Initialize s1 stack
(setq s2 nil) ; Initialize s2 stack

(enqueue 'q1 'a)
(enqueue 'q1 'b)
(enqueue 'q1 'c)

(print (dequeue 'q1)) 
(print (dequeue 'q1))
(print (dequeue 'q1))


