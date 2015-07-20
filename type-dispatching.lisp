(defmethod add ((a number) (b number))
  (+ a b))

(defmethod add ((a list) (b list))
  (append a b))
