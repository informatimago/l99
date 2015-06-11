

;; Here is a version giving the codes as integers:

(defun gray (n)
  (let ((result (make-array (expt 2 n))))
    (setf (aref result 0) 0
          (aref result 1) 1)
    (loop
       :for i :from 2 :to n
       :do )
    (if (= 1 n)
        (list 0 1)
        (let ((gray-1 (gray (1- n))))
          (nconc gray-1
                 (mapcar (lambda (code) (dpb 1 (byte 1 (1- n)) code))
                         (reverse gray-1)))))))

