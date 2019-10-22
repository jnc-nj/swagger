(in-package #:swagger.process)

(defun parse (path) 
  (let ((data (read-file-into-string (pathname path))))
    (sanity-clause:load (find-class 'swagger-object)
			(jonathan:parse data :as :alist))))
