(in-package #:swagger.process)

(defun process (path)
  (let* ((content (open-file (pathname path)))
	 (decode (jonathan:parse content :as :alist)))
    (cast decode +class-map+)))
