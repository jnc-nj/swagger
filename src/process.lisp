(in-package #:swagger.process)

(defun swagger-normalize (processed)
  (labels ((convert (items)
	     (when (listp items)
	       (loop for item in items
		  collect (cons (car item) (cast (cdr item) +class-map+))))))
    (cond ((eq +class-swagger-object+ (class-of processed))
	   (with-slots (swagger_json) processed
	     (swagger-normalize swagger_json)))
	  ((eq +class-openapi-object+ (class-of processed))
	   (with-slots (paths) processed
	     (setf paths (convert paths))
	     (dolist (path paths)
	       (with-slots (get put post delete options head patch trace) (cdr path)
		 (when get
		   (with-slots (responses) get
		     (setf responses (convert responses))))
		 (when put
		   (with-slots (responses) put
		     (setf responses (convert responses))))
		 (when post
		   (with-slots (responses) post
		     (setf responses (convert responses))))
		 (when delete
		   (with-slots (responses) delete
		     (setf responses (convert responses))))
		 (when options
		   (with-slots (responses) options
		     (setf responses (convert responses))))
		 (when head
		   (with-slots (responses) head
		     (setf responses (convert responses))))
		 (when patch
		   (with-slots (responses) patch
		     (setf responses (convert responses))))
		 (when trace
		   (with-slots (responses) trace
		     (setf responses (convert responses)))))))))) 
  processed)

(defun swagger-p (processed)
  (or (and (eq +class-swagger-object+ (class-of processed))
	   (with-slots (swagger_json meta) processed
	     (and (eq +class-openapi-object+ (class-of swagger_json))
		  (eq +class-meta-object+ (class-of meta))
		  (swagger-p swagger_json))))
      (and (eq +class-openapi-object+ (class-of processed))
	   (with-slots (paths info) processed (and paths info)))))

(defun swagger-process (path &key ignore-path malform-path)
  (cond ((cl-fad:directory-exists-p path) 
	 (let ((file-count (length (cl-fad:list-directory path)))
	       (ignored (when ignore-path (split "\\n" (open-file ignore-path))))
	       (counter 0)
	       collect)
	   (log:info "[process][directory has ~d files]" file-count)
	   (cl-fad:walk-directory
	    (truename path)
	    #'(lambda (item)
		(incf counter)
		(unless (member (namestring item) ignored :test #'string=)
		  (let ((processed (swagger-process item)))
		    (log:info "[~d/~d][processing ~d]" counter file-count item)
		    (cond ((and processed (swagger-p processed))
			   (when ignore-path (add-line (namestring item) ignore-path))
			   (push processed collect)) 
			  (malform-path (add-line (namestring item) malform-path))))))
	    :if-does-not-exist :ignore
	    :test #'(lambda (item) (string= (get-extension item) ".json")))
	   collect))
	((cl-fad:file-exists-p path)
	 (let* ((content (open-file (pathname path)))
		(decode (jonathan:parse content :as :alist))
		(cast (cast decode +class-map+)))
	   (swagger-normalize cast)))
	(t (log:warn "[error][provided path ~d does not exist]" path))))

(defun search-swaggerhub (&key spec-type visibility state owner query page limit sort order)
  (let (collect params)
    (when spec-type (push (list "specType" spec-type) params))
    (when visibility (push (list "visibility" visibility) params))
    (when state (push (list "state" state) params))
    (when owner (push (list "owner" owner) params))
    (when query (push (list "query" query) params))
    (when page (push (list "page" page) params))
    (when limit (push (list "limit" limit) params))
    (when sort (push (list "sort" sort) params))
    (when order (push (list "order" order) params))
    (with-query "api.swaggerhub.com" ("specs" nil :params params)
      (dolist (api (agethash :apis http-body*))
	(let* ((url (agethash :url (car (agethash :properties api))))
	       (json (dex:get url))
	       (decode (jonathan:parse json :as :alist))
	       (cast (cast decode +class-map+)))
	  (when (swagger-p cast) (push (swagger-normalize cast) collect))))
      collect)))
