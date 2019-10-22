(in-package #:cl-user)

(asdf:defsystem swagger
  :author "Jack Nai-Chieh Chou <jacknchou@icloud.com>"
  :maintainer "Jack Nai-Chieh Chou <jacknchou@icloud.com>"
  :version "0.0.1"
  :serial t
  :components ((:file "src/packages")
	       (:file "src/classes")
	       (:file "src/process"))
  :depends-on (:jack-tools
	       :sanity-clause
	       :jonathan
	       :alexandria))
