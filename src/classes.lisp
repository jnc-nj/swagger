(in-package #:swagger.classes)

;;; https://swagger.io/specification/

(defclass _has-required ()
  ((required :initarg :required :initform nil)))

(defclass _has-operation-id ()
  ((operation-id :initarg :operation-id :initform "")))

(defclass _has-callbacks ()
  ((callbacks :initarg :callbacks :initform '())))

(defclass _has-allow-reserved ()
  ((allow-reserved :initarg :allow-reserved :initform nil)))

(defclass _has-explode ()
  ((explode :initarg :explode :initform nil)))

(defclass _has-style ()
  ((style :initarg :style :initform "")))

(defclass _has-$ref ()
  (($ref :initarg :$ref :initform "")))

(defclass _has-example ()
  ((example :initarg :example :initform "")))

(defclass _has-schema ()
  ((schema :initarg :schema :initform nil)))

(defclass _has-url ()
  ((url :initarg :url :initform "")))

(defclass _has-summary ()
  ((summary :initarg :summary :initform "")))

(defclass _has-in ()
  ((in :initarg :in :initform "")))

(defclass _has-description ()
  ((description :initarg :description :initform "")))

(defclass _has-name ()
  ((name :initarg :name :initform "")))

(defclass _has-type ()
  ((type :initarg :type :initform "")))

(defclass oauth-flow-object ()
  ((authorization-url :initarg :authorization-url :initform "")
   (token-url :initarg :token-url :initform "")
   (refresh-url :initarg :refresh-url :initform "")
   (scopes :initarg :scopes :initform (list (cons "" "")))))

(defclass oauth-flows-object ()
  ((implicit :initarg :implicit :initform (make-instance 'oauth-flow-object))
   (password :initarg :password :initform (make-instance 'oauth-flow-object))
   (client-credentials :initarg :client-credentials :initform (make-instance 'oauth-flow-object))
   (authorization-code :initarg :authorization-code :initform (make-instance 'oauth-flow-object))))

(defclass security-scheme-object (_has-description _has-name _has-in _has-type)
  ((scheme :initarg :scheme :initform "")
   (bearer-format :initarg :bearer-format :initform "")
   (flows :initarg :flows :initform (make-instance 'oauth-flows-object))
   (open-id-connect-url :initarg :open-id-connect-url :initform "")))

(defclass xml-object (_has-name)
  ((namespace :initarg :namespace :initform "")
   (prefix :initarg :prefix :initform "")
   (attribute :initarg :attribute :initform nil)
   (wrapped :initarg :wrapped :initform nil)))

(defclass discriminator-object ()
  ((property-name :initarg :property-name :initform "")
   (mapping :initarg :mapping :initform (list (cons "" "")))))

(defclass tag-object (_has-name _has-description _has-external-docs)
  nil)

(defclass link-object (_has-operation-id _has-parameters _has-description)
  ((operation-ref :initarg :operation-ref :initform "")
   (request-body :initarg :request-body :initform "")
   (server :initarg :server :initform (make-instance 'server-object))))

(defclass _has-links ()
  ((links :initarg :links :initform (list (cons "" (make-instance 'link-object))))))

(defclass example-object (_has-summary _has-description)
  ((value :initarg :value :initform "")
   (external-value :initarg :external-value :initform "")))

(defclass _has-examples ()
  ((examples :initarg :examples :initform (list (cons "" (make-instance 'example-object))))))

(defclass response-object (_has-description _has-headers _has-content _has-links)
  nil)

(defclass responses-object ()
  ((default :initarg :default :initform (make-instance 'response-object))))

(defclass encoding-object (_has-headers _has-style _has-explode _has-allow-reserved)
  ((content-type :initarg :content-type :initform "")))

(defclass media-type-object (_has-schema _has-example _has-examples)
  ((encoding :initarg :encoding :initform (list (cons "" (make-instance 'encoding-object))))))

(defclass _has-content ()
  ((content :initarg :content :initform (list (cons "" (make-instance 'media-type-object))))))

(defclass request-body-object (_has-description _has-content _has-required) 
  nil)

(defclass header-object (_has-description _has-schema _has-example _has-examples
					  _has-required _has-content _has-style
					  _has-explode _has-allow-reserved)
  ((deprecated :initarg :deprecated :initform nil)
   (allow-empty-value :initarg :allow-empty-value :initform nil)))

(defclass parameter-object (_has-name _has-in header-object)
  nil)

(defclass _has-headers ()
  ((headers :initarg :headers :initform '(()))))

(defclass _has-parameters ()
  ((parameters :initarg :parameters :initform (list (make-instance 'parameter-object)))))

(defclass external-documentation-object (_has-description _has-url) 
  nil)

(defclass _has-external-docs ()
  ((external-docs :initarg :external-docs :initform (make-instance 'external-documentation-object))))

(defclass operation-object (_has-description _has-operation-id _has-summary _has-servers
					     _has-external-docs _has-parameters _has-callbacks)
  ((tags :initarg :tags :initform (list ""))
   (request-body :initarg :request-body :initform (make-instance 'request-body-object))
   (responses :initarg :responses :initform (make-instance 'responses-object)) 
   (deprecated :initarg :deprecated :initform nil)
   (security :initarg :security :initform '(()))))

(defclass path-item-object (_has-description _has-summary _has-servers _has-parameters _has-$ref)
  ((get :initarg :get :initform (make-instance 'operation-object))
   (put :initarg :put :initform (make-instance 'operation-object))
   (post :initarg :post :initform (make-instance 'operation-object))
   (delete :initarg :delete :initform (make-instance 'operation-object))
   (options :initarg :options :initform (make-instance 'operation-object))
   (head :initarg :head :initform (make-instance 'operation-object))
   (patch :initarg :patch :initform (make-instance 'operation-object))
   (trace :initarg :trace :initform (make-instance 'operation-object))))

(defclass components-object (_has-examples _has-headers _has-links _has-callbacks)
  ((schemas :initarg :schemas :initform "")
   (responses :initarg :responses :initform (list (cons "" (make-instance 'response-object))))
   (parameters :initarg :parameters :initform (list (cons "" (make-instance 'parameter-object)))) 
   (request-bodies :initarg :request-bodies :initform (list (cons "" (make-instance 'request-body-object)))) 
   (security-schemes :initarg :security-schemes :initform (list (cons "" (make-instance 'security-scheme-object))))))

(defclass server-variable-object (_has-description)
  ((enum :initarg :enum :initform (list ""))
   (initform :initarg :initform :initform "")))

(defclass server-object (_has-description _has-url)
  ((variables :initarg :map-field :initform (list (cons "" (make-instance 'server-variable-object))))))

(defclass _has-servers ()
  ((servers :initarg :servers :initform (list (make-instance 'server-object)))))

(defclass license-object (_has-name _has-url) 
  nil)

(defclass contact-object (_has-name _has-url)
  ((email :initarg :email :initform "")))

(defclass info-object (_has-description)
  ((title :initarg :title :initform "") 
   (terms-of-service :initarg :terms-of-service :initform "")
   (version :initarg :version :initform "")
   (contact :initarg :contact :initform (make-instance 'contact-object))
   (license :initarg :license :initform (make-instance 'license-object))))

(defclass openapi-object (_has-servers _has-external-docs)
  ((openapi :initarg :openapi :initform "")
   (info :initarg :info :initform (make-instance 'info-object))
   (paths :initarg :paths :initform '(()))
   (components :initarg :components :initform (make-instance 'components-object))
   (security :initarg :security :initform '(()))
   (tags :initarg :tags :initform (list (make-instance 'tag-object)))))

(defclass property-object (_has-url _has-type)
  ((value :initarg :value :initform "")))

(defclass meta-object (_has-name _has-description) 
  ((properties :initarg :properties :initform (list (make-instance 'property-object)))
   (tags :initarg :tags :initform (list ""))))

(defclass swagger-object ()
  ((swagger-json :initarg :swagger-json :initform (make-instance 'openapi-object))
   (meta :initarg :meta :initform (make-instance 'meta-object))))

;;; CLASS MAP
(defvar +class-map+
  (create-class-map
   '_has-required '_has-operation-id '_has-callbacks '_has-allow-reserved '_has-explode
   '_has-style '_has-$ref '_has-example '_has-schema '_has-url '_has-summary '_has-in
   '_has-description '_has-name

   'oauth-flow-object 'oauth-flows-object 'security-scheme-object
   'xml-object 'discriminator-object 'tag-object

   'link-object '_has-links

   'example-object '_has-examples

   'response-object 'responses-object 'encoding-object

   'media-type-object '_has-content

   'request-body-object

   'header-object 'parameter-object '_has-headers '_has-parameters

   'external-documentation-object '_has-external-docs

   'operation-object 'path-item-object 'components-object 'server-variable-object

   'server-object '_has-servers

   'license-object 'contact-object 'info-object 'openapi-object 'property-object 'meta-object 'swagger-object))
