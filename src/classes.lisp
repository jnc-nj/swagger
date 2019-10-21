(in-package #:swagger.classes)

;;; https://swagger.io/specification/

(defclass _has-content ()
  ((content :field-type :map
	    :key-field string
	    :value-field media-type-object
	    :initarg :content))
  (:metaclass sanity-clause:validated-metaclass))

(defclass _has-required ()
  ((required :type boolean :initarg :required))
  (:metaclass sanity-clause:validated-metaclass))

(defclass _has-operation-id ()
  ((operation-id :data-key "operationId"
		 :type string
		 :initarg :operation-id))
  (:metaclass sanity-clause:validated-metaclass))

(defclass _has-callbacks ()
  ((callbacks :initarg :callbacks))
  (:metaclass sanity-clause:validated-metaclass))

(defclass _has-links ()
  ((links :field-type :map
	  :key-field string
	  :value-field (or link-object t)
	  :initarg :links))
  (:metaclass sanity-clause:validated-metaclass))

(defclass _has-allow-reserved ()
  ((allow-reserved :data-key "allowReserved"
		   :type boolean
		   :initarg :allow-reserved))
  (:metaclass sanity-clause:validated-metaclass))

(defclass _has-explode ()
  ((explode :type boolean :initarg :explode))
  (:metaclass sanity-clause:validated-metaclass))

(defclass _has-style ()
  ((style :type string :initarg :style))
  (:metaclass sanity-clause:validated-metaclass))

(defclass _has-examples ()
  ((examples :field-type :map
	     :key-field string
	     :value-field (or example-object t)
	     :initarg :examples))
  (:metaclass sanity-clause:validated-metaclass))

(defclass _has-example ()
  ((example :initarg :example))
  (:metaclass sanity-clause:validated-metaclass))

(defclass _has-headers ()
  ((headers :field-type :map
	    :key-field string
	    :value-field (or header-object t)
	    :initarg :headers))
  (:metaclass sanity-clause:validated-metaclass))

(defclass _has-external-docs ()
  ((external-docs :data-key "externalDocs"
		  :type external-documentation-object
		  :field-type :nested
		  :element-type external-documentation-object
		  :initarg :external-docs))
  (:metaclass sanity-clause:validated-metaclass))

(defclass _has-parameters ()
  ((parameters :field-type :list
	       :element-type (or parameter-object t)
	       :initarg :parameters))
  (:metaclass sanity-clause:validated-metaclass))

(defclass _has-schema ()
  ((schema :type t :initarg :schema))
  (:metaclass sanity-clause:validated-metaclass))

(defclass _has-servers ()
  ((servers :field-type :list
	    :element-type server-object
	    :initarg :servers))
  (:metaclass sanity-clause:validated-metaclass))

(defclass _has-url ()
  ((url :type string :initarg :url))
  (:metaclass sanity-clause:validated-metaclass))

(defclass _has-summary ()
  ((summary :type string :initarg :summary))
  (:metaclass sanity-clause:validated-metaclass))

(defclass _has-in ()
  ((in :type string :initarg :in :required t))
  (:metaclass sanity-clause:validated-metaclass))

(defclass _has-description ()
  ((description :type string :initarg :description))
  (:metaclass sanity-clause:validated-metaclass))

(defclass _has-name ()
  ((name :type string :initarg :name))
  (:metaclass sanity-clause:validated-metaclass))

(defclass oauth-flow-object ()
  ((authorization-url :data-key "authorizationUrl"
		      :type string
		      :initarg :authorization-url
		      :required t)
   (token-url :data-key "tokenUrl"
	      :type string
	      :initarg :token-url
	      :required t)
   (refresh-url :data-key "refreshUrl"
		:type string
		:initarg :refresh-url)
   (scopes :field-type :map
	   :key-field string
	   :value-field string
	   :initarg :scopes
	   :required t))
  (:metaclass sanity-clause:validated-metaclass))

(defclass oauth-flows-object ()
  ((implicit :type oauth-flow-object :initarg :implicit)
   (password :type oauth-flow-object :initarg :password)
   (client-credentials :data-key "clientCredentials"
		       :type oauth-flow-object
		       :initarg :client-credentials)
   (authorization-code :data-key "authorizationCode"
		       :type oauth-flow-object
		       :initarg :authorization-code))
  (:metaclass sanity-clause:validated-metaclass))

(defclass security-scheme-object (_has-description _has-name _has-in)
  ((type :type string :initarg :type :required t)
   (scheme :type string :initarg :scheme :required t)
   (bearer-format :data-key "bearerFormat"
		  :type string
		  :initarg :bearer-format)
   (flows :type oauth-flows-object :initarg :flows :required t)
   (open-id-connect-url :data-key "openIdConnectUrl"
			:type string
			:initarg :open-id-connect-url
			:required t))
  (:metaclass sanity-clause:validated-metaclass))

(defclass xml-object (_has-name)
  ((namespace :type string :initarg :namespace)
   (prefix :type string :initarg :prefix)
   (attribute :type boolean :initarg :attribute)
   (wrapped :type boolean :initarg :wrapped))
  (:metaclass sanity-clause:validated-metaclass))

(defclass descriminator-object ()
  ((property-name :data-key "propertyName"
		  :type string
		  :initarg :property-name
		  :required t)
   (mapping :field-type :map
	    :key-field string
	    :value-field string
	    :initarg :mapping))
  (:metaclass sanity-clause:validated-metaclass))

(defclass tag-object (_has-name _has-description _has-external-docs)
  (:metaclass sanity-clause:validated-metaclass))

(defclass link-object (_has-operation-id _has-parameters _has-description)
  ((operation-ref :data-key "operationRef"
		  :type string
		  :initarg :operation-ref)
   (request-body :data-key "requestBody"
		 :initarg :request-body)
   (server :type server-object :initarg :server))
  (:metaclass sanity-clause:validated-metaclass))

(defclass example-object (_has-summary _has-description)
  ((value :initarg :value)
   (external-value :data-key "externalValue"
		   :type string
		   :initarg :external-value))
  (:metaclass sanity-clause:validated-metaclass))

(defclass response-object (_has-description _has-headers _has-content _has-links)
  (:metaclass sanity-clause:validated-metaclass))

(defclass encoding-object (_has-headers _has-style _has-explode _has-allow-reserved)
  ((content-type :data-key "contentType"
		 :type string
		 :initarg :content-type))
  (:metaclass sanity-clause:validated-metaclass))

(defclass media-type-object (_has-schema _has-example _has-examples)
  ((encoding :field-type :map
	     :key-field string
	     :value-field encoding-object
	     :initarg :encoding))
  (:metaclass sanity-clause:validated-metaclass))

(defclass request-body-object (_has-description _has-content _has-required) 
  (:metaclass sanity-clause:validated-metaclass))

(defclass parameter-object (_has-name _has-description _has-in _has-schema _has-example
				      _has-examples _has-required _has-content
				      _has-style _has-explod _has-allow-reserved)
  ((deprecated :type boolean :initarg :deprecated)
   (allow-empty-value :data-key "allowEmptyValue"
		      :type boolean
		      :initarg :allow-empty-value))
  (:metaclass sanity-clause:validated-metaclass))

(defclass external-documentation-object (_has-description _has-url) 
  (:metaclass sanity-clause:validated-metaclass))

(defclass operation-object (_has-description _has-operation-id _has-summary _has-servers
					     _has-external-docs _has-parameters _has-callbacks)
  ((tags :field-type :list
	 :element-type string
	 :initarg :tags)      
   (request-body :data-key "requestBody"
		 :type (or request-body-object t)
		 :field-type :nested
		 :element-type (or request-body-object t) 
		 :initarg :request-body)
   (responses :initarg :responses) 
   (deprecated :type boolean :initarg :deprecated)
   (security :initarg :security))
  (:metaclass sanity-clause:validated-metaclass))

(defclass path-item-object (_has-description _has-summary _has-servers _has-parameters)
  (($ref :type string :initarg :$ref)   
   (get :type operation-object
	:field-type :nested
	:element-type operation-object
	:initarg :get)
   (put :type operation-object
	:field-type :nested
	:element-type operation-object
	:initarg :put)
   (post :type operation-object
	 :field-type :nested
	 :element-type operation-object
	 :initarg :post)
   (delete :type operation-object
	   :field-type :nested
	   :element-type operation-object
	   :initarg :delete)
   (options :type operation-object
	    :field-type :nested
	    :element-type operation-object
	    :initarg :options)
   (head :type operation-object
	 :field-type :nested
	 :element-type operation-object
	 :initarg :head)
   (patch :type operation-object
	  :field-type :nested
	  :element-type operation-object
	  :initarg :patch)
   (trace :type operation-object
	  :field-type :nested
	  :element-type operation-object
	  :initarg :trace))
  (:metaclass sanity-clause:validated-metaclass))

(defclass components-object (_has-examples _has-headers _has-links _has-callbacks)
  ((schemas :field-type :map
	    :key-field string
	    :value-field t
	    :initarg :schemas)
   (responses :field-type :map
	      :key-field string
	      :value-field (or response-object t)
	      :initarg :responses)
   (parameters :field-type :map
	       :key-field string
	       :value-field (or parameter-object t)
	       :initarg :parameters) 
   (request-bodies :data-key "requestBodies"
		   :field-type :map
		   :key-field string
		   :value-field (or request-bodies-object t)
		   :initarg :request-bodies) 
   (security-schemes :data-key "securitySchemes"
		     :field-type :map
		     :key-field string
		     :value-field (or security-scheme-object t)
		     :initarg :security-schemes))
  (:metaclass sanity-clause:validated-metaclass))

(defclass server-variable-object (_has-description)
  ((enum :field-type :list
	 :element-type string
	 :initarg :enum)
   (default :type string :initarg :default :required t))
  (:metaclass sanity-clause:validated-metaclass))

(defclass server-object (_has-description _has-url)
  ((variables :field-type :map
	      :key-field string
	      :value-field server-variable-object
	      :initarg :map-field))
  (:metaclass sanity-clause:validated-metaclass))

(defclass license-object (_has-name _has-url) 
  (:metaclass sanity-clause:validated-metaclass))

(defclass contact-object (_has-name _has-url)
  ((email :type string :initarg :email))
  (:metaclass sanity-clause:validated-metaclass))

(defclass info-object (_has-description)
  ((title :type string :initarg :title :required t) 
   (terms-of-service :data-key "termsOfService"
		     :type string
		     :initarg :terms-of-service)
   (version :type string :initarg :version :required t)
   (contact :type contact-object
	    :field-type :nested
            :element-type contact-object
	    :initarg :contact)
   (license :type license-object
	    :field-type :nested
            :element-type license-object
	    :initarg :license))
  (:metaclass sanity-clause:validated-metaclass))

(defclass openapi-object (_has-servers)
  ((openapi :type string :initarg :openapi :required t)
   (info :type info-object
	 :field-type :nested
	 :element-type info-object
	 :initarg :info
	 :required t) 
   (paths :initarg :paths
	  :required t))
  (:metaclass sanity-clause:validated-metaclass))
