(in-package #:cl-user)

(defpackage #:swagger.classes
  (:use #:cl)
  (:export #:_HAS-REQUIRED
	   #:_HAS-OPERATION-ID
	   #:_HAS-CALLBACKS
	   #:_HAS-ALLOW-RESERVED
	   #:_HAS-EXPLODE
	   #:_HAS-STYLE
	   #:_HAS-$REF 
	   #:_HAS-EXAMPLE
	   #:_HAS-SCHEMA
	   #:_HAS-URL
	   #:_HAS-SUMMARY
	   #:_HAS-IN
	   #:_HAS-DESCRIPTION
	   #:_HAS-NAME

	   #:SECURITY-REQUIREMENT-OBJECT
	   #:OAUTH-FLOW-OBJECT
	   #:OAUTH-FLOWS-OBJECT
	   #:SECURITY-SCHEME-OBJECT
	   #:XML-OBJECT
	   #:DESCRIMINATOR-OBJECT
	   #:TAG-OBJECT
	   
	   #:LINK-OBJECT
	   #:_HAS-LINKS

	   #:EXAMPLE-OBJECT
	   #:_HAS-EXAMPLES

	   #:RESPONSE-OBJECT
	   #:RESPONSES-OBJECT
	   #:ENCODING-OBJECT

	   #:MEDIA-TYPE-OBJECT
	   #:_HAS-CONTENT

	   #:REQUEST-BODY-OBJECT

	   #:PARAMETER-OBJECT
	   #:_HAS-HEADERS
	   #:_HAS-PARAMETERS

	   #:EXTERNAL-DOCUMENTATION-OBJECT
	   #:_HAS-EXTERNAL-DOCS

	   #:OPERATION-OBJECT
	   #:PATH-ITEM-OBJECT
	   #:PATHS-OBJECT
	   #:COMPONENTS-OBJECT
	   #:SERVER-VARIABLE-OBJECT

	   #:SERVER-OBJECT
	   #:_HAS-SERVERS

	   #:LICENSE-OBJECT
	   #:CONTACT-OBJECT
	   #:INFO-OBJECT
	   #:OPENAPI-OBJECT
	   #:PROPERTY-OBJECT
	   #:META-OBJECT
	   #:SWAGGER-OBJECT)
  (:shadowing-import-from #:sanity-clause))

(defpackage #:swagger.process
  (:use #:cl
	#:alexandria 
	#:swagger.classes)
  (:export #:PARSE))
