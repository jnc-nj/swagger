(in-package #:cl-user)

(defpackage #:swagger.classes
  (:use #:cl
	#:jack.tools.objects) 
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
	   #:_HAS-TYPE

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

	   #:HEADER-OBJECT
	   #:PARAMETER-OBJECT
	   #:_HAS-HEADERS
	   #:_HAS-PARAMETERS

	   #:EXTERNAL-DOCUMENTATION-OBJECT
	   #:_HAS-EXTERNAL-DOCS

	   #:OPERATION-OBJECT
	   #:PATH-ITEM-OBJECT 
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
	   #:SWAGGER-OBJECT

	   #:SWAGGER_JSON
	   #:META
	   #:PROPERTIES
	   #:SWAGGER
	   #:OPENAPI
	   #:INFO
	   #:PATHS
	   #:COMPONENTS
	   #:TITLE
	   #:TERMS-OF-SERVICE
	   #:VERSION
	   #:CONTACT
	   #:LICENSE
	   #:EMAIL
	   #:VARIABLES
	   #:ENUM
	   #:INITFORM
	   #:SCHEMAS
	   #:REQUEST-BODIES
	   #:SECURITY-SCHEMES
	   #:GET
	   #:PUT
	   #:POST
	   #:DELETE
	   #:OPTIONS
	   #:HEAD
	   #:PATCH
	   #:TRACE
	   #:SERVERS
	   #:TAGS
	   #:RESPONSES
	   #:SECURITY
	   #:DEPRECATED
	   #:ALLOW-EMPTY-VALUE
	   #:ENCODING
	   #:CONTENT-TYPE
	   #:DEFAULT
	   #:CONTENT
	   #:HEADERS
	   #:EXAMPLES
	   #:VALUE
	   #:EXTERNAL-VALUE
	   #:LINKS
	   #:PARAMETERS
	   #:OPERATION-REF
	   #:REQUEST-BODY
	   #:SERVER
	   #:EXTERNAL-DOCS
	   #:PROPERTY-NAME
	   #:MAPPING
	   #:NAMESPACE
	   #:PREFIX
	   #:ATTRIBUTE
	   #:WRAPPED
	   #:TYPE
	   #:SCHEME
	   #:BEARER-FORMAT
	   #:FLOWS
	   #:OPEN-ID-CONNECT-URL
	   #:IMPLICIT
	   #:PASSWORD
	   #:CLIENT-CREDENTIALS
	   #:AUTHORIZATION-CODE
	   #:AUTHORIZATION-URL
	   #:TOKEN-URL
	   #:REFRESH-URL
	   #:SCOPES
	   #:NAME
	   #:DESCRIPTION
	   #:IN
	   #:SUMMARY
	   #:URL
	   #:SCHEMA
	   #:EXAMPLE
	   #:$REF
	   #:STYLE
	   #:EXPLODE
	   #:ALLOW-RESERVED
	   #:CALLBACKS
	   #:OPERATION-ID
	   #:REQUIRED

	   #:+CLASS-SWAGGER-OBJECT+
	   #:+CLASS-OPENAPI-OBJECT+
	   #:+CLASS-META-OBJECT+
	   #:+CLASS-MAP+))

(defpackage #:swagger.process
  (:use #:cl
	#:cl-json
	#:cl-ppcre
	#:jack.tools.withs
	#:jack.tools.lists
	#:jack.tools.filesystems
	#:jack.tools.objects
	#:swagger.classes)
  (:export #:SWAGGER-NORMALIZE
	   #:SWAGGER-P
	   #:SWAGGER-PROCESS 
	   #:SEARCH-SWAGGERHUB))

