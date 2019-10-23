# Dependencies
* jnc-nj/jack-tools

# Usage
* (ql:quickload :swagger)

## swagger.process:swagger-normalize
* fixes internals of generated object so that jonathan:to-json works on it

## swagger.process:swagger-p
* takes a converted object, checks if it fits proper openapi specs

## swagger.process:swagger-process
* takes a directory or path, returns openapi object

## swagger.process:search-swaggerhub
* takes swaggerhub search keys (spec-type, visibility, state, owner, query, page, limit, sort ,order)
* returns all search results as openapi objects
