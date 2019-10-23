# Dependencies
* jnc-nj/jack-tools

# Usage
* (ql:quickload :swagger)

## swagger.process:swagger-p
* takes a converted object, checks if it fits proper openapi specs

## swagger.process:process
* takes a directory or path, returns openapi object

## swagger.process:search-swaggerhub
* takes swaggerhub search keys (spec-type, visibility, state, owner, query, page, limit, sort ,order)
* returns all search results as openapi objects
