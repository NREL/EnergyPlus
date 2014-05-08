SET(SRC epfilter.f90) 

ADD_EXECUTABLE( ExpandObjects ${SRC} )

SET_TARGET_PROPERTIES(ExpandObjects PROPERTIES
  COMPILE_FLAGS "-std=legacy -ffree-line-length-200 ${SHARED_FLAGS}"
)
