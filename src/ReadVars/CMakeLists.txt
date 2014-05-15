project(ReadVars Fortran)

SET(SRC ReadVarsESO.f90) 

ADD_EXECUTABLE( ReadVarsESO ${SRC} )

install(TARGETS ReadVarsESO DESTINATION ./)

