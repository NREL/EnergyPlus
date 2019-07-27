project(Basement Fortran)

set( CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )
set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )

set(SRC
  3DBasementHT.f90
  DataGlobals.f90
  DataStringGlobals.f90
  EPWRead.f90
  UtilityRoutines.f90
  DataPrecisionGlobals.f90
  EPWPrecisionGlobals.f90
  InputProcessor.f90
  BasementSimData.f90
)

set(STATIC_EXE FALSE)

if(APPLE)
  if(CMAKE_Fortran_COMPILER MATCHES "ifort")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -static-intel")
  else()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffloat-store" )
  endif()
elseif(UNIX)
  set(CMAKE_SKIP_RPATH TRUE)
  if(CMAKE_Fortran_COMPILER MATCHES "ifort")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -static-intel")
  else()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffloat-store" )
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -static" )
    set(STATIC_EXE TRUE)
  endif()
else()  # Windows
  set(STATIC_EXE TRUE)
  if(CMAKE_Fortran_COMPILER MATCHES "ifort")
    # Set release flags to be empty
    set(CMAKE_Fortran_FLAGS_RELEASE "")
    set(CMAKE_Fortran_FLAGS "/nologo /debug:minimal /O2 /Oy- /assume:buffered_io /noaltparam /fpscomp:nolibs /fpe:0 /fp:fast=2  /traceback /check:uninit /libs:static /threads /c /assume:protect-parens /Qprec-div /Qprec-sqrt /Qimf-precision:high /Qimf-arch-consistency:true /Qvec-")
  else()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -static")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffloat-store")
  endif()
endif()

set(SKIP_BUILD_RPATH TRUE)
add_executable( Basement ${SRC} )

set_target_properties(Basement PROPERTIES
  COMPILE_FLAGS "-O1 -ffree-line-length-200 ${SHARED_FLAGS}"
)

install( TARGETS Basement DESTINATION PreProcess/GrndTempCalc )
if(NOT ${STATIC_EXE})
  include("${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/ProjectMacros.cmake")
  install_target_prereqs( Basement PreProcess/GrndTempCalc )
endif()
install(FILES "${CMAKE_CURRENT_SOURCE_DIR}/../../idd/BasementGHT.idd" DESTINATION "PreProcess/GrndTempCalc/")

