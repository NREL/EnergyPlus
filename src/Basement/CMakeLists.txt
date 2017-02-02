project(Basement Fortran)

set( CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )
set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )

SET(SRC
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
  endif()
else()  # Windows
  if(CMAKE_Fortran_COMPILER MATCHES "ifort")
    # Set release flags to be empty
    set(CMAKE_Fortran_FLAGS_RELEASE "")
    set(CMAKE_Fortran_FLAGS "/nologo /debug:minimal /O2 /Oy- /assume:buffered_io /noaltparam /fpscomp:nolibs /fpe:0 /fp:fast=2  /traceback /check:uninit /libs:static /threads /c /assume:protect-parens /Qprec-div /Qprec-sqrt /Qimf-precision:high /Qimf-arch-consistency:true /Qvec-")
  else()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -static")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffloat-store")
  endif()
endif()

set(SKIP_BUILD_RPATH true)
ADD_EXECUTABLE( Basement ${SRC} )

SET_TARGET_PROPERTIES(Basement PROPERTIES
  COMPILE_FLAGS "-O1 -ffree-line-length-200 ${SHARED_FLAGS}"
)

include("${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/ProjectMacros.cmake")
install_and_fixup_exe_target( Basement PreProcess/GrndTempCalc )
install(FILES "${CMAKE_CURRENT_SOURCE_DIR}/../../idd/BasementGHT.idd" DESTINATION "PreProcess/GrndTempCalc/")

