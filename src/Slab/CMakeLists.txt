project(Slab Fortran)

set( CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )
set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )

SET(SRC
  3DSlabHT.f90
  DataGlobals.f90
  DataStringGlobals.f90
  EPWRead.f90
  General.f90
  MainProgram.f90
  UtilityRoutines.f90
  DataPrecisionGlobals.f90
  EPWPrecisionGlobals.f90
  EPWWeather.f90
  InputProcessor.f90
  SimData.f90
)

IF(UNIX) # Mac and Linux
ELSE()  # Windows
  IF(CMAKE_Fortran_COMPILER MATCHES "ifort")
    # Set release flags to be empty
    SET(CMAKE_Fortran_FLAGS_RELEASE "")
    SET(CMAKE_Fortran_FLAGS "/nologo /debug:minimal /O2 /Oy- /assume:buffered_io /noaltparam /fpscomp:nolibs /fpe:0 /fp:fast=2  /traceback /check:uninit /libs:static /threads /c /assume:protect-parens /Qprec-div /Qprec-sqrt /Qvec-")
  ELSE()
    SET(CMAKE_Fortran_FLAGS "-static")
  ENDIF()
ENDIF()

ADD_EXECUTABLE( Slab ${SRC} )

SET_TARGET_PROPERTIES(Slab PROPERTIES
  COMPILE_FLAGS "-ffree-line-length-200 ${SHARED_FLAGS}"
)

include("${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/ProjectMacros.cmake")
install_and_fixup_exe_target( Slab PreProcess/GrndTempCalc )
install(FILES "${CMAKE_CURRENT_SOURCE_DIR}/../../idd/SlabGHT.idd" DESTINATION "PreProcess/GrndTempCalc/")

