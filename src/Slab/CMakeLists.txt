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

if(APPLE)
  if(CMAKE_Fortran_COMPILER MATCHES "ifort")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -static-intel" )
  endif()
elseif(UNIX)
  set(CMAKE_SKIP_RPATH TRUE)
  if(CMAKE_Fortran_COMPILER MATCHES "ifort")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -static-intel" )
  else()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -static" )
  endif()
else()
  if(CMAKE_Fortran_COMPILER MATCHES "ifort")
    # Set release flags to be empty
    set(CMAKE_Fortran_FLAGS_RELEASE "")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /libs:static")
  else()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -static")
  endif()
endif()

set(SKIP_BUILD_RPATH true)
ADD_EXECUTABLE( Slab ${SRC} )

SET_TARGET_PROPERTIES(Slab PROPERTIES
  COMPILE_FLAGS "-ffree-line-length-200 ${SHARED_FLAGS}"
)

include("${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/ProjectMacros.cmake")
install_and_fixup_exe_target( Slab PreProcess/GrndTempCalc )
install(FILES "${CMAKE_CURRENT_SOURCE_DIR}/../../idd/SlabGHT.idd" DESTINATION "PreProcess/GrndTempCalc/")

