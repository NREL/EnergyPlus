project(Transition Fortran)

set( CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )
set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )

include_directories( ${CMAKE_CURRENT_BINARY_DIR} )
configure_file( CreateNewIDFUsingRulesV8_2_0.in.f90 "${CMAKE_CURRENT_BINARY_DIR}/CreateNewIDFUsingRulesV8_2_0.f90" )
configure_file( IDDAssignV8_2_0.in.f90 "${CMAKE_CURRENT_BINARY_DIR}/IDDAssignV8_2_0.f90" )

SET(SRC
  Transition.f90
  DataGlobals.f90
  DataStringGlobals.f90
  DataVCompareGlobals.f90
  DisplayRoutines.f90
  General.f90
  InputProcessor.f90
  platformDepUtilityRoutines.f90
  SortAndStringUtilities.f90
  UtilityRoutines.f90
  VCompareGlobalRoutines.f90
  VCompareUtilityRoutines.f90
)

list( APPEND VERSIONS 1_0_0 )
list( APPEND VERSIONS 1_0_1 )
list( APPEND VERSIONS 1_0_2 )
list( APPEND VERSIONS 1_0_3 )
list( APPEND VERSIONS 1_1_0 )
list( APPEND VERSIONS 1_1_1 )
list( APPEND VERSIONS 1_2_0 )
list( APPEND VERSIONS 1_2_1 )
list( APPEND VERSIONS 1_2_2 )
list( APPEND VERSIONS 1_2_3 )
list( APPEND VERSIONS 1_3_0 )
list( APPEND VERSIONS 1_4_0 )
list( APPEND VERSIONS 2_0_0 )
list( APPEND VERSIONS 2_1_0 )
list( APPEND VERSIONS 2_2_0 )
list( APPEND VERSIONS 3_0_0 )
list( APPEND VERSIONS 3_1_0 )
list( APPEND VERSIONS 4_0_0 )
list( APPEND VERSIONS 5_0_0 )
list( APPEND VERSIONS 6_0_0 )
list( APPEND VERSIONS 7_0_0 )
list( APPEND VERSIONS 7_1_0 )
list( APPEND VERSIONS 7_2_0 )
list( APPEND VERSIONS 8_0_0 )
list( APPEND VERSIONS 8_1_0 )
list( APPEND VERSIONS 8_2_0 )

IF(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
  IF( WIN32 )
    SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp")
    SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-none")
    SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -static")
  ELSE()
    SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp")
    SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-none")
  ENDIF()
ENDIF()


IF(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
  SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fpp ")
  SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -libs:static")
ENDIF()

list( LENGTH VERSIONS VERSIONS_SIZE)

math(EXPR end "${VERSIONS_SIZE} - 1")

FOREACH( i RANGE 1 ${end} )

math(EXPR ii "${i} - 1")

LIST(GET VERSIONS ${ii} OLD_VERSION)
LIST(GET VERSIONS ${i} NEW_VERSION)

STRING( REPLACE _ - OLD-VERSION ${OLD_VERSION} )
STRING( REPLACE _ - NEW-VERSION ${NEW_VERSION} )

SET(LAST_NAME "Transition-V${OLD-VERSION}-to-V${NEW-VERSION}" )

SET(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${LAST_NAME} )
ADD_EXECUTABLE( "${LAST_NAME}" ${SRC} )
SET_PROPERTY( TARGET "${LAST_NAME}"
              PROPERTY COMPILE_DEFINITIONS V${NEW_VERSION} )

ENDFOREACH()

include("${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/ProjectMacros.cmake")
install_and_fixup_exe_target( "${LAST_NAME}" PreProcess/IDFVersionUpdater )
if( APPLE )
  install_and_fixup_exe_target( "${LAST_NAME}" PreProcess/IDFVersionUpdater/IDFVersionUpdater.app/Contents/MacOS/ )
endif()

