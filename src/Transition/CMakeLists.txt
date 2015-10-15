project(Transition Fortran)

set( CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )
set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )

include_directories( ${CMAKE_CURRENT_BINARY_DIR} )
#configure_file( CreateNewIDFUsingRulesV8_3_0.in.f90 "${CMAKE_CURRENT_BINARY_DIR}/CreateNewIDFUsingRulesV8_3_0.f90" )
#configure_file( IDDAssignV8_3_0.in.f90 "${CMAKE_CURRENT_BINARY_DIR}/IDDAssignV8_3_0.f90" )

# first create a static library of shared stuff
SET(LIB_SRC
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
add_library( TransitionLib STATIC ${LIB_SRC} )

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
list( APPEND VERSIONS 8_3_0 )
list( APPEND VERSIONS 8_4_0 )
list( APPEND VERSIONS 8_5_0 )

if(APPLE)
  if( "${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU" )
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-none")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fbounds-check")
  else()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fpp")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -static-intel")
  endif()
elseif(UNIX)
  set(CMAKE_SKIP_RPATH TRUE)
  if( "${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU" )
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -static" )
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-none")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fbounds-check")
  else()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fpp")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -static-intel")
  endif()
else()  # Windows
  if( "${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU" )
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -static")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-none")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fbounds-check")
  else()
    # Set release flags to be empty
    set(CMAKE_Fortran_FLAGS_RELEASE "")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /libs:static")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /fpp")
  endif()
endif()

list( LENGTH VERSIONS VERSIONS_SIZE)

math(EXPR end "${VERSIONS_SIZE} - 1")

include("${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/ProjectMacros.cmake")

FOREACH( i RANGE 1 ${end} )

math(EXPR ii "${i} - 1")

LIST(GET VERSIONS ${ii} OLD_VERSION)
LIST(GET VERSIONS ${i} NEW_VERSION)

# then create all the binaries using just the Transition source and the appropriate version of the main sub
SET(SRC
  Transition.f90
  CreateNewIDFUsingRulesV${NEW_VERSION}.f90
)

STRING( REPLACE _ - OLD-VERSION ${OLD_VERSION} )
STRING( REPLACE _ - NEW-VERSION ${NEW_VERSION} )

SET(LAST_NAME "Transition-V${OLD-VERSION}-to-V${NEW-VERSION}" )

SET(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${LAST_NAME} )
ADD_EXECUTABLE( "${LAST_NAME}" ${SRC} )
TARGET_LINK_LIBRARIES( "${LAST_NAME}" TransitionLib )

install_and_fixup_exe_target( "${LAST_NAME}" PreProcess/IDFVersionUpdater )

ENDFOREACH()

# then install all the support files
install( FILES "SupportFiles/Report Variables 1-0-0-023 to 1-0-1.csv" DESTINATION "PreProcess/IDFVersionUpdater" )
install( FILES "SupportFiles/Report Variables 1-0-1-042 to 1-0-2.csv" DESTINATION "PreProcess/IDFVersionUpdater" )
install( FILES "SupportFiles/Report Variables 1-0-2-008 to 1-0-3.csv" DESTINATION "PreProcess/IDFVersionUpdater" )
install( FILES "SupportFiles/Report Variables 1-0-3-019 to 1-1-0.csv" DESTINATION "PreProcess/IDFVersionUpdater" )
install( FILES "SupportFiles/Report Variables 1-1-0-020 to 1-1-1.csv" DESTINATION "PreProcess/IDFVersionUpdater" )
install( FILES "SupportFiles/Report Variables 1-1-1-012 to 1-2-0.csv" DESTINATION "PreProcess/IDFVersionUpdater" )
install( FILES "SupportFiles/Report Variables 1-2-0-029 to 1-2-1.csv" DESTINATION "PreProcess/IDFVersionUpdater" )
install( FILES "SupportFiles/Report Variables 1-2-1-012 to 1-2-2.csv" DESTINATION "PreProcess/IDFVersionUpdater" )
install( FILES "SupportFiles/Report Variables 1-2-2-030 to 1-2-3.csv" DESTINATION "PreProcess/IDFVersionUpdater" )
install( FILES "SupportFiles/Report Variables 1-2-3-031 to 1-3-0.csv" DESTINATION "PreProcess/IDFVersionUpdater" )
install( FILES "SupportFiles/Report Variables 1-3-0-018 to 1-4-0.csv" DESTINATION "PreProcess/IDFVersionUpdater" )
install( FILES "SupportFiles/Report Variables 1-4-0-025 to 2-0-0.csv" DESTINATION "PreProcess/IDFVersionUpdater" )
install( FILES "SupportFiles/Report Variables 2-0-0-025 to 2-1-0.csv" DESTINATION "PreProcess/IDFVersionUpdater" )
install( FILES "SupportFiles/Report Variables 2-1-0-023 to 2-2-0.csv" DESTINATION "PreProcess/IDFVersionUpdater" )
install( FILES "SupportFiles/Report Variables 2-2-0-023 to 3-0-0.csv" DESTINATION "PreProcess/IDFVersionUpdater" )
install( FILES "SupportFiles/V3-0-0-ObjectRenames.txt" DESTINATION "PreProcess/IDFVersionUpdater" )
install( FILES "SupportFiles/Report Variables 3-0-0-028 to 3-1-0.csv" DESTINATION "PreProcess/IDFVersionUpdater" )
install( FILES "SupportFiles/Report Variables 3-1-0-027 to 4-0-0.csv" DESTINATION "PreProcess/IDFVersionUpdater" )
install( FILES "SupportFiles/Report Variables 4-0-0-024 to 5-0-0.csv" DESTINATION "PreProcess/IDFVersionUpdater" )
install( FILES "SupportFiles/Report Variables 5-0-0-031 to 6-0-0.csv" DESTINATION "PreProcess/IDFVersionUpdater" )
install( FILES "SupportFiles/Report Variables 6-0-0-023 to 7-0-0.csv" DESTINATION "PreProcess/IDFVersionUpdater" )
install( FILES "SupportFiles/Report Variables 7-0-0-036 to 7-1-0.csv" DESTINATION "PreProcess/IDFVersionUpdater" )
install( FILES "SupportFiles/Report Variables 7-1-0-012 to 7-2-0.csv" DESTINATION "PreProcess/IDFVersionUpdater" )
install( FILES "SupportFiles/Report Variables 7-2-0-006 to 8-0-0.csv" DESTINATION "PreProcess/IDFVersionUpdater" )
install( FILES "SupportFiles/Report Variables 8-0-0-007 to 8-1-0.csv" DESTINATION "PreProcess/IDFVersionUpdater" )
install( FILES "SupportFiles/Report Variables 8-1-0-009 to 8-2-0.csv" DESTINATION "PreProcess/IDFVersionUpdater" )
install( FILES "SupportFiles/Report Variables 8-2-0 to 8-3-0.csv" DESTINATION "PreProcess/IDFVersionUpdater" )
install( FILES "SupportFiles/Report Variables 8-3-0 to 8-4-0.csv" DESTINATION "PreProcess/IDFVersionUpdater" )
