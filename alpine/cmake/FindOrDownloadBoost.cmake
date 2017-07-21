if( WIN32 )
  if(CMAKE_CL_64)
    set(BOOST_ZIP_FILENAME "boost_1_55_0_msvc2013_64_static.zip")
    set(BOOST_ZIP_EXPECTED_MD5 "3ff90a94d025f139e290296ed0b41263")
  else()
    set(BOOST_ZIP_FILENAME "boost_1_55_0_msvc2013_32_static.zip")
    set(BOOST_ZIP_EXPECTED_MD5 "4ebc2a84ef13ef1853fcf3e6c27fc10c")
  endif()
elseif( APPLE )
    set(BOOST_ZIP_FILENAME "boost_1_55_0_osx.tar.gz")
    set(BOOST_ZIP_EXPECTED_MD5 "9e4c01a3f5c63e3bbf5cc76021eceb75")
elseif(EXISTS "/etc/redhat-release")
  set(BOOST_ZIP_FILENAME "boost_1_55_0_redhat.tar.gz")
  set(BOOST_ZIP_EXPECTED_MD5 "de952e0c36900a3ad000d74a1c41b7d2")
else()
  find_program(LSB_RELEASE lsb_release)
  execute_process(COMMAND ${LSB_RELEASE} -r
    OUTPUT_VARIABLE LSB_RELEASE_ID_SHORT
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
#message("LSB_RELEASE_ID_SHORT = ${LSB_RELEASE_ID_SHORT}")
if(LSB_RELEASE_ID_SHORT MATCHES "16.04")
  # ubuntu 16.04
    set(BOOST_ZIP_FILENAME "boost_1_55_0_xenial.tar.gz")
    set(BOOST_ZIP_EXPECTED_MD5 "45bf97fbdcde070aca3cc6181cd69362")
else()
  # ubuntu 14.04
    set(BOOST_ZIP_FILENAME "boost_1_55_0_linux.tar.gz")
    set(BOOST_ZIP_EXPECTED_MD5 "6b206a76268cd1a98d00d4aafb897ac9")
endif()
endif()

set(BOOST_ZIP_LOCAL_PATH "${CMAKE_BINARY_DIR}/${BOOST_ZIP_FILENAME}")
if(EXISTS "${BOOST_ZIP_LOCAL_PATH}")
  file(MD5 "${BOOST_ZIP_LOCAL_PATH}" BOOST_ZIP_MD5)
endif()

if( NOT BOOST_ZIP_MD5 STREQUAL BOOST_ZIP_EXPECTED_MD5)
  file(DOWNLOAD "http://openstudio-resources.s3.amazonaws.com/dependencies/${BOOST_ZIP_FILENAME}"
    ${BOOST_ZIP_LOCAL_PATH}
    INACTIVITY_TIMEOUT 120
    SHOW_PROGRESS
    EXPECTED_MD5 ${BOOST_ZIP_EXPECTED_MD5})
    execute_process(COMMAND ${CMAKE_COMMAND} -E tar xfz ${BOOST_ZIP_LOCAL_PATH} WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")
endif()
set(BOOST_ROOT "${CMAKE_BINARY_DIR}/Boost-install/")

set(Boost_USE_STATIC_LIBS ON)
find_package(Boost 1.55.0 REQUIRED COMPONENTS filesystem regex)
include_directories(SYSTEM ${Boost_INCLUDE_DIR})
mark_as_advanced(
  Boost_DIR
  BOOST_THREAD_LIBRARY
)
