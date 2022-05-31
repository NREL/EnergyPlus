cmake_minimum_required(VERSION 3.5.1)
project(ConvertInputFormat)

if(APPLE OR UNIX)
  add_executable(ConvertInputFormat main.cpp)
else() # windows
  add_executable(ConvertInputFormat main.cpp) #  "${CMAKE_CURRENT_BINARY_DIR}/energyplus.rc" )
endif()

# Detect OpenMP support in a compiler. If the compiler supports OpenMP, the
# flags to compile with OpenMP are returned and added.
find_package(OpenMP COMPONENTS CXX)
if(OpenMP_CXX_FOUND)
  set(CMAKE_INSTALL_OPENMP_LIBRARIES TRUE PARENT_SCOPE)
  target_link_libraries(ConvertInputFormat PRIVATE OpenMP::OpenMP_CXX)
endif()

target_link_libraries(ConvertInputFormat PRIVATE energyplusparser project_options project_warnings)

# See https://en.cppreference.com/w/cpp/filesystem#Notes
if ((CMAKE_COMPILER_IS_GNUCXX AND CMAKE_CXX_COMPILER_VERSION VERSION_LESS 9.1) OR
    ( ( "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "AppleClang") AND (CMAKE_CXX_COMPILER_VERSION VERSION_LESS 9.0)))
  target_link_libraries( ConvertInputFormat PRIVATE stdc++fs )
endif()
set_target_properties(ConvertInputFormat PROPERTIES VERSION ${ENERGYPLUS_VERSION})
set_target_properties(ConvertInputFormat PROPERTIES FOLDER Auxiliary)

install(TARGETS ConvertInputFormat DESTINATION ./)

if(BUILD_TESTING)

  set(TEST_DIR "${PROJECT_BINARY_DIR}/tst") # build/src/ConverInputFormat/tst
  set(IDF_FILE "${PROJECT_SOURCE_DIR}/../../testfiles/1ZoneUncontrolled.idf")

  file(MAKE_DIRECTORY ${TEST_DIR})
  configure_file(${IDF_FILE} "${TEST_DIR}/1ZoneUncontrolled.idf" COPYONLY)
  configure_file(${IDF_FILE} "${TEST_DIR}/1.ZoneUncontrolled.idf" COPYONLY)

  add_test(NAME ConvertInputFormat2.RegularFile_AbsolutePath
    COMMAND  "${Python_EXECUTABLE}" "${PROJECT_SOURCE_DIR}/test_convert_input_format.py"
             --convertinputformat-exe $<TARGET_FILE:ConvertInputFormat>
             --out-dir "${TEST_DIR}/RegularFile_AbsolutePath"
             --expected-filename 1ZoneUncontrolled.epJSON
             --input-file "${TEST_DIR}/1ZoneUncontrolled.idf"
  )

  add_test(NAME ConvertInputFormat2.RegularFile_RelativePath
    COMMAND  "${Python_EXECUTABLE}" "${PROJECT_SOURCE_DIR}/test_convert_input_format.py"
             --convertinputformat-exe $<TARGET_FILE:ConvertInputFormat>
             --out-dir "RegularFile_AbsolutePath"
             --expected-filename 1ZoneUncontrolled.epJSON
             --input-file "1ZoneUncontrolled.idf"
    WORKING_DIRECTORY ${TEST_DIR}
  )

  add_test(NAME ConvertInputFormat2.ExtraDotFile_AbsolutePath
    COMMAND  "${Python_EXECUTABLE}" "${PROJECT_SOURCE_DIR}/test_convert_input_format.py"
             --convertinputformat-exe $<TARGET_FILE:ConvertInputFormat>
             --out-dir "${TEST_DIR}/ExtraDotFile_AbsolutePath"
             --expected-filename 1.ZoneUncontrolled.epJSON
             --input-file "${TEST_DIR}/1.ZoneUncontrolled.idf"
  )

  add_test(NAME ConvertInputFormat2.ExtraDotFile__RelativePath
    COMMAND  "${Python_EXECUTABLE}" "${PROJECT_SOURCE_DIR}/test_convert_input_format.py"
             --convertinputformat-exe $<TARGET_FILE:ConvertInputFormat>
             --out-dir "ExtraDotFile__RelativePath"
             --expected-filename 1.ZoneUncontrolled.epJSON
             --input-file "1.ZoneUncontrolled.idf"
    WORKING_DIRECTORY ${TEST_DIR}
  )
endif()
