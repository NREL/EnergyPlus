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

  # This will make use of ctest Fixtures, because add_test doesn't support more than one "COMMAND" argument
  # Setup Fixture
  add_test(NAME ConvertInputFormat.Setup.RemoveDir COMMAND ${CMAKE_COMMAND} -E rm -Rf ${TEST_DIR})
  add_test(NAME ConvertInputFormat.Setup.RemakeDir COMMAND ${CMAKE_COMMAND} -E make_directory ${TEST_DIR})

  add_test(NAME ConvertInputFormat.Setup.CopyRegular
           COMMAND ${CMAKE_COMMAND} -E copy_if_different ${IDF_FILE} "${TEST_DIR}/1ZoneUncontrolled.idf")
  add_test(NAME ConvertInputFormat.Setup.CopyNoDot
           COMMAND ${CMAKE_COMMAND} -E copy_if_different ${IDF_FILE} "${TEST_DIR}/1.ZoneUncontrolled.idf")
  set_tests_properties(ConvertInputFormat.Setup.RemoveDir PROPERTIES FIXTURES_SETUP ConvertInputFormat.SetupFixture)
  set_tests_properties(ConvertInputFormat.Setup.RemakeDir PROPERTIES DEPENDS ConvertInputFormat.Setup.RemoveDir FIXTURES_SETUP ConvertInputFormat.SetupFixture)
  set_tests_properties(ConvertInputFormat.Setup.CopyRegular PROPERTIES DEPENDS ConvertInputFormat.Setup.RemakeDir FIXTURES_SETUP ConvertInputFormat.SetupFixture)
  set_tests_properties(ConvertInputFormat.Setup.CopyNoDot PROPERTIES DEPENDS ConvertInputFormat.Setup.RemakeDir FIXTURES_SETUP ConvertInputFormat.SetupFixture)


  add_test(NAME ConvertInputFormat.RegularFile_AbsolutePath
    COMMAND ConvertInputFormat --output "${TEST_DIR}/RegularFile_AbsolutePath" "${TEST_DIR}/1ZoneUncontrolled.idf"
  )
  add_test(NAME ConvertInputFormat.RegularFile_AbsolutePath.TestOK
    COMMAND ${CMAKE_COMMAND} -E cat "${TEST_DIR}/RegularFile_AbsolutePath/1ZoneUncontrolled.epJSON" # cat will fail if it doesn't exist
  )
  set_tests_properties(ConvertInputFormat.RegularFile_AbsolutePath.TestOK PROPERTIES FIXTURES_CLEANUP RegularFile_AbsolutePathFixture)
  set_tests_properties(ConvertInputFormat.RegularFile_AbsolutePath PROPERTIES FIXTURES_REQUIRED "ConvertInputFormat.SetupFixture; RegularFile_AbsolutePathFixture")


  add_test(NAME ConvertInputFormat.RegularFile_RelativePath
    COMMAND ConvertInputFormat --output RegularFile_RelativePath "1ZoneUncontrolled.idf"
    WORKING_DIRECTORY ${TEST_DIR}
  )
  add_test(NAME ConvertInputFormat.RegularFile_RelativePath.TestOK
    COMMAND ${CMAKE_COMMAND} -E cat "${TEST_DIR}/RegularFile_RelativePath/1ZoneUncontrolled.epJSON" # cat will fail if it doesn't exist
  )
  set_tests_properties(ConvertInputFormat.RegularFile_RelativePath.TestOK PROPERTIES FIXTURES_CLEANUP RegularFile_RelativePathFixture)
  set_tests_properties(ConvertInputFormat.RegularFile_RelativePath PROPERTIES FIXTURES_REQUIRED "ConvertInputFormat.SetupFixture; RegularFile_RelativePathFixture")



  add_test(NAME ConvertInputFormat.ExtraDotFile_AbsolutePath
    COMMAND ConvertInputFormat --output "${TEST_DIR}/ExtraDotFile_AbsolutePath" "${TEST_DIR}/1.ZoneUncontrolled.idf"
  )
  add_test(NAME ConvertInputFormat.ExtraDotFile_AbsolutePath.TestOK
    COMMAND ${CMAKE_COMMAND} -E cat "${TEST_DIR}/ExtraDotFile_AbsolutePath/1.ZoneUncontrolled.epJSON" # cat will fail if it doesn't exist
  )
set_tests_properties(ConvertInputFormat.ExtraDotFile_AbsolutePath.TestOK PROPERTIES FIXTURES_CLEANUP ExtraDotFile_AbsolutePathFixture)
  set_tests_properties(ConvertInputFormat.ExtraDotFile_AbsolutePath PROPERTIES FIXTURES_REQUIRED "ConvertInputFormat.SetupFixture; ExtraDotFile_AbsolutePathFixture")


  add_test(NAME ConvertInputFormat.ExtraDotFile_RelativePath
    COMMAND ConvertInputFormat --output ExtraDotFile_RelativePath "1.ZoneUncontrolled.idf"
    WORKING_DIRECTORY ${TEST_DIR}
  )
  add_test(NAME ConvertInputFormat.ExtraDotFile_RelativePath.TestOK
    COMMAND ${CMAKE_COMMAND} -E cat "${TEST_DIR}/ExtraDotFile_RelativePath/1.ZoneUncontrolled.epJSON" # cat will fail if it doesn't exist
  )
  set_tests_properties(ConvertInputFormat.ExtraDotFile_RelativePath.TestOK PROPERTIES FIXTURES_CLEANUP ExtraDotFile_RelativePathFixture)
  set_tests_properties(ConvertInputFormat.ExtraDotFile_RelativePath PROPERTIES FIXTURES_REQUIRED "ConvertInputFormat.SetupFixture; ExtraDotFile_RelativePathFixture")

endif()
