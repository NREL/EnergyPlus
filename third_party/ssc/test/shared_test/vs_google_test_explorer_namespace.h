// vs_google_test_explorer_namespace.h
//
// This is an example of a file/macro that could be used to trick the Google Test Adapter into
// sorting tests into namespaces in C++
//

#ifndef NAMESPACE_GTEST_INCLUDE_GTEST_GTEST_H_
#define NAMESPACE_GTEST_INCLUDE_GTEST_GTEST_H_

#include <gtest/gtest.h>

// A copy of GTEST_TEST_CLASS_NAME_, but with handling for namespace name.

#define NAMESPACE_GTEST_TEST_CLASS_NAME_(namespace_name, test_case_name, test_name) \
  namespace_name##_##test_case_name##_##test_name##_Test

// A copy of GTEST_TEST_, but with handling for namespace name.

#define NAMESPACE_GTEST_TEST_(namespace_name, test_case_name, test_name, parent_class, parent_id)\
class NAMESPACE_GTEST_TEST_CLASS_NAME_(namespace_name, test_case_name, test_name) : public parent_class {\
 public:\
  NAMESPACE_GTEST_TEST_CLASS_NAME_(namespace_name, test_case_name, test_name)() {}\
 private:\
  virtual void TestBody();\
  static ::testing::TestInfo* const test_info_ GTEST_ATTRIBUTE_UNUSED_;\
  GTEST_DISALLOW_COPY_AND_ASSIGN_(\
      NAMESPACE_GTEST_TEST_CLASS_NAME_(namespace_name, test_case_name, test_name));\
};\
\
::testing::TestInfo* const NAMESPACE_GTEST_TEST_CLASS_NAME_(namespace_name, test_case_name, test_name)\
  ::test_info_ =\
    ::testing::internal::MakeAndRegisterTestInfo(\
        #namespace_name "." #test_case_name, #test_name, NULL, NULL, /* <-- Defines the test as "Namespace.Classname" */ \
        ::testing::internal::CodeLocation(__FILE__, __LINE__), \
        (parent_id), \
        parent_class::SetUpTestCase, \
        parent_class::TearDownTestCase, \
        new ::testing::internal::TestFactoryImpl<\
            NAMESPACE_GTEST_TEST_CLASS_NAME_(namespace_name, test_case_name, test_name)>);\
void NAMESPACE_GTEST_TEST_CLASS_NAME_(namespace_name, test_case_name, test_name)::TestBody()

// Simple macro

#define NAMESPACE_TEST(namespace_name, test_case_name, test_name) \
  NAMESPACE_GTEST_TEST_(namespace_name, test_case_name, test_name,\
    ::testing::Test, ::testing::internal::GetTestTypeId())

#endif  // NAMESPACE_GTEST_INCLUDE_GTEST_GTEST_H_
