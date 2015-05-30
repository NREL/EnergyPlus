// JSON unit tests

// Google test headers
#include <gtest/gtest.h>

#include <json/json.h>

TEST( JsonCppTests, Simple )
{
  std::string string("{\"name\":\"EnergyPlus\", \"value\":1, \"string array\":[\"yellow\", \"blue\"]}");
  Json::Reader reader;
  Json::Value root;
  EXPECT_TRUE(reader.parse(string, root));
  ASSERT_FALSE(root.isNull());
  ASSERT_TRUE(root.isObject());
  EXPECT_EQ(root["name"], "EnergyPlus");
  EXPECT_TRUE(root["string array"].isArray());
  EXPECT_EQ(2, root["string array"].size());
  int i = 0;
  for (const Json::Value &value : root["string array"]) {
    ASSERT_TRUE(value.isString());
    switch (i) {
    case 0:
      EXPECT_EQ(value, "yellow");
      break;
    case 1:
      EXPECT_EQ(value, "blue");
      break;
    }
    ++i;
  }
  EXPECT_FALSE(root["not a key"]);
}


