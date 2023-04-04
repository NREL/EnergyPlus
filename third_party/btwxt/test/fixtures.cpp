/* Copyright (c) 2018 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

#include "fixtures.hpp"
using namespace Btwxt;

void btwxt_message(MsgLevel msglevel, const std::string_view &msg, void *) {
  static std::map<MsgLevel, std::string_view> severity_str{{MsgLevel::MSG_DEBUG, "  DEBUG: "},
                                                           {MsgLevel::MSG_INFO, "  NOTE: "},
                                                           {MsgLevel::MSG_WARN, "  WARNING: "},
                                                           {MsgLevel::MSG_ERR, "  ERROR: "}};
  std::cout << severity_str[msglevel] << msg << std::endl;
}
