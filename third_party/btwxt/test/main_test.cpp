/* Copyright (c) 2018 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */


#include "gtest/gtest.h"

// btwxt
#include <btwxt.h>
#include <griddeddata.h>
#include <error.h>


using namespace Btwxt;


// use custom callback function b/c we don't want to exit tests on error.
void my_callback(MsgLevel messageType,
                 std::string message, void *contextPtr);


void my_callback(
        const MsgLevel messageType,
        const std::string message,
        void *//contextPtr
) {
    if (messageType == MsgLevel::MSG_ERR) {
        std::cout << "  ERROR: " << message << std::endl;
        throw std::invalid_argument(stringify("  ERROR: ", message));
    } else {
        if (static_cast<int>(messageType) >= Btwxt::LOG_LEVEL) {
            std::string prefix("  DEBUG: ");
            if (messageType == MsgLevel::MSG_WARN) {
                prefix = "  WARNING: ";
            } else if (messageType == MsgLevel::MSG_INFO) {
                prefix = "  NOTE: ";
            }
            std::cout << prefix << message << std::endl;
        }
    }
}

int main(int argc, char **argv) {
    Btwxt::LOG_LEVEL = 1;
    ::testing::InitGoogleTest(&argc, argv);

    int *my_context_ptr = nullptr;
    setMessageCallback(my_callback, my_context_ptr);

    return RUN_ALL_TESTS();
};
