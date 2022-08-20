#ifndef ERROR_HANDLING_TK205_H_
#define ERROR_HANDLING_TK205_H_

#include <functional>
#include <string>
#include <nlohmann/json.hpp>
#include <iostream>

namespace tk205 {

    enum class MsgSeverity : unsigned int {
        DEBUG_205, 
        INFO_205, 
        WARN_205, 
        ERR_205
    };

    using msg_handler = std::function<void(MsgSeverity, const std::string &, void *)>;

    void set_error_handler(msg_handler handler, void *caller_info);
    void show_message(MsgSeverity severity, const std::string& message);
}


#endif