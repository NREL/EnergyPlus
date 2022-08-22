#include "error_handling_tk205.h"
#include <map>
#include <iostream>
#include <string_view>

namespace tk205 {

    msg_handler error_handler_;
    void *caller_info_;

    void set_error_handler(msg_handler handler, void *caller_info)
    {
        error_handler_ = std::move(handler);
        caller_info_ = caller_info;
    }

    void show_message(MsgSeverity severity, const std::string &message)
    {
        static std::map<MsgSeverity, std::string_view> severity_str {
            {MsgSeverity::DEBUG_205, "DEBUG"},
            {MsgSeverity::INFO_205, "INFO"},
            {MsgSeverity::WARN_205, "WARN"},
            {MsgSeverity::ERR_205, "ERR"}
        };
        if (!error_handler_)
        {
            //std::cout << severity_str[severity] << ": " << message << std::endl;
        }
        else
        {
            error_handler_(severity, message, caller_info_);
        }
    }
}

