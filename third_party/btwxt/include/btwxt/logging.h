/* Copyright (c) 2018 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

#pragma once

#include <courierr/courierr.h>

namespace Btwxt {

    class BtwxtLogger : public Courierr::Courierr {
    public:
        void error(const std::string_view message) override { write_message("ERROR", message); }

        void warning(const std::string_view message) override { write_message("WARNING", message); }

        void info(const std::string_view message) override { write_message("NOTE", message); }

        void debug(const std::string_view message) override { write_message("DEBUG", message); }

    protected:
        void write_message(const std::string_view message_type, const std::string_view message) {
            std::string context_string =
                    message_context
                    ? fmt::format(" ({})", *(reinterpret_cast<std::string *>(message_context)))
                    : "";
            std::cout << fmt::format("  [{}]{} {}", message_type, context_string, message) << std::endl;
        }
    };

    class BtwxtException : public Courierr::CourierrException {
    public:
        explicit BtwxtException(const std::string &message, Courierr::Courierr &logger)
                : CourierrException(message, logger) {
        }
    };
}