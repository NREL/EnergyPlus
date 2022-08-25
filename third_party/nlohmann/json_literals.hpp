#ifndef JSON_STRING_LITERALS_HPP
#define JSON_STRING_LITERALS_HPP

#include <hedley.hpp>

namespace nlohmann
{

inline namespace literals
{
inline namespace json_literals
{


/// @brief user-defined string literal for JSON values
/// @sa https://json.nlohmann.me/api/basic_json/operator_literal_json/
JSON_HEDLEY_NON_NULL(1)
inline nlohmann::json operator "" _json(const char* s, std::size_t n)
{
    return nlohmann::json::parse(s, s + n);
}

/// @brief user-defined string literal for JSON pointer
/// @sa https://json.nlohmann.me/api/basic_json/operator_literal_json_pointer/
JSON_HEDLEY_NON_NULL(1)
inline nlohmann::json::json_pointer operator "" _json_pointer(const char* s, std::size_t n)
{
    return nlohmann::json::json_pointer(std::string(s, n));
}

}
}
}

#if JSON_USE_GLOBAL_UDLS
    using nlohmann::literals::json_literals::operator "" _json; // NOLINT(misc-unused-using-decls,google-global-names-in-headers)
    using nlohmann::literals::json_literals::operator "" _json_pointer; //NOLINT(misc-unused-using-decls,google-global-names-in-headers)
#endif


#include <hedley_undef.hpp>

#endif
