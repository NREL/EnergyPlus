#ifndef TYPEINFO_205_H_
#define TYPEINFO_205_H_

#include <string_view>

namespace tk205 {

    struct enum_info
    { 
        std::string_view enumerant_name; 
        std::string_view display_text; 
        std::string_view description;
    };
}

#endif // TYPEINFO_205_H_