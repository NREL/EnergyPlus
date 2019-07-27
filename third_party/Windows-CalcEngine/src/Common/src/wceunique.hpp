#ifndef WINDOWS_CALCENGINE_WCEUNIQUE_H
#define WINDOWS_CALCENGINE_WCEUNIQUE_H

#include <memory>

// C++ 11 does not support make_unique. Use this header file to have make_unique functionality.
// When using later C++ standards, remove this header file

namespace wce {
    template<typename T, typename... Args>
    std::unique_ptr<T> make_unique(Args&&... args) {
        return std::unique_ptr<T>(new T(std::forward<Args>(args)...));
    }
}

#endif //WINDOWS_CALCENGINE_WCEUNIQUE_H