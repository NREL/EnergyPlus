#!/bin/bash

find src \( -name "*.hpp" -o -name "*.h" -o -name "*.hh" -o -name "*.cc" -o -name "*.cpp" -o -name "*.c" \) | xargs -n 1 -P 10 -I '{}' clang-format -style=file -i "{}"
find tst \( -name "*.hpp" -o -name "*.h" -o -name "*.hh" -o -name "*.cc" -o -name "*.cpp" -o -name "*.c" \) | xargs -n 1 -P 10 -I '{}' clang-format -style=file -i "{}"

