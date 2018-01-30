#!/bin/bash

find src -name "*.hpp" -o -name "*.h" -o -name "*.hh" -o -name "*.cc" -o -name "*.cpp" -o -name "*.c" | xargs -n 1 -I '{}' clang-format-3.9 "{}"

