#!/bin/bash


#diff --unchanged-line-format="" --new-line-format="" --old-line-format="{ \"filename\": \"$1\", \"line\": %dn, \"messagetype\": \"warning\", \"message\": \"Formatting does not meet standards, See .clang_format and apply formatting.\", \"tool\": \"format-checker\" }%c'\012'" "$1" <(clang-format-3.9 "$1")


#diff -I "^ *\S\+.*"  --unchanged-line-format="" --new-line-format="" --old-line-format="{ \"filename\": \"$1\", \"line\": %dn, \"messagetype\": \"warning\", \"message\": \"Formatting does not meet standards, See .clang_format and apply formatting.\", \"tool\": \"format-checker\", \"old\": \"%l\", \"new\": \"%>\" }%c'\012'" "$1" <(clang-format-3.9 "$1")


# Pre-filter on files that use tabs for indentation anywhere

grep -q -P "^ *\t *" "$1"

if [ $? -eq 0 ]
then
  diff -I "^ *\S\+.*" --unchanged-line-format="" --new-line-format="" --old-line-format="{ \"filename\": \"$1\", \"line\": %dn, \"messagetype\": \"warning\", \"message\": \"Formatting does not meet standards, See .clang_format and apply formatting.\", \"tool\": \"format-checker\" }%c'\012'" "$1" <(clang-format "$1")
fi


# diff  "$1" <(clang-format-3.9 "$1")

