# Third-party code changes

This file captures descriptive changes made to libraries in the third_party folder.
Occasionally, some of the third party library code we use needs small modifications to work.
This could be due to a bug in the code or a required enhancement.
These changes have a strong tendency to get overridden when the library code is updated from upstream.
This generally results in diffs or warnings that we then have to go back and slog through to see what had changed.
This file is intended to be a freeform list of the changes made, so that when a library is updated, we can quickly apply those again.
Each library should have a heading here, and the content of each section could be bullet list, a descriptive paragraph, a table, and would optimally include either SHAs or links to the PRs where the changes were made.

## Eigen

The Eigen library causes some build warnings when brought in as-is.
A couple minor changes clean these build warnings up.
This was most recently applied here: https://github.com/NREL/EnergyPlus/commit/cd8f41aeab471832824c9c7ac9f1d4acabebd8ae#diff-a6f7a8949616a88e6a39813eef96c9ca

## doj

The doj library had an issue with ordering numerics, so this was patched here: https://github.com/NREL/EnergyPlus/pull/7036

## json

As of [nlhohmann/json](https://github.com/nlohmann/json) version 3.7.3, the only changes needed are the use of the `doj` library for comparator.

```
--- a/third_party/nlohmann/json.hpp
+++ b/third_party/nlohmann/json.hpp
@@ -47,6 +47,7 @@ SOFTWARE.
 #include <string> // string, stoi, to_string
 #include <utility> // declval, forward, move, pair, swap
 #include <vector> // vector
+#include <doj/alphanum.hpp>

 // #include <nlohmann/adl_serializer.hpp>

@@ -14931,9 +14932,9 @@ class basic_json
 #if defined(JSON_HAS_CPP_14)
     // Use transparent comparator if possible, combined with perfect forwarding
     // on find() and count() calls prevents unnecessary string construction.
-    using object_comparator_t = std::less<>;
+    using object_comparator_t = doj::alphanum_less<>;
 #else
-    using object_comparator_t = std::less<StringType>;
+    using object_comparator_t = doj::alphanum_less<StringType>;
 #endif
```

## valijson

The `validation_visitor.hpp` was modified to include better error messages and use RE2 instead of std::regex.
