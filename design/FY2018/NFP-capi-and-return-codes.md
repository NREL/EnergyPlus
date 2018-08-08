Add C-API for EnergyPlus DLL and API Method that uses Return Codes instead of std::exit()
=========================================================================================

**Michael O'Keefe, Big Ladder Software**

- August 8, 2018
 

## Justification for New Feature ##

Calls to `std::exit()` within the EnergyPlusAPI make the resulting library
incompatible with many external callers. Furthermore, because API functions are
not exposed with a C-API interface, they cannot be called by non-C++ languages
(or even C++ from a different compiler).


## E-mail and Conference Call Conclusions ##

TBD


## Overview ##

As a clear example of what is not working correctly, please see file
`src/EnergyPlus/test_ep_as_library.cc`.

Change the file to the following format and recompile:

```C++
// file src/EnergyPlus/test_ep_as_library.cc
// Header Snipped ...
#include "EnergyPlusPgm.hh"
#include <iostream>

void message_callback_handler(std::string const &message)
{
    std::cout << "EnergyPlusLibrary (message): " << message << std::endl;
}

void progress_callback_handler(int const progress)
{
    std::cout << "EnergyPlusLibrary (progress): " << progress << std::endl;
}

int main(int argc, char *argv[])
{
    std::cout << "Using EnergyPlus as a library." << std::endl;
    StoreMessageCallback(message_callback_handler);
    StoreProgressCallback(progress_callback_handler);

    if (argc < 2) {
        std::cout << "Call this with a path to run EnergyPlus as the only argument" << std::endl;
        return 1;
    } else {
        EnergyPlusPgm(argv[1]);
    }
    // **** BEGIN ADDED ****
    // Note: the reason for the following stream handling functions is
    //       explained in the prose below this code.
    if (!std::cin.good()) std::cin.clear();
    if (!std::cerr.good()) std::cerr.clear();
    if (!std::cout.good()) std::cout.clear();
    // The following two lines should appear in the output
    std::cerr << "THIS LINE SHOULD DISPLAY." << std::endl;
    std::cout << "THIS LINE SHOULD DISPLAY, TOO." << std::endl;
    return 0;
    // **** END ADDED ****
}
```

When the executable `TestEnergyPlusCallbacks.exe` is run (which uses
`test_ep_as_library.cc`), the lines "THIS LINE SHOULD DISPLAY." and "THIS LINE
SHOULD DISPLAY, TOO." never appear because `EnergyPlusPgm(...)` (indirectly)
calls `std::exit(...)` which exits the program (and any program calling
EnergyPlus in the same process) and thus your code never returns to the caller
from `EnergyPlusPgm` to do any follow-on tasks. This makes the EnergyPlus DLL
impossible to use from the same process for all but the most trivial of
use-cases. This new feature proposal proposes fixing this specific issue by
creating a new API method, `int EnergyPlusPgmReturnCodes(String)`, which does
**not** call `std::exit(...)` to allow use of the API for programs running in the
same process. Note that the existing API method, `void EnergyPlusPgm(String)`
is proposed to be retained with the same behavior for backward compatibility
for any and all who were depending on that function and its behavior.

In addition, we propose exposing a new C-API. A C-API allows a DLL to be more
easily called from non-C++ languages. The C [Application Binary Interface] (ABI) is
stable and does not experience the [name-mangling] of the C++ compiler. The
exposure of a C-API in a C++ DLL is commonly recommended for allowing calling
from non-C++ programming languages (see [1], [2], and [3]).

[1]: https://msdn.microsoft.com/en-us/library/wf2w9f6x.aspx
[2]: https://msdn.microsoft.com/en-us/library/dt232c9t.aspx
[3]: http://rvelthuis.de/articles/articles-dlls.html
[name-mangling]: https://www.geeksforgeeks.org/extern-c-in-c/
[Application Binary Interface]: https://en.wikipedia.org/wiki/Application_binary_interface

Providing a C-API is easily done by creating an `extern "C"` block. In
addition, arguments to functions and callbacks must be translated so as to be
C-compatible as well. For example, the directory path argument of `void
EnergyPlusPgm(std::string)` is currently a C++ String Object which is not
possible to create outside of C++. Instead, a "c-style string" (`char*`) along
with the string length (`int`) is required; almost all popular programming
languages can marshall data to these basic C types. Behind the scenes, these
C-API functions would then call the C++ API functions they correspond
to. It is also worth-while to explicitly set the calling convention on Windows
to `__stdcall` which is the same as the Windows 32-bit API; again, for ease of
interoperation for Windows developers.

By adding a C-compatible API to the EnergyPlus DLL, the DLL can be called from
many other non-C++ languages such as Python (via ctypes), C, Delphi, Ruby (via
the FFI gem), and others.

As an aside, it was discovered that stdin and stdout are placed into
ObjexxFCL's global streams and some subtleties about how they are handled
within gio.cc (specifically, the `inquire` function) and `Streams.hh` in the
ObjexxFCL library cause stdin and stdout to be put into an errored state and
unusable after the call.

The lines to check the "goodness" of the standard streams and clear their error
codes in the modifications to `test_ep_as_library.cc` given above show one way
to work around this situation although it may be advisable for the EnergyPlus
team to consider preventing stdin, stdout, and stderr from being placed into
the global streams of the ObjexxFCL library to begin with.

This work will not modify ObjexxFCL and the issue has been discussed with
ObjexxFCL's owner offline and a fix is being considered for an upcoming release
of ObjexxFCL. The calls to
[\*.good()](http://www.cplusplus.com/reference/ios/ios/good/) and
[\*.clear()](http://www.cplusplus.com/reference/ios/ios/clear/) are sufficient
to clear the error state of the streams for the purposes of this effort.

## Approach ##

We propose:

- adding a new C++ API interface function called `int
  EnergyPlusPgmReturnCodes(String)` which runs an EnergyPlus simulation (same
  as the current `void EnergyPlusPgm(String)`) but returns the code
  `EXIT_SUCCESS` (0) for success and `EXIT_FAILURE` (1) for failure **without**
  calling `std::exit(...)`
- preserving the current interface's name (`EnergyPlusPgm`) and behavior (to exit
  via `std::exit`) for backwards compatibility
- adding a new C-API interface with three functions declared as `extern "C"`
  along with the `__stdcall` calling convention on Windows to allow ease of
  interoperation with non-C++ languages (including possibly C++ written with a
  different compiler version than EnergyPlus was compiled with) as follows:
    - `int CAPI_EnergyPlusPgmReturnCodes(const char*, int)` corresponding to
      `int EnergyPlusPgmReturnCodes(String)`
    - `int CAPI_StoreMessageCallback(MsgCallback)` corresponding to
      `void StoreMessageCallback(void (*f)(std::string const&))`
    - `int CAPI_StoreProgressCallback(ProgressCallback)` corresponding to
      `void StoreProgressCallback(void (*f)(int const))`
- update `test_ep_as_library.cc` to call `EnergyPlusPgmReturnCodes` and
  demonstrate additional functionality after returning to the caller

Although we have proposed new names for the C-API and return-codes version of
`EnergyPlusPgm` above, alternative name suggestions are welcome.

In the above definitions, the type definitions for the message and progress
callbacks are:

```C++
typedef void (CALLCONV * MsgCallback)(const char *);
typedef void (CALLCONV * ProgressCallback)(int const);
```

## Testing/Validation/Data Sources ##

A test that the return codes functionality is working can be provided by
checking for the strings "THIS LINE SHOULD DISPLAY" in the output of a call to
`src/EnergyPlus/test_ep_as_library.cc`. A test of the C-API can be made by
calling the DLL, for example, with a Python script such as the following which
uses the [ctypes] library to access the C-API:

[ctypes]: https://docs.python.org/2/library/ctypes.html

```python
"""
This python module demonstrates calling into the Windows DLL for EnergyPlus
using a C-API.

This must be called with a 32-bit version of Python (using Python 2.7 for
compatibility with EnergyPlus building). This example assumes calling from
Windows.

Author: Michael O'Keefe, 2018-08-08
Copyright 2018 Big Ladder Software
"""
import sys
import os.path
import ctypes

BUFFER_SIZE = 200

dll_path = ("energyplusapi.dll")

print("{} exists: {}".format(dll_path, os.path.exists(dll_path)))

eplus = ctypes.WinDLL(dll_path)

# StoreMessageCallback
msg_callback_fn = ctypes.WINFUNCTYPE(None, ctypes.c_char_p)
set_msg_callback = eplus.CAPI_StoreMessageCallback
set_msg_callback.argtypes = [msg_callback_fn]
set_msg_callback.restype = ctypes.c_int
def msg_callback(msg):
    print("EnergyPlus Message: \"{}\"".format(msg))
wrapped_msg_callback = msg_callback_fn(msg_callback)
out1 = set_msg_callback(wrapped_msg_callback)
print("StoreMessageCallback called; return: {}".format(out1))

# StoreProgressCallback
progress_callback_fn = ctypes.WINFUNCTYPE(None, ctypes.c_int)
store_progress_callback = eplus.CAPI_StoreProgressCallback
store_progress_callback.argtypes = [progress_callback_fn]
store_progress_callback.restype = ctypes.c_int
def progress_callback(progress):
    print("EnergyPlus: {:03d} % Complete".format(progress))
wrapped_progress_callback = progress_callback_fn(progress_callback)
out2 = store_progress_callback(wrapped_progress_callback)
print("StoreProgressCallback called; return: {}".format(out2))

# RunEPlus
f = eplus.CAPI_EnergyPlusPgmReturnCodes
f.argtypes = [ctypes.c_char_p, ctypes.c_int]

if __name__ == "__main__":
	print("Preparing to Run EnergyPlus")
	len_args = len(sys.argv)
	if len_args == 2:
		path = sys.argv[1]
	print("Running with directory: " + path)
	path_count = len(path) 
	out3 = f(path, path_count)
	print("Called EnergyPlus: {}".format(out3))

	print("Done!")
```

Running the above script, which must be run with a 32-bit Python, will
demonstrate that the C-API can be called from non-C++ languages.

## Input Output Reference Documentation ##

Not applicable. However, the API functions of the DLL do not appear to be
documented anywhere. The EnergyPlus team may wish to document the API at a
future date.

## Input Description ##

Not applicable

## Outputs Description ##

Not applicable

## Engineering Reference ##

Not applicable

## Example File and Transition Changes ##

Not applicable

## References ##

1 -- https://msdn.microsoft.com/en-us/library/wf2w9f6x.aspx

2 -- https://msdn.microsoft.com/en-us/library/dt232c9t.aspx

3 -- http://rvelthuis.de/articles/articles-dlls.html

ctypes -- https://docs.python.org/2/library/ctypes.html

