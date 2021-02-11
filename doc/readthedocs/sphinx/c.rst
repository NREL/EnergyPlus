EnergyPlus C API
----------------

Functionality that was kept within EnergyPlus is now being exposed via a C API.
Although it is expected that most consumers of the API will use the Python bindings, the C API allows direct connection to the engine without the overhead of a Python interpreter layer.

The underlying C API is built with Doxygen, but included with this Sphinx documentation at `this page <index_c.html>`_.
