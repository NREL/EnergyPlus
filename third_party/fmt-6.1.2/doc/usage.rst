*****
Usage
*****

To use the {fmt} library, add :file:`fmt/core.h`, :file:`fmt/format.h`,
:file:`fmt/format-inl.h`, :file:`src/format.cc` and optionally other headers
from a `release archive <https://github.com/fmtlib/fmt/releases/latest>`_ or
the `Git repository <https://github.com/fmtlib/fmt>`_ to your project.
Alternatively, you can :ref:`build the library with CMake <building>`.

.. _building:

Building the Library
====================

The included `CMake build script`__ can be used to build the fmt
library on a wide range of platforms. CMake is freely available for
download from http://www.cmake.org/download/.

__ https://github.com/fmtlib/fmt/blob/master/CMakeLists.txt

CMake works by generating native makefiles or project files that can
be used in the compiler environment of your choice. The typical
workflow starts with::

  mkdir build          # Create a directory to hold the build output.
  cd build
  cmake ..  # Generate native build scripts.

where :file:`{<path/to/fmt>}` is a path to the ``fmt`` repository.

If you are on a \*nix system, you should now see a Makefile in the
current directory. Now you can build the library by running :command:`make`.

Once the library has been built you can invoke :command:`make test` to run
the tests.

You can control generation of the make ``test`` target with the ``FMT_TEST``
CMake option. This can be useful if you include fmt as a subdirectory in
your project but don't want to add fmt's tests to your ``test`` target.

If you use Windows and have Visual Studio installed, a :file:`FMT.sln`
file and several :file:`.vcproj` files will be created. You can then build them
using Visual Studio or msbuild.

On Mac OS X with Xcode installed, an :file:`.xcodeproj` file will be generated.

To build a `shared library`__ set the ``BUILD_SHARED_LIBS`` CMake variable to
``TRUE``::

  cmake -DBUILD_SHARED_LIBS=TRUE ...

__ http://en.wikipedia.org/wiki/Library_%28computing%29#Shared_libraries

Installing the Library
======================

After building the library you can install it on a Unix-like system by running
:command:`sudo make install`.

Usage with CMake
================

You can add the ``fmt`` library directory into your project and include it in
your ``CMakeLists.txt`` file::

   add_subdirectory(fmt)

or

::

   add_subdirectory(fmt EXCLUDE_FROM_ALL)

to exclude it from ``make``, ``make all``, or ``cmake --build .``.

You can detect and use an installed version of {fmt} as follows::

   find_package(fmt)
   target_link_libraries(<your-target> fmt::fmt)

Setting up your target to use a header-only version of ``fmt`` is equally easy::

   target_link_libraries(<your-target> PRIVATE fmt::fmt-header-only)

Building the Documentation
==========================

To build the documentation you need the following software installed on your
system:

* `Python <https://www.python.org/>`_ with pip and virtualenv
* `Doxygen <http://www.stack.nl/~dimitri/doxygen/>`_
* `Less <http://lesscss.org/>`_ with ``less-plugin-clean-css``.
  Ubuntu doesn't package the ``clean-css`` plugin so you should use ``npm``
  instead of ``apt`` to install both ``less`` and the plugin::

    sudo npm install -g less less-plugin-clean-css.

First generate makefiles or project files using CMake as described in
the previous section. Then compile the ``doc`` target/project, for example::

  make doc

This will generate the HTML documentation in ``doc/html``.

Conda
=====

fmt can be installed on Linux, macOS and Windows with
`Conda <https://docs.conda.io/en/latest/>`__, using its
`conda-forge <https://conda-forge.org>`__
`package <https://github.com/conda-forge/fmt-feedstock>`__, as follows::

  conda install -c conda-forge fmt

Android NDK
===========

fmt provides `Android.mk file`__ that can be used to build the library
with `Android NDK <https://developer.android.com/tools/sdk/ndk/index.html>`_.
For an example of using fmt with Android NDK, see the
`android-ndk-example <https://github.com/fmtlib/android-ndk-example>`_
repository.

__ https://github.com/fmtlib/fmt/blob/master/Android.mk

Homebrew
========

fmt can be installed on OS X using `Homebrew <http://brew.sh/>`_::

  brew install fmt
