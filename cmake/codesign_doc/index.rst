Welcome to CMake Custom Modules documentation
======================================================

.. toctree::
   :maxdepth: 2
   :caption: Contents:

.. cmake-manual-description: CMake Modules Reference

CMake Modules
*************

Below you find a list of CMake Modules included in this project.

Codesigning Modules
^^^^^^^^^^^^^^^^^^^

Influencial configuration variables

.. cmake:variable:: CPACK_CODESIGNING_DEVELOPPER_ID_APPLICATION

  This is something like ``Developer ID Application: <The Name> (<TEAMID>)``.
  Refer to :cmake:command:`setup_macos_codesigning_variables` which pre-populates it with options from ``security-find-identity -v -p codesign``

.. cmake:variable:: CPACK_CODESIGNING_NOTARY_PROFILE_NAME

  Should be set to the name you used during ``xcrun notarytool store-credentials``

Here is the list of Utility Modules

.. cmake-module:: ../CodeSigning.cmake

.. cmake-module:: ../CPackSignAndNotarizeDmg.cmake
