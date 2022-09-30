add_library(fortran_project_options INTERFACE)
set(FORTRAN_STATIC_EXE FALSE)
set(FORTRAN_SKIP_RPATH FALSE)

if(APPLE)
  if(CMAKE_Fortran_COMPILER MATCHES "ifort")
    target_compile_options(fortran_project_options INTERFACE -fpp)
    target_link_options(fortran_project_options INTERFACE -static-intel)
  else()
    if(NOT "Ninja" STREQUAL ${CMAKE_GENERATOR})
      target_compile_options(fortran_project_options INTERFACE -cpp)
    endif()

    # `-static` isn't possible on mac. From gcc man page:
    # > This option will not work on Mac OS X unless all libraries (including libgcc.a) have also been compiled with -static.
    # > Since neither a static version of libSystem.dylib nor crt0.o are provided, this option is not useful to most people.

    # master of gcc has a fix via a new `-static-libquadmath` but it's not even in gcc 12.2.0. So instead we have to do all kinds of shenanigans

    enable_language(Fortran)  # Needed to populate ${CMAKE_Fortran_IMPLICIT_LINK_DIRECTORIES}
    # Debug:
    # set(Fortran_VERBOSE "-v")
    # Find the static libquadmath maually
    find_library(static-libquadmath NAMES quadmath.a libquadmath.a REQUIRED PATHS ${CMAKE_Fortran_IMPLICIT_LINK_DIRECTORIES})
    message(STATUS "Found static-libquadmath at ${static-libquadmath} by searching in CMAKE_Fortran_IMPLICIT_LINK_DIRECTORIES=${CMAKE_Fortran_IMPLICIT_LINK_DIRECTORIES}")
    target_compile_options(fortran_project_options INTERFACE
      ${Fortran_VERBOSE}
    )
    target_link_options(fortran_project_options INTERFACE
      ${Fortran_VERBOSE}
      -nodefaultlibs
      -static-libgfortran
      -lgfortran
      -static-libgcc
      -lgcc
      -lemutls_w
      ${static-libquadmath}
      -lSystem
    )

    message(DEBUG "CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES-${CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES}")
    #    => CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES-gfortran;emutls_w;gcc;quadmath;emutls_w;gcc;gcc
    list(REMOVE_DUPLICATES CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES)
    list(TRANSFORM CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES REPLACE "quadmath" " ${static-libquadmath}")
    message(DEBUG "CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES-${CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES}")

    # We unset this so that CMake doesn't try to read -lquadmath etc
    unset(CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES)

    # We could technically just set it to TRUE here. But it shouldn't do anything anyways, and it's a fallback
    # set(FORTRAN_STATIC_EXE TRUE)
  endif()

elseif(UNIX)
  set(FORTRAN_SKIP_RPATH TRUE)
  if(CMAKE_Fortran_COMPILER MATCHES "ifort")
    target_compile_options(fortran_project_options INTERFACE -fpp)
    target_link_options(fortran_project_options INTERFACE -static-intel)
  else()
    if(NOT "Ninja" STREQUAL ${CMAKE_GENERATOR})
      target_compile_options(fortran_project_options INTERFACE -cpp)
    endif()
    set(FORTRAN_STATIC_EXE TRUE)
    target_link_options(fortran_project_options INTERFACE -static)
  endif()
else() # Windows
  set(FORTRAN_STATIC_EXE TRUE)
  if(CMAKE_Fortran_COMPILER MATCHES "ifort")
    # Set release flags to be empty
    set(CMAKE_Fortran_FLAGS_RELEASE "")
    target_compile_options(fortran_project_options INTERFACE /fpp)
    target_link_options(fortran_project_options INTERFACE /libs:static)
  else()
    if(NOT "Ninja" STREQUAL ${CMAKE_GENERATOR})
      target_compile_options(fortran_project_options INTERFACE -cpp)
    endif()
    target_link_options(fortran_project_options INTERFACE -static)
  endif()
endif()
target_compile_options(fortran_project_options INTERFACE -ffree-line-length-275)
