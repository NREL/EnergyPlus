cmake_minimum_required (VERSION 2.8)

project(WillisThirdParty)
include(ExternalProject)

ExternalProject_Add(llvm
  GIT_REPOSITORY "https://github.com/llvm-project/llvm-project-20170507.git"
  GIT_TAG release_40
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -G${CMAKE_GENERATOR} 
    -DCMAKE_BUILD_TYPE:STRING=release 
    -DLLVM_ENABLE_PROJECTS:STRING=clang 
    -DLLVM_TOOL_CLANG_BUILD:BOOL=ON 
    -DCMAKE_INSTALL_PREFIX:PATH=${CMAKE_CURRENT_BINARY_DIR}/llvm-prefix/src/llvm-install 
    ${CMAKE_CURRENT_BINARY_DIR}/llvm-prefix/src/llvm/llvm
)

ExternalProject_Add(openjdk-src
  HG_REPOSITORY "http://hg.openjdk.java.net/jdk7u/jdk7u/jdk"
  HG_TAG jdk7u80-b32
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ""
)

if( APPLE )
  set(openjdk_url http://openstudio-resources.s3.amazonaws.com/openjdk-1.7.0-u80-unofficial-macosx-x86_64-image.zip)
  set(openjdk_md5 542e8f7d2c3f10aaa6b7b2f97732fb08)
elseif( UNIX )
  set(openjdk_url http://openstudio-resources.s3.amazonaws.com/openjdk-1.7.0-u80-unofficial-linux-amd64-image.zip)
  set(openjdk_md5 c8f7f9762788be42f9855d3d702b302e)
endif()

ExternalProject_Add(openjdk-build
  URL "${openjdk_url}"
  URL_MD5 "${openjdk_md5}"
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ""
)

ExternalProject_Add(avian
  GIT_REPOSITORY "https://github.com/ReadyTalk/avian.git"
  GIT_TAG 6f6bdd7d5c171e0c4c807d0a4f3aa676461307de
  DEPENDS openjdk-build openjdk-src
  CONFIGURE_COMMAND ""
  BUILD_COMMAND make JAVA_HOME=${CMAKE_CURRENT_BINARY_DIR}/openjdk-build-prefix/src/openjdk-build openjdk=${CMAKE_CURRENT_BINARY_DIR}/openjdk-build-prefix/src/openjdk-build openjdk-src=${CMAKE_CURRENT_BINARY_DIR}/openjdk-src-prefix/src/openjdk-src/src
  INSTALL_COMMAND ""
  BUILD_IN_SOURCE 1
)

ExternalProject_Add(openmp
  GIT_REPOSITORY https://github.com/llvm-mirror/openmp.git
  GIT_TAG e4c0f2b8d38151e64235df419e68a6bcb0799e53
  CMAKE_CACHE_ARGS 
    -DCMAKE_INSTALL_PREFIX:PATH=${CMAKE_CURRENT_BINARY_DIR}/openmp-prefix/src/openmp-install
)

if( APPLE )
ExternalProject_Add(jmodelica
  DEPENDS openmp
  URL http://openstudio-resources.s3.amazonaws.com/JModelica.tar.gz
  URL_MD5 d375a0143358a3613b7bb05956fbc447
  CONFIGURE_COMMAND CFLAGS=-I${CMAKE_CURRENT_BINARY_DIR}/openmp-prefix/src/openmp-install/include/ ${CMAKE_CURRENT_BINARY_DIR}/jmodelica-prefix/src/jmodelica/configure --without-sundials --without-casadi --without-ipopt --prefix=${CMAKE_CURRENT_BINARY_DIR}/jmodelica-prefix/src/jmodelica-install/
  BUILD_COMMAND CFLAGS=-I${CMAKE_CURRENT_BINARY_DIR}/openmp-prefix/src/openmp-install/include/ ${jmodelica_cflags} make 
  INSTALL_COMMAND make install
)
else()
ExternalProject_Add(jmodelica
  DEPENDS openmp
  URL http://openstudio-resources.s3.amazonaws.com/JModelica.tar.gz
  URL_MD5 d375a0143358a3613b7bb05956fbc447
  CONFIGURE_COMMAND  ${CMAKE_CURRENT_BINARY_DIR}/jmodelica-prefix/src/jmodelica/configure CMAKE_C_FLAGS=-stdc11 CFLAGS=-I${CMAKE_CURRENT_BINARY_DIR}/openmp-prefix/src/openmp-install/include/\ -std=c11 --without-sundials --without-casadi --without-ipopt --prefix=${CMAKE_CURRENT_BINARY_DIR}/jmodelica-prefix/src/jmodelica-install/
  BUILD_COMMAND  make CMAKE_C_FLAGS=-stdc11 CFLAGS=-I${CMAKE_CURRENT_BINARY_DIR}/openmp-prefix/src/openmp-install/include/
  INSTALL_COMMAND make CMAKE_C_FLAGS=-stdc11 install
)
endif()
