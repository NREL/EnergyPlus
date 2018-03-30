## Build

### Linux 64 bit Ubuntu version 16.04

Install cmake 3.8.x from https://cmake.org/download/


Earlier versions of CMake may also work if the 'JAVA_HOME' environment variable
is set:
```
  $ export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64
```

This list could be incomplete

```
  sudo apt-get install build-essential 
  sudo apt-get install gfortran
  sudo apt-get install mercurial
  sudo apt-get install git
  sudo apt-get install python
  sudo apt-get install python-dev 
  sudo apt-get install python-numpy
  sudo apt-get install cython
  sudo apt-get install openjdk-8-jdk
  sudo apt-get install ant
  sudo apt-get install zlib1g-dev
  
  mkdir build-third-party
  cd build-third-party
  cmake ../alpine/third-party
  make -j8
  cd ../
  mkdir build
  cd build
  cmake ../
  make -j8
```

### MacOS

Download and install XCode from the App store
Install Homebrew

```
xcode-select --install
brew install cmake
brew install hg
brew install gcc
```

Parts of JModelica may only compile on Mac with Python 3. Brew can install python 3, 
but it wont be called python it will be called python3, so do this.
```
brew install python3
ln -s /usr/local/bin/python3 /usr/local/bin/python
```
Start a new terminal session to get python3 in the path as python
pip3 install numpy
pip3 install cython
brew install hg
```

Maybe other brew install commands...

```
mkdir build-third-party
cd build-third-party
cmake ../alpine/third-party
make -j8
cd ../
mkdir build
cd build
cmake ../
make -j8 alpinedemo
```

### Running

Not update to date with the vdp model, so this example is temporarily broken.

```
cd <project-root>
./build/alpine/alpinedemo alpine/vdp.mo
```





