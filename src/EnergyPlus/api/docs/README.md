# API Documentation

The C API is documented using Doxygen and the Python API is documented using Sphinx.
At some point we might combine them, but not for now.
Right now I am planning on hosting the docs on our gh-pages site, that may also change later.

## Building the C Documentation
Make sure your system has Doxygen, which on Debian based Linux is just: `apt-get install doxygen`.
Move into the C API doc folder: `cd eplusrepo/src/EnergyPlus/api/docs/c`.
Run Doxygen: `doxygen`.
Browse the build docs at: `.../api/docs/c/_build/html/index.html`

## Building the Python Documentation
Make sure that your currently active Python version has Sphinx installed (pip install sphinx).
Move into the Python API doc folder: `cd eplusrepo/src/EnergyPlus/api/docs/python`.
Run the Sphinx built Makefile: `make html`.
Browse the built docs at: `.../api/docs/python/_build/html/index.html`

## Updating gh-pages
The Github Pages site is simply based on the contents in the gh-pages branch of the repo.
Nothing special is required to update this except to push changes to the gh-pages branch.
We don't currently use anything dynamic on the site, so it is just plain html.

The structure of the site is currently:

```
*root*
  index.html -- links to the API page only, for now
  api/
    index.html -- links to each API page
    c/
      index.html -- root of the Doxygen generated C API doc
    python/
      index.html -- root of the Sphinx generated Python API doc
```

To update, simply rebuild the docs according to the instructions above, and then replace the existing `api/c` and `api/python` directories with the correct version of the `_build/html` folders from above.
Verify the structure looks right with the index.html files in the right locations, commit and push the branch.

## Versioning Notes
The API "version" number is declared in multiple places - the src/EnergyPlus/CMakeLists.txt as well as the documentation configs.
Eventually it would be good to CMake-configure the API doc build files, but for now they just need to be synced up.

