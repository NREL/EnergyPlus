# ReadTheDocs Stuff

OK, so we are going to start pushing a little of the more dynamic documentation out into ReadTheDocs.
ReadTheDocs works really great when you have a pure Sphinx documentation project.
Sphinx works really great when you have a pure Python project.
Unfortunately we have a bit of a mixed bag of stuff to document, including Python, C, and Freeform documentation.
The Python and "freeform" stuff are set up to be directly written in Sphinx RST and work really well.
The C code is, however, preprocessed with Sphinx and then the preprocessed docs are kept in the repo itself.
At some point it would be really great to get things working all together, but I could not get Breathe working at this time, so it was punted for later.
Even still, the current process is pretty straightforward.

## Building the Doxygen (C) Documentation
The C documentation is built with Doxygen, which is available on Debian with `apt install doxygen`.
While you can technically just run `doxygen` in the `doc/readthedocs/doxygen/` folder, a wrapper script is provided which also updates the _prebuilt_ docs.
From any folder, just execute the wrapper script: `doc/readthedocs/update_c_api_docs.py`, which will execute Doxygen and also copy the output into the sphinx directory structure.
Then you can browse the built docs at: `doc/readthedocs/sphinx/static/c_prebuilt/index.html`.

## Building the Sphinx (Python) Documentation
The Sphinx documentation is essentially the main documentation engine, since it is what gets executed by ReadTheDocs.
Make sure that your currently active Python version has Sphinx installed (`pip install sphinx`).
Move into the Python API doc folder: `cd doc/readthedoc/sphinx/`.
Run the Sphinx built Makefile: `make html`.
Browse the built docs at: `doc/readthedoc/sphinx/_build/html/index.html`

## Adding Documentation
The Python and C API documentation are generated directly from the API sources (.py files and .h files) in the api source directory: `src/EnergyPlus/api`.
- To edit the documentation just edit the code there and rebuild the documentation.
- To edit the C API documentation configuration, just edit the `doc/readthedocs/doxygen/Doxyfile` configuration file and rebuild.
- To edit the Python API documentation configuration, just edit the `doc/readthedocs/sphinx/conf.py` configuration file and rebuild.
- To add more content to the documentation that is hosted on ReadTheDocs, just add more `rst` sources or content in the `doc/readthedocs/sphinx` directory and make sure to update the root `doc/readthedocs/sphinx/index.rst` file, then rebuild.

## Updating ReadTheDocs
ReadTheDocs will build "latest" and "stable" versions of EnergyPlus commits by default.  
[`latest`](https://eplus.readthedocs.io/en/latest/) always points to the latest commit made to the `develop` branch, almost often due to a pull request merge.
[`stable`](https://eplus.readthedocs.io/en/stable/) always points to the last release tag made to EnergyPlus.

In addition, any number of other versions can be added.  This should be done at each major release of EnergyPlus.
To accomplish this, simply go into ReadTheDocs [version configuration](https://readthedocs.org/projects/eplus/versions/) and "Activate a Version".
This will trigger a build of that tag.  Make sure that build is labeled as "active", and not "hidden", because this should be in the main list of official versions.

In the same way, if a branch is specifically modifying the documentation, and it is desired to get ReadTheDocs building commits to that branch, just activate that branch but keep it hidden.
The developer will still be able to see the build results, but they won't be advertised in the main list of official release documentation versions.
