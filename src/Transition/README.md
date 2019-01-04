Transition
-----------

This **Transition** program is written in Fortran and is responsible for handling changes from one IDD version to the next, in order to maintain compatibility as much as possible.

Currently the **Transition** program can take idf from one **major** version (where PATCH=0, eg: '8.x.0') (starting with version 8.2.0, prior to that major versions were 8.x.y)
to the current iteration version represented by `CMAKE_VERSION_MAJOR.CMAKE_VERSION_MINOR.CMAKE_VERSION_PATCH`.
To do this it uses CMake to configure the file `CreateNewIDFUsingRules.in.f90` by injecting the last official major version and the current iteration version.

It is not possible to go from a minor version (iteration) to current at this time.
The capability of the transition program is naturally dependent on the transition rules being maintained throughout the iteration cycle.

Maintenance of the **Transition** program consists of updating the rules during feature development.
Rules should come in with feature branches if transition is required.
It also requires managing some files at major release events.
Here are the steps as they are known at this time.

**Task List for a new major release:**

* Append the last major release to the `VERSIONS` list in `src/Transition/CMakeLists.txt`.
* Copy `CreateNewIDFUsingRules.in.f90` to `CreateNewIDFUsingRulesVX_Y_Z.f90` and remove CMake tokens. You can also go to `build/src/Transition/Transition-VXXXX/` to get the last generated f90 file. You'll need to ensure that in `SetThisVersionVariables` you use only MAJOR and MINOR versions (not PATCH, eg: '9.0', not '9.0.1')
* Clean out `CreateNewIDFUsingRules.in.f90` by removing out the portions where code is actually added by users and that are labeled as such inside the file.
* Create a new ReportVariables...csv files named according to the upcoming major release and previous major release.

**TODO:**

* Not sure about naming conventions: should it include the next official version in the name? Should the 'template' in.f90 file be left alone in the repo in that case?
* Not sure about how to manage the ReportVariables.csv file either

