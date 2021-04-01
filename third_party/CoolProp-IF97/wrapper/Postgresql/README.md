Wrapper of IF97 for postgreSQL
================================================

This wrapper will provide Functions in postgreSQL that provide thermodynamic and transport properties for water/steam at specified state points based on the IAPWS Industrial Formulation for the Properties of Water and Steam.

This wrapper been developed and tested on postgreSQL 9 and 10

------

To Use
======

* Build the wrapper as a shared lib

* Install .so file into the postgres external module directory (given by pg_config --pkglibdir)  

* run the .sql file to notify you postgreSQL database to use the shared lib

* use provided function : if97_hmass_Tp() and if97_Tsat97() as SQL functions

------

To Build
========

Follow the build procedures below to create the IF97 module for postgreSQL.

Pre-Requisites
--------------

* You will need to have postgreSQL *server* installed 
* You will need g++

Download the IF97 Repository
----------------------------

* Open a Git window at the drive location where you want to create your local IF97 repository

* Clone the CoolProp/IF97 repository to a local repository (If you haven't already cloned it recursively with CoolProp).::

    git clone https://github.com/CoolProp/IF97

* Change directory (cd) to the IF97 directory you just created.::

    cd IF97

Build and Install
-----------------------------

* Go to the wrapper/Postgresql directory run `build.sh`

* Connect to your postgreSQL server with your favorite client and execute if97.sql commands
