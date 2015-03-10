# Input Concepts

In EnergyPlus, input and output are accomplished by means of ASCII (text) files. On the input side, there are two files:

#. the Input Data Dictionary (IDD) that describes the types (classes) of input objects and the data associated with each object;
#. the Input Data File (IDF) that contains all the data for a particular simulation.

Each EnergyPlus module is responsible for getting its own input. Of course, EnergyPlus provides services to the module that make this quite easy. The first task of a module developer is to design and insert a new entry into the Input Data Dictionary.