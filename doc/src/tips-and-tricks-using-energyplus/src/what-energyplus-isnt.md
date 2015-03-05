# What EnergyPlus Isn't

- a user interface. It is intended to be the simulation engine around which a third-party interface can be wrapped. Inputs and outputs are simple ASCII text that is decipherable but may be best left to a GUI (graphical user interface). The current known third-party interfaces/tools can be found at

~~~~~~~~~~~~~~~~~~~~

    http://apps1.eere.energy.gov/buildings/energyplus/interfaces_tools.cfm
~~~~~~~~~~~~~~~~~~~~

- a life cycle cost analysis tool. It produces results that can then be fed into an LCC program.
- an architect or design engineer replacement. It does not check input, verify the acceptability or range of various parameters (expect for a limited number of very basic checks), or attempt to interpret the results. However, it does have several reporting features to help you do exactly that.