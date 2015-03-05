# Modeling Reflective Radiant Barriers

*Can EnergyPlus model reflective radiant barriers?*

1. For radiant barriers which are exposed to a thermal zone, such as an attic space, specify a reduced thermal absorptance for the innermost material layer.

For example, an attic roof construction might be (outer to inner)

~~~~~~~~~~~~~~~~~~~~

    Asphalt shingles,R-30 insulation,Radiant barrier;
~~~~~~~~~~~~~~~~~~~~

The radiant barrier material would be a thin layer with some small resistance with a low thermal absorptance value. This will reduce the radiant heat transfer from the roof surface to other surfaces in the attic zone.

2. If the radiant barrier is within a cavity which is not modeled as a separate thermal zone, then there is not an easy way to model its impact. For example, a wall construction:

~~~~~~~~~~~~~~~~~~~~

    Brick,
    R-12 insulation,
    Radiant barrier,
    Air gap,
    Gypsum board;
~~~~~~~~~~~~~~~~~~~~

Here, the radiant barrier would reduce the radiant transfer across the air gap. But EnergyPlus air gaps are a fixed thermal resistance, specified in the Material:Airgap object. The user would need to compute an average effective resistance which represents the reduced radiant heat transfer across the air gap due to the radiant barrier. This resistance could then be assigned to the radiant barrier material layer.