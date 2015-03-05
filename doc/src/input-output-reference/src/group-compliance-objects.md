# Group – Compliance Objects

Not necessarily related directly to simulation, the Compliance Object group provides a repository for a group of objects that can be used directly or indirectly in reporting compliance (such as ASHRAE 90.1) to building standards.

## Compliance:Building

The [Compliance:Building](#compliancebuilding) object describes parameters related to compliance to building standards, building codes, and beyond energy code programs.

### Inputs

#### Field: Building Rotation for Appendix G

[Building](#building) Rotation for Appendix G allows for the building model to be rotated for use with compliance such as ASHRAE 90.1 Appendix G. Appendix G requires the building to be rotated 0, 90, 180 and 270 degrees and the values averaged to establish baseline energy use. This input works with relative or world coordinate systems.

An example from an IDF:

~~~~~~~~~~~~~~~~~~~~

    Compliance:Building,
        90;                 Building Rotation for Appendix G
~~~~~~~~~~~~~~~~~~~~