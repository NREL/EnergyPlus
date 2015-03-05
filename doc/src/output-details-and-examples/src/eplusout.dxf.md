# eplusout.dxf

The DXF output report file is formatted according to the "Data Exchange Format" standard rules for representing CADD type coordinates. The file can be used in several inexpensive, shareware or freeware viewers. Quickview Plus ™ can display DXF files as shown in Figure 1 below. A free program originally from Autocad™, Voloview Express™, can display solid model rendering as shown in Figure 2. Other viewers are available from Microstation™, Visio™ and other shareware or freeware vendors.

This file is generated when the following line is included in the IDF.

~~~~~~~~~~~~~~~~~~~~

    Output:Surfaces:Drawing, DXF;
~~~~~~~~~~~~~~~~~~~~

You can ask it to triangulate surfaces with >4 sides:

~~~~~~~~~~~~~~~~~~~~

    Output:Surfaces:Drawing, DXF, Triangulate3dface;
~~~~~~~~~~~~~~~~~~~~

In addition to the building shape (including detached shading elements), the DXF view includes a "true north" arrow (at ground level) and the name from the BUILDING object.

![Quick View Plus version of DXF file](media/quick-view-plus-version-of-dxf-file.png)


Even in the Quick View version, you can see that the different building elements have different colors. These are the "original" colors used in EnergyPlus. The current default color scheme is shown in the following figure of the solid model.

![Voloview 3D Solid view](media/voloview-3d-solid-view.png)


The DXF file of itself is an ASCII file, with a specific structure as specified in the standard. An excerpt of the file is shown below:

~~~~~~~~~~~~~~~~~~~~

    SECTION
      2
    ENTITIES
      0
    TEXT
      8
    1
      6
    CONTINUOUS
     62
      3
     10
          -11.00000
     20
            3.00000
     30
            0.10000
     40
     .25
      1
    True North
     41
     0.0
      7
    MONOTXT
    210
    0.0
    220
    0.0
    230
    1.0
      0
    <reduced for brevity>
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    3DFACE
      8
    1
     62
      3
     10
          -10.00000
     20
            3.00000
     30
            0.10000
     11
          -10.00000
     21
            3.00000
     31
            0.00000
     12
          -10.00000
     22
            0.00000
     32
            0.00000
     13
          -10.00000
     23
            0.00000
     33
            0.10000
      0
    ENDSEC
      0
    EOF
    999
    DXF created from EnergyPlus
    999
    Program Version,EnergyPlus, <version>
~~~~~~~~~~~~~~~~~~~~