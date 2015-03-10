# Group – Solar Collectors

Solar collectors are thermal devices that convert solar energy into thermal energy by raising the temperature of a circulating heat transfer fluid. The fluid can then be used to heat water for domestic hot water usage or space heating.

In EnergyPlus solar collectors are components that are connected to the plant loop. A solar heating system can be constructed with a combination of solar collectors, pumps, and hot water tanks.

Flate plate solar collectors are defined using two objects:  [SolarCollector:FlatPlate:Water](#solarcollectorflatplatewater) and [SolarCollectorPerformance:FlatPlate](#solarcollectorperformanceflatplate). Similarly, Integral-Collector-Storage (ICS) solar collectors are defined using two objects: [SolarCollector:IntegralCollectorStorage](#solarcollectorintegralcollectorstorage), and [SolarCollectorPerformance:IntegralCollectorStorage](#solarcollectorperformanceintegralcollectorstorage).  The [SolarCollector:FlatPlate:Water](#solarcollectorflatplatewater) and [SolarCollector:IntegralCollectorStorage](#solarcollectorintegralcollectorstorage) objects describe the plant component connections. These object also reference  [SolarCollectorPerformance:FlatPlate](#solarcollectorperformanceflatplate) and [SolarCollectorPerformance:IntegralCollectorStorage](#solarcollectorperformanceintegralcollectorstorage) performance objects which contains the thermal and optical performance test data for a specific make and model of collector. Parameters are defined separately so that these values can be organized into a reference data set and need only be entered once if for an array of the same type of collectors.

## SolarCollector:FlatPlate:Water

The flat-plate solar collector model simulates glazed, unglazed, and tubular (i.e. evacuated tube) collectors. The [SolarCollector:FlatPlate:Water](#solarcollectorflatplatewater) object represents a single collector module connected to the plant loop. The thermal and optical properties of the collector module are taken from the referenced [SolarCollectorPerformance:FlatPlate](#solarcollectorperformanceflatplate) object. A surface or shading object defines the collector tilt, azimuth, and gross area. The collector surface participates normally in all shading calculations if the "FullExterior," "FullInteriorAndExterior," "FullExteriorWithReflections ", or "FullInteriorAndExteriorWithReflections" flags are set in the Solar Distribution field of the [Building](#building) object. Inlet and outlet nodes are specified for plant connections on the demand side of the plant loop.

### Inputs

#### Field: Name

The unique name of the [SolarCollector:FlatPlate:Water](#solarcollectorflatplatewater) object.

#### Field: Solar Collector Performance Name

Reference name of a [SolarCollectorPerformance:FlatPlate](#solarcollectorperformanceflatplate) object that defines the thermal and optical properties of the collector.

#### Field: Surface Name

Reference to one of the many different types of surfaces such as the [BuildingSurface:Detailed](#buildingsurfacedetailed) or the [Shading:Zone:Detailed](#shadingzonedetailed) objects. The surface named here is used to define the solar collector tilt, azimuth, and gross area.

#### Field: Inlet Node Name

The name of the inlet node connection to the plant loop.

#### Field: Outlet Node Name

The name of the outlet node connection to the plant loop.

#### Field: Maximum Flow Rate

The maximum flow rate [m^3^/s] allowed through the collector. This field is optional. If not specified, the collector will allow as much flow as the rest of the plant can deliver.

An example follows.

~~~~~~~~~~~~~~~~~~~~

     SolarCollector:FlatPlate:Water,
      Collector 1,                            !- Name
      ACR Solar International Fireball 2001,  !- Solar Collector Performance Name
      Collector Surface,                      !- Surface Name
      Collector Inlet Node,                   !- Inlet Node Name
      Collector Outlet Node,                  !- Outlet Node Name
      0.00005;                                !- Maximum Flow Rate (m3/s)
~~~~~~~~~~~~~~~~~~~~

### Outputs

The following output variables are reported for the [SolarCollector:FlatPlate:Water](#solarcollectorflatplatewater) object:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Solar Collector Incident Angle Modifier []
    HVAC,Average,Solar Collector Efficiency []
    HVAC,Average,Solar Collector Heat Transfer Rate [W]
    HVAC,Average,Solar Collector Heat Gain Rate [W]
    HVAC,Average,Solar Collector Heat Loss Rate [W]
    HVAC,Sum,Solar Collector Heat Transfer Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Solar Collector Incident Angle Modifier [ ]

The incident angle modifier is an important intermediate value used in the SRCC calculation of solar collector performance.  The value reported here is the combined result for the current time that includes incident angles of beam solar, diffuse solar from sky, and diffuse solar from ground.

#### Solar Collector Efficiency [ ]

The overall collector efficiency.  This is the ratio of collected energy and the incident solar energy.  The efficiency can be greater than 1 at times when the outdoor air temperature is warm enough.

#### Solar Collector Heat Transfer Rate [W]

#### Solar Collector Heat Transfer Energy [J]

These are the overall rate (in W) and amount of energy ( in J) transferred to the collector's circulating fluid.  Positive values indicate heating of the fluid while negative values indicate cooling of the fluid.

#### Solar Collector Heat Gain Rate [W]

This is the overall rate of heat addition to the collector's circulating fluid in Watts.  Values are always positive or zero.  If the fluid is actually cooled then the value is zero.

#### Solar Collector Heat Loss Rate [W]

This is the overall rate of heat loss from the collector's circulating fluid in Watts.  Values are always positive or zero.  If the fluid is actually heated then the value is zero.

In addition, several surface variables are also relevant for the collector's surface object ([BuildingSurface:Detailed](#buildingsurfacedetailed) or Shading:Zone:Detailed):

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,Surface Outside Face Sunlit Area [m2]
    Zone,Average,Surface Outside Face Sunlit Fraction []
    Zone,Average,Surface Outside Face Incident Solar Radiation Rate per Area [W/m2]
    Zone,Average,Surface Outside Face Incident Beam Solar Radiation Rate per Area [W/m2]
    Zone,Average,Surface Outside Face Incident Sky Diffuse Solar Radiation Rate per Area [W/m2]
    Zone,Average,Surface Outside Face Incident Ground Diffuse Solar Radiation Rate per Area [W/m2]
    Zone,Average,Surface Outside Face Beam Solar Incident Angle Cosine Value []
~~~~~~~~~~~~~~~~~~~~

The temperatures at the inlet and outlet nodes and the collector mass flow rate can be monitored using the system node output variables:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,System Node Temperature [C]
    HVAC,Average,System Node Mass Flow Rate [kg/s]
~~~~~~~~~~~~~~~~~~~~

## SolarCollectorPerformance:FlatPlate

The [SolarCollectorPerformance:FlatPlate](#solarcollectorperformanceflatplate) object contains the thermal and optical performance parameters for a single collector module. These parameters are based on the testing methodologies described in ASHRAE Standards 93 and 96. The Solar Rating and Certification Corporation (SRCC) applies these standards in their rating procedures of solar collectors. The ratings for commercially available collectors in North America are published in the *Directory of SRCC Certified Solar Collector Ratings*. The SRCC database has also been converted into an EnergyPlus data set of [SolarCollectorPerformance:FlatPlate](#solarcollectorperformanceflatplate) objects that is included with the program (see SolarCollectors.idf in the DataSets folder).

The coefficients for the energy conversion efficiency and incident angle modifier allow first order (linear) or second order (quadratic) correlations. To use a first order correlation, the second order coefficient must be left blank or set to zero.

In order for the model to work correctly, the test conditions for which the performance coefficients were measured must be specified in the fields:  *Test Fluid*, *Test Volumetric Flow Rate*, and *Test Correlation Type*. Currently, only water is allowed as the *Test Fluid*.

For more detailed information about the performance coefficients, see the *EnergyPlus Engineering Reference Document*.

### Inputs

#### Field: Name

The unique name of the [SolarCollectorPerformance:FlatPlate](#solarcollectorperformanceflatplate) object.

#### Field: Gross Area

The gross area of the collector module [m^2^]. This value is mainly for reference. The area of the associated collector surface object is used in all calculations.

#### Field: Test Fluid

The fluid that was used in the testing procedure that resulted in the thermal and optical performance coefficients below. Currently only "Water" is allowed.  This the fluid during the collector testing, not the fluid used during a particular EnergyPlus run.

#### Field: Test Flow Rate

The volumetric flow rate during testing [m^3^/s]. If the value is available as flow rate per unit area, it is recommended to multiply by the *Gross Area* of the collector module, not the net aperture area.

#### Field: Test Correlation Type

This field specifies type of temperature used to develop the correlation equations.  The testing procedure is based on an experimental correlation using either "Inlet," "Average," or "Outlet" temperature.  Enter one of these choices. The ASHRAE Standards 93 and 96 always use Inlet temperature.

#### Field: Coefficient 1 of Efficiency Equation

First coefficient of efficiency equation for energy conversion [dimensionless].  This is the Y-intercept term.

#### Field: Coefficient 2 of Efficiency Equation

Second coefficient of efficiency equation for energy conversion [W/m^2^-K].  This is the first-order term.

#### Field: Coefficient 3 of Efficiency Equation

Third coefficient of efficiency equation for energy conversion [W/m^2^-K^2^]. This field is optional. This is the second-order term. If left blank or set to zero, a first-order linear correlation is used.

#### Field: Coefficient 2 of Incident Angle Modifier

Second coefficient of the incident angle modifier equation. This the first-order term. (There is no *Coefficient 1 of Incident Angle Modifier* because that number is always 1.0.)

#### Field: Coefficient 3 of Incident Angle Modifier

Third coefficient of the incident angle modifier equation. This is the second-order term. This field is optional. If left blank or set to zero, a first order linear correlation is used.

An example of this object follows.

~~~~~~~~~~~~~~~~~~~~

      SolarCollectorPerformance:FlatPlate,
        Alternate Energy Technologies AE-32,  !- Name
        2.9646,                  !- Gross Area {m2}
        WATER,                   !- Test Fluid
        0.0000388,               !- Test Flow Rate {m3/s}
        INLET,                   !- Test Correlation Type
        0.691,                   !- Coefficient 1 of Efficiency Equation {dimensionless}
        -3.396,                  !- Coefficient 2 of Efficiency Equation {W/m2-K}
        -0.00193,                !- Coefficient 3 of Efficiency Equation {W/m2-K2}
        -0.1939,                 !- Coefficient 2 of Incident Angle Modifier
        -0.0055;                 !- Coefficient 3 of Incident Angle Modifier
~~~~~~~~~~~~~~~~~~~~

### Outputs

This object does not generate any output; see [SolarCollector:FlatPlate:Water](#solarcollectorflatplatewater) Output

## SolarCollector:IntegralCollectorStorage

The Integral-Collector-Storage (ICS) solar collector model simulates glazed collectors with integral storage unit. The [SolarCollector:IntegralCollectorStorage](#solarcollectorintegralcollectorstorage) object represents a single collector module connected to the plant loop. The thermal and optical properties of the collector module are calculated from inputs in [SolarCollectorPerformance:IntegralCollectorStorage](#solarcollectorperformanceintegralcollectorstorage) object. A surface or shading object defines the collector tilt, and azimuth. The collector surface participates normally in all shading calculations if the "FullExterior," "FullInteriorAndExterior," "FullExteriorWithReflections ", or "FullInteriorAndExteriorWithReflections" flags are set in the Solar Distribution field of the [Building](#building) object. Inlet and outlet nodes are specified for plant connections on the demand side of the plant loop.  The [SurfaceProperty:ExteriorNaturalVentedCavity](#surfacepropertyexteriornaturalventedcavity), object is required to describe the surface properties, the characteristics of the cavity and opening for natural ventilation if OtherSideConditionsModel is specified as the collector bottom surface outside boundary condition type.

### Inputs

#### Field: Name

The unique name of the [SolarCollector:IntegralCollectorStorage](#solarcollectorintegralcollectorstorage) object.

#### Field: Solar Collector Performance Name

Reference name of a [SolarCollectorPerformance:IntegralCollectorStorage](#solarcollectorperformanceintegralcollectorstorage) object that defines the thermal and optical properties of the collector.

#### Field: Surface Name

Reference to one of the many different types of surfaces such as the [BuildingSurface:Detailed](#buildingsurfacedetailed) or the [Shading:Zone:Detailed](#shadingzonedetailed) objects. The surface named here is used to define the solar collector tilt, and azimuth. The collector shades the surface it is mounted on and hence impacts the surface heat balance.

#### Field: Bottom surface Boundary Conditions Type

This field contains the type of boundary conditions applicable to the ICS collector bottom surface. Allowed boundary condition types are: AmbientAir and OtherSideConditionsModel.  If the other side conditions model is selected, specify the name of the [SurfaceProperty:OtherSideConditionsModel](#surfacepropertyothersideconditionsmodel) object in the next input field, otherwise, leave the next input field blank.  The AmbientAir boundary condition uses outdoor air temperature as boundary condition, hence the subsurface is assumed to be exposed to the sun and wind.

#### Field: Other Side Conditions Model Name

This field contains the name of a [SurfaceProperty:OtherSideConditionsModel](#surfacepropertyothersideconditionsmodel) object declared elsewhere in the input file. This will connect the collector to the exterior boundary conditions for the underlying heat transfer surface specified above..

#### Field: Inlet Node Name

The name of the inlet node connection to the plant loop.

#### Field: Outlet Node Name

The name of the outlet node connection to the plant loop.

#### Field: Maximum Flow Rate

The maximum flow rate [m3/s] allowed through the collector. This field is optional. If not specified, the collector will allow as much flow as the rest of the plant can deliver.

An example follows.

~~~~~~~~~~~~~~~~~~~~

    SolarCollector:IntegralCollectorStorage,
      Collector 1,                            !- Name
      ICS Solar Collector,                    !- Solar Collector Performance Name
      ICS Collector Surface,                  !- Surface Name
      OtherSideConditionsModel,               !- Bottom Surface Boundary Conditions Type
      ICS OSCM,                               !- Boundary Condition Model Name
      Collector Inlet Node,                   !- Inlet Node Name
      Collector Outlet Node,                  !- Outlet Node Name
      0.00005;                                !- Maximum Flow Rate (m3/s)
~~~~~~~~~~~~~~~~~~~~

## SolarCollectorPerformance:IntegralCollectorStorage

The [SolarCollectorPerformance:IntegralCollectorStorage](#solarcollectorperformanceintegralcollectorstorage) object contains the thermal and optical performance parameters for a single collector module. The transmittance-absorptance product of the absorber and cover system is determined from optical properties specified. For more detailed information about the calculation procedure, see the *EnergyPlus Engineering Reference* Document.

### Inputs

#### Field: Name

The unique name of the [SolarCollectorPerformance:IntegralCollectorStorage](#solarcollectorperformanceintegralcollectorstorage) object.

#### Field: ICS Collector Type

**This input field is the ICS collector type.   Currently only RectangularTank type is allowed.**

#### Field: Gross Area

This input field is the gross area of the collector module in m2.  This gross area is used in the energy balance equations.

#### Field: Collector Water Volume

This input field is the volume of water in the solar collector in m3.

#### Field: Bottom Heat Loss Conductance

This input field is the collector bottom heat loss conductance in W/m2K.  This value is calculated from thermal conductivity and thickness of the bottom insulation.

#### Field: Side Heat Loss Conductance

This input field is the collector side heat loss conductance in W/m2K.  This value is calculated from thermal conductivity and thickness of the side insulation.

#### Field: Collector Aspect Ratio

This input field is the ratio of the short side (width) of the collector to the long side (length) of the collector.  This value is used only for calculating the collector side area along with the collector side height specified in the next input filed.  This ratio is less or equal to 1.0.

#### Field: Collector Side Height

This input field is height of collector side in m.  This height is used to estimate the collector side area for heat loss calculations along with heat loss coefficient specified in the input field above.

#### Field: Thermal Mass of Absorber Plate

This input field is thermal-mass of the absorber plate per unit area of the collector in [J/m2K].  This input value multiplied by the absorber gross area determines the thermal mass of the absorber plate. It is estimated from the specific heat, density and average thickness of the absorber plate. If zero is specified then the absorber plate energy balance reduces to steady state form.

#### Field: Number of Covers

Number of transparent collector covers.  Common practice is to use two covers: glass as the outer cover and Teflon as the inner cover. If single cover is specified leave the inner cover optical and thermal properties input fields blank.

#### Field: Cover Spacing

This input field provides the spacing between the two transparent covers, and the spacing between the inner cover and the absorber plate in m.  Default value is 0.05m.

#### Field: Refractive Index of Outer Cover

This is the average Refractive index for solar spectrum range of the outer transparent cover material.  Glass is used as the outer cover.  Average refractive index value for non-absorbing glass used in solar collectors over solar spectrum range is 1.526.

#### Field: Extinction Coefficient Times Thickness of Outer Cover

This input field is the product of the extinction coefficient and the thickness of the out cover material.  The extinction coefficient for glass types approximately varies from 4m^-1^ to 32 m^-1^.  The extinction coefficient for low-iron glass, which is the default outer cover material, is 15 m^-1^. The default value for extinction coefficient times thickness (KL) is 0.045 (=15.0 x0.003), which is the product of the default extinction coefficient of 15m^-1^ and 3.0mm thick glass.

#### Field: Emissivity of Outer Cover

This input field value is thermal emissivity of the outer collector cover.  The default value assumes low-iron glass with thermal emissivity of 0.88.

#### Field: Refractive Index of Inner Cover

This input field is the average Refractive index of the inner transparent cover of the collector.  Commonly Teflon (PolytetraFluoroethylene) is used as the inner cover.  The average refractive index value over the solar spectrum range for Teflon is 1.37.

#### Field: Extinction Coefficient Times Thickness of Inner Cover

This input field is the product of the extinction coefficient (K) and the thickness (L) of the inner cover material.  The inner cover material is more transparent than the out cover, very thin and hence their thickness can be assumed to be negligible. The default value for extinction coefficient times thickness (KL) is 0.008 (=40.0x0.0002), which is the product of extinction coefficient of 40m^-1^ and a thickness of 0.2mm.

#### Field: Emissivity of Inner Cover

This input field value is thermal emissivity of the inner transparent collector cover.  The default value assumes plastic sheet with thermal emissivity of 0.30.  This value is used in the thermal analysis only.

#### Field: Absorptance of Absorber Plate

This input field is shortwave or solar absorptance of the absorber plate.  The default value is 0.96.

#### Field: Emissivity of Absorber Plate

This input field value is thermal emissivity of the absorber plate.  Default value is 0.30.  This input value is used in the thermal analysis only.

An example follows.

~~~~~~~~~~~~~~~~~~~~

       SolarCollectorPerformance:IntegralCollectorStorage,
        ICS Experimental,        !- Name
        RectangularTank,         !- ICS Collector Type
        0.37275,                 !- Gross Area {m2}
        0.0195875,               !- Collector Water Volume {m3}
        0.10,                    !- Bottom Heat Loss Conductance
        1.00,                    !- Side Heat Loss Conductance
        0.8,                     !- Collector Aspect Ratio {dimensionless}
        0.08,                    !- Collector Side Height {m}
        5800.0,                     !- Thermal Mass of Absorber Plate {J/m2K}
        1,                       !- Number of Covers {dimensionless}
        0.05,                    !- Cover Spacing {m}
        1.526,                   !- Refractive Index of Outer Cover {dimensionless}
        0.0125,      !- Extinction Coefficient Times Thickness of Outer Cover {dimensionless}
        0.88,                    !- Emmissivity of Outer Cover
        1.126,                   !- Refractive Index of Inner Cover {dimensionless}
        0.0126,      !- Extinction Coefficient Times Thickness of Inner Cover {dimensionless}
        0.88,                    !- Emmissivity of Inner Cover {dimensionless}
        0.96,                    !- Absorptance of Absorber Plate {dimensionless}
        0.60;                    !- Emmissivity of Absorber Plate {dimensionless}
~~~~~~~~~~~~~~~~~~~~

### Outputs

The following output variables are reported for the [SolarCollector:IntegralCollectorStorage](#solarcollectorintegralcollectorstorage) object:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Solar Collector Storage Water Temperature [C]
    HVAC,Average,Solar Collector Absorber Plate Temperature [C]
    HVAC,Average,Solar Collector Overall Top Heat Loss Coefficient [W/m2-C]
    HVAC,Average,Solar Collector Thermal Efficiency []
    HVAC,Average,Solar Collector Storage Heat Transfer Rate [W]
    HVAC,Sum,Solar Collector Storage Heat Transfer Energy [J]
    HVAC,Average,Solar Collector Heat Transfer Rate [W]
    HVAC,Sum,Solar Collector Heat Transfer Energy [J]
    HVAC,Average,Solar Collector Skin Heat Transfer Rate [W]
    HVAC,Sum, Solar Collector Skin Heat Transfer Energy [J]
    HVAC,Average,Solar Collector Transmittance Absorptance Product []
~~~~~~~~~~~~~~~~~~~~

#### Solar Collector Storage Water Temperature [C]

This output variable is the ICS collector stored water average temperature at a given time steps in degree Celsius. This temperature is the same as the collector ICS collector leaving water temperature.

#### Solar Collector Absorber Plate Temperature [C]

This output variable is the ICS collector absorber plate average temperature at a given time steps in degree Celsius.

#### Solar Collector Thermal Efficiency [ ]

This output variable is the instantaneous thermal efficiency of the ICS solar collector in per cent.  This value is determined from net useful energy collected and the total incident solar radiation for each time step. The net useful energy collected is the sum of the energy stored in the collector and net useful energy delivered.

#### Solar Collector Storage Heat Transfer Rate [W] 

#### Solar Collector Storage Heat Transfer Energy [J] 

These output variables are the instantaneous rate of change of the energy and the change in energy of the water in the ICS solar collector in Watts, and Joules, respectively.

#### Solar Collector Skin Heat Transfer Rate [W]

#### Solar Collector Skin Heat Transfer Energy [J]

These output variables are the instantaneous skin heat loss rate and the heat loss energy of the ICS solar collector for each time steps in Watts, and Joules respectively.  The skin heat loss rate is the sum of the heat losses through the top, bottom and sides of the collector surfaces.  This value is mostly negative, but can have a positive value (heat gain) when the outdoor air temperature is warmer than the collector.

#### Solar Collector Heat Transfer Rate [W]

#### Solar Collector Heat Transfer Energy [J]

This output variable is the heat rate and Energy transferred from the ICS collector to the collector loop fluid (water) in Watts and Joule, respectively.  This value is determined from the collector water mass flow rate, specific heat of water and the temperature difference between the collector water outlet and inlet nodes at each time step. The value is positive when the fluid is heated or negative when cooled.

#### Solar Collector Transmittance Absorptance Product [ ]

This output variable is the transmittance-absorptance product of the covers and absorber system of the ICS solar collector.  This value ranges from 0.0 to less than 1.0.

#### Solar Collector Overall Top Heat Loss Coefficient [W/m2-C]

This output variable is the overall heat loss coefficient from the absorber plate to the ambient air calculated for each time step.

## SolarCollector:FlatPlate:PhotovoltaicThermal

This object is used to model hybrid photovoltaic-thermal (PVT) solar collectors that convert incident solar energy into both electricity and useful thermal energy.  This object describes the PVT solar collector by referencing other objects that provide more detail or connections to other parts of the EnergyPlus model.

The PVT solar collectors need to be connected to either an HVAC air system or a plant loop for collected thermal energy to be utilized.  The input field for the type of thermal working fluid informs the program how the PVT collector is expected to be connected.  If the the working fluid is air, then the PVT collectors are modeled as a ventilation air pretreatment component and connected to an outdoor air system.  If the working fluid is water, then the PVT collectors are modeled as a hot water solar collector and are connected to a plant loop with a water thermal storage tank.

### Inputs

#### Field: Name

This field should contain a unique name chosen by the user to identify a specific PVT collector in the building model.

#### Field: Surface Name

This field is the user-defined name of a surface object (defined elsewhere) to which the PVT module is attached.  These can be any type of building surface that is exposed to the exterior environment.  The model uses the named surface's geometry for the PVT solar collector.

#### Field: Photovoltaic-Thermal Model Performance Name

This field is the user-defined name of an object (defined elsewhere) that provides the performance details of the PVT module.  This should be the name of a [SolarCollectorPerformance:PhotovoltaicThermal:Simple](#solarcollectorperformancephotovoltaicthermalsimple) object.  Multiple different [SolarCollector:FlatPlate:PhotovoltaicThermal](#solarcollectorflatplatephotovoltaicthermal) objects can reference the same object that provides performance details.

#### Field: Photovoltaic Generator Name

This field is the user-defined name of a [Generator:Photovoltaic](#generatorphotovoltaic) object (defined elsewhere) that will be used to model the solar electric portion of the PVT solar collector.  The PVT models make any adjustments needed to model PV performance in the context of the PVT collector.

#### Field: Thermal Working Fluid Type

This field is the user's choice for the type of fluid used to collect thermal energy.  PVT solar collectors can capture thermal energy in either air or water streams.  The choices available for this field are "Water" or "Air."  If the choice is "Air" then the PVT collector needs to be connected to an HVAC air system loop.  The PVT collector should be situated as the first component on an outdoor air inlet stream.  If the choice is "Water" then the PVT collector needs to be connected to a Plant water system loop.  The connections are made via node names which are defined in the following fields, depending on the working fluid type.

#### Field: Water Inlet Node Name

This field is the name of Plant loop node that serves as the inlet to the PVT collector.  This field is only used if the Thermal Working Fluid Type is set to "Plant/Water."

#### Field: Water Outlet Node Name

This field is the name of a plant loop node that seves as the outlet from the PVT collector.  This field is only used if the Thermal Working Fluid Type is set to "Plant/Water."

#### Field: Air Inlet Node Name

This field is the name of HVAC air loop node that serves as the inlet to the PVT collector.  This field is only used if the Thermal Working Fluid Type is set to "HVAC/Air."

#### Field: Air Outlet Node Name

This field is the name of HVAC air loop node that serves as the outlet from the PVT collector.  This field is only used if the Thermal Working Fluid Type is set to "HVAC/Air."

#### Field: Design Flow Rate 

This field is used to describe the nominal volume flow rate of the thermal working fluid.  The units are m3/s.  The volume flow rate is autosizable.

An example of this object follows.

~~~~~~~~~~~~~~~~~~~~

      SolarCollector:FlatPlate:PhotovoltaicThermal,
        PVT: 1_Ceiling ,                       !- Name
        1_Ceiling ,                            !- Surface Name
        30percentPVThalfArea ,                 !- Photovoltaic-Thermal Model Performance Name
        PV:ZN_1_FLR_1_SEC_1_Ceiling ,          !- Photovoltaic Name
        Air ,                                  !- Thermal Working Fluid Type
        ,                                      !- Water Inlet Node Name
        ,                                      !- Water Outlet Node Name
        ZN_1_FLR_1_SEC_1:Sys_OAInlet Node ,    !- Air Inlet Node Name
        PVT:ZN_1_FLR_1_SEC_1_Ceiling Outlet ,  !- Air Outlet Node Name
        Autosize ;                             !- Design Flow Rate
~~~~~~~~~~~~~~~~~~~~

### Outputs

The output variables that are available for flat plate PVT include the following.

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Generator Produced Thermal Rate [W]
    HVAC,Sum,Generator Produced Thermal Energy [J]
    HVAC,Average,Generator PVT Fluid Bypass Status [ ]
    HVAC,Average,Generator PVT Fluid Inlet Temperature [C]
    HVAC,Average,Generator PVT Fluid Outlet Temperature [C]
    HVAC,Average,Generator PVT Fluid Mass Flow Rate [kg/s]
~~~~~~~~~~~~~~~~~~~~

#### Generator Produced Thermal Rate [W]

#### Generator Produced Thermal Energy [J]

These outputs are the thermal energy and power produced by the PVT collector.  PVT collectors are a type of cogenerator, producing both electrical and thermal power and these variables report the thermal portion in the same manner as other fuel-based cogenerators.  The thermal energy is placed on "HeatProduced" meter and is attributed to ‘SolarWater' or ‘SolarAir' depending on the type of working fluid.  The generator thermal production is also reported at the load center level.

#### Generator PVT Fluid Bypass Status [ ]

This output variable indicates the status a bypass damper.  It is only available for air-based PVT.  There are no dimensions and the range is between 0.0 and 1.0.  If the value is 0.0, then there is no bypassing and all the working fluid goes through the collector.  If the value is 1.0, then there is complete bypassing and all the working fluid goes around the collector.  If the value is between 0.0 and 1.0, then the model is effectively mixing bypass and collector streams to target a temperature setpoint placed on the outlet node.

#### Generator PVT Fluid Inlet Temperature [C]

This report is the inlet temperature of the working fluid that enters the PVT collector

#### Generator PVT Fluid Outlet Temperature [C]

This report is the outlet temperature of the working fluid that leaves the PVT collector

#### Generator PVT Fluid Mass Flow Rate [kg/s]

This report is the mass flow rate of the working fluid through the PVT collector.  This is the overall mass flow rate, portions of the flow may be internally bypassed around the collector itself for control modulation.

## SolarCollectorPerformance:PhotovoltaicThermal:Simple

This object is used to provide performance details for the simple PVT model.  This is a simple user-defined efficiency model.  Thermal conversion efficiency is a constant or scheduled value.  There are no output variable for this object, reporting is done by the parent PVT object.

### Inputs

#### Field: Name

This field is the unique name for this object.

#### Field: Fraction of Surface Area with Active Thermal Collector

This field is the fraction of the surface area that is active.  It should be a decimal fraction between 0.0 and 1.0.  The area of the PVT's surface will be multiplied by this fraction to determine the active area of the PVT collector(s).

#### Field: Thermal Conversion Efficiency Input Mode Type

This field is used to determine how the thermal efficiency is input.  There are two choices, "Fixed" or "Scheduled."  If this field is set to Fixed, then a constant value for thermal efficiency will be used (set in next field).  If this field is set to Scheduled, then the thermal efficiency values are defined in a schedule.

#### Field: Value for Thermal Conversion Efficiency if Fixed

This field is used to provide a value for the efficiency with which solar energy is collected in the working fluid.  This field is only used if the input mode is set to "Fixed" in the previous field.  Efficiency is defined as the thermal energy collected divided by the incident solar radiation.  The value should be between 0.0 and 1.0.  The user should be careful that the thermal efficiency and the electrical efficiency be consistent with each other because the overall efficiency of the PVT collector is the combination of both thermal and electrical.

#### Field: Name of Schedule for Thermal Conversion Efficiency

This field is used for the name of a schedule that provides values for the efficiency with which solar energy is collected in the working fluid.  This field is only used if the input mode is set to "Scheduled" in the field above.  Efficiency is defined as the thermal energy collected divided by the incident solar radiation.  The values in the named schedule should be between 0.0 and 1.0.  The user should be careful that the thermal efficiency and the electrical efficiency be consistent with each other because the overall efficiency of the PVT collector is the combination of both thermal and electrical.

#### Field: Front Surface Emittance

This field is used to describe an average value for the total hemispherical emittance of the collector's front face exposed to the sky.  This is used to model cooling applications where the PVT collectors are operated at night to cool the working fluid.

An example input object follows.

~~~~~~~~~~~~~~~~~~~~

      SolarCollectorPerformance:PhotovoltaicThermal:Simple,
        20percentEffPVhalfArea ,         !- Name
        0.5 ,                            !- Fraction of Surface Area with Active Thermal Collector
        Fixed ,                          !- Thermal Conversion Efficiency Input Mode Type
        0.2 ,                            !- Value for Thermal Conversion Efficiency if Fixed
         ,                               !- Name of Schedule for Thermal Conversion Efficiency
        0.84 ;                           !- Front Surface Emittance
~~~~~~~~~~~~~~~~~~~~

## Solar Collector Heating System Plant Connections

This section provides an overview of how to model solar heating systems.  A solar heating system can be constructed using a combination of solar collectors, pumps, water tanks and water heaters. The solar collector must be connected on the demand side of the plant loop. Multiple collector modules can be combined in series and parallel using the normal plant connection rules. The supply side of the plant loop should contain a water heater with the solar collector loop connecting to the *Source Side Inlet* and *Source Side Outlet* nodes. As usual, the pump must be the first component on the supply side.

If the solar heating system is for domestic hot water (or service water heating) usage only, the field *Use Flow Rate Fraction Schedule Name* of the [WaterHeater:Mixed](#waterheatermixed) object can be used to avoid additional plant connections. If the system has more complicated hot water requirements or if the system is for space heating, the *Use Side Inlet* and *Use Side Outlet* nodes must be connected to another plant loop to serve zone and non-zone equipment. (See the [WaterHeater:Mixed](#waterheatermixed) object documentation for more information.)

![Solar Collector Plant Loop Connection Diagram](media/solar-collector-plant-loop-connection-diagram.png)


> NOTE:  The EnergyPlus plant simulation requires the pump to be the first component on the supply side. This may be different from the way the solar heating system is actually configured. This should not affect the validity of the simulation results.

In order to realize energy savings with a solar heating system, it is best to use a two-tank system with a storage tank and auxiliary water heater. The storage tank gathers heat directly from the solar collectors and stores it for later use. The storage tank is modeled using a [WaterHeater:Mixed](#waterheatermixed) object with the *Heater Maximum Capacity* set to zero. The auxiliary water heater is positioned downstream of the storage tank on the supply side of the main plant loop. The auxiliary water heater, or booster water heater, provides additional heat if the storage tank water is not hot enough. The auxiliary water heater can be modeled as an instantaneous/tankless water heater or as a standard tanked water heater with heating source (see [WaterHeater:Mixed](#waterheatermixed)).

![Two-Tank Solar Heating System Connection Diagram](media/two-tank-solar-heating-system-connection.png)


Another strategy to consider for solar heating systems is to allow the storage tank to reach a much higher temperature than necessary for the end use. This allows the tank to store more energy from the solar collectors, when it is available. However, for applications such as domestic hot water, it is undesirable and unsafe to supply excessive hot water temperatures at the point of demand. To take advantage of higher storage temperatures, yet still avoid scalding temperatures at the faucet, the hot water leaving the storage tank can be tempered with cold water using a three-way valve to achieve the target temperature. See the [TemperingValve](#temperingvalve) object documentation for more details.

A complete two-tank solar heating system with tempering valve is shown below.

![Two-Tank Solar Heating System with Tempering Valve](media/two-tank-solar-heating-system-with-tempering.png)


## Solar Heating System Control

There are several options for controlling a solar heating system in EnergyPlus. Since the solar collectors request a constant flow demand based on their *Maximum Flow Rate*, the limiting factor is actually the flow rate determined by the loop pump. Therefore the entire system can be controlled using the *Pump Flow Rate Schedule* of the pump. If the schedule is omitted, the pump and system will run all the time (without any other controls specified). This is usually not the best way to operate a solar heating system.

To better control the collector loop, a differential thermostat can be used to compare the temperature in the water heater to the temperature in the collector so that the pump is only turned on when there is a useful heat gain. The differential thermostat is simulated using the [AvailabilityManager:DifferentialThermostat](#availabilitymanagerdifferentialthermostat) object. For a typical system, the *Hot Node Name* field refers to an outlet node of one of the collector modules. The *Cold Node Name* field refers to the *Source Side Outlet* node, i.e. the cold storage water leaving the water heater. The fields *Temperature Difference On Limit* and *Temperature Difference Off Limit* are usually 8 –12 C and 1 – 3 C respectively. If the two temperature differences are too close, it is possible for the system to turn on and off rapidly without much useful heat gain. This can also occur if the flow rate through the collector is too high. Without flow the fluid in the collector heats up more quickly; when high flow is turned on, all of the hot fluid is removed and the temperature drops, forcing the system off again.

Another control method is to use a photovoltaic panel to power the pump. The system begins pumping when there is enough solar radiation to operate the pump. This is not yet implemented in EnergyPlus.

### Freeze Prevention

In climates with a cold season, the solar heating system must be designed to avoid the risk of fluid freezing in the solar collector or exposed pipes and causing damage. This is not a problem if air is the heat transfer fluid. With water, however, there are several strategies that can minimize the risk.

*Seasonal schedule*. The simplest strategy is to not use the system during the cold season. This is a hassle because it requires the collector to be manually drained of all fluid. The benefits of the solar heating system are also lost during this time. This can be simulated in EnergyPlus with the appropriate pump schedule for the collector system.

*Antifreeze*. The freezing point of the liquid is decreased by adding antifreeze to the water or using a different heat transfer liquid with a lower freezing point. This cannot yet be simulated in EnergyPlus because only pure water is currently allowed in plant loops.

*Drain-back system*. This strategy automatically empties the collector when the pump is not running. This scenario is modeled by default in EnergyPlus, although the extra pump energy required to start the system is not taken into account.

*Recirculation system*. This strategy automatically recirculates warm liquid from the storage tank back through the collector to maintain the system above the freezing point. There are system losses using this method. This can be simulated in EnergyPlus by using [AvailabilityManager:LowTemperatureTurnOn](#availabilitymanagerlowtemperatureturnon) to force the system to turn on when the outdoor air temperature or collector outlet temperature falls below a specified minimum.

### Additional Controls

In addition to freeze prevention, it is also necessary to prevent the system from becoming too hot. This is usually a safety issue for the water heater. For this case it is important to have a high temperature cutoff to stop the pump before damaging the water heater. This is accomplished with a [AvailabilityManager:HighTemperatureTurnOff](#availabilitymanagerhightemperatureturnoff).

### System Availability Manager List Example

To use the availability managers for the control cases described above, a [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist) must be defined and referenced in the [PlantLoop](#plantloop) object of the collector loop. An example of a differential thermostat, recirculation for freeze prevention, and high temperature cutoff is shown below:

~~~~~~~~~~~~~~~~~~~~

    AvailabilityManagerAssignmentList,
      Collector Loop Availability Manager List,       !- Name
      AvailabilityManager:HighTemperatureTurnOff,     !- Availability Manager 1 Object Type
      High Temperature Turn Off Availability Manager, !- Availability Manager 1 Name
      AvailabilityManager:HighTemperatureTurnOn,      !- Availability Manager 2 Object Type
      Low Temperature Turn On Availability Manager,   !- Availability Manager 2 Name
      AvailabilityManager:DifferentialThermostat,     !- Availability Manager 3 Object Type
      Differential Thermostat Availability Manager;   !- Availability Manager 3 Name

     AvailabilityManager:HighTemperatureTurnOff,      ! For water heater safety
      High Temperature Turn Off Availability Manager, !- Name
      Water Heater Use Outlet Node,                   !- Sensor Node Name
      60.0;                                           !- Temperature (C)

     AvailabilityManager:HighTemperatureTurnOn,       ! For freeze prevention by recirculation
      Low Temperature Turn On Availability Manager,   !- Name
      Collector Outlet Node,                          !- Sensor Node Name
      0.0;                                            !- Temperature (C)

     AvailabilityManager:DifferentialThermostat,      ! For useful heat gain from collector to tank
      Differential Thermostat Availability Manager,   !- Name
      Collector Outlet Node,                          !- Hot Node Name
      Water Heater Source Outlet Node,                !- Cold Node Name
      10.0,                                           !- Temperature Difference On Limit (delta C)
      2.0;                                            !- Temperature Difference Off Limit (delta C)
~~~~~~~~~~~~~~~~~~~~

The [AvailabilityManager:DifferentialThermostat](#availabilitymanagerdifferentialthermostat) object must always be the last manager in the availability manager list. See the [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist) object documentation for more information.

## SolarCollector:UnglazedTranspired

This object is used to model unglazed transpired solar collectors (UTSC) used to condition outdoor air. These collectors are generally used to heat air drawn through perforated absorbers that are heated by the sun and also recover heat conducted out through the underlying wall. The SolarCollector:UnglazedTranspired ****object represents a single collector attached to one or more BuildingSurface:Detailed ****objects and to one or more outdoor air systems. Therefore the transpired collector is part of both the thermal envelope and the HVAC system. An example file is provided called TranspiredCollectors.idf.

The area and orientation of the collector is obtained from [BuildingSurface:Detailed](#buildingsurfacedetailed) ****objects, which are referenced by name. Although the collector surface itself is slightly detached from the underlying building wall (or roof), no additional surface object is needed to represent the collector itself. When modeling transpired collectors, it is important to consider the size of the collector when developing the building model's [BuildingSurface:Detailed](#buildingsurfacedetailed) ****objects because the underlying surfaces must match the collector. For example, if the collector covers only part of the wall, then that wall should be split into separate surfaces where one matches the size of the collector. A single collector can be associated with as many [BuildingSurface:Detailed](#buildingsurfacedetailed) objects as desired (although if you need to use more than 10 surfaces, then the IDD will need to be extended). The collector can be arranged at any tilt angle by describing the surfaces appropriately. The surfaces need not be contiguous nor have the same orientation, but the program will issue warnings if surfaces have widely ranging tilts and azimuths.

The collector conditions outdoor air and is connected to the outdoor air system using the usual method of specifying node names. Using the UTSC model requires specifying a relatively complete HVAC air system that includes an outdoor air path. This will typically require using a set of objects that, at a minimum, will include:  [AirLoopHVAC:ControllerList](#airloophvaccontrollerlist), [AirLoopHVAC:OutdoorAirSystem:EquipmentList](#airloophvacoutdoorairsystemequipmentlist), [AirLoopHVAC:OutdoorAirSystem](#airloophvacoutdoorairsystem), [OutdoorAir:NodeList](#outdoorairnodelist), [OutdoorAir:Mixer](#outdoorairmixer), [SetpointManager:MixedAir](#setpointmanagermixedair), and [Controller:OutdoorAir](#controlleroutdoorair). A single UTSC can serve more than one outdoor air system but requires also using a separate object, called SolarCollector:UnglazedTranspired:MultiSystem to specify node connections.

Controls for the UTSC involve setting the rate of air flow and the status of a bypass damper. If the bypass damper is open, then all the ventilation air goes straight into the outdoor air mixer; if it closed, then all the air first passes through the UTSC. The bypass damper is modeled as completely open or completely closed. The UTSC bypass damper control is determined by an availability manager, the airflow set by the outdoor air mixer controls, and thermostatic type controls that decide if heating is useful. An availability schedule is used to bypass the collector for certain times of the year, eg. summer cooling season. The air flow rates are set by controls associated with the outdoor air mixer (see [SetpointManager:MixedAir](#setpointmanagermixedair), and ****Controller:OutdoorAir). Thermostatic type control decides if the collector will provide useful heating based on either of two types of setpoints. The first type of temperature setpoint is managed by [SetpointManager:MixedAir](#setpointmanagermixedair), where the UTSC model looks at a control node, usually the mixed air node. The second type is an extra setpoint especially for free heating that is managed within this object where the UTSC model looks at the zone air node.

### Inputs

#### Field: Name

This field contains a unique name for the unglazed transpired solar collector.

#### Field: Boundary Conditions Model Name

This field contains the name of a [SurfaceProperty:OtherSideConditionsModel](#surfacepropertyothersideconditionsmodel) object declared elsewhere in the input file. This will connect the collector to the exterior boundary conditions for the underlying heat transfer surface.

#### Field: Availability Schedule Name

This field contains the name of a schedule that determines whether or not the UTSC is available. When the schedule value is less than or equal to zero, the UTSC is always bypassed. When the schedule value is greater than zero, the UTSC is available and will be used when other conditions are met, such as outdoor air requested by mixer and preheating has been determined to be beneficial based on thermostatic control. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Inlet Node Name

This field contains the name of an air node that provides air into the UTSC. This node name should also be assigned to be an outdoor air node using the OutdoorAir:NodeList ****or OutdoorAir:Node ****objects. This node should also be named as the actuated node in a Controller:OutdoorAir ****object. If the UTSC is connected to more than one air system, then this field can be left blank and the SolarCollector:UnglazedTranspired:MultiSystem object should be used to define the nodes.

#### Field: Outlet Node Name

This field contains the name of an air node that is the outlet of the UTSC. This node name will typically be the inlet to the [OutdoorAir:Mixer](#outdoorairmixer) (if there is no other equipment on the outdoor air path). If the UTSC is connected to more than one air system, then this field can be left blank and the SolarCollector:UnglazedTranspired:MultiSystem ****object should be used to define the nodes.

#### Field: Setpoint Node Name

This field contains the name of an air node that has a setpoint manager controlling its temperature setpoint. This node name will typically be named as the control node in a a Controller:OutdoorAir ****object. If the UTSC is connected to more than one air system, then this field can be left blank and the SolarCollector:UnglazedTranspired:MultiSystem ****object should be used to define the nodes.

#### Field: Zone Node Name

This field contains the name of an air node for a thermal zone that is ultimately connected to the air system. This node is used with the setpoint schedule, defined in the following field, to provide an added layer of thermostatic control for the UTSC without affecting the control of auxiliary heating. If there is a single air system that is connected to more than one zone, then a single zone should be selected based on where the thermostat might be located. If the UTSC is connected to more than one air system, then this field can be left blank and the SolarCollector:UnglazedTranspired:MultiSystem ****object should be used to define the nodes.

#### Field: Free Heating Setpoint Schedule Name

This field contains the name of a temperature schedule defined elsewhere in the input file. This schedule should define temperatures *desired* in the zone, but not necessarily *required*. This secondary setpoint schedule is used to allow the UTSC to operate as if it has its own thermostat that is separate from the primary control mechanism. When the UTSC is used with auxiliary heating, the usual setpoint managers and temperature controllers will determine how the auxiliary heaters are controlled. This allows using a higher zone air temperature setpoint for controlling UTSC bypass than for the auxiliary heating system.

#### Field: Diameter of Perforations in Collector

This field is used to enter the effective diameter of the perforations in the collector surface. The diameter should be entered in meters. For perforations other than round, use an equivalent diameter for a round hole that would have the same area.

#### Field: Distance Between Perforations in Collector

This field is used to enter the pitch, or average, shortest distance between perforations.

#### Field: Thermal Emissivity of Collector Surface

This field is used to enter the thermal emissivity of the collector. This surface property is for longwave infrared radiation. The property is used for both sides of collector. Most painted materials have an emissivity of 0.9.

#### Field: Solar Absorbtivity of Collector Surface

This field is used to enter the solar absorbtivity of the collector. This surface property is for shortwave, solar radiation. The property is used for the front side of the collector that faces the environment. Darker colors have a higher absorbtivity. While black is the highest performance, other colors might be used to match the color scheme of the rest of the facade. The following table provides sample solar absorbtivities for different colors (source:  Conserval Engineering Inc., Toronto, Ontario, Canada).

Color Name of Kynar® Paint|Solar Absorptivity
--------------------------|------------------
Black|0.94
Classic Bronze|0.91
Chocolate Brown|0.90
Hartford Green|0.90
Med. Bronze|0.89
Boysenberry|0.86
Rocky Grey|0.85
Regal Blue|0.85
Forest Green|0.84
Hemlock Green|0.82
Slate Blue|0.80
Redwood|0.79
Teal|0.79
Slate Grey|0.79
Patina Green|0.77
Mint Green|0.71
Dove Grey|0.69
Mission Red|0.69
Sierra Tan|0.65
Brite Red|0.59
Rawhide|0.57
Sandstone|0.54
Silversmith|0.53
Coppertone|0.51
Concord Cream|0.45
Ascot White|0.40
Bone White|0.30

#### Field: Effective Overall Height of Collector

This field is used to enter a nominal height for the collector. This value is used in the program to determine a length scale in the vertical direction for the buoyancy-driven portion of natural ventilation that occurs when the collector is inactive. (Note that most of the geometry information is obtained from the underlying surfaces.)  The value entered here is adjusted inside the program to account for tilt of the collector. While the value here would generally correspond to the actual distance/height, its value is not critical and it can be used to adjust modeling the air exchange rates in passive mode. If the collector is horizontal, then the length scale is obtained from the following field.

#### Field: Effective Gap Thickness of Plenum Behind Collector

This field is used to enter a nominal gap thickness for the collector. This distance value is only used when the collector is near horizontal to determine a length scale in the vertical direction for buoyancy calculations. For example, if the collector is mounted on a flat roof, its tilt-adjusted height is zero and the program will use this gap thickness as a length scale rather than the height from the previous field.

#### Field: Effective Cross Section Area of Plenum Behind Collector

This field is used to enter the nominal cross sectional area of the gap behind the collector. This area is used to determine a velocity scale for surface convection heat transfer correlations when the collector is active. This value is generally the average gap thickness times the average width of the collector.

#### Field: Hole Layout Pattern for Pitch

This field is used to describe the pattern of perforations in the collector surface. There are currently two choices available:  Square and Triangle. Note that the hole layout pattern should be consistent with how the value for pitch was determined.

#### Field: Heat Exchange Effectiveness Correlation

This field is used to select which correlation is used to model heat transfer from the collector surface to the incoming air when the collector is active. There are two choices available:  Kutscher1994, and VanDeckerHollandsBrunger2001. See the Engineering Reference for details and references.

#### Field: Ratio of Actual Collector Surface Area to Projected Surface Area

This field is used to enter a factor that accounts for the extra surface area resulting from corrugations in the collector surface. Corrugations help stiffen the collector. The projected surface area is obtained by the program from the (flat) underlying surfaces. If the collector is flat then this ratio is 1.0. If the collector is corrugated, then this ratio will be greater than one. A typical value might be 1.165.

#### Field: Roughness of Collector

This field is used to describe the relative roughness of the collector material. This field is similar to one in the Material ****object. This parameter only influences the convection coefficients, more specifically the outside convection coefficient. A special keyword is expected in this field with the options being "VeryRough", "Rough", "MediumRough", "MediumSmooth", "Smooth", and "VerySmooth" in order of roughest to smoothest options.

#### Field: Collector Thickness

This field is used to enter the thickness of the collector material. This value is only needed for the Van Decker Hollands Brunger 2001 correlation. The material thickness should be entered in meters.

#### Field: Effectiveness for Perforations with Respect to Wind

This field is used to enter a value for the coefficient used to determine natural air exchanges from wind, or Cv. When the collector is inactive, wind will cause exterior air to move in and out of the collector. Cv is an arbitrary coefficient used to model the effectiveness of openings and depends on opening geometry and the orientation with respect to the wind. Cv should probably be in the range 0.25 to 0.65. Increasing Cv will increase the amount of natural ventilation.

#### Field: Discharge Coefficient for Openings with Respect to Buoyancy Driven Flow

This field is used to enter a value for the coefficient used to determine natural air exchanges from buoyancy, or Cd. When the collector is inactive, stack or buoyancy effects will cause exterior air to move in and out of the collector. Cd is an arbitrary discharge coefficient that depends on the geometry of the opening. Cd should probably be in the range 0.4 to 1.0. Increasing Cd will increase the amount of natural ventilation.

#### Field: Surface <#> Name

The remaining fields are used to name the BuildingSurface:Detailed ****objects that are associated with the UTSC. These are the underlying heat transfer surfaces and are defined elsewhere in the input file. These other surfaces should all specify OtherSideConditionsModel as their exterior environment. The input object can currently accommodate up to ten surfaces, but it is extensible.

An example of this object follows.

~~~~~~~~~~~~~~~~~~~~

    SolarCollector:UnglazedTranspired,
        Shop OA UTSC ZN11,                ! Name
        UTSC OSCM ZN11,                   ! Boundary Conditions Model Name
        HeatingAvailSched ,               ! Availability Schedule Name
        Outside Air Inlet Node ZN11 ,     ! Inlet Node Name
        UTSC Outlet Node ZN11 ,           ! Outlet Node Name
        Mixed Air Node ZN11 ,             ! Setpoint Node Name
        ZN11 Node,                        ! Zone Node Name
        ShopFreeHeatingSetpoints,         ! Free Heating Setpoint Schedule Name
        0.0016,                           ! Diameter of Perforations in Collector
        0.01689,                          ! Distance Between Perforations in Collector
        0.9,                              ! Thermal Emissivity of Collector Surface
        0.9,                              ! Solar Absorbtivity of Collector Surface
        4.0,                              ! Effective Overall Height of Collector
        0.1,                              ! Effective Gap Thickness of Plenum Behind Collector
        2.0,                              ! Effective Cross Section Area of Plenum Behind Collector
        Triangle,                         ! Hole Layout Pattern for Pitch
        Kutscher1994,                     ! Heat Exchange Effectiveness Correlation
        1.165,                      ! Ratio of Actual Collector Surface Area to Projected Surface Area
        MediumRough ,                     ! Roughness of Collector
        0.00086,                          ! Collector Thickness
        0.25,                             ! Effectiveness for Perforations with Respect to Wind
        0.5,                 ! Discharge Coefficient for Openings with Respect to Buoyancy Driven Flow
        ZN11_Shop_1:ExtWall:South;        ! Surface 1 Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

In addition to related output that can be obtained for air nodes and surfaces, these outputs are available for UTSC systems:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Solar Collector Heat Exchanger Effectiveness []
    HVAC,Average,Solar Collector Leaving Air Temperature [C]
    HVAC,Average,Solar Collector Outside Face Suction Velocity [m/s]
    HVAC,Average,Solar Collector Surface Temperature [C]
    HVAC,Average,Solar Collector Plenum Air Temperature [C]
    HVAC,Average,Solar Collector Sensible Heating Rate [W]
    Zone,Meter,SolarAir:Facility [J]
    Zone,Meter,SolarAir:HVAC [J]
    Zone,Meter,HeatProduced:SolarAir [J]
    HVAC,Sum,Solar Collector Sensible Heating Energy [J]
    HVAC,Average,Solar Collector Natural Ventilation Air Change Rate [ACH]
    HVAC,Average,Solar Collector Natural Ventilation Mass Flow Rate [kg/s]
    HVAC,Average,Solar Collector Wind Natural Ventilation Mass Flow Rate [kg/s]
    HVAC,Average,Solar Collector Buoyancy Natural Ventilation Mass Flow Rate [kg/s]
    HVAC,Average,Solar Collector Incident Solar Radiation [W/m2]
    HVAC,Average,Solar Collector System Efficiency []
    HVAC,Average,Solar Collector Surface Efficiency []
~~~~~~~~~~~~~~~~~~~~

#### Solar Collector Heat Exchanger Effectiveness []

The results from UTSC correlations defined by ![](media/image329.png) .

#### Solar Collector Leaving Air Temperature [C]

The temperature of air entering the plenum after being heated by the collector.

#### Solar Collector Outside Face Suction Velocity [m/s]

The bulk velocity of air approaching the collector.

#### Solar Collector Surface Temperature [C]

The surface temperature of the collector itself.

#### Solar Collector Plenum Air Temperature [C]

The temperature of air inside, and leaving, the plenum behind the collector.

#### Solar Collector Sensible Heating Rate [W]

The overall rate at which heat is being added to the outdoor air stream.

#### SolarAir:Facility [J]

A meter that includes the heating energy provided by the UTSC.

#### SolarAir:HVAC [J]

A meter that includes the heating energy provided by the UTSC.

#### HeatProduced:SolarAir [J]

A meter that includes the heating energy provided by the UTSC.

#### Solar Collector Sensible Heating Energy [J]

The overall sum of energy added to the outdoor air stream.

#### Solar Collector Natural Ventilation Air Change Rate [ACH]

The rate of natural ventilation air exchange between the plenum and ambient when the collector is inactive in Air Changes per Hour.

#### Solar Collector Natural Ventilation Mass Flow Rate [kg/s]

The mass flow rate of natural ventilation air exchange between the plenum and ambient when the collector is inactive.

#### Solar Collector Wind Natural Ventilation Mass Flow Rate [kg/s]

The part of mass flow rate of natural ventilation air exchange between the plenum and ambient when the collector is inactive due to wind-driven forces.

#### Solar Collector Buoyancy Natural Ventilation Mass Flow Rate [kg/s]

The part of mass flow rate of natural ventilation air exchange between the plenum and ambient when the collector is inactive due to bouyancy-driven forces.

#### Solar Collector Incident Solar Radiation [W/m2]

The intensity of solar radiation incident on the UTSC collector from all sources.

#### Solar Collector System Efficiency []

The overall efficiency of the UTSC system including collected solar energy and heat recovered from the underlying surface.

#### Solar Collector Surface Efficiency []

The efficiency of the UTSC solar collector.

## SolarCollector:UnglazedTranspired:MultiSystem

This object is used to model unglazed transpired solar collectors (UTSC) that are connected to multiple outdoor air systems. This object supplements the [SolarCollector:UnglazedTranspired](#solarcollectorunglazedtranspired) object and is only necessary if more than one air system is connected to a single transpired collector. After the name field, there are sets of four node names used to define the connections of each air system. Each set contains node names for inlet, outlet, control, and zone. If more than five air systems are needed, this object is extensible.

### Field: Solar Collector Name

This field is used to identify the name of the [SolarCollector:UnglazedTranspired](#solarcollectorunglazedtranspired) object that needs to be connected to more than one air system. This field must match the name.

### Field Set: Inlet Node, Outlet Node, Mixed Air Node, Zone Node 

The following four fields form a repeating set of four fields.  One set is used for each outdoor air system that is connected to the collector.

### Field: Outdoor Air System <#> Collector Inlet Node

This field contains the name of an air node that provides air into the UTSC. This node name should also be assigned to be an outdoor air node using the [OutdoorAir:NodeList](#outdoorairnodelist) and [OutdoorAir:Node](#outdoorairnode) objects. This node is also be named as the actuator node in a Controller:OutdoorAir ****object.

### Field: Outdoor Air System <#> Collector Outlet Node

This field contains the name of an air node that is the outlet of the UTSC. This node name will typically be the Outdoor Air Stream Node Name in the [OutdoorAir:Mixer](#outdoorairmixer) (if there is no other equipment on the outdoor air path).

### Field: Outdoor Air System <#> Mixed Air Node

This field contains the name of an air node that has a setpoint manager controlling its temperature setpoint. This node name will typically be named as the mixed air node in a Controller:OutdoorAir ****object.

### Field: Outdoor Air System <#> Zone Node

This field contains the name of an air node for a thermal zone that is ultimately connected to the air system. This node is used with the setpoint schedule, defined in the following field, to provide an added layer of thermostatic control for the UTSC without affecting the control of auxiliary heating. If there is a single air system that is connected to more than one zone, then a single zone should be selected based on where the thermostat might be located.

An example of this object follows.

~~~~~~~~~~~~~~~~~~~~

      SolarCollector:UnglazedTranspired:Multisystem,
        OFFICE MultiSystem OA UTSC ,  ! Solar Collector Name
        Outside Air Inlet Node ZN1,   ! Outdoor Air System 1 Collector Inlet Node
        UTSC Outlet Node ZN1,         ! Outdoor Air System 1 Collector Outlet Node
        Mixed Air Node ZN1,           ! Outdoor Air System 1 Mixed Air Node
        ZN1 Node,                     ! Outdoor Air System 1 Zone Node
        Outside Air Inlet Node ZN2,   ! Outdoor Air System 2 Collector Inlet Node
        UTSC Outlet Node ZN2,         ! Outdoor Air System 2 Collector Outlet Node
        Mixed Air Node ZN2,           ! Outdoor Air System 2 Mixed Air Node
        ZN2 Node,                     ! Outdoor Air System 2 Zone Node
        Outside Air Inlet Node ZN3,   ! Outdoor Air System 3 Collector Inlet Node
        UTSC Outlet Node ZN3,         ! Outdoor Air System 3 Collector Outlet Node
        Mixed Air Node ZN3,           ! Outdoor Air System 3 Mixed Air Node
        ZN3 Node,                     ! Outdoor Air System 3 Zone Node
        Outside Air Inlet Node ZN4,   ! Outdoor Air System 4 Collector Inlet Node
        UTSC Outlet Node ZN4,         ! Outdoor Air System 4 Collector Outlet Node
        Mixed Air Node ZN4,           ! Outdoor Air System 4 Mixed Air Node
        ZN4 Node,                     ! Outdoor Air System 4 Zone Node
        Outside Air Inlet Node ZN5,   ! Outdoor Air System 5 Collector Inlet Node
        UTSC Outlet Node ZN5,         ! Outdoor Air System 5 Collector Outlet Node
        Mixed Air Node ZN5,           ! Outdoor Air System 5 Mixed Air Node
        ZN5 Node;                     ! Outdoor Air System 5 Zone Node
~~~~~~~~~~~~~~~~~~~~