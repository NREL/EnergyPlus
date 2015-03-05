# Advanced Input Considerations

Creating a new module/adding a new feature to EnergyPlus is a good accomplishment.  However, it is likely that future additions will be done and will impact any objects created.  In this regard, we ask that module developers take a longer view than "just getting my thing" going.

For example, in the "Fan Coil" object, prior to the V1.2 release, the object definition specified a cooilng coil name.  *But it did not specify a cooling coil type.*  Rather than restrict coil names to be unique over all coils (which becomes difficult as more coil types are added), the developers only have unique names within a type.  Thus, it would become difficult for the Fan Coil module to get the proper link to the correct cooling coil.

In the V1.2 release, a cooling coil type was added to the object.  But the resulting object is less readable now. For upwards compatibility, the coil type was placed at the end of the object whereas the coil name is in the middle.  If this had been thought of earlier, then the coil type and coil name could be in succeeding fields in the object definition.

The "standard" for describing such fields would be to list the "coil type" and then the "coil name" fields, such as in the UNITARYSYSTEM:HEATPUMP:AIRTOAIR object.

The point – try to envision future changes in making up objects, even if you think "that will never happen".  You do not have to try to address every future case, only the most likely.

~~~~~~~~~~~~~~~~~~~~

    ZoneHVAC:FourPipeFanCoil,
            \min-fields 21
       A1 , \field Name
            \required-field
       A2 , \field Availability Schedule Name
            \required-field
            \type object-list
            \object-list ScheduleNames
       N1 , \field Maximum Supply Air Flow Rate
            \required-field
            \autosizable
            \units m3/s
       N2 , \field Maximum Outdoor Air Flow Rate
            \required-field
            \autosizable
            \units m3/s
       <snip>
       A11, \field Cooling Coil Name
            \required-field
            \type object-list
            \object-list CoolingCoilsWater
       N3 , \field Maximum Cold Water Flow Rate
            \required-field
            \autosizable
            \units m3/s
            \ip-units gal/min
      <snip>
       A13; \field Cooling Coil Object Type
            \required-field
            \type choice
            \key Coil:Cooling:Water
            \key Coil:Cooling:Water:DetailedGeometry
            \key CoilSystem:Cooling:Water:HeatExchangerAssisted
~~~~~~~~~~~~~~~~~~~~