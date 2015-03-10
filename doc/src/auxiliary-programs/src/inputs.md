# Inputs

First step in curve generation is to select appropriate parameters from the dropdown menu.  These inputs define the DX Coil Type, Curve Type, Independent Variable and the Units type. The choices available for each input parameters are described in table-1. Once these input parameters are selected the tool read in the values and automatically populates labels for each of the independent and dependent variables. The labels guide users to enter the data for each variable in the corresponding worksheet input range.   Two sets of input data are required for curve generation: Rated, and Performance Data.

Table: Input parameters description

------------------------------------------------------------------------
    Input Parameter     Description of Inputs
----------------------- ------------------------------------------------
     DX Coil Type       **Cooling**: applicable for DX cooling coil
                        single speed

                        **Heating**: applicable for DX heating coil
                        single speed

                        **Other**: applicable for any equipment that
                        use the three curve types

 Independent Variables  TemperatureFlow

      Curve Types       **Biquadratic**: Capacity and EIR as a function
                        of temperature

                        **Cubic**: Capacity and EIR as a function of
                        flow fraction or temperature

                        **Quadratic**: capacity and EIR as a function
                        of flow fraction

         Units          **IP**: Temperature in °F, Capacity in kBtu/h,
                        Power in kW, and Flow in CFM

                        **SI**: Temperature in °C, Capacity in kW,
                        Power in kW, and Flow in m^3^/s

   Curve Object Name    This input is optional. This string is appended
                        to the default curve object name, or if left
                        blank the default curve object name will be
                        displayed. A curve object is named is created
                        by concatenation as follows:

                        ![](media/image35.png)
------------------------------------------------------------------------
