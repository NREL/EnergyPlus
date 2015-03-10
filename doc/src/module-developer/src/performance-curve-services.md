# Performance Curve Services

Some HVAC equipment models in EnergyPlus use performance curves. These are polynomials in one or two independent variables that are used to modify rated equipment performance for performance at the current, off-rated conditions. Most often the curves are functions of temperature – entering wetbulb and outside drybulb, for instance – or of the part load fraction. EnergyPlus provides services to input, store, and retrieve curve data and to evaluate curves given values of the independent variables. There are 3 curve objects: CURVE:QUADRATIC, CURVE:CUBIC, and CURVE:BIQUADRATIC.

## GetCurveIndex

This function takes a curve name as input and returns an internal pointer to the curve. Curve values will always be accessed via the pointer not the name during the simulation for reasons of efficiency. This function is usually called once for each curve during the input phase.

~~~~~~~~~~~~~~~~~~~~

    USE CurveManage, ONLY: GetCurveIndex
    . . .
    DXCoil(DXCoilNum)%CCapFTemp = GetCurveIndex(Alphas(5))
      IF (DXCoil(DXCoilNum)%CCapFTemp .EQ. 0) THEN
        CALL ShowSevereError('COIL:DX:BF-Empirical not found=' &
                             //TRIM(Alphas(5)))
        ErrorsFound = .TRUE.
      END IF
~~~~~~~~~~~~~~~~~~~~

## GetCurveCheck

This function uses a curve name as well as an error indicator and object name to "get" a curve index and perform error checking in one call.  The calling routine will need to check the value of the error flag and perform appropriate action.

~~~~~~~~~~~~~~~~~~~~

    FUNCTION GetCurveCheck(alph, errFlag, ObjName)  &
                 RESULT (GetCurveCheckOut)
~~~~~~~~~~~~~~~~~~~~

The curve index (as in GetCurveIndex) is the result. Curve values will always be accessed via the pointer not the name during the simulation for reasons of efficiency. This function would be called during an input phase for an object.

~~~~~~~~~~~~~~~~~~~~

    USE CurveManager, ONLY: GetCurveCheck
    ...
    GasAbsorber(AbsorberNum)%CoolCapFTCurve      =   &
          GetCurveCheck(AlphArray(8),  ErrorsFound, ChillerName)
    GasAbsorber(AbsorberNum)%FuelCoolFTCurve     =   &
          GetCurveCheck(AlphArray(9),  ErrorsFound, ChillerName)
~~~~~~~~~~~~~~~~~~~~

## GetCurveType

This function will tell the calling routine what the "curve type" that was input.  This function may be useful if your module does different calculations depending on a curve type (i.e. cubic vs quadratic) or if it should not use a specific curve type.  This function would be called during input phase for an object.

~~~~~~~~~~~~~~~~~~~~

    CHARACTER(len=20) FUNCTION GetCurveType (CurveIndex)
~~~~~~~~~~~~~~~~~~~~

Example of use:

~~~~~~~~~~~~~~~~~~~~

    USE CurveManager,   ONLY: GetCurveIndex, GetCurveType
    …
    SELECT CASE(GetCurveType(DXCoil(DXCoilNum)%CCapFTemp))
~~~~~~~~~~~~~~~~~~~~

## CurveValue

This function takes the curves index and one or two independent variables as input and returns the curve value.

~~~~~~~~~~~~~~~~~~~~

    USE CurveManage, ONLY: CurveValue
    . . .
    !  Get total capacity modifying factor (function of temperature)
    !  for off-rated conditions
    50 TotCapTempModFac = CurveValue(DXCoil(DXCoilNum)%CCapFTemp,                                 InletAirWetbulbC, &                                 OutDryBulbTemp)
~~~~~~~~~~~~~~~~~~~~