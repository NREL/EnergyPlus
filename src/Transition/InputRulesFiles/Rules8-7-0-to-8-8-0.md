Input Changes
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc. 
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.

# Object Change: `Table:TwoIndependentVariables`

Insert new blank field 14 (A7) (for External File Name). 
Shift all later fields down by one.

# Object Change: `Output:Surfaces:List`

The only change is for field F1, which is A1.  Logic to apply:

if key = 'DecayCurvesfromZoneComponentLoads', change to = 'DecayCurvesFromComponentLoadsSummary'

# Object Change: `WindowMaterial:Blind:EquivalentLayer`

The only change is for field F6 value. Th number of fields remains the same:

if SlatAngle < 90, New SlatAngle = SlatAngle
if SlatAngle >= 90, New SlatAngle =  90.0 - SlatAngle