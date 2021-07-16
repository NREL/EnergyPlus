Input Changes version 9.5.0 to 9.6.0
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc.
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.

# Object Change: VentilationRateProcedure to Standard62.1VentilationRateProcedure in Sizing:System and Controller:MechanicalVentilation
Summary: The implementation of the ASHRAE Standard 62.1 Simplified Procedure introduced a new `System Outdoor Air Method` for `Sizing:System`. To be consistent with the existing Standard 62.1 Summary report, it was decided to use the `Standard62.1` prefix for both the `VentilationRateProcedure` and `SimplifiedProcedure`.

If Field 27 in `Sizing:System` is `VentilationRateProcedure`, it is replaced by `Standard62.1VentilationRateProcedure`. If Field 4 in `Controller:MechanicalVentilation` is `VentilationRateProcedure`, it is replace by `Standard62.1VentilationRateProcedure`.

See [8891](https://github.com/NREL/EnergyPlus/pull/8891)
