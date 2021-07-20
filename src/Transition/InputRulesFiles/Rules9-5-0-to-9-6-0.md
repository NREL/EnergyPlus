Input Changes version 9.5.0 to 9.6.0
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc.
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.

# Object Change: AirLoopHVAC:OutdoorAirSystem

Summary: The fourth field (Availability Manager name) was being ignored by EnergyPlus other than to verify that it was a valid availability manager (if it wasn’t blank).  It was not a required input and E+ wasn’t doing anything with it.  It was decided to remove this field to avoid any confusion.

Field 1-3 remain the same.
Field 4 has been eliminated/deleted and not replace with anything else.
