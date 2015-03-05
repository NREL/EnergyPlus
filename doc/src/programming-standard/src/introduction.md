# Introduction

EnergyPlus is a building simulation program written in a modular fashion using the Fortran 90 programming language. Most programs have progenitors: EnergyPlus is no exception. It is built upon the DOE-2 and iBLAST building simulation programs. Both of these programs were written in Fortran 77 without any consistent style or structure. Consequently, both programs had become difficult to understand, maintain and extend.

The EnergyPlus Programming Standard is intended to be a coding guideline for EnergyPlus software developers. The rules and standards described in this document are intended to impose a consistent structure and style on all code written for EnergyPlus. This consistency should aid all present and future developers in understanding, maintaining, and adding to EnergyPlus.

All Fortran code will be separated from text and formatted using the following notation to distinguish it from other information:

~~~~~~~~~~~~~~~~~~~~

    SUBROUTINE ReportZoneConditions     ! EnergyPlus Subroutine
    INTEGER ZoneNum
    DO ZoneNum=1,MaxNumZones
~~~~~~~~~~~~~~~~~~~~