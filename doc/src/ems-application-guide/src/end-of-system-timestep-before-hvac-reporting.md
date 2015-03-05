# End of System Timestep Before HVAC Reporting

The calling point called "EndOfSystemTimestepBeforeHVACReporting" occurs near the end of a system timestep but before output variable reporting is finalized. It is useful for custom output variables that use the SystemTimestep reporting frequency.