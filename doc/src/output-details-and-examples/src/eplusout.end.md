# eplusout.end

This is a simple one line synopsis of the simulation. Successful or Not Successful, including number of errors/warnings:

~~~~~~~~~~~~~~~~~~~~

    EnergyPlus Completed Successfully-- 8 Warning; 0 Severe Errors
~~~~~~~~~~~~~~~~~~~~

If no file is produced, it is really *not* successful and EnergyPlus has probably crashed during the run. This file and its contents are intended for interfaces that will put friendly front-ends onto EnergyPlus. This file is also used by the EP-Launch program so that it can determine if the run was successful or not – if not, the user should review the eplusout.err file. (Actually, the eplusout.err file should always be reviewed but often is ignored in haste to view the results.)