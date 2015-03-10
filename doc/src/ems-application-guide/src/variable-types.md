# Variable Types

Various types of output variables in EnergyPlus can be used as sensors in the EMS. It is important to understand some distinctions.

Reported variables have two update frequencies: **zone** and **system**. Zone variables are updated for each zone timestep. System variables are updated for each system timestep.

Reported variables have two types of content: **averaged** and **summed**. Averaged variables are state variables such as temperature and mass flow rate. Summed variables are quantities of energy such as electricity use.