# AirflowNetwork and EarthTube

*When I use an Earthtube with an AirFlowNetwork, I get a "Orphan Object" warning.*

Currently, Earthtube and AirFlowNetworks do not work together.  If both objects co-exist, the AirflowNetwork mode supersedes the Earthtube mode at two control choices. Since this causes the Earthtube objects to not be used, the "orphan" warning appears.

There are four control choices in the second field of the AirflowNetwork Simulation object (spaces included for readability)

- MULTIZONE WITH DISTRIBUTION
- MULTIZONE WITHOUT DISTRIBUTION
- MULTIZONE WITH DISTRIBUTION ONLY DURING FAN OPERATION
- NO MULTIZONE OR DISTRIBUTION

When the first two choices are selected, the AirflowNetwork model takes over airflow calculation. The earthtube objects are not used in the airflow calculation, causing the "orphan" warning. The example file, AirflowNetwork_Multizone_SmallOffice.idf, uses the first choice.  When the second choice is used, the AirflowNetwork model is only used during HVAC operation time. During system off time, the earthtube model is used to calculate airflows.  Thus, no "orphan" warning will be given, but the earthtube may be being used less than expected.  The example file, AirflowNetwork_Simple_House.idf, uses the third choice.