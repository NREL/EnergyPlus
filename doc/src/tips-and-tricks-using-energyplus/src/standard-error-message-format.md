# Standard Error Message Format

Standard error message format changes depending on where the error message is coming from. The standard error message format for GetInput goes something like this:

~~~~~~~~~~~~~~~~~~~~

    <modulename><routine name>: <object name> = <name field> "condition"
    <several lines with more information may follow>
~~~~~~~~~~~~~~~~~~~~

The <modulename>(optional) <routinename> part is so that people answering support questions can more easily find the code, if necessary and without running the input file through the debugger.

As noted elsewhere, errors come in several flavors with typical user responses required.

<insert table>

In the examples for this section, the severity (Warning/Severe/Fatal) will be left off the message unless necessary for the rest of the example. For example:

~~~~~~~~~~~~~~~~~~~~

    GetPlantLoopData/GetPlantAvailabilityManager: AvailabilityManagerAssignmentList=ALWAYS_ON not found in lists.  No availability will be used.
~~~~~~~~~~~~~~~~~~~~

Here the routine GetPlantLoopData/GetPlantAvailabilityManager for object AvailabilityManagerAssignmentList with name Always_On is not found. And then the result is shown.  (This is a warning level error, by the way).

The development team is working to standardize the error format, as time allows. So, sometimes you will likely see something like:

~~~~~~~~~~~~~~~~~~~~

    Check input. Pump nominal power or motor efficiency is set to 0, for pump=HEAT RECOVERY CIRC PUMP
~~~~~~~~~~~~~~~~~~~~

Here, at least you know which pump (Heat Recovery Circ Pump) has the power or motor efficiency of 0.