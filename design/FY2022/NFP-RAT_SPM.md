Return Air Temperature Reset Setpoint Manager
================

**Jeremy Lerond, Jian Zhang PNNL**

 - Original Date: 10/27/2021
 - Revision Date: N/A
 

## Justification for New Feature ##

Currently, EnergyPlus does not support Supply Air Temperature (SAT) setpoint reset based on a system's Return Air Temperature (RAT). While not necessarily used for high-performance building, this type of SAT reset control strategy is widely used in the industry. A request to add such setpoint manager to EnergyPlus was made. The request proposed implementing two approaches: 1) an approach similar to the `SetpointManager:OutdoorAirReset` object but looking at RAT instead of outdoor air temperature, and 2) an approach similar to the `SetpointManager:ReturnTemperature:ChilledWater` object but applied to an air loop. After discussing with industry experts and gathering supportive information, it was found that 1) is the most widely used approach, the latter is mostly used for single-zone systems (which could use the `SetpointManager:SingleZone:Heating/Cooling` objects) or systems with very large air flows. Following this information, this NFP was developed to support the addition of 1).

## E-mail and  Conference Call Conclusions ##

## Overview ##

A new SetPoint Manager (SPM), `SetpointManager:ReturnAirReset`, will be added to EnergyPlus. This new SPM will set the SAT setpoint of an air loop (more specifically at a user-specified system node or node list) based on a user-defined linear relationship between the RAT and SAT. This SPM will be very similar to the `SetpointManager:OutdoorAirReset`.

## Approach ##

This SPM will set a temperature setpoint at a user-defined node or node list based on the temperature at another user-defined node which is assumed to be the system's return air node. The setpoint is determined using a linear relation defined by the user (see Input Description). The figure below shows a typical SAT=f(RAT) reset (from Guanghua, Wei et al.).

![Typical SAT reset based on RAT](NFP-RAT_SPM.png)

Because the approach used by this SPM is so similar to the existing `SetpointManager:OutdoorAirReset`, the calculations done by for the existing object will be reused and wrapped into a new SPM object.

## Testing/Validation/Data Sources ##

Unit tests checking the correct functioning of the new SPM will be implemented. Tests will include cases where both temperature is within and outside the temperature bounds of the linear relationship.

## Input Output Reference Documentation ##

A new subsection for the new SPM will be added to the I/O reference guide as follows:

\subsection{SetpointManager:ReturnAirReset}\label{setpointmanagerreturnairreset}

The Return Air Reset Setpoint Manager is used to place a setpoint temperature on a system node according to the return air temperature (or any other user-specified node) using a reset rule. The reset rule is determined by 2 points: the setpoint temperature at the return air high temperature (TSetAtRetHigh) and the setpoint temperature at the return air low temperature (TSetAtRetLow). If the return air temperature is above the return air high temperature limit, the setpoint temperature is set to TSetAtRetHigh. If the return air temperature is below the return air low temperature limit, the setpoint temperature is set to TSetAtRetLow. If the return air temperature is between the return air high and return air low temperatures limits, the setpoint temperature is linearly interpolated between TSetAtRetHigh and TSetAtRetLow. The return air temperature is obtained from the user-specified system node during the simulation. This setpoint manager can be used to place a setpoint temperature on air loop and plant loop system nodes.

The input consists of the setpoint manager name, a node list name of the nodes affected by the setpoint, the name of the return air node name, and the data for the reset rule: the setpoint temperature at the return air low temperature, the return air low temperature limit, the setpoint temperature at the return air high temperature, and the return air high temperature limit.

\subsubsection{Inputs}

\paragraph{Field: Name}

A unique, user-assigned name for an instance of a return air reset setpoint manager. Anywhere in the input that this setpoint manager is used, it is referred to by this name.

\paragraph{Field: Setpoint at Return Low Temperature}\label{field-setpoint-at-return-low-temperature}

The supply air temperature setpoint in \(^{o}\)C at the return low temperature for the reset rule.

\paragraph{Field: Return Low Temperature}\label{field-return-low-temperature}

The return air low temperature in \(^{o}\)C for the first supply air temperature reset rule. Generally, at this return air temperature the supply temperature is at its maximum.

\paragraph{Field: Setpoint at Return High Temperature}\label{field-setpoint-at-return-high-temperature}

The supply air temperature setpoint in \(^{o}\)C at the return high temperature for the reset rule.

\paragraph{Field: Return High Temperature}\label{field-return-high-temperature}

The return air high temperature in \(^{o}\)C for the first supply air temperature reset rule. Generally, at this return air temperature the supply temperature is at its minimum.

\paragraph{Field: Return Node Name}\label{field-return-node-name}

The name of an airloop return air system node which will be used as a reference to determine the setpoint established by this setpoint manager.

\paragraph{Field: Setpoint Node or NodeList Name}\label{field-setpoint-node-or-nodelist-name}

The name of a \hyperref[nodelist]{NodeList} object containing the names of the HVAC system nodes or the HVAC System Node Name for which temperature setpoints will be established by this setpoint manager.

Below is an example of the input for a Return Air Reset Setpoint Manager:

\begin{lstlisting}

SetpointManager:ReturnAirReset,
  Supply Air Temp Manager,    !- Name
  16.67,                      !- Setpoint at Return Low Temperature {C}
  20.0,                       !- Return Low Temperature {C}
  12.8,                       !- Setpoint at Return High Temperature {C}
  23.89,                      !- Return High Temperature {C}
  Return Air Node,            !- Return Air Node Name
  Supply Air Temp Nodes;      !- Setpoint Node or NodeList Name
\end{lstlisting}

## Input Description ##

```
SetpointManager:ReturnAirReset,
     \memo This Setpoint Manager is used to place a setpoint temperature on a system node
     \memo according to the return air temperature using a reset rule. The return air
     \memo temperature is obtained by retrieving the temperature of the user specified
     \memo return system node.
  A1 , \field Name
       \required-field
  N1 , \field Setpoint at Return Air Low Temperature
       \units C
       \required-field
  N2 , \field Return Air Low Temperature
       \units C
       \required-field
  N3 , \field Setpoint at Return Air High Temperature
       \units C
       \required-field
  N4 , \field Return Air High Temperature
       \units C
       \required-field
  A2 , \field Return Air Node Name
       \note Air loop return node
       \required-field
       \type node
  A3 ; \field Setpoint Node or NodeList Name
       \note Node(s) at which temperature will be set
       \required-field
       \type node
```

## Outputs Description ##

No new outputs will be created.

## Engineering Reference ##

A new subsection under Setpoint Managers will be created and contain the following text:

The input object SetpointManager:ReturnAirReset provides a setpoint manager that places a setpoint temperature on a system node according to the return air temperature (or any other user-specified system node) using a reset strategy. The user defines a reset rule for this strategy by specifying two setpoint temperatures at two return air temperatures. Generally the lower setpoint temperature is matched with the higher return air temperature and the higher setpoint temperature is matched with the lower return air temperature.

The parameter \(T_{set}\) is determined as per the following pseudo code:
```
If \({T_{ret,low}} < {T_{ret,high}}\) ~then
     If \({T_{ret}} \le {T_{ret,low}}\) ~then
          \({T_{set}} = {T_{set,atRetlow}}\)
     End If
     If \({T_{Ret}} \ge {T_{ret,high}}\) ~then
          \({T_{set}} = {T_{set,atRethigh}}\)
     Else
          \({T_{set}} = {T_{set,atRetlow}} - (({T_{ret}} - {T_{ret,low}})/({T_{ret,high}} - {T_{ret,low}}))\cdot ({T_{set,atRetlow}} - {T_{set,atRethigh}})\)
     End If
Else
     \({T_{set}} = 0.5({T_{set,atRetlow}} + {T_{set,atRethigh)}}\)
End IF
```
\emph{\({T_{set}}\)}will be applied to the node or nodes specified in the \emph{SetpointManager:ReturnAirReset} object input.

## Example File and Transition Changes ##

A new example file will be created for each new setpoint manager.

No transition changes are expected.

## References ##

- Guanghua, Wei & Turner, W. & Claridge, David & Liu, Mao. (2003). Single-Duct Constant Air Volume System Supply Air Temperature Reset: Using Return Air Temperature or Outside Air Temperature?. 10.1061/40699(2003)23. 