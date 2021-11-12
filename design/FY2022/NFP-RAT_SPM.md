Return Air Temperature & Humidity Reset Setpoint Manager
================

**Jeremy Lerond, Wooyoung Jung, Jian Zhang PNNL**

 - Original Date: 10/27/2021
 - Revision Date: N/A


## Justification for New Feature ##

Currently, EnergyPlus does not support (1) Supply Air Temperature (SAT) setpoint reset based on a system's Return Air Temperature (RAT) and (2) Supply Air Humidity (SAH) setpoint reset based on a system's Return Air Humidity (RAH). While not necessarily used for high-performance building, the SAT or SAH reset control strategy is widely used in the industry. Hence, a request to add such setpoint managers to EnergyPlus has been made and this new feature will deliver such needs.

The request can take two approaches: (1) an approach similar to the `SetpointManager:OutdoorAirReset` object, but looking at RAT or RAH instead of outdoor air temperature or outdoor air humidity, and (2) an approach similar to the `SetpointManager:ReturnTemperature:ChilledWater` object but applied to an air loop. After discussing with industry experts and gathering supportive information, it was found that the former approach is the most widely used, the latter is mostly used for single-zone systems (which could use the `SetpointManager:SingleZone:Heating/Cooling` objects) or systems with very large air flows. Following this information, this NFP was selected the former approach.

## E-mail and  Conference Call Conclusions ##

## Overview ##

A new SetPoint Manager (SPM), `SetpointManager:ReturnAirReset`, will be added to EnergyPlus. The former will set the SAT (or SAH) setpoint of an air loop - more specifically at a user-specified system node or node list - based on a user-defined linear relationship between the RAT and SAT (or between the RAH and SAH). This SPM will be very similar to the `SetpointManager:OutdoorAirReset`.  

## Approach ##

This SPM will set a temperature setpoint at a user-defined node or node list based on the temperature (or humidity) at another user-defined node which is assumed to be the system's return air node. The setpoint is determined using a linear relation defined by the user (see Input Description). The figure below shows a typical SAT=f(RAT) reset [Guanghua et al. 2002].

![Typical SAT reset based on RAT](NFP-RAT_SPM.png)

Because the approach used by this SPM is similar to the existing `SetpointManager:OutdoorAirReset`, the calculations for the existing object will be similarly replicated into the new SPM object.

## Testing/Validation/Data Sources ##

Unit tests checking the correct functioning of the new SPM will be implemented. Tests will include cases where both temperature (or humidity) is within and outside the temperature (or humidity) bounds of the linear relationship.

## Input Output Reference Documentation ##

A new subsection for the new SPM will be added to the I/O reference guide as follows:

\subsection{SetpointManager:ReturnAirReset}\label{setpointmanagerreturnairreset}

The Return Air Reset Setpoint Manager is used to place a setpoint temperature or a setpoint humidity on a system node (or any other user-specified node) according to the return air using a reset rule. The reset rule is determined by 2 points: the setpoint temperature (or humidity) at the return air high temperature (or humidity) (TSetAtRetHigh (or HSetAtRetHigh)) and the setpoint temperature (or humidity) at the return air low temperature (or humidity) (TSetAtRetLow (or HSetAtRetLow)). If the return air temperature (or humidity) is above the return air high temperature (or humidity) limit, the setpoint temperature (or humidity) is set to TSetAtRetHigh (or HSetAtRetHigh). If the return air temperature (or humidity) is below the return air low temperature (or humidity) limit, the setpoint temperature/humidity is set to TSetAtRetLow (or HSetAtRetLow). If the return air temperature (or humidity) is between the return air high and return air low temperature (or humidity) limits, the setpoint temperature (or humidity) is linearly interpolated between TSetAtRetHigh (or HSetAtRetHigh) and TSetAtRetLow (or HSetAtRetLow). The return air temperature (or humidity) is obtained from the user-specified system node during the simulation. This setpoint manager can be used to place a setpoint temperature (or humidity) on air loop and plant loop system nodes.

The input consists of the setpoint manager name, the control variable (either temperature or humidity), a node list name of the nodes affected by the setpoint, the name of the return air node name, and the data for the reset rule: the setpoint temperature at the return air low temperature, the return air low temperature limit, the setpoint temperature at the return air high temperature, the return air high temperature limit, the setpoint humidity ratio at the return air low humidity ratio, the return air low humidity ratio limit, the setpoint humidity ratio at the return air high humidity ratio, the return air high humidity ratio limit.

\subsubsection{Inputs}

\paragraph{Field: Name}

A unique, user-assigned name for an instance of a return air reset setpoint manager. Anywhere in the input that this setpoint manager is used, it is referred to by this name.

\paragraph{Field: Control Variable}

The type of variable that will be controlled. There are two key choices for this type of set point manager:
\begin{itemize}
  \item Temperature
  \item Humidity Ratio

\paragraph{Field: Setpoint at Return Low Temperature}\label{field-setpoint-at-return-low-temperature}

The supply air temperature setpoint in \(^{o}\)C at the return low temperature for the reset rule.

\paragraph{Field: Return Low Temperature}\label{field-return-low-temperature}

The return air low temperature in \(^{o}\)C for the first supply air temperature reset rule. Generally, at this return air temperature the supply temperature is at its maximum.

\paragraph{Field: Setpoint at Return High Temperature}\label{field-setpoint-at-return-high-temperature}

The supply air temperature setpoint in \(^{o}\)C at the return high temperature for the reset rule.

\paragraph{Field: Return High Temperature}\label{field-return-high-temperature}

The return air high temperature in \(^{o}\)C for the first supply air temperature reset rule. Generally, at this return air temperature the supply temperature is at its minimum.

\paragraph{Field: Setpoint at Return Low Humidity Ratio}\label{field-setpoint-at-return-low-humidity-ratio}

The supply air humidity ratio setpoint at the return low humidity ratio (kgWater/kgDryAir) for the reset rule.

\paragraph{Field: Return Low Humidity Ratio}\label{field-return-low-humidity-ratio}

The return air low humidity ratio (kgWater/kgDryAir) for the first supply air humidity ratio reset rule. Generally, at this return air humidity ratio, the supply humidity ratio is at its maximum.

\paragraph{Field: Setpoint at Return High Humidity Ratio}\label{field-setpoint-at-return-high-humidity-ratio}

The supply air humidity ratio setpoint at the return high humidity ratio (kgWater/kgDryAir) for the reset rule.

\paragraph{Field: Return High Humidity Ratio}\label{field-return-high-humidity-ratio}

The return air high humidity ratio (kgWater/kgDryAir) for the first supply air humidity ratio reset rule. Generally, at this return air humidity ratio, the supply humidity ratio is at its minimum.

\paragraph{Field: Return Node Name}\label{field-return-node-name}

The name of an airloop return air system node which will be used as a reference to determine the setpoint established by this setpoint manager.

\paragraph{Field: Setpoint Node or NodeList Name}\label{field-setpoint-node-or-nodelist-name}

The name of a \hyperref[nodelist]{NodeList} object containing the names of the HVAC system nodes or the HVAC System Node Name for which temperature setpoints will be established by this setpoint manager.

Below is an example of the input for a Return Air Reset Setpoint Manager:

\begin{lstlisting}

SetpointManager:ReturnAirReset,
  Supply Air Temp Manager,    !- Name
  Temperature,                !- Control Variable
  16.67,                      !- Setpoint at Return Low Temperature {C}
  20.0,                       !- Return Low Temperature {C}
  12.8,                       !- Setpoint at Return High Temperature {C}
  23.89,                      !- Return High Temperature {C}
  ,                           !- Setpoint at Return Low Humidity Ratio
  ,                           !- Return Low Humidity Ratio
  ,                           !- Setpoint at Return High Humidity Ratio
  ,                           !- Return High Humidity Ratio
  Return Air Node,            !- Return Air Node Name
  Supply Air Temp Nodes,      !- Setpoint Node or NodeList Name
\end{lstlisting}

\begin{lstlisting}

SetpointManager:ReturnAirReset,
  Supply Air HumidityRatio Manager,    !- Name
  HumidityRatio,                       !- Control Variable
  ,                                    !- Setpoint at Return Low Temperature {C}
  ,                                    !- Return Low Temperature {C}
  ,                                    !- Setpoint at Return High Temperature {C}
  ,                                    !- Return High Temperature {C}
  0.010,                               !- Setpoint at Return Low Humidity Ratio
  0.004,                               !- Return Low Humidity Ratio
  0.001,                               !- Setpoint at Return High Humidity Ratio
  0.015,                               !- Return High Humidity Ratio
  Return Air Node,                     !- Return Air Node Name
  Supply Air Temp Nodes,               !- Setpoint Node or NodeList Name
\end{lstlisting}


## Input Description ##

```
SetpointManager:ReturnAirReset,
     \memo This Setpoint Manager is used to place a setpoint temperature/humidity ratio on a system node
     \memo according to the return air temperature/humidity ratio using a reset rule. The return air
     \memo temperature/humidity ratio is obtained by retrieving the temperature/humidity ratio of the user specified
     \memo return system node.
  A1 , \field Name
       \required-field
  A2 , \field Control Variable
       \required-field
       \type choice
       \key Temperature
       \key HumidityRatio
  N1 , \field Setpoint at Return Air Low Temperature
       \units C
       \note Applicable only if Control variable is Temperature
  N2 , \field Return Air Low Temperature
       \units C
       \note Applicable only if Control variable is Temperature
  N3 , \field Setpoint at Return Air High Temperature
       \units C
       \note Applicable only if Control variable is Temperature
  N4 , \field Return Air High Temperature
       \units C
       \note Applicable only if Control variable is Temperature
  N5 , \field Setpoint at Return Low Humidity Ratio
       \units kgWater/kgDryAir
       \note Applicable only if Control variable is HumidityRatio
       \type real
  N6 , \field Return Low Humidity Ratio
       \units kgWater/kgDryAir
       \type real
       \note Applicable only if Control variable is HumidityRatio
  N7 , \field Setpoint at Return High Humidity Ratio
       \units kgWater/kgDryAir
       \type real
       \note Applicable only if Control variable is HumidityRatio
  N8 , \field Return High Humidity Ratio
       \units kgWater/kgDryAir
       \type real
       \note Applicable only if Control variable is HumidityRatio
  A3 , \field Return Air Node Name
       \note Air loop return node
       \required-field
       \type node
  A4 ; \field Setpoint Node or NodeList Name
       \note Node(s) at which temperature will be set
       \required-field
       \type node
```

## Outputs Description ##

No new outputs will be created.

## Engineering Reference ##

A new subsection under Setpoint Managers will be created and contain the following text:

The input object SetpointManager:ReturnAirReset provides a setpoint manager that places a setpoint temperature or humidity ratio on a system node according to the return air temperature or humidity ratio (or any other user-specified system node) using a reset strategy. The user defines a reset rule for this strategy by specifying two setpoint temperatures or humidity ratios at two return air temperatures or humidity ratios. Generally the lower setpoint temperature (or humidity ratio) is matched with the higher return air temperature (or humidity ratio) and the higher setpoint temperature (or humidity ratio) is matched with the lower return air temperature (or humidity ratio).

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
When humidity ratio is selected as the control variable, the same pseudo code, but addressing humidity ratio, will be applied.

## Example File and Transition Changes ##

A new example file will be created for each new setpoint manager.

No transition changes are expected.

## References ##

- Guanghua, Wei & Turner, W. & Claridge, David & Liu, Mao. (2003). Single-Duct Constant Air Volume System Supply Air Temperature Reset: Using Return Air Temperature or Outside Air Temperature?. 10.1061/40699(2003)23.
