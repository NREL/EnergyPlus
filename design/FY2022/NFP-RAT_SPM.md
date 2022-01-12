Return Air Temperature & Humidity Reset Setpoint Manager
================

**Jeremy Lerond, Wooyoung Jung, Jian Zhang PNNL**

 - Original Date: 10/27/2021
 - Revision Date: N/A


## Justification for New Feature ##

Currently, EnergyPlus does not support Supply Temperature or Humidity Ratio Setpoint Reset based on a system's Return Temperature or Humidity ratio. While this method is not often used for high-performance building, these reset control strategies are widely used in the industry. Hence, a request to add such setpoint managers to EnergyPlus has been made and this new feature will deliver such needs.

The request can take two approaches: (1) an approach similar to the `SetpointManager:OutdoorAirReset` object, but looking at return temperature or humidity ratio to determine the setpoint, and (2) an approach similar to the `SetpointManager:ReturnTemperature:ChilledWater` object but applicable to an air loop as well. After discussing with industry experts and gathering supportive information, it was found that the former approach is the most widely used, the latter is mostly used for single-zone systems (which could use the `SetpointManager:SingleZone:Heating/Cooling` objects) or systems with very large air flows. Hence, this NFP will take the former approach.

## E-mail and  Conference Call Conclusions ##

## Overview ##

A new SetPoint Manager (SPM), `SetpointManager:SystemNodeReset`, will be added to EnergyPlus. This new SPM will set the temperature or humidity ratio setpoint at a user-specified system node or node list based on a user-defined linear relationship between the return temperature or humidity ratio and supply temperature or humidity ratio. This SPM will be similarly structured with the `SetpointManager:OutdoorAirReset`, but will be adjusted as it covers humidity ratio as one of the control variables.

## Approach ##

This SPM will set a temperature or humidity ratio setpoint at a user-defined node or node list based on the temperature or humidity ratio at another user-defined node which is set to be the system's return node. The setpoint is determined using a linear relationship defined by the user (see Input Description). The figure below shows a typical Supply Temperature = f(Return Temperature) reset [Guanghua et al. 2002]. The blue line represents the relationship between the return temperature and the supply temperature setpoint.

![Typical Supply Air Temperature reset based on Return Air Temperature](NFP-RAT_SPM.png)

The approach used by this SPM is similar to the existing `SetpointManager:OutdoorAirReset`, hence the calculations for the existing object will be similarly devised in the new SPM object.

## Testing/Validation/Data Sources ##

Unit tests checking the correct functioning of the new SPM will be implemented. Tests will include cases where both temperature or humidity ratio is within and outside the temperature or humidity ratio bounds of the linear relationship.

## Input Output Reference Documentation ##

A new subsection for the new SPM will be added to the I/O reference guide as follows:

\subsection{SetpointManager:SystemNodeReset}\label{setpointmanagersystemnodereset}

The System Node Reset Setpoint Manager is used to place a temperature or humidity ratio setpoint on a system node or any other user-specified node according to the return temperature or humidity ratio of a system node using a reset rule. The reset rule is determined by two points: the maximum and the minimum supply temperature or humidity ratio setpoints (MaxSpTemp and MinSpTemp or MaxSpHumRat and MinSpHumRat). In general, the higher the return temperature or humidity ratio, the lower the supply temperature or humidity ratio. Specifically, the maximum supply temperature or humidity ratio setpoints start decreasing linearly to the minimum supply temperature or humidity ratio setpoints once the return temperature or humidity ratio becomes higher than a certain point, i.e., the maximum return temperature or humidity ratio at maximum supply temperature or humidity ratio setpoints (MaxRetTempAtMaxSpTemp or MaxRetHumRatAtMaxSpHumRat). Also, once the return temperature or humidity ratio becomes higher than a certain point, the return temperature or humidity setpoint becomes the minimum, i.e., the minimum return temperature at minimum supply temperature or humidity ratio setpoint (MinRetTempAtMinSpTemp or MinRetHumRatAtMinSpHumRat).
The return temperature or humidity ratio is obtained from the user-specified system node during the simulation. This setpoint manager can be used to place a temperature or humidity ratio setpoint on HVAC system nodes.

The input consists of the setpoint manager name, the control variable (either temperature or humidity), a node list name of the nodes affected by the setpoint, the name of the return node name, and the data for the reset rule: the maximum and minimum temperature setpoints, the maximum return temperature at the maximum supply temperature setpoint, the temperature setpoint at the return high temperature, the return high temperature limit, the humidity ratio setpoint at the return low humidity ratio, the return low humidity limit, the humidity ratio setpoint at the return high humidity, the return high humidity limit.

\subsubsection{Inputs}

\paragraph{Field: Name}

A unique, user-assigned name for an instance of a return reset setpoint manager. Anywhere in the input that this setpoint manager is used, it is referred to by this name.

\paragraph{Field: Control Variable}

The type of variable that will be controlled. There are two key choices for this type of set point manager:
\begin{itemize}
  \item Temperature
  \item Humidity Ratio

\paragraph{Field: Maximum Supply Temperature Setpoint}\label{field-maximum-supply-temperature-setpoint}

The maximum supply temperature setpoint in \(^{o}\)C for the reset rule. e

\paragraph{Field: Minimum Supply Temperature Setpoint}\label{field-minimum-supply-temperature-setpoint}

The minimum supply temperature setpoint in \(^{o}\)C for the reset rule.

\paragraph{Field: Maximum Return Temperature at Maximum Supply Temperature Setpoint}\label{field-maximum-return-temperature-at-maximum-supply-temperature-setpoint}

The maximum return temperature in \(^{o}\)C that the maximum supply temperature setpoint gets applied. In other words, when the return temperature is lower than this value, the maximum return temperature will be applied.

\paragraph{Field: Minimum Return Temperature at Minimum Supply Temperature Setpoint}\label{field-minimum-return-temperature-at-minimum-supply-temperature-setpoint}

The minimum return temperature in \(^{o}\)C that the minimum supply temperature setpoint gets applied. In other words, when the return temperature is higher than this value, the minimum return temperature will be applied.

\paragraph{Field: Maximum Supply Humidity Ratio Setpoint}\label{field-maximum-supply-humidity-ratio-setpoint}

The maximum supply humidity ratio setpoint in kgWater/kgDryAir for the reset rule.

\paragraph{Field: Maximum Supply Humidity Ratio Setpoint}\label{field-maximum-supply-humidity-ratio-setpoint}

The minimum supply humidity ratio setpoint in kgWater/kgDryAir for the reset rule.

\paragraph{Field: Maximum Return Humidity Ratio at Maximum Supply Humidity Ratio Setpoint}\label{field-maximum-return-humidity-ratio-at-maximum-supply-humidity-ratio-setpoint}

The maximum return humidity ratio in kgWater/kgDryAir where the maximum supply humidity ratio gets applied. In other words, when the return humidity ratio is lower than this value, the maximum return humidity ratio will be applied.

\paragraph{Field: Minimum Return Humidity Ratio at Minimum Supply Humidity Ratio Setpoint}\label{field-return-high-humidity-ratio}

The minimum return humidity ratio in kgWater/kgDryAir where the minimum supply humidity ratio gets applied. In other words, when the return humidity ratio is higher than this value, the minimum return humidity ratio will be applied.

\paragraph{Field: Return Node Name}\label{field-return-node-name}

The name of a return system node which will be used as a reference to determine the setpoint established by this setpoint manager.

\paragraph{Field: Setpoint Node or NodeList Name}\label{field-setpoint-node-or-nodelist-name}

The name of a \hyperref[nodelist]{NodeList} object containing the names of the HVAC system nodes or the HVAC System Node Name for which temperature setpoints will be established by this setpoint manager.

Below are examples, showing the inputs for SetpointManager:SystemNodeReset.

\begin{lstlisting}

SetpointManager:SystemNodeReset,
  Supply Air Temp Manager,    !- Name
  Temperature,                !- Control Variable
  16.7,                       !- Maximum Supply Temperature Setpoint {C}
  20.0,                       !- Minimum Supply Temperature Setpoint {C}
  12.8,                       !- Maximum Return Temperature at Maximum Supply Temperature Setpoint {C}
  23.9,                       !- Minimum Return Temperature at Minimum Supply Temperature Setpoint {C}
  ,                           !- Maximum Supply Humidity Ratio Setpoint {kgWater/kgDryAir}
  ,                           !- Minimum Supply Humidity Ratio Setpoint {kgWater/kgDryAir}
  ,                           !- Maximum Return Humidity Ratio at Maximum Supply Humidity Ratio Setpoint {kgWater/kgDryAir}
  ,                           !- Minimum Return Humidity Ratio at Minimum Supply Humidity Ratio Setpoint {kgWater/kgDryAir}
  Return Air Node,            !- Return Node Name
  Supply Air Temp Nodes,      !- Setpoint Node or NodeList Name

SetpointManager:SystemNodeReset,
  Supply Humidity Manager,       !- Name
  Humidity Ratio,                !- Control Variable
  ,                              !- Maximum Supply Temperature Setpoint {C}
  ,                              !- Minimum Supply Temperature Setpoint {C}
  ,                              !- Maximum Return Temperature at Maximum Supply Temperature Setpoint {C}
  ,                              !- Minimum Return Temperature at Minimum Supply Temperature Setpoint {C}
  0.008,                         !- Maximum Supply Humidity Ratio Setpoint {kgWater/kgDryAir}
  0.004,                         !- Minimum Supply Humidity Ratio Setpoint {kgWater/kgDryAir}
  0.003,                         !- Maximum Return Humidity Ratio at Maximum Supply Humidity Ratio Setpoint {kgWater/kgDryAir}
  0.010,                         !- Minimum Return Humidity Ratio at Minimum Supply Humidity Ratio Setpoint {kgWater/kgDryAir}
  Return Air Node,               !- Return Node Name
  Supply Air Temp Nodes,         !- Setpoint Node or NodeList Name
\end{lstlisting}

## Input Description ##
A new `SetpointManager:SystemNodeReset` object will be added as follows:

```
SetpointManager:SystemNodeReset,
     \memo This Setpoint Manager is used to place a temperature or humidity ratio setpoint on a system node
     \memo according to the return temperature or humidity ratio using a reset rule. The return temperature or humidity ratio
     \memo is obtained by retrieving the temperature or humidity ratio of the user specified return system node.
  A1 , \field Name
       \required-field
  A2 , \field Control Variable
       \required-field
       \type choice
       \key Temperature
       \key Humidity Ratio
  N1 , \field Maximum Supply Temperature Setpoint
       \units C
       \note Applicable only if Control Variable is Temperature
  N2 , \field Minimum Supply Temperature Setpoint
       \units C
       \note Applicable only if Control Variable is Temperature
  N3 , \field Maximum Return Temperature at Maximum Supply Temperature Setpoint
       \units C
       \note Applicable only if Control Variable is Temperature
  N4 , \field Minimum Return Temperature at Minimum Supply Temperature Setpoint
       \units C
       \note Applicable only if Control Variable is Temperature
  N1 , \field Maximum Supply Humidity Ratio Setpoint
       \units kgWater/kgDryAir
       \note Applicable only if Control Variable is Humidity Ratio
  N2 , \field Minimum Supply Humidity Ratio Setpoint
       \units kgWater/kgDryAir
       \note Applicable only if Control variable is Humidity Ratio
  N3 , \field Maximum Humidity Ratio at Maximum Supply Humidity Ratio Setpoint
       \units kgWater/kgDryAir
       \note Applicable only if Control variable is Humidity Ratio
  N4 , \field Minimum Humidity Ratio at Minimum Supply Humidity Ratio Setpoint
       \units kgWater/kgDryAir
       \note Applicable only if Control variable is Humidity Ratio    
  A3 , \field Return Node Name
       \note The name of an HVAC system node
       \required-field
       \type node
  A4 ; \field Setpoint Node or NodeList Name
       \note Node(s) for which temperature or humidity ratio will be set
       \required-field
       \type node
```

## Outputs Description ##

No new outputs will be created.

## Engineering Reference ##

A new subsection under Setpoint Managers will be created and contain the following text:

The input object SetpointManager:SystemNodeReset provides a setpoint manager that places a temperature or humidity setpoint on a system node (or any other user-specified system node) according to the return temperature or humidity using a reset strategy. The user defines a reset rule for this strategy by specifying two supply temperature or humidity ratio setpoints at two return air temperatures or humidity ratios. Generally the lower supply temperature (or humidity ratio) setpoint is matched with the higher return air temperature (or humidity ratio) and vice versa.

The parameter \(T_{set}\) is determined as per the following pseudo code:
```
If \({T_{ret,low}} < {T_{ret,high}}\) ~then
     If \({T_{ret}} \le {T_{ret,low}}\) ~then
          \({T_{set}} = {T_{set,atRetlow}}\)
     End If
     If \({T_{ret}} \ge {T_{ret,high}}\) ~then
          \({T_{set}} = {T_{set,atRethigh}}\)
     Else
          \({T_{set}} = {T_{set,atRetlow}} - (({T_{ret}} - {T_{ret,low}})/({T_{ret,high}} - {T_{ret,low}}))\cdot ({T_{set,atRetlow}} - {T_{set,atRethigh}})\)
     End If
Else
     \({T_{set}} = 0.5({T_{set,atRetlow}} + {T_{set,atRethigh)}}\)
End IF
```
\emph{\({T_{set}}\)} will be applied to the node or nodes specified in the \emph{SetpointManager:SystemNodeReset} object input.

When humidity ratio is selected as the control variable, the same pseudo code, but addressing humidity ratio, will be applied.

## Example File and Transition Changes ##

A new example idf file using SetpointManager:SystemNodeReset will be created to demonstrate how this new feature can be used. The Output:Variable objects in the example idf will be added so that this example file produces the output files showing how SetpointManager:SystemNodeReset plays a role to determine the setpoint.

No transition changes are expected.

## References ##

- Guanghua, Wei & Turner, W. & Claridge, David & Liu, Mao. (2003). Single-Duct Constant Air Volume System Supply Air Temperature Reset: Using Return Air Temperature or Outside Air Temperature?. 10.1061/40699(2003)23.

# Design Document #

This new feature revises modules: SetpointManager.

## Changes to the IDD file ##
A new object called SetpointManager:SystemNodeReset, as shown in the Input Description Section, will be added to the idd file.

## Setpoint Manager ##
This code change adds a new structure called `DefineSystemNdResetSetPointManager` to the heading file (`SetPointManager.hh`) and the other changes are made to `SetPointManager.cc`.

The inputs will be grabbed through the `GetSetPointManagerInputData` function, similar with other SetpointManager objects. The following are the cases that the error gets produced: (1) when the invalid control variable is inputted, (2) the maximum temperature or humidity ratio setpoint is higher than the minimum temperature or humidity ratio setpoint.

The new function called `DefineSystemNdResetSetPointManager::calculate` will determine which input parameters will be used for setpoint calculation depending on the selected control variable. In addition, the function called `DefineSystemNdResetSetPointManager::CalcSetPoint` will calculate the setpoint using the logic introduced in the pseudo code above. The calculated setpoint gets applied through `InitSetPointManagers`, `SimSetPointManagers`, and `UpdateSetPointManagers` functions, similar with other SetpointManager objects.
