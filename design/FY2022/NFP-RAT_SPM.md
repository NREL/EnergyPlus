System Node Temperature & Humidity Ratio Reset Setpoint Manager
================

**Jeremy Lerond, Wooyoung Jung, Jian Zhang PNNL**

 - Original Date: 10/27/2021
 - Revision Date: 01/14/2022

## Justification for New Feature ##

Currently, EnergyPlus does not support Temperature or Humidity Ratio Setpoint Reset based on a system's Reference (e.g., Return) Temperature or Humidity ratio. While this method is not often used for high-performance building, these reset control strategies are widely used in the industry. Hence, a request to add such SetPoint Managers (SPMs) to EnergyPlus has been made and this new feature will deliver such needs.

The request can take two approaches: (1) an approach similar to the `SetpointManager:OutdoorAirReset` object, but looking at reference temperature or humidity ratio to determine the setpoint, and (2) an approach similar to the `SetpointManager:ReturnTemperature:ChilledWater` object but applicable to an air loop as well. After discussing with industry experts and gathering supportive information, it was found that the former approach is the most widely used, the latter is mostly used for single-zone systems (which could use the `SetpointManager:SingleZone:Heating/Cooling` objects) or systems with very large air flows. Hence, this NFP will take the former approach.

## E-mail and  Conference Call Conclusions ##

On 11/01/21 in Slack, the initial NFP was announced using temperature and including humidity as one of the control variables was suggested & accepted.

On 01/26/22 during the technicality call after the design document announcement on 01/14 in Slack, it was determined by the team that two objects (`SetpointManager:SystemNodeReset:Temperature` and `SetpointManager:SystemNodeReset:Humidity`) will be developed for the clarity.

## Overview ##

Two new SPMs, `SetpointManager:SystemNodeReset:Temperature` and `SetpointManager:SystemNodeReset:Humidity`, will be added to EnergyPlus. These new SPMs will set the temperature or humidity ratio setpoint at a user-specified system node or node list based on a user-defined linear relationship between the reference temperature or humidity ratio and temperature or humidity ratio setpoints. This SPM will be similarly structured with the `SetpointManager:OutdoorAirReset`, but will be adjusted as it covers humidity ratio as one of the control variables.

## Approach ##

These SPMs will set a temperature or humidity ratio setpoint at a user-defined node or node list based on the temperature or humidity ratio at another user-defined node which is set to be the system's reference node. The setpoint is determined using a linear relationship defined by the user (see Input Description). The figure below shows a typical Temperature Setpoint = f(Reference Temperature) reset [Guanghua et al. 2002]. The blue line represents the relationship between the reference temperature and the temperature setpoint.

![Typical Temperature Setpoint reset based on Reference (i.e., Return) Air Temperature](NFP-RAT_SPM.png)

The approach used by this SPM is similar to the existing `SetpointManager:OutdoorAirReset`, hence the calculations for the existing object will be similarly devised in the new SPM object.

## Testing/Validation/Data Sources ##

Two new unit tests checking the correct functioning of the new SPMs will be implemented in the existing SetPointManager.unit.cc. The tests will include cases where both temperature and humidity ratio are within and outside the temperature and humidity ratio bounds of the linear relationship.

## Input Output Reference Documentation ##

A new subsection for the new SPM will be added to the I/O reference guide as follows:

\subsection{SetpointManager:SystemNodeReset:Temperature}\label{setpointmanagersystemnoderesettemperature}

The System Node Reset Setpoint Manager is used to place a temperature setpoint on a system node according to the reference temperature (e.g., return air or outdoor air temperature) of a system node using a linear interpolation between two user-specified reference values and two user-specified setpoint values. In general, the higher the reference temperature, the lower the setpoint. During the simulation, the reference temperature is obtained from the user-specified reference system node.

The input consists of the setpoint manager name, the control variable, the name of the node or node list affected by the setpoint, the name of the reference node name, and the data for the reset rule: setpoints at low and high reference temperatures, and low and high reference temperatures.

\subsubsection{Inputs}

\paragraph{Field: Name}

A unique, user-assigned name for an instance of a system node reset setpoint manager. Anywhere in the input that this setpoint manager is used, it is referred to by this name.

\paragraph{Field: Control Variable}

The type of variable that will be controlled. There are three key choices for this type of set point manager:
\begin{itemize}
  \item Temperature
  \item MaximumTemperature
  \item MinimumTemperature

\paragraph{Field: Setpoint at Low Reference Temperature}\label{field-setpoint-at-low-reference-temperature}

The temperature setpoint in \(^{o}\)C at the low reference temperature for the reset rule.

\paragraph{Field: Setpoint at High Reference Temperature}\label{field-setpoint-at-low-reference-temperature}

The temperature setpoint in \(^{o}\)C at the high reference temperature for the reset rule.

\paragraph{Field: Low Reference Temperature}\label{field-low-reference-temperature}

The low reference temperature in \(^{o}\)C for the reset rule. When the reference temperature is lower than this value, the temperature setpoint is at its maximum.

\paragraph{Field: High Reference Temperature}\label{field-high-reference-temperature}

The high reference temperature in \(^{o}\)C for the reset rule. When the reference temperature is higher than this value, the temperature setpoint is at its minimum.

\paragraph{Field: Reference Node Name}\label{field-reference-node-name}

The name of a reference system node which will be used as a reference to determine the setpoint established by this setpoint manager.

\paragraph{Field: Setpoint Node or NodeList Name}\label{field-setpoint-node-or-nodelist-name}

The name of a \hyperref[nodelist]{NodeList} object containing the names of the HVAC system nodes or the HVAC System Node Name for which temperature setpoints will be established by this setpoint manager.

Below are examples, showing the inputs for SetpointManager:SystemNodeReset:Temperature.

\begin{lstlisting}

SetpointManager:SystemNodeReset:Temperature,
  Supply Temp Manager,              !- Name
  Temperature,                      !- Control Variable
  16.7,                             !- Setpoint at Low Reference Temperature {C}
  12.8,                             !- Setpoint at High Reference Temperature {C}
  20.0,                             !- Low Reference Temperature {C}
  23.3,                             !- High Reference Temperature {C}
  Plenum-1 Out Node,                !- Reference Node Name
  VAV Sys 1 Outlet Node;            !- Setpoint Node or NodeList Name
\end{lstlisting}

\subsection{SetpointManager:SystemNodeReset:Humidity}\label{setpointmanagersystemnoderesethumidity}

The System Node Reset Setpoint Manager is used to place a humidity ratio setpoint on a system node according to the reference humidity ratio (e.g., return air or outdoor air humidity ratio) of a system node using a linear interpolation between two user-specified reference values and two user-specified setpoint values. In general, the higher the reference humidity ratio, the lower the setpoint. During the simulation, the reference humidity ratio is obtained from the user-specified reference system node.

The input consists of the setpoint manager name, the control variable, the name of the node or node list affected by the setpoint, the name of the reference node name, and the data for the reset rule: setpoints at low and high reference humidity ratios, and low and high reference humidity ratios.

\subsubsection{Inputs}

\paragraph{Field: Name}

A unique, user-assigned name for an instance of a system node reset setpoint manager. Anywhere in the input that this setpoint manager is used, it is referred to by this name.

\paragraph{Field: Control Variable}

The type of variable that will be controlled. There are three key choices for this type of set point manager:
\begin{itemize}
  \item HumidityRatio
  \item MaximumHumidityRatio
  \item MinimumHumidityRatio

\paragraph{Field: Setpoint at Low Reference Humidity Ratio}\label{field-setpoint-at-low-reference-humidity-ratio}

The humidity ratio setpoint in \kg\of{W}\per\kg\of{DA} at the low reference humidity ratio for the reset rule.

\paragraph{Field: Setpoint at High Reference Humidity Ratio}\label{field-setpoint-at-high-reference-humidity-ratio}

The humidity ratio setpoint in \kg\of{W}\per\kg\of{DA} at the high reference humidity ratio for the reset rule.

\paragraph{Field: Low Reference Humidity Ratio}\label{field-low-reference-humidity-ratio}

The low reference humidity ratio in \kg\of{W}\per\kg\of{DA} for the reset rule. When the reference humidity ratio is lower than this value, the humidity ratio setpoint is at its maximum.

\paragraph{Field: High Reference Humidity Ratio}\label{field-high-reference-humidity-ratio}

The high reference humidity ratio in \kg\of{W}\per\kg\of{DA} for the reset rule. When the reference humidity ratio is higher than this value, the humidity ratio setpoint is at it minimum.

\paragraph{Field: Reference Node Name}\label{field-reference-node-name}

The name of a reference system node which will be used as a reference to determine the setpoint established by this setpoint manager.

\paragraph{Field: Setpoint Node or NodeList Name}\label{field-setpoint-node-or-nodelist-name}

The name of a \hyperref[nodelist]{NodeList} object containing the names of the HVAC system nodes or the HVAC System Node Name for which temperature setpoints will be established by this setpoint manager.

Below are examples, showing the inputs for SetpointManager:SystemNodeReset:Humidity.

\begin{lstlisting}

SetpointManager:SystemNodeReset:Humidity,
  Supply Humidity Manager,                !- Name
  MaximumHumidityRatio,                   !- Control Variable
  0.00924,                                !- Setpoint at Low Reference Humidity Ratio (kgWater/kgDryAir)
  0.00600,                                !- Setpoint at High Reference Humidity Ratio (kgWater/kgDryAir)
  0.00850,                                !- Low Reference Humidity Ratio (kgWater/kgDryAir)
  0.01000,                                !- High Reference Humidity Ratio (kgWater/kgDryAir)
  Plenum-1 Out Node,                      !- Reference Node Name
  DOAS Cooling Coil Outlet;               !- Setpoint Node or NodeList Name
\end{lstlisting}

## Input Description ##
Two new `SetpointManager:SystemNodeReset:Temperature` and `SetpointManager:SystemNodeReset:Humidity` objects will be added as follows:

```
SetpointManager:SystemNodeReset:Temperature,
     \memo This Setpoint Manager is used to place a temperature setpoint on a system node according to the reference (e.g., return) temperature using a reset rule.
     \memo The temperature setpoint is obtained by retrieving the temperature of the user specified reference system node.
  A1 , \field Name
       \required-field
  A2 , \field Control Variable
       \required-field
       \type choice
       \key Temperature
       \key MaximumTemperature
       \key MinimumTemperature
  N1 , \field Setpoint at Low Reference Temperature
       \units C
  N2 , \field Setpoint at High Reference Temperature
       \units C
  N3 , \field Low Reference Temperature
       \units C
  N4 , \field High Reference Temperature
       \units C
  A3 , \field Reference Node Name
       \note The name of an HVAC system node.
       \required-field
       \type node
  A4 ; \field Setpoint Node or NodeList Name
       \note Node(s) for which the temperature setpoint will be set.
       \required-field
       \type node
```

```
SetpointManager:SystemNodeReset:Humidity,
     \memo This Setpoint Manager is used to place a humidity ratio setpoint on a system node according to the reference (e.g., return) humidity ratio using a reset rule.
     \memo The humidity ratio setpoint is obtained by retrieving the humidity ratio of the user specified reference system node.
  A1 , \field Name
       \required-field
  A2 , \field Control Variable
       \required-field
       \type choice
       \key HumidityRatio
       \key MaximumHumidityRatio
       \key MinimumHumidityRatio
  N1 , \field Setpoint at Low Reference Humidity Ratio
       \units kgWater/kgDryAir
  N2 , \field Setpoint at High Reference Humidity Ratio
       \units kgWater/kgDryAir
  N3 , \field Low Reference Humidity Ratio
       \units kgWater/kgDryAir
  N4 , \field High Reference Humidity Ratio
       \units kgWater/kgDryAir
  A3 , \field Reference Node Name
       \note The name of an HVAC system node.
       \required-field
       \type node
  A4 ; \field Setpoint Node or NodeList Name
       \note Node(s) for which the humidity ratio will be set.
       \required-field
       \type node
```

## Outputs Description ##

No new outputs will be created.

## Engineering Reference ##

Two new subsections under the SPMs section will be created and contain the following text:

The input object SetpointManager:SystemNodeReset:Temperature provides a setpoint manager that places a temperature setpoint on a system node according to the reference (e.g., return) temperature using a reset strategy. The user defines a reset rule for this strategy by specifying two temperature setpoints at two reference temperatures. Generally the lower temperature setpoint is matched with the higher reference temperature.

The parameter \(T_{set}\) is determined as per the following pseudo code:
```
If \({T_{ref,low}} < {T_{ref,high}}\) ~then
     If \({T_{ref}} \le {T_{ref,low}}\) ~then
          \({T_{set}} = {T_{set,atReflow}}\)
     End If
     If \({T_{ref}} \ge {T_{ref,high}}\) ~then
          \({T_{set}} = {T_{set,atRefhigh}}\)
     Else
          \({T_{set}} = {T_{set,atReflow}} - (({T_{ref}} - {T_{ref,low}})/({T_{ref,high}} - {T_{ref,low}}))\cdot ({T_{set,atReflow}} - {T_{set,atRefhigh}})\)
     End If
Else
     \({T_{set}} = 0.5({T_{set,atReflow}} + {T_{set,atRefhigh)}}\)
End IF
```
\emph{\({T_{set}}\)} will be applied to the node or nodes specified in the \emph{SetpointManager:SystemNodeReset:Temperature} object input.

The input object SetpointManager:SystemNodeReset:Humidity provides a setpoint manager that places a humidity ratio setpoint on a system node according to the reference (e.g., return) humidity ratio using a reset strategy. The user defines a reset rule for this strategy by specifying two temperature setpoints at two reference temperatures. Generally the lower temperature setpoint is matched with the higher reference temperature.

The parameter \(H_{set}\) is determined as per the following pseudo code:
```
If \({H_{ref,low}} < {H_{ref,high}}\) ~then
     If \({H_{ref}} \le {H_{ref,low}}\) ~then
          \({H_{set}} = {H_{set,atReflow}}\)
     End If
     If \({H_{ref}} \ge {H_{ref,high}}\) ~then
          \({H_{set}} = {H_{set,atRefhigh}}\)
     Else
          \({H_{set}} = {H_{set,atReflow}} - (({H_{ref}} - {H_{ref,low}})/({H_{ref,high}} - {H_{ref,low}}))\cdot ({H_{sef,atReflow}} - {H_{set,atRefhigh}})\)
     End If
Else
     \({H_{set}} = 0.5({H_{set,atReflow}} + {H_{set,atRefhigh)}}\)
End IF
```
\emph{\({H_{set}}\)} will be applied to the node or nodes specified in the \emph{SetpointManager:SystemNodeReset:Humidity} object input.

## Example File and Transition Changes ##

A new example idf file using the new `SetpointManager:SystemNodeReset:Temperature` and `SetpointManager:SystemNodeReset:Humidity` objects will be created to demonstrate how this new feature can be used. This example file will demonstrate how this new SetpointManager will be applied to air loop and plant loop nodes. Also, the necessary `Output:Variable` objects in the example idf will be added so that this example file produces the output files showing how `SetpointManager:SystemNodeReset:Temperature` and `SetpointManager:SystemNodeReset:Humidity` plays a role to determine the setpoints.

No transition changes are expected.

## References ##

- Guanghua, Wei & Turner, W. & Claridge, David & Liu, Mao. (2003). Single-Duct Constant Air Volume System Supply Air Temperature Reset: Using Return Air Temperature or Outside Air Temperature?. 10.1061/40699(2003)23.

# Design Document #

This new feature revises modules: SetpointManager.

## Changes to the IDD file ##
A new object called `SetpointManager:SystemNodeReset:Temperature`, `SetpointManager:SystemNodeReset:Humidity`, as shown in the Input Description Section, will be added to the idd file.

## Setpoint Manager ##
This code change adds one new structure called `DefineSysNodeResetSetPointManager` to the header file (`SetPointManager.hh`), the other changes are made to `SetPointManager.cc`.

The inputs will be grabbed through the `GetSetPointManagerInputData` function, similar with other SetpointManager objects. The following are the cases that the error gets produced: (1) when the invalid control variable is inputted, (2) the maximum temperature or humidity ratio setpoint is higher than the minimum temperature or humidity ratio setpoint.

A new functions called `DefineSysNodeResetSetPointManager::calculate` will calculate the setpoint based on (1) the inputs provided by the users and (2) the reference node. Since the calculation for the setpoint is the same as the SetpointManager:OutdoorAirReset (`DefineOutsideAirSetPointManager::CalcSetPoint`), the function used to calculate its value will be moved outside the `DefineOutsideAirSetPointManager` struct and called `CalcSetPointLinInt`. Then, this function is called within `DefineOutsideAirSetPointManager::calculate` and  `DefineSysNodeResetSetPointManager::calculate`.

The calculated setpoint gets applied through `InitSetPointManagers`, `SimSetPointManagers`, and `UpdateSetPointManagers` functions, similar with other SetpointManager objects.
