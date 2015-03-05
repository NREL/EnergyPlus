# Coupling EnergyPlus with Functional Mock-up Units for co-simulation

The Functional Mock-up Unit (FMU) for co-simulation import for EnergyPlus allows EnergyPlus to conduct co-simulation with various programs that are packaged as FMUs. A FMU is a component which implements the Functional Mock-up Interface (FMI) standard (http://www.modelisar.com).

A FMU is distributed in the form of a zip file that may contain physical models, model descriptions, source code, and executable programs for various platforms. The FMU for co-simulation import provides EnergyPlus with a standard interface to conduct and control co-simulation with an arbitrary number of FMUs without any middle-ware, such as the Building Controls Virtual Test Bed (BCVTB Documentation, 2011).

The FMU for co-simulation import allows coupling of continuous-time and discrete-time models exported from different simulation programs. In the current implementation, EnergyPlus is implemented as the co-simulation master. It controls the data exchange between the subsystems and the synchronization of all slave simulation programs.

The FMU for co-simulation import enables the direct link between the EnergyPlus kernel and other simulation tools. It will make the co-simulation easier to conduct as no middle-ware is involved. This direct link will decrease run-time by eliminating the transaction layer. In addition, by separating the co-simulation interface from the EnergyPlus kernel, the FMU interface is reusable when EnergyPlus is updated. Furthermore, the FMU contains executable files that have the same interface to EnergyPlus regardless of their original programming environment. Some commercial tools allow running their FMU without licensing requirement.

**Notes:**

- **The c**urrent implementation of FMU for co-simulation is only supported on Windows and Linux.
- *FMUs* must be in a folder to which the user has *write* access.

## Data exchange between EnergyPlus and FMUs

Prior to describing the data exchange between EnergyPlus and FMUs, some definitions and terminologies used in the remainder of this document will be introduced.

A variable of a system described by a system of differential algebraic equations (DAE) is defined as differential variable if its derivatives are present in the DAE. A variable of a system described by a system of DAE is defined as algebraic if its derivatives do not appear explicitly in the DAE (Fabian et al., 2008).

![System with two variables that could be either differential or algebraic variables.](media/system-with-two-variables.png)

Because in subsequent discussions, it will be distinguished between algebraic and differential variables, a notation for different system of equations that involve algebraic and differential variables will be introduced. Let ![](media/image11.png) , then

- If ![](media/image12.png) and ![](media/image13.png)  are differential variables, then the system is

![](media/image14.png)  with *F*: ℝ^n^ x ℝ^n^ x ℝ^m^ x ℝ^m^ x ℝ^q^ x ℝ → ℝ^n+m^.

- If ![](media/image15.png) is a differential variable and ![](media/image16.png)  is an algebraic variable, then the system is

![](media/image17.png)  with *G:* ℝ^n^ x ℝ^n^ x ℝ^m^ x ℝ^q^ x ℝ → ℝ^n+m^.

- If ![](media/image18.png)  is an algebraic variable and ![](media/image19.png)  is a differential variable, then the system is

![](media/image20.png)  with *H:* ℝ^n^ x ℝ^m^ x ℝ^m^ x ℝ^q^ x ℝ → ℝ^n+m^.

- If ![](media/image21.png)  is an algebraic variable and ![](media/image22.png)  is an algebraic variable, then the system is

![](media/image23.png)  **with *I:* ℝ^n^ x ℝ^m^ x ℝ^q^ x ℝ → ℝ^n+m^.

Figure 6 shows a case where a FMU is linked to an EnergyPlus model for co-simulation. The FMU and EnergyPlus could be linked through differential or algebraic variables.

![System with one FMU linked to EnergyPlus.](media/system-with-one-fmu-linked-to-energyplus.png)

Table 2 shows the different system configurations that are possible.

- In the first case, the variable ![](media/image25.png)  and ![](media/image26.png)  are differential variables in both systems.
- In the second case, the variable ![](media/image27.png)  is a differential variable and the variable ![](media/image28.png)  is an algebraic variable.
- In the third case, the variable ![](media/image29.png)  is an algebraic variable and the variable ![](media/image30.png)  is a differential variable.
- In the fourth case, the variable ![](media/image31.png)  is an algebraic variable and the variable ![](media/image32.png) is an algebraic variable.

**In the current implementation,** it will be focused on the first and the second cases since the third and the fourth cases will constrain the FMU to be solved numerically in the iteration solver loop of EnergyPlus. This will necessitate the ability of the FMU to reject time steps (Modelisar, 2010) which is currently not implemented in the EnergyPlus FMU for co-simulation import. Applications for case 1 and 2 are described in the next sections.

Table 2: Use cases with different system configurations

Case |EnergyPlus|FMU (e.g. from Modelica)
-----|----------|------------------------
(1)|Model1 (Differential variable)|Model2 (Differential variable)
(2)|Model1 (Differential variable)|Model2 (Algebraic variable)
(3)|Model1 (Algebraic variable)|Model2 (Differential variable)
(4)|Model1 (Algebraic variable)|Model2 (Algebraic variable)

## Case 1: Linking two systems through differential variables

This case could be for an application where a wall with a phase change material (PCM) is modeled in a FMU and is linked to a room model in EnergyPlus. The room air temperature is the differential variable in EnergyPlus and the temperature of the wall with PCM is the differential variable in the FMU. Each system solves a differential equation that is connected to the differential equation of the other system. For simplicity, we assume that *y~1~(.)* = *x~1~(.) and y~2~(.)* = *x~2~(.).*The systems are described by the ordinary differential equations

dx~1~/dt = f~1~(x~1~, x~2~), with x~1~(0) = x~1,0~,

dx~2~/dt = f~2~(x~2~, x~1~), with x~2~(0) = x~2,0 .~

Let ![](media/image33.png)  denote the number of time steps and let *t~k~* with *![](media/image34.png)  denote the time steps. We will use the subscripts 1* and *2* to denote the variables and the functions that compute the next state variable of the simulator *1* and *2*, respectively.

The first system computes, for![](media/image35.png) and some![](media/image36.png) : ℝ^n^ x ℝ^m^ x ℝ x ℝ → ℝ^n^, the sequence

x~1~(t~k+1~) = ![](media/image37.png) (x~1~(t~k~), x~2~(t~k~), t~k~, t~k+1~)

and, similarly, the simulator *2* computes for some ![](media/image38.png) : ℝ^m^ x ℝ^n^ x ℝ x ℝ → ℝ^m^ the sequence

x~2~(t~k+1~) = ![](media/image39.png) (x~2~(t~k~), x~1~(t~k~), t~k~, t~k+1~)

with initial conditions *x~1~(0) = x~1,0~* and *x~2~(0) = x~2,0.~* ![](media/image40.png)  and ![](media/image41.png)  **are the functions that are used **to compute the value of the state variables at the new time step

To advance from time *t~k~* to *t~k+1~*, each system uses its own time integration algorithm. At the end of the time step, EnergyPlus sends the new state *x~1~(t~k+1~)* to the FMU and it receives the state *x~2~(t~k+1~)* from the FMU. The same procedure is done with the FMU.

## Case 2: Linking two systems through algebraic and differential variables

This case could be for an application where a fan is modeled in a FMU and is linked to a room model in EnergyPlus. The room temperature is the differential variable in EnergyPlus and the pressure difference of the fan is the algebraic variable in the FMU. For simplicity, we assume that *y~1~(.)* = *x~1~(.) and y~2~(.)* = *x~2~(.).* In this application, the systems are described by the following equations

dx~1~/dt = g~1~(x~1~, x~2~), with x~1~(0) = x~1,0,~

0 = g~2~(x~2~, x~1~).

Let ![](media/image42.png)  denote the number of time steps and let *t~k~* with ![](media/image43.png)  denote the time steps. We use the same subscripts *1* and *2* as for the first case to denote the variable and the function that computes the next variable of the simulator *1* and *2*, respectively.

The first system computes, for![](media/image44.png) and some![](media/image45.png) : ℝ^n^ x ℝ^m^ x ℝ x ℝ → ℝ^n^, the sequence

x~1~(t~k+1~) = ![](media/image46.png) (x~1~(t~k~), x~2~(t~k~), t~k~, t~k+1~)

and, similarly, the simulator *2* computes for some ![](media/image47.png) : ℝ^m^ x ℝ^n^ x ℝ → ℝ^m^ the sequence

x~2~(t~k+1~) = ![](media/image48.png) (x~2~(t~k+1~), x~1~(t~k+1~), t~k+1~)

with initial condition *x~1~(0) = x~1,0~*. ![](media/image49.png) and ![](media/image50.png)  **are the functions that compute the value of the variables at the new time step.

To advance from time *t~k~* to *t~k+1~*, each system uses its own time integration algorithm. At the end of the time step, EnergyPlus sends the new value *x~1~(t~k+1~)* to the FMU and it receives the value *x~2~(t~k+1~)* from the FMU. The same procedure is done with the FMU. **

## Requirements

The current implementation for linking EnergyPlus with the FMUs has the following requirements:

- The data exchange between EnergyPlus and the FMUs is done at the zone time step of EnergyPlus.
- Each FMU is linked to EnergyPlus only through a differential variable in EnergyPlus (see Figure 7 for one FMU).

![System with one FMU linked to EnergyPlus.](media/system-with-one-fmu-linked-to-energyplus.png)

- Two or multiple FMUs are linked together only through differential variables in EnergyPlus (see Figure 8 for two FMUs).

![System with two FMUs linked to EnergyPlus.](media/system-with-two-fmus-linked-to-energyplus.png)

