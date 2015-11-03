uction finitSurface Heat Balance Manager / Processes
========================================

Conduction Through The Walls
----------------------------

### Conduction Transfer Function Module

The most basic time series solution is the response factor equation which relates the flux at one surface of an element to an infinite series of temperature histories at both sides as shown by Equation :

<div>$${q''_{ko}}(t) = \sum\limits_{j = 0}^\infty  {{X_j}} {T_{o,t - j\delta }} - \sum\limits_{j = 0}^\infty  {{Y_j}} {T_{i,t - j\delta }}$$</div>

where q” is heat flux, T is temperature, i signifies the inside of the building element, o signifies the outside of the building element, t represents the current time step, and X and Y are the response factors.

While in most cases the terms in the series decay fairly rapidly, the infinite number of terms needed for an exact response factor solution makes it less than desirable.  Fortunately, the similarity of higher order terms can be used to replace them with flux history terms.  The new solution contains elements that are called conduction transfer functions (CTFs).  The basic form of a conduction transfer function solution is shown by the following equation:

<div>$${q''_{ki}}(t) =  - {Z_o}{T_{i,t}} - \sum\limits_{j = 1}^{nz} {{Z_j}} {T_{i,t - j\delta }} + {Y_o}{T_{o,t}} + \sum\limits_{j = 1}^{nz} {{Y_j}} {T_{o,t - j\delta }} + \sum\limits_{j = 1}^{nq} {{\Phi_j}{{q''}_{ki,t - j\delta }}} $$</div>

for the inside heat flux, and

<div>$${q''_{ko}}(t) =  - {Y_o}{T_{i,t}} - \sum\limits_{j = 1}^{nz} {{Y_j}} {T_{i,t - j\delta }} + {X_o}{T_{o,t}} + \sum\limits_{j = 1}^{nz} {{X_j}} {T_{o,t - j\delta }} + \sum\limits_{j = 1}^{nq} {{\Phi_j}{{q''}_{ko,t - j\delta }}} $$</div>

for the outside heat flux (q²=q/A)

where:

X*<sub>j</sub>*  = Outside CTF coefficient, j= 0,1,...nz.

Y*<sub>j</sub>* = Cross CTF coefficient, j= 0,1,...nz.

Z*<sub>j</sub>* = Inside CTF coefficient, j= 0,1,...nz.

F*<sub>j</sub>* = Flux CTF coefficient, j = 1,2,...nq.

T*<sub>i</sub>* = Inside face temperature

T*<sub>o</sub>* = Outside face temperature

<span>\({q''_{ko}}\)</span> = Conduction heat flux on outside face

<span>\(q''\)</span> = Conduction heat flux on inside face

The subscript following the comma indicates the time period for the quantity in terms of the time step d.  Note that the first terms in the series (those with subscript 0) have been separated from the rest in order to facilitate solving for the current temperature in the solution scheme.  These equations state that the heat flux at either face of the surface of any generic building element is linearly related to the current and some of the previous temperatures at both the interior and exterior surface as well as some of the previous flux values at the interior surface.

The final CTF solution form reveals why it is so elegant and powerful.  With a single, relatively simple, linear equation with constant coefficients, the conduction heat transfer through an element can be calculated.  The coefficients (CTFs) in the equation are constants that only need to be determined once for each construction type.  The only storage of data required are the CTFs themselves and a limited number of temperature and flux terms.  The formulation is valid for any surface type and does not require the calculation or storage of element interior temperatures.

### Calculation of Conduction Transfer Functions

The basic method used in EnergyPlus for CTF calculations is known as the state space method (Ceylan and Myers 1980; Seem 1987; Ouyang and Haghighat 1991).  Another common, older method used Laplace transformations to reach the solution;  the Laplace method was  used in BLAST (Hittle, 1979; Hittle & Bishop, 1983).  The basic state space system is defined by the following linear matrix equations:

<div>$$\frac{{d\left[ {\bf{x}} \right]}}{{dt}} = \left[ {\bf{A}} \right]\left[ {\bf{x}} \right] + \left[ {\bf{B}} \right]\left[ {\bf{u}} \right]$$</div>

<div>$$\left[ {\bf{y}} \right] = \left[ {\bf{C}} \right]\left[ {\bf{x}} \right] + \left[ {\bf{D}} \right]\left[ {\bf{u}} \right]$$</div>

where x is a vector of state variables, u is a vector of inputs, y is the output vector, t is time, and A, B, C, and D are coefficient matrices.  Through the use of matrix algebra, the vector of state variables (x) can be eliminated from the system of equations, and the output vector (y) can be related directly to the input vector (u) and time histories of the input and output vectors.

This formulation can be used to solve the transient heat conduction equation by enforcing a finite difference grid over the various layers in the building element being analyzed.  In this case, the state variables are the nodal temperatures, the environmental temperatures (interior and exterior) are the inputs, and the resulting heat fluxes at both surfaces are the outputs.  Thus, the state space representation with finite difference variables would take the following form:

<div>$$
  \frac{d\left[\begin{array}{c}T_1 \\ \vdots \\ T_n\end{array}\right]}{dt} = \left[\bf{A}\right]\left[\begin{array}{c}T_1 \\ \vdots \\ T_n\end{array}\right]+\left[\bf{B}\right]\left[\begin{array}{c}T_i \\ T_o\end{array}\right]
$$</div>

<div>$$
  \left[\begin{array}{c}{q''}_i \\ {q''}_o\end{array}\right] = \left[\bf{C}\right]\left[\begin{array}{c}T_1 \\ \vdots \\ T_n\end{array}\right]+\left[\bf{D}\right]\left[\begin{array}{c}T_i \\ T_o\end{array}\right]
$$</div>

where T1, T2, ..., Tn-1, Tn are the finite difference nodal temperatures, n is the number of nodes, Ti and To are the interior and exterior environmental temperatures, and q"i and q"o are the heat fluxes (desired output).

Seem (1987) shows that for a simple one layer slab with two interior nodes as in Figure 7 and convection at both sides the resulting finite difference equations are given by:

<div>$$C\frac{{d{T_1}}}{{dt}} = hA\left( {{T_o} - {T_1}} \right) + \frac{{{T_2} - {T_1}}}{R}$$</div>

<div>$$C\frac{{d{T_2}}}{{dt}} = hA\left( {{T_i} - {T_2}} \right) + \frac{{{T_1} - {T_2}}}{R}$$</div>

<div>$$q{``_i} = h\left( {{T_i} - {T_2}} \right)$$</div>

<div>$$q{``_o} = h\left( {{T_1} - {T_o}} \right)$$</div>

where:

<span>\(R = \frac{\ell }{{kA}}\)</span>,

<span>\(C = \frac{{\rho {c_p}\ell A}}{2}\)</span>, and

A is the area of the surface exposed to the environmental temperatures.

In matrix format:

<div>$$
  \left[\begin{array}{c}\frac{dT_1}{dt} \\ \frac{dT_2}{dt}\end{array}\right] = 
     \left[\begin{array}{cc}-\frac{1}{RC}-\frac{hA}{C} & \frac{1}{RC} \\ \frac{1}{RC} & -\frac{1}{RC}-\frac{hA}{C}\end{array}\right]\left[\begin{array}{c}T_1 \\ T_2\end{array}\right] + 
     \left[\begin{array}{cc}\frac{hA}{C} & 0 \\ 0 & \frac{hA}{C}\end{array}\right]\left[\begin{array}{c}T_o \\ T_i\end{array}\right]
$$</div>

<div>$$
  \left[\begin{array}{c}{q''}_o \\ {q''}_i\end{array}\right] = 
    \left[\begin{array}{cc}0 & -h \\ h & 0\end{array}\right] \left[\begin{array}{c}T_1 \\ T_2\end{array}\right] +
    \left[\begin{array}{cc}0 & h \\ -h & 0\end{array}\right] \left[\begin{array}{c}T_o \\ T_i\end{array}\right]
$$</div>

![](media/image168.svg.png)

Figure 10.  Two Node State Space Example.

The important aspect of the state space technique is that through the use of matrix algebra the state space variables (nodal temperatures) can be eliminated to arrive at a matrix equation that gives the outputs (heat fluxes) as a function of the inputs (environmental temperatures) only.  This eliminates the need to solve for roots in the Laplace domain.  In addition, the resulting matrix form has more physical meaning than complex functions required by the Laplace transform method.

The accuracy of the state space method of calculating CTFs has been addressed in the literature.  Ceylan and Myers (1980) compared the response predicted by the state space method to various other solution techniques including an analytical solution.  Their results showed that for an adequate number of nodes the state space method computed a heat flux at the surface of a simple one layer slab within 1% of the analytical solution.  Ouyang and Haghighat (1991) made a direct comparison between the Laplace and state space methods.  For a wall composed of insulation between two layers of concrete, they found almost no difference in the response factors calculated by each method.

Seem (1987) summarizes the steps required to obtain the CTF coefficients from the A, B, C, and D matrices.  While more time consuming than calculating CTFs using the Laplace Transform method, the matrix algebra (including the calculation of an inverse and exponential matrix for A) is easier to follow than root find algorithms.  Another difference between the Laplace and State Space methods is the number of coefficients required for a solution.  In general, the State Space method requires more coefficients.  In addition, the number of temperature and flux history terms is identical (nz=nq).  Note that as with the Laplace method that the actual number of terms will vary from construction to construction.

Two distinct advantages of the State Space method over the Laplace method that are of interest when applying a CTF solution for conduction through a building element are the ability to obtain CTFs for much shorter time steps and the ability to obtain 2- and 3-D conduction transfer functions.  While not implemented in the Toolkit, both Seem (1987) and Strand (1995) have demonstrated the effectiveness of the State Space method in handling these situations that can have important applications in buildings.

### Conduction Transfer Function (CTF) Calculations in EnergyPlus

Conduction transfer functions are an efficient method to compute surface heat fluxes because they eliminate the need to know temperatures and fluxes within the surface.  However, conduction transfer function series become progressively more unstable as the time step decreases.  This became a problem as investigations into short time step computational methods for the zone/system interactions progressed because, eventually, this instability caused the entire simulation to diverge.  This phenomenon was most apparent for thermally massive constructions with long characteristic times and, correspondingly, requiring a large number of terms in the CTF series. This indicates that the problem is related to round-off and truncation error and is in no way an indictment of the CTF method itself.  Methods that develop CTF series from finite difference approximations to the heat conduction equation (Meyers, 1980; Seem, 1987) were considered to address this problem.  Seem's method did give better accuracy and stability at short time steps than the current BLAST technique but, the method still had difficulty computing stable CTF series for time steps of less than 1/4 hour for the heaviest constructions in the BLAST library.

The zone heat gains consist of specified internal heat gains, air exchange between zones, air exchange with the outside environment, and convective heat transfer from the zone surfaces.  Of these, the surface convection load requires the most complicated calculations because a detailed energy balance is required at the inside and outside surface of each wall, floor, and roof.  In addition, the transient heat conduction in the material between the surfaces must be solved.  This solution gives the inside and outside temperatures and heat fluxes that must be known in order to calculate the convection component to the zone load for each zone surface.  BLAST uses a conduction transfer function CTF method attributed to Hittle (1980) to solve the transient conduction problem for each surface.  The method results in a time series of weighting factors that, when multiplied by previous values of the surface temperatures and fluxes and the current inside and outside surface temperatures, gives the current inside and outside heat flux.  The method is easily applied to multilayered constructions for which analytical solutions are unavailable.  In addition, determining the series of CTF coefficients is a one-time calculation, making the method much faster than finite difference calculations.

A problem with CTF methods is that the series time step is fixed; that is, a CTF series computed for a one hour time step takes information at t-1 hours, t-2 hours, etc. and computes conditions at the current time t.  As time advances the oldest term in the input series is dropped and the data moved back one time step to allow the newest value to be added to the series.  For convenience, the time step used to determine the CTF series should be the same as the time step used to update the zone mean air temperature in the zone energy balance.  But, as the time step used to calculate the CTF series gets shorter, the number of terms in the series grows.  Eventually, with enough terms, the series becomes unstable due to truncation and round-off error.  Heavy constructions, such as slab-on-grade floors (12" heavyweight concrete over 18" dirt), have accuracy and stability problems at time steps as large as 0.5 hours when modeled by Hittle's CTF method.  In an attempt to overcome this problem, Hittle's method was replaced by Seem's method (1987) in IBLAST.  This resulted in some improvement in stability at shorter time steps, but not enough to allow IBLAST to run at a 0.1 hour time step without restricting the types of surfaces that could be used.

Even though CTF methods require that values of the surface temperatures and fluxes be stored for only a few specific times before the current time, the temperature and flux histories are, actually, continuous functions between those discrete points.  However, there is no way to calculate information at these intermediate times once a series has been initialized.  The terms in the temperature and flux histories are out of phase with these points.  However, they can be calculated by shifting the phase of the temperature and flux histories by only a fraction of a time step.  This procedure would allow a CTF series computed for a time step Dt, to be used to compute information at times t+Dt/2, t+Dt/3, t+Dt/4, or any other arbitrary fraction of the time step, so long as the surface temperatures and flux values were still Dt apart.  Several ways of doing this are described below.

The method shown in the Figure 11 maintains two sets of histories out of phase with each other.  The figure shows how this would work for two sets of histories out of phase by one half of a time step.  More sets of temperature and flux histories could be used, allowing the simulation time step to take on values: 1/3, 1/4, 1/5, etc., of the minimum time step allowed for the CTF calculations.  The time step between inputs to the CTF series would be the smallest convenient interval at which the CTF series is stable.  This scenario is illustrated in this figure for two separate sets of temperature and flux histories.  Cycling through each history, in order, allowed calculations of the zone energy balance to be performed with updated surface information at a shorter time step than one CTF history series would otherwise allow.  This method required no interpolation between the series once each set of histories was initialized.  However, if the smallest time step for a stable CTF series was large compared to the zone temperature update time step, significant memory was required to store all the sets of histories.

![](media/image169.svg.png)

Figure 11. Multiple, staggered time history scheme

Another method is shown in Figure 12. Sequential interpolation of new histories that uses successive interpolations to determine the next set of temperature and flux histories.  The current history is interpolated directly from the previous history set using the required time phase shift between the two.  This method required permanent storage for only one set of temperature and flux histories at a time, but smoothed out temperature and flux data as more interpolations were performed.  As a result, at concurrent simulation times current values of history terms were different form previous "in phase" history terms.  This was unacceptable from, a physical point of view, because it allowed current information to change data from a previous time.

![](media/image170.svg.png)

Figure 12. Sequential interpolation of new histories

A final method, shown in Figure 13. Master history with interpolation, was something of a hybrid of the previous two methods.  One "master" history set was maintained and updated for all time; this solved the problem of current events propagating information backwards in time.  When surface fluxes needed to be calculated at times out of phase with this master history a new, temporary history was interpolated from the master values.  This method proved to be the best of the three options described because it eliminated propagation of information backwards in time and only required concurrent storage of two sets of temperature and flux histories. This method was subsequently incorporated into the IBLAST program in conjunction with Seem's procedure for calculating the coefficients of the CTF series.

![](media/image171.svg.png)

Figure 13. Master history with interpolation

### Conduction Transfer Function (CTF) Calculations Special Case: R-Value Only Layers

Most users will elect to enter materials with four parameters that are of interest for calculating conduction transfer functions: thickness, conductivity, density, and specific heat.  For these materials, EnergyPlus will divide each material layer within a construction into between 6 and 18 nodes for the application of the state-space method.  For multi-layered constructions, nodes are also placed at the interface between two layers.  These interface nodes consist of half a node of the first layer and half a node of the second layer.

In some cases, either due to a lack of information or a desire to simplify input, a user may choose to enter a material layer as a “no mass” or “R-Value only” material.  This assumption essentially says that these layers add nothing to the thermal mass of the overall construction and only add to the overall resistance or R-Value of the construction as a whole.  While this is not recommended, it is allowed and in some cases is not a poor assumption for extremely lightweight materials such as some types of insulation.

In the past, when a user enters such a “no mass” material into EnergyPlus, internally the properties of this layer are converted to approximate the properties of air (density, specific heat, and conductivity) with the thickness adjusted to maintain the user’s desired R-Value.  This allowed such layers to be handled internally in the same way as other layers without any additional changes to the code.  This solution was deemed accurate enough as air has very little thermal mass and it made the coding of the state space method simpler.

It is possible to account for layers that have no thermal mass in the state space solution without resorting to the assignment of fictitious material properties.  The EnergyPlus internal equations for assigning values to portions of the A, B, C, and D matrices as shown in the previous subsections have been altered to account for the potential presence of R-Value only (or no mass) layers without resorting to assigning these materials the properties of air.  This is handled by assuming that the “no mass” layer is a single node layer.  As nodes are defined that the interface between material layers, the “no mass” layer is essentially two “half nodes” that are shared with the surrounding layers.  This allows the surrounding material layers to provide thermal capacitance for each of the nodes at the material interfaces.

In EnergyPlus, there are two possible cases for the existence of “no mass” layers: either between two other solid, thermally massive layers (multiple “no mass” layers next to each other are simply combined in this approach) or at the inner or outer most layers of a construction.  There are potential issues with having a resistance-only layer at either the inner or outer most layers of a construction.  A little or no mass layer there could receive intense thermal radiation from internal sources or the sun causing the temperature at the inner or outer surface to achieve very high levels.  This is undesirable from a simulation standpoint as there are limits to temperature levels in EnergyPlus that could be exceeded causing the simulation to terminate and is likely unrealistic from a real-world perspective.  Thus, for such potentially problematic calculational scenarios, EnergyPlus will continue to convert a “no mass” layer at either the inner or outer most layer of a construction into a thermal mass layer using the properties of air as has been done in the past.

The case where a resistance-only layer is defined anywhere except the inner or outer layer of a construction is handled by treating the “no mass” layer as a single node layer.  This will result in a node at each interface as in the standard material layer cases.  When a “no mass” material is present, the R-Value only layer will not add any thermal capacitance to the nodes at the interfaces at either side of the material.  It will simply add resistance between the two nodes.

![](media/image172.png)

Figure 14. Illustration of no-mass layer between two mass layers

From the EnergyPlus code, the A matrix (AMat) is assigned with values at the interface using the following equations (taken from the actual code):

````
cap = ( rho(Layer)\*cp(Layer)\*dx(Layer) + rho(Layer+1)\*cp(Layer+1)\*dx(Layer+1) ) \* 0.5D0
AMat(Node,Node-1) = rk(Layer)/dx(Layer)/cap           ! Assign matrix values for the current node
AMat(Node,Node)   = -1.0D0 \* ( rk(Layer)/dx(Layer)+rk(Layer+1)/dx(Layer+1) ) / cap
AMat(Node,Node+1) = rk(Layer+1)/dx(Layer+1)/cap       ! node.
````

Note that these equations do not change.  For “no mass” layers, the density (rho) and the specific heat (cp) variables will be assigned zero values.  In addition, the thickness (dx) will be equated with the user-defined R-Value and conductivity (rk) will be assigned a value of unity.  In addition, the number of nodes for the “no mass” layer will be set to 1.

This handles resistive layers correctly without resorting to assigning the properties of air to the “no mass” layer.  The only potential problem with this is if two resistive layers are placed next to each other.  In that case, the interface between the two resistive layers would have no mass (variable “cap” would equal zero) and a divide by zero would result.  To avoid this, adjacent “no mass” layers are combined internally so that the user does not have to do this and also to avoid any divide by zero errors.

While from a results standpoint, the difference in output between assigning air properties for specific heat, density, etc. and handling the no mass materials explicitly is negligible, handling the no mass layers properly does provide better code efficiency from a calculation speed standpoint.

### References

Ceylan, H. T., and G. E. Myers. 1980. Long-time Solutions to Heat Conduction Transients with Time-Dependent Inputs. ASME Journal of Heat Transfer, Volume 102, No. 1, pp. 115-120.

Hittle, D. C. 1979. Calculating Building Heating and Cooling Loads Using the Frequency Response of Multilayered Slabs, Ph.D. Thesis, University of Illinois, Urbana, IL.

Hittle, D. C., and R. Bishop. 1983. An Improved Root-Finding Procedure for Use in Calculating Transient Heat Flow Through Multilayered Slabs. International Journal of Heat and Mass Transfer, Vol. 26, No. 11, pp. 1685-1693.

Ouyang, K., and F. Haghighat. 1991. A Procedure for Calculating Thermal Response Factors of Multi-layered Walls--State Space Method. Building and Environment, Vol. 26, No. 2, pp. 173-177.

Seem, J. E. 1987. Modeling of Heat Transfer in Buildings, Ph.D. Thesis, University of Wisconsin, Madison, WI.

Strand, R. K. 1995. Heat Source Transfer Functions and Their Application to Low Temperature Radiant Heating Systems, Ph.D. Thesis, University of Illinois, Urbana, IL.

Taylor, R. D., C.O. Pedersen, D.E. Fisher, R. J. Liesen, L.K. Lawrie. 1990. *Simultaneous Simulation of Buildings and Mechanical Systems in Heat Balance Based Energy Analysis Programs*, Proceedings of the 3rd International Conference on System Simulation in Buildings, Liege, Belgium, December 3-5, 1990.

Taylor, R.D., C.O. Pedersen, D.E. Fisher, R. J. Liesen, L.K. Lawrie. 1991. Impact of Simultaneous Simulation of Buildings and Mechanical Systems in Heat Balance Based Energy Analysis Programs on System Response and Control, Conference Proceedings IBPSA Building Simulation '91, Nice, France, August 20-22, 1991.

Conduction Finite Difference Solution Algorithm
-----------------------------------------------

### Basic Finite Difference Solution Approach

EnergyPlus models follow fundamental heat balance principles very closely in almost all aspects of the program. However, the simulation of building surface constructions has relied on a conduction transfer function (CTF) transformation carried over from BLAST. This has all the usual restrictions of a transformation-based solution: constant properties, fixed values of some parameters, and do not produce results for the interior of the surface. As the energy analysis field moves toward simulating more advanced constructions, such as phase change materials (PCM), it becomes necessary to step back from transformations to more fundamental forms. Accordingly, a conduction finite difference (CondFD) solution algorithm has been incorporated into EnergyPlus. This does not replace the CTF solution algorithm, but complements it for cases where the user needs to simulate phase change materials or variable thermal conductivity. It is also possible to use the finite difference algorithm for zone time steps as short as one minute.

EnergyPlus includes two different options for the specific scheme or formulation used for the finite difference model.  The first scheme is referred to as Crank-Nicholson and was the formulation used in EnergyPlus prior to version 7.  As of version 7 a second scheme was added and is referred to as fully implicit.  The selection between the two can be made by the user with the HeatBalanceSettings:ConductionFiniteDifference input object.  Once selected, the same scheme is used throughout the simulation.  Although the two different schemes differ in their fundamental heat transfer equations, they share nearly all the same supporting models for material properties, data storage, solution schemes, and spatial discretization algorithms.

The Crank-Nicholson scheme is semi-implicit and based on an Adams-Moulton solution approach. It is considered second-order in time.  The algorithm uses an implicit finite difference scheme coupled with an enthalpy-temperature function to account for phase change energy accurately. The implicit formulation for an internal node is shown in the equation below.

<div>$$
  C_p \rho \Delta x \frac{T_i^{j+1}-T_i^j}{\Delta t} = 
     \frac{1}{2}\left(k_W\frac{T_{i+1}^{j+1}-T_{i}^{j+1}}{\Delta x} +
                      k_E\frac{T_{i-1}^{j+1}-T_{i}^{j+1}}{\Delta x} + 
                      k_W\frac{T_{i+1}^{j}-T_{i}^{j}}{\Delta x} +
                      k_E\frac{T_{i-1}^{j}-T_{i}^{j}}{\Delta x}\right)
$$</div>

Where:

T <sub>=</sub> node temperature

Subscripts:

i = node being modeled

i+1 = adjacent node to interior of construction

i-1 = adjacent node to exterior of construction

j+1 = new time step

j = previous time step

Dt = calculation time step

Dx = finite difference layer thickness (always less than construction layer thickness)

C<sub>p</sub> = specific heat of material

k<sub>w</sub> = thermal conductivity for interface between i node and i+1 node

k<sub>E</sub> = thermal conductivity for interface between i node and i-1 node

r  = density of material

Then, this equation is accompanied by a second equation that relates enthalpy and temperature.

<div>$${h_i} = HTF\left( {{T_i}} \right)$$</div>

where HTF is an enthalpy-temperature function that uses user input data.

The fully implicit scheme is also based on an Adams-Moulton solution approach. It is considered first order in time.  The model equation for this scheme is show in the following equation.

<div>$${C_p}\rho {\rm{\Delta x}}\frac{{T_i^{j + 1} - T_i^j}}{{{\rm{\Delta }}t}} = \left( {{k_W}\frac{{\left( {T_{i + 1}^{j + 1} - T_i^{j + 1}} \right)}}{{{\rm{\Delta x}}}} + {k_E}\frac{{\left( {T_{i - 1}^{j + 1} - T_i^{j + 1}} \right)}}{{{\rm{\Delta x}}}}} \right)$$</div>

For both schemes, EnergyPlus uses the following four types of nodes, as shown in the figure below (1) interior surface nodes, (2) interior nodes, (3) material interface nodes and (4) external surface nodes. The grid for each material is established by specifying a half node for each edge of a material and equal size nodes for the rest of the material. Equations such as are formed for all nodes in a construction. The formulation of all node types is basically the same.

![Description: C:\Griffith\AnalysisTools\CondFDpaper\Figures\Figures\Figure2.tif](media/image176.png)

Figure 15. Node depiction for Conduction Finite Difference Model

In the CondFD model, surface discretization depends on the thermal diffusivity of the material (α) and time step (Δt) selected, as shown in the equation below. The default value of 3 for the space discretization constant, *C*, is basically the inverse of the Fourier Number:

<div>$$
Fo = \frac{\alpha \Delta t}{\Delta x^2}
$$</div>

and is based on the stability requirement for the explicit mode that requires values higher than 2, or a Fourier number lower than 0.5. However, CondFD uses implicit schemes that do not have the same stability requirements as the explicit mode. Thus, the default 3 was originally set rather arbitrary.  As of version 7, the value of this constant can be controlled by the user with the input field called Space Discretization Constant in the HeatBalanceSettings:ConductionFiniteDifference input object.   The discretization method allows CondFD to assign different node spacing or grid size to different material layers in a wall or roof, as building walls and roofs typically consist of several layers of different materials having different thermal properties.

<div>$${\rm{\Delta x}} = \sqrt {{\rm{C\alpha \Delta t}}} $$</div>

The actual integer number of nodes for each layer is then calculated by rounding off the result from dividing the length of the material layer by the result of the equation above. After this, Δx is recalculated by dividing the length of the material by the number of nodes. A full node is equal to two half nodes. Lower values for the Space Discretization Constant yield more nodes, with higher values yield fewer nodes.

Because the solution is implicit, a Gauss-Seidell iteration scheme is used to update to the new node temperatures in the construction and under-relaxation is used for increased stability.  The Gauss-Seidell iteration loop is the inner-most solver and is called for each surface.  It is limited to 30 iterations but will exit early when the sum of all the node temperatures changes between the last call and the current call, normalized by the sum of the temperature values, is below  0.000001C. This convergence criteria is typically met after 3 iterations, except when PCMs are simulated as it takes an average of 2-3 more iterations when PCM are changing phase. If the number if iterations needed to met convergence criteria start to increase, an automatic internal relaxation factor stabilities the solution and in most cases keep the number of iterations less than 10.

EnergyPlus also uses a separate, outer iteration loop across all the different inside surface heat balances so that internal long-wave radiation exchange can be properly solved.  For CTF formulations, this iteration is controlled by a maximum allowable temperature difference of 0.002C for inside face surface temperatures from one iteration to the next (or a limit of 100 iterations). CondFD uses the same default value for allowable temperature difference as CTF. However, this parameter was found to often need to be smaller for stability and so the inside surface heat balance manager uses a separate allowable maximum temperature difference when modeling CondFD.  The user can control the value of the relaxation factor by using the input field called  Inside Face Surface Temperature Convergence Criteria in the HeatBalanceSettings:ConductionFiniteDifference input object. In addition, if the program detects that there is instability by watching for excessive numbers of iterations in this outer loop and may decrease the relaxation factor. Users can also output the number of iterations inside of CondFD loop for each surface and the outer internal heat balance loop for each zone with “CondFD Inner Solver Loop Iterations” and “Heat Balance Inside Surfaces Calculation Iterations” respectively.

<div>$${T_{i,new}} = {T_{i,old}} + \left( {{T_{i,new}} - {T_{i,old}}} \right)*Relax$$</div>

Because of the iteration scheme used for CondFD,  the node enthalpies get updated each iteration, and then they are used to develop a variable Cp if a phase change material is being simulated. This is done by including a third equation for Cp.

<div>$$Cp = \frac{{{h_{i,new}} - {h_{i,old}}}}{{{T_{i,new}} - {T_{i,old}}}}$$</div>

The iteration scheme assures that the correct enthalpy, and therefore the correct Cp is used in each time step, and the enthalpy of the material is accounted for accurately. Of course, if the material is regular, the user input constant Cp is used.

The algorithm also has a provision for including a temperature coefficient to modify the thermal conductivity. The thermal conductivity is obtained from:

<div>$$k = {k_o} + {k_1}\left( {{T_i} - 20} \right)$$</div>

where:

k<sub>o</sub> is the 20°C value of thermal conductivity (normal idf  input)

k<sub>1</sub> is the change in conductivity per degree temperature difference from 20°C

As of Version 7, the CondFD implementation was changed to evaluate the thermal conductivity at the interface between nodes, as shown below. In this case, EnergyPlus uses a linear interpolation between nodal points.

<div>$${C_p}\rho {\rm{\Delta x}}\frac{{T_i^{j + 1} - T_i^j}}{{{\rm{\Delta }}t}} = \frac{1}{2}\left[ {\left( {{k_W}\frac{{\left( {T_{i + 1}^{j + 1} - T_i^{j + 1}} \right)}}{{{\rm{\Delta x}}}} + {k_E}\frac{{\left( {T_{i - 1}^{j + 1} - T_i^{j + 1}} \right)}}{{{\rm{\Delta x}}}}} \right) + \left( {{k_W}\frac{{\left( {T_{i + 1}^j - T_i^j} \right)}}{{{\rm{\Delta x}}}} + {k_E}\frac{{\left( {T_{i - 1}^j - T_i^j} \right)}}{{{\rm{\Delta x}}}}} \right)} \right]$$</div>

Where,

<div>$${k_W} = \frac{{\left( {k_{i + 1}^{j + 1} + k_i^{j + 1}} \right)}}{2}$$</div>

<div>$${{\rm{k}}_{\rm{E}}} = \frac{{\left( {{\rm{k}}_{{\rm{i}} - 1}^{{\rm{j}} + 1} + {\rm{k}}_{\rm{i}}^{{\rm{j}} + 1}} \right)}}{2}$$</div>

These additional property information values are put into the input file as explained in the Input/Output Reference Document, but it consists simply of a value for k1 and set of enthalpy temperature pairs that describe the enthalpy of the phase change material in straight line segments with respect to temperature.

A graph showing the effect of a large PCM on the outside surface of a zone is shown below. The phase change temperature was 30°C, and the flat temperature response during the phase change is obvious. This example was run with a zone time step of one minute to show that such small time steps can be done with the finite difference solution technique. It is more efficient to set the zone time step shorter than those used for the CTF solution algorithm. It should be set to 20 time steps per hour or greater, and can range up to 60. The finite difference algorithm actually works better with shorter zone time steps. The computation time has a minimum at a zone time step around two minutes (30 time steps/hr), and increases for shorter or longer zone time steps.

![Description: Phase Change Graph OutsidePCM](media/image185.png)

Figure 16. Effects of Large PCM on Outside Zone Surface

### Finite Difference Node Arrangement in Surfaces

The Conduction Finite Difference algorithm determines the number of nodes in each layer of the surface based on the Fourier stability criteria.  The node thicknesses are normally selected so that the time step is near the explicit solution limit in spite of the fact that the solution is implicit. For very thin, high conductivity layers, a minimum of two nodes is used.  This means two half thickness nodes with the node temperatures representing the inner and outer faces of the layer. All thicker layers also have two half thickness nodes at their inner and outer faces. These nodes produce layer interface temperatures.

The ConductionFiniteDifferenceSimplified capability was removed as of Version 7.2.

### Conduction Finite Difference Variable Thermal Conductivity

The Conduction Finite Difference algorithm has also been given the capability to use an expanded thermal conductivity function. This function, explained in the input/output document, is similar to the temperature enthalpy function. It consists of pairs of temperature and thermal conductivity values that form a linear segmented function. It is established with the MaterialProperty:VariableThermalConductivity object.

### Conduction Finite Difference Source Sink Layers

The Conduction Finite Difference algorithm can also invoke the source/sink layer capability by using the **Construction:InternalSource** object.

### Conduction Finite Difference Heat Flux Outputs

The Conduction Finite Difference algorithm can output the heat flux across the interface between nodes and the heat capacitance of each node.  During the CondFD solution iterations, the heat capacitance of each node (CondFD Surface Heat Capacitance Node < n  >) is stored:

<div>$${CpDelXRhoSCp_{i} = Cp_{i}*\Delta x_{i}*\rho_{i}}$$</div>

For nodes which are at the interface of two materials the heat capacitance of the two parts are added together.

After the CondFD node temperatures have been solved for a given timestep, the heat fluxes (CondFD Surface Heat Flux From Node < i > To Node < i+1 >) are calculated beginning with the inside face of the surface.

<div>$${QDreport_{N-1} = Q_{inside}+CpDelXRhoSCp_{N}* \frac{\left(T_{N,new}-T_{N,old}\right)}{\Delta t}}$$</div>
 
for the remaining nodes

<div>$${QDreport_{i} = QDreport_{i+1}+CpDelXRhoSCp_{i+1}* \frac{\left(T_{i+1,new}-T_{i+1,old}\right)}{\Delta t}}$$</div>

Where:

QDReport = CondFD Surface Heat Flux From Node < i > To Node < i+1 

Q<sub>inside</sub> = Surface Inside Face Conduction Heat Transfer Rate per Area [W/m<sup>2</sup>]

N = total number of nodes in a surface including the surface inside face node (Note that the variable *TotNodes* used in the source code is actually N-1. The surface inside face node is referenced as *TotNodes+1*.)



### References

Pedersen C.O., Enthalpy Formulation of conduction heat transfer problems involving latent heat, Simulation, Vol 18, No. 2, February 1972

Versteeg, H. and Malalasekra, W. 1996. An introduction to computational fluid dynamic: the finite volume method approach. Prentice Hall.

[Tabares-Velasco, P.C. and Griffith, B. 2012. Diagnostic Test Cases for Verifying Surface Heat Transfer Algorithms and Boundary Conditions in Building Energy Simulation Programs, Journal of Building Performance Simulation, doi:10.1080/19401493.2011.595501](../../../Documents%20and%20Settings/ptabares/My%20Documents/Paulo/CV/Tabares%20Velasco%20CV%20UPDATED2.doc)

Tabares-Velasco, P.C., Christensen, C. and Bianchi, M. 2012. Verification and Validation of EnergyPlus Phase Change Material Model for Opaque Wall Assemblies, Building and Environment 54: 186-196.

Combined Heat and Moisture Transfer (HAMT) Model
------------------------------------------------

### Overview

The combined heat and moisture transfer finite (HAMT) solution algorithm is a completely coupled, one-dimensional, finite element, heat and moisture transfer model simulating the movement and storage of heat and moisture in surfaces simultaneously from and to both the internal and external environments. As well as simulating the effects of moisture buffering, HAMT is also be able to provide temperature and moisture profiles through composite building walls, and help to identify surfaces with high surface humidity.

### HAMT Nomenclature

Dependencies on moisture content are indicated by a superscript <sup>w</sup>, on heat by a superscript <sup>h</sup> and vapor pressure by a superscript <sup>v</sup>.

Table 2. Combined Heat and Moisture Transfer Model Nomenclature

<table class="table table-striped">
<tr>
<th>Symbol</th>
<th>Units</th>
<th>Meaning</th>
</tr>
<tr>
<td>T</td>
<td>°C</td>
<td>Temperature</td>
</tr>
<tr>
<td>RH,φ</td>
<td>%, fraction</td>
<td>Relative humidity</td>
</tr>
<tr>
<td>W</td>
<td>kg/m<sup>3</sup></td>
<td>Moisture Content</td>
</tr>
<tr>
<td><span>\({\raise0.7ex\hbox{${\partial H}$} \!\mathord{\left/ {\vphantom {{\partial H} {\partial T}}}\right.}\!\lower0.7ex\hbox{${\partial T}$}}\)</span></td>
<td>J/m<sup>3</sup>C</td>
<td>Moisture dependent heat storage capacity</td>
</tr>
<tr>
<td><span>\({\raise0.7ex\hbox{${\partial w}$} \!\mathord{\left/ {\vphantom {{\partial w} {\partial \phi }}}\right.}\!\lower0.7ex\hbox{${\partial \phi }$}}\)</span></td>
<td>kg/m<sup>3</sup></td>
<td>Moisture dependent moisture storage capacity</td>
</tr>
<tr>
<td><span>\({k^w}\)</span></td>
<td>W/mC</td>
<td>Moisture dependent thermal conductivity</td>
</tr>
<tr>
<td><span>\({h_v}\)</span></td>
<td>J/kg</td>
<td>Evaporation enthalpy of water (= 2, 489, 000J/kg)</td>
</tr>
<tr>
<td><span>\(\delta \)</span></td>
<td>kg/msPa</td>
<td>Vapor diffusion coefficient in air</td>
</tr>
<tr>
<td><span>\(\mu \)</span></td>
<td>-</td>
<td>Moisture dependent vapor diffusion resistance factor</td>
</tr>
<tr>
<td>P</td>
<td>Pa</td>
<td>Vapor pressure</td>
</tr>
<tr>
<td>p<sub>ambient</sub></td>
<td>Pa</td>
<td>Ambient air pressure</td>
</tr>
<tr>
<td>C</td>
<td>J/kgC</td>
<td>Specific heat capacity of dry material</td>
</tr>
<tr>
<td>c<sup>w</sup></td>
<td>J/KgC</td>
<td>Specific heat capacity of water (=4,180J/kg<sup>o</sup>C@ 20<sup>o</sup>C)</td>
</tr>
<tr>
<td><span>\(\rho \)</span></td>
<td>kg/m<sup>3</sup></td>
<td>Material Density</td>
</tr>
<tr>
<td><span>\({\rho ^w}\)</span></td>
<td>kg/m<sup>3</sup></td>
<td>Density of water (= 1000kg/m3)</td>
</tr>
<tr>
<td><span>\({D^w}\)</span></td>
<td>m<sup>2</sup>/s</td>
<td>Liquid Transport Coefficient</td>
</tr>
<tr>
<td>A</td>
<td>m<sup>2</sup></td>
<td>Contact Surface area</td>
</tr>
<tr>
<td><span>\(\Delta {V_i}\)</span></td>
<td>m<sup>3</sup></td>
<td>Cell Volume</td>
</tr>
<tr>
<td><span>\(t\)</span></td>
<td>s</td>
<td>Time</td>
</tr>
<tr>
<td><span>\(\Delta \tau \)</span></td>
<td>s</td>
<td>Time step between calculations</td>
</tr>
<tr>
<td>x</td>
<td>m</td>
<td>Distance between cell centres</td>
</tr>
<tr>
<td><span>\(C_i^h\)</span></td>
<td>J/C</td>
<td>Heat Capacitance of cell i</td>
</tr>
<tr>
<td><span>\(C_i^w\)</span></td>
<td>kg</td>
<td>Moisture Capacitance of cell i</td>
</tr>
<tr>
<td><span>\(R_{ij}^h\)</span></td>
<td>C/W</td>
<td>Heat Resistance between cells i and j</td>
</tr>
<tr>
<td><span>\(R_{ij}^v\)</span></td>
<td>sPa/kg</td>
<td>Vapor Resistance between cells i and j</td>
</tr>
<tr>
<td><sup><span>\(R_{ij}^w\)</span></sup></td>
<td>s/kg</td>
<td>Liquid Moisture Resistance between cells i and j</td>
</tr>
<tr>
<td><sup><span>\(q_i^v\)</span></sup></td>
<td>W</td>
<td>Heat due to Vaporisation</td>
</tr>
<tr>
<td><sup><span>\(q_i^{adds}\)</span></sup></td>
<td>W</td>
<td>Heat from additional Sources</td>
</tr>
<tr>
<td>P</td>
<td>m3/m3</td>
<td>Material Porosity</td>
</tr>
<tr>
<td>p(as a superscript)</td>
<td>s</td>
<td>Present Time Step</td>
</tr>
<tr>
<td>i,j</td>
<td>-</td>
<td>Cell indices</td>
</tr>

</table>

### HAMT Model Description

Equations and are derived from heat and moisture balance equations and are taken from [Künzel, H.M. (1995)]. They describe a theoretical model for the transfer of heat and moisture through a material.

<div>$$\frac{{\partial H}}{{\partial T}}\frac{{\partial T}}{{\partial \tau }} = \frac{\partial }{{\partial x}}\left( {{k^w}\frac{{\partial T}}{{\partial x}}} \right) + {h_v}\frac{\partial }{{\partial x}}\left( {\frac{\delta }{\mu }\frac{{\partial T}}{{\partial x}}} \right)$$</div>

The three terms in equation describe the storage, transport and generation of heat respectively.

<div>$$\frac{{\partial w}}{{\partial \phi }}\frac{{\partial \phi }}{{\partial \tau }} = \frac{\partial }{{\partial x}}\left( {{D^w}\frac{{\partial w}}{{\partial \phi }}\frac{{\partial \phi }}{{\partial x}}} \right) + \frac{\partial }{{\partial x}}\left( {\frac{\delta }{\mu }\frac{{\partial T}}{{\partial x}}} \right)$$</div>

The three terms in equation describe the storage of moisture, the transport of liquid moisture and the transport of vapor respectively. The equation to calculate the vapor diffusion coefficient in air (<span>\(\delta \)</span>) used in the third term of both equations, is also taken from Künzel,

<div>$$\delta  = \frac{{\left( {2 \times {{10}^{ - 7}} \times {{\left( {T + 273.15} \right)}^{0.81}}} \right)}}{{{P_{ambient}}}}$$</div>

The heat storage capacity (<span>\(\frac{\partial H}{\partial T}\)</span>) depends on the moisture content w of the material by the following equation.

<div>$$\frac{{\partial H}}{{\partial T}} = \left( {c\rho  + {c^w}w} \right)$$</div>

The moisture content of the material w and the vapor diffusion resistance factor μ depend on the relative humidity inside the material. The parameters <span>\(\frac{\partial w}{\partial \phi}\)</span>, <span>\({k^w}\)</span> and <span>\({D^w}\)</span>are also moisture dependent parameters.

The following sections describe how the above equations are used within the HAMT model.

#### Surfaces, Material Layers and Cells

“Surfaces” are made of a number of layers of potentially any combination of materials. Each surface is split into its constituent materials and is then split up further into cells through its depth. HAMT will generate no more than 10 cells per material with widths that are thinner near the boundaries of each material where most changes are expected and detail is needed.

#### Heat Transfer

Equation 1 can be re-written and used to describe the heat storage and transfer through the i<sup>th</sup> cell in a surface.

<div>$$\left( {{c_i}{\rho_i} + {c^w}{w_i}} \right)\Delta {V_i}\frac{{T_i^{p + 1} - T_i^p}}{{\Delta \tau }} = \sum\limits_j {k_{ij}^w{A_{ij}}\frac{{T_j^{p + 1} - T_i^{p + 1}}}{{{x_{ij}}}}}  + \sum\limits_j {{h_v}\frac{{{\delta_{ij}}}}{{{\mu_{ij}}}}{A_{ij}}\frac{{p_j^{p + 1} - p_i^{p + 1}}}{{{x_{ij}}}}} $$</div>

In the one dimensional case there are only two adjacent cells each labelled j. The heat generated due to vaporisation<span>\(q_i^v\)</span> can be calculated separately.

<div>$$q_i^v = \sum\limits_j {{h_v}\frac{{{\delta_{ij}}}}{{{\mu_{ij}}}}{A_{ij}}\frac{{p_j^{p + 1} - p_i^{p + 1}}}{{{x_{ij}}}}} $$</div>

Rearranging equation and including other sources of heat (<span>\(q_i^{adds}\)</span>) such as radiation from other surfaces in the calculation gives the temperature in a cell in the next time step as,

<div>$$T_i^{p + 1} = \frac{{\sum\nolimits_j {\frac{{T_j^{p + 1}}}{{R_{ij}^h}}}  + q_i^v + q_i^{adds} + C_i^h\frac{{T_i^p}}{{\Delta \tau }}}}{{\frac{{C_i^h}}{{\Delta \tau }} + \sum\nolimits_j {\frac{1}{{R_{ij}^h}}} }}$$</div>

where<span>\(C_i^h = \left( {{c_i}{\rho_i} + {c^w}{w_i}} \right)\Delta {V_i}\)</span> is thermal heat capacitance of cell i and <span>\(R_{ij}^h = \frac{x_{ij}}{k_{ij}A_{ij}}\)</span> is the thermal resistance between cells i and j.

This equation can be solved using the Gauss-Seidel iteration technique. The i<sup>th</sup> cell temperature is calculated whilst the j<sup>th</sup> cell temperatures are kept as up to date as possible. The iteration is stopped when the maximum difference between two consecutive calculations in all cells is less than a threshold of 0.002°C.

#### Moisture Content w

The moisture content (w) of a cell is needed for the calculation of the heat transfer through the cell as it affects the thermal resistance and heat capacitance. The moisture content of cells is calculated from the relative humidity (RH) of the material. The relationship between w and the RH for each material is known as the sorption isotherm and measured data points are entered into EnergyPlus as a series of coordinates. HAMT interpolates between the measurements to obtain the moisture content of a material for any RH value. The sorption isotherm input is via the MaterialProperty:HeatAndMoistureTransfer:SorptionIsotherm object and is described in the Input Output Reference document.

#### Porosity P

The porosity of a material (P) is an input variable and defined as the maximum fraction, by volume, of a material that can be taken up with moisture. It is used to calculate the maximum point on the sorption isotherm curve.  The porosity is entered for each material via the MaterialProperty:HeatAndMoistureTransfer:Settings object, as described in the Input Output Reference document.

#### Moisture Dependant Thermal Conductivity k<sup>w</sup>

The thermal conductivity (k<sup>w</sup>) of the cell is determined by interpolating between data points of thermal conductivity versus the moisture content of the material, entered into EnergyPlus via the MaterialProperty:HeatAndMoistureTransfer:ThermalConductivity object. The moisture content is determined via the sorption isotherm which gives the moisture content as a function of Relative Humidity.

#### Moisture Dependant Moisture Diffusion Coefficient μ

This is used in the third term of equation to describe the heat transfer due to vapor movement. It is determined by interpolating between data points of moisture diffusion coefficient versus the moisture content of the material, entered into EnergyPlus via the MaterialProperty:HeatAndMoistureTransfer:Diffusion object. A simple linear interpolation is used to obtain the conductivity between measured points.

#### Moisture Transfer

Moisture, as well as heat, is transported through materials as either liquid (w) or vapor (p). There are two different potentials that control the movement though the material. Liquid transfer is driven by differences in relative humidity whereas vapor transfer is driven by differences in vapor pressure. Materials also have a capacity to store moisture. Equation can be re-written for a discrete cell in a continuous material.

<div>$$\frac{{dw}}{{d{\phi_i}}}\Delta {V_i}\frac{{\phi_i^{p + 1} - \phi_i^p}}{{\Delta \tau }} = \sum\limits_j {{k_{ij}}{A_{ij}}\frac{{\phi_j^{p + 1} - \phi_i^{p + 1}}}{{{x_{ij}}}}}  + \sum\limits_j {\frac{{{\delta_{ij}}}}{{{\mu_{ij}}}}{A_{ij}}\frac{{p_j^{p + 1} - p_i^{p + 1}}}{{{x_{ij}}}}} $$</div>

Equation can be rearranged to provide the relative humidity of the i<sup>th</sup> cell in the next time step.

<div>$$\phi_i^{p + 1} = \frac{{\sum\nolimits_j {\frac{{\phi_j^{p + 1}}}{{R_{ij}^w}}}  + \sum\nolimits_j {\frac{{p_i^{p + 1}}}{{R_{ij}^v}}}  + C_i^w\frac{{\phi_i^p}}{{\Delta \tau }}}}{{\frac{{C_i^w}}{{\Delta \tau }} + \sum\nolimits_j {\frac{1}{{R_{ij}^w}} + \sum\nolimits_j {\frac{{p_i^{sat}}}{{R_{ij}^v}}} } }}$$</div>

where <span>\(C_i^w = \frac{dw}{d\phi_{i}}\Delta {V_i}\)</span> is the "Moisture Capacitance" of cell i,

<div>$$R_{ij}^w = \frac{{{x_{ij}}}}{{{A_{ij}}D_{ij}^w\frac{{dw}}{{d\phi }}}}$$</div>

is the moisture resistance between cells i and j and <span>\(R_{ij}^v = \frac{\mu_{ij}x_{ij}}{A_{ij}\delta_{ij}}\)</span> is the vapor resistance between cells i and j.

Equation can be used together with the heat equation in an alternate step by step fashion to calculate the new temperature and relative humidity profiles for each cell for the next time step.

#### Liquid Transport Coefficient D<sup>w</sup>

The Moisture Dependant Liquid Transport Coefficient is entered as a series of moisture density and liquid transport coefficient data points. There are two different coefficients, one for suction, where the surface is wet due to rain, and one for redistribution where the surface is no longer wet. If the weather file has a rain flag it is used to switch between these two types of coefficient. HAMT-SUCTION and HAMT-REDISTRIBUTION.

#### Moisture Dependent Moisture Capacity <span>\(\frac{\partial w}{\partial \phi}\)</span>

This is simply the gradient of moisture sorption isotherm at the RH of the material.

#### Convective Heat Transfer

The internal and external heat transfer coefficients are used to calculate the thermal resistance of the boundary layer between the zone air and the surface of the surface. They are either supplied by the user via the advanced surface concepts object “SurfaceProperty:ConvectionCoefficients” or, if these are not provided, dynamic values are calculated.

#### Convective Vapor Transfer

The internal and external vapor transfer coefficients are used to calculate the resistance to vapour transfer of the boundary layer between the zone air and the surface of the surface. They are also either supplied by the user via the advanced surface concept object SurfaceProperties:VaporCoefficients. If these are not provided then dynamic values are calculated based on the convective heat transfer coefficients.

#### Initial Moisture Content

At the start of an EnergyPlus simulation “warm up” days are used to ensure that the temperatures of surfaces come to equilibrium with the environment before the simulation starts proper. Moisture content within some building fabrics can take a very long time to come to equilibrium with its environment and it is therefore necessary to set initial or typical values of moisture content for each material to be used at the start of the simulation. These initial values are entered for each material via the MaterialProperty:HeatAndMoistureTransfer:Settings object as described in the Input Output Reference document.

#### Using the Model

As an illustration of the use of the Heat and Moisture Transfer (HAMT) model, the material properties for a small sample of six generic materials have been provided in the EnergyPlus Reference DataSets (MoistureMaterials.idf). The properties were synthesised from the Annex 24 database [Kumar Kumaran, M. (1996)], supplemented, when required, by data from the database of the WUFI model [WUFI (1999)] and are therefore not related to any unique, measured material. Users should consult material property catalogues and other primary sources when the properties of a specific material are required.

Moisture and heat from the surfaces are used by EnergyPlus to calculate the room air temperature and moisture content. EnergyPlus with HAMT works best with as short a time step as possible. However the optimum time step which gives a good prediction for a short computing time will very much depend on the nature of the weather and type of building. Buildings with frequent and large changes in internal and external temperature will need a small time step, maybe even 60 steps per hour. Slowly evolving temperatures and relative humidity’s will not require such a short time step and 20, or even 6, steps per hour may be sufficient.

### References

Künzel, H.M. (1995) Simultaneous Heat and Moisture Transport in Building Components. One- and two-dimensional calculation using simple parameters. IRB Verlag 1995

Holman, J.P. (2002) Heat Transfer, Ninth Edition. McGraw-Hill

Winterton, R.H.S. (1997) Heat Transfer. (Oxford Chemistry Primers; 50) Oxford University Press

Kumar Kumaran, M. (1996) IEA ANNEX 24, Final Report, Volume 3

WUFI (1999) version 2.2 Simultaneous Heat and Moisture Transport in Building components. Fraunhofer IBP, Holzkirchen, Germany

Effective Moisture Penetration Depth (EMPD) Model
---------------------------------------------------------------------------

### Overview

Moisture has little effect on heating system performance, but a profound effect on the performance of air conditioning systems.  In order to accurately describe building performance during periods when cooling is needed, it is very important to know the moisture conditions of the building.  If one assumes that all building moisture is contained in the room air, then one ignores the fact that the materials that bound the room (e.g. wall surfaces, furnishings, linens, etc.) store and release moisture.  Thus, to assume that the only moisture that effects cooling system performance is contained in the room air is a false, and it can lead to significant error in the prediction of room moisture conditions and cooling system loads.

The EMPD (Effective Moisture Penetration Depth) model is a simplified, lumped approach to simulate surface moisture adsorption and desorption.

### EMPD Model Description

The EMPD concept assumes that a thin layer (δ<sub>M</sub>) close to the wall surface behaves dynamically and exchanges moisture with the air domain when exposed to cyclic air moisture pulses.  For short periods where the cyclic integral of the total moisture adsorption and desorption is near zero (i.e. there is no net moisture storage), the EMPD concept has been shown to be a reasonable approximation of reality (Kerestecioglu et al, 1989).  In other words, the following constraint must be met:

<div>$$\int_{{\tau_1}}^{{\tau_2}} {\frac{{dU}}{{d\tau }}} d\tau  = 0$$</div>

where, τ<sub>2</sub>-τ<sub>1</sub> denotes the finite time interval over which the equation holds.  The EMPD model assumes no spatial distribution of moisture content across the thickness (L) of the solid; rather, a thin layer (δ<sub>M</sub>) of uniform moisture content (U) is assumed to represent the total moisture content of the solid. This may be mathematically stated as:

<div>$$\int_0^L {U(x)dx = U{\delta_M}} $$</div>

For most building materials, the equilibrium moisture sorption isotherm can be defined by the following general equation (Kerestecioglu et al. 1988):

<div>$$U = a{\varphi ^b} + c{\varphi ^d}$$</div>

where

<div>$$\varphi  \approx \frac{{{W^*}}}{{{W_{sat}}^*}}$$</div>

and

<div>$${W_{sat}}^* = \frac{1}{{{R_v}{\rho_a}{T^*}}}\exp \left( {23.7093 - \frac{{4111}}{{{T^*} - 35.45}}} \right)$$</div>

 Given that U=U(W<sup>\*</sup>,T<sup>\*</sup>), the moisture content may be differentiated with respect to time in the following manner:

<div>$$\frac{{du}}{{d\tau }} = \frac{{\partial U}}{{\partial {W^*}}}\frac{{d{W^*}}}{{d\tau }} + \frac{{\partial U}}{{\partial {T^*}}}\frac{{d{T^*}}}{{d\tau }} = {A_T}\frac{{d{W^*}}}{{d\tau }} - {B_\rho }\frac{{d{T^*}}}{{d\tau }}$$</div>

where A<sub>T</sub> and B<sub>ρ</sub> are the isothermal moisture capacity and thermo-gradient coefficient, respectively.  From Eqs. , and , they can be expressed as:

<div>$${A_T} = \frac{{ab{\varphi ^b} + cd{\varphi ^d}}}{{{W^*}}}$$</div>

and

<div>$${B_\rho } =  - \left[ {\frac{1}{{{T^*}}} - \frac{{4111}}{{{{({T^*} - 35.45)}^2}}}} \right]*(ab{\varphi ^b} + cd{\varphi ^d})$$</div>

The lumped mass transfer equation for the i-th solid domain may be written as

<div>$${(A{\rho_b}{\delta_M})_i}\frac{{d{U_i}}}{{d\tau }} = {h_{M,i}}{A_i}({W_r} - {W_i}^*)$$</div>

Using Eqs. , , and , one obtains the final equation needed for closure moisture transfer at internal surface.

<div>$${({A_i}{\rho_b}{\delta_M}{A_T})_i}\frac{{d{W_i}^*}}{{d\tau }} = {h_{M,i}}{A_i}({W_r} - {W_i}^*) + {(A{\rho_b}{\delta_M}{B_\rho })_i}\frac{{d{T_i}^*}}{{d\tau }}$$</div>

The energy equation for the envelope contains the surface temperature and is given by the conduction equation

<div>$$\rho {C_p}\frac{{dT}}{{d\tau }} = \nabla  \cdot (k\nabla T)$$</div>

with the boundary conditions at interior surface

<div>$$ - k\nabla T =  - {q_T}`` + {h_T}({T^*} - {T_r}) + \lambda {h_M}({W^*} - {W_r})$$</div>

A more detailed account of the numerical solution procedure can be found in Kerestecioglu et al. (1988).

### EMPD Value Determination

An effective moisture penetration depth may be determined from either experimental or detailed simulation data by using actual surface areas and moisture vapor diffusivity.  An empirical function derived from the detailed simulation may be used to determine the EMPD value (Kerestecioglu et al, 1989):

<div>$${\delta_M} = 12.567024 - 12.21373*\exp \left( { - 267.0211*{D_v}^{0.7}*{\xi ^{ - 0.7}}} \right)$$</div>

where

<div>$$\xi  = \left| {\frac{{\Delta \varphi }}{{\Delta \tau }}} \right|$$</div>

Figure 17 gives the EMPD values to be used for various vapor diffusivities evaluated at different ambient excitations.

![](media/image241.svg.png)

Figure 17. Limit of Effective Penetration Depth Values for Various Vapor Diffusivities at Different Ambient Excitations.

### EMPD Nomenclature

A                = Area [m<sup>2</sup>]

A<sub>T</sub>               = Isothermal moisture capacity [m<sup>3</sup>/kg]

B<sub>ρ</sub>               = Thermo-gradient coefficient [kg/kg-K]

C<sub>p</sub>               = Specific heat [J/kg.K]

h<sub>M</sub>               = Convective mass transfer coeff. [kg/m<sup>2</sup>-s]

h<sub>T</sub>               = Convective heat transfer coeff. [W/m<sup>2</sup>-K]

k                = Thermal conductivity [W/m-K]

L                = Length [m]

q"<sub>T</sub>              = Imposed heat flux [W/m<sup>2</sup>]

R<sub>v</sub>               = Ideal gas constant [461.52 J/kg-K]

T                = Temperature [K]

U                = Moisture content [kg/kg]

W               = Humidity ratio [kg/kg]

#### Greek letters

δ<sub>M</sub>              = Effective penetration depth for moisture equation [m]

λ                = Heat of vaporization [J/kg]

ρ                = Density [kg/m<sup>3</sup>]

τ                 = Time [s]

φ                = Relative humidity [0 to 1]

ξ                 = Ambient moisture excitation rate [1/h]



#### Subscripts and superscripts

a                = Air

b                = Bulk

\*                 = Surface

i                 = i-th surface

### References

Kerestecioglu, A., Swami, M., Dabir, R., Razzaq, N., and Fairey, P., 1988, "Theoretical and Computational Investigation of Algorithms for Simultaneous Heat and Moisture Transport in Buildings," FSEC-CR-191-88, Florida Solar Energy Center, Cape Canaveral, FL.

Kerestecioglu, A., M. Swami and A. Kamel, 1989, "Theoretical and Computational Investigation of Simultaneous Heat and Moisture Transfer in Buildings: Effective Penetration Depth Theory."  ASHRAE Winter Meeting, Atlanta, GA.

Kerestecioglu, A., M. V. Swami, P. Brahma, L. Gu, P. Fairey, and S. Chandra, 1989, “FSEC 1.1 User’s Manual,” Florida Solar Energy Center, Cape Canaveral, FL

Outside Surface Heat Balance
----------------------------

![](media/image242.png)

Figure 18. Outside Heat Balance Control Volume Diagram

The heat balance on the outside face is:

<div>$${q''_{\alpha sol}} + {q''_{LWR}} + {q''_{conv}} - {q''_{ko}} = 0$$</div>

where:

<sub><span>\({q''_{\alpha sol}}\)</span> </sub>= Absorbed direct and diffuse solar (short wavelength) radiation heat flux.

<sub><span>\({q''_{LWR}}\)</span> </sub>= Net long wavelength (thermal) radiation flux exchange with the air and surroundings.

<sub><span>\({q''_{conv}}\)</span> </sub>= Convective flux exchange with outside air.

<sub><span>\({q''_{ko}}\)</span> </sub> = Conduction heat flux (q/A) into the wall.

All terms are positive for net flux to the face except the conduction term, which is traditionally taken to be positive in the direction from outside to inside of the wall.  Simplified procedures generally combine the first three terms by using the concept of a *sol-air temperature*.  Each of these heat balance components is introduced briefly below.

### External Shortwave Radiation

<sub><span>\({q''_{\alpha sol}}\)</span> </sub> is calculated using procedures presented later in this manual and includes both direct and diffuse incident solar radiation absorbed by the surface face.  This is influenced by location, surface facing angle and tilt, surface face material properties, weather conditions, etc.

### External Longwave Radiation

<span>\({q''_{LWR}}\)</span> is a standard radiation exchange formulation between the surface, the sky, and the ground.  The radiation heat flux is calculated from the surface absorptivity, surface temperature, sky and ground temperatures, and sky and ground view factors.

The longwave radiation heat exchange between surfaces is dependent on surface temperatures, spatial relationships between surfaces and surroundings, and material properties of the surfaces. The relevant material properties of the surface, emissivity e and absorptivity a, are complex functions of temperature, angle, and wavelength for each participating surface. However, it is generally agreed that reasonable assumptions for building loads calculations are (Chapman 1984; Lienhard 1981):

·        each surface emits or reflects diffusely and is gray and opaque (a = e, t = 0, r = 1- e)

·        each surface is at a uniform temperature

·        energy flux leaving a surface is evenly distributed across the surface,

·        the medium within the enclosure is non-participating.

These assumptions are frequently used in all but the most critical engineering applications.



Table 3. Nomenclature List of Variables.

<table class="table table-striped">
<tr>
<th>Mathematical variable</th>
<th>Description</th>
<th>Units</th>
<td>Range</td>
</tr>
<tr>
<td>q&quot;<sub>LWR</sub></td>
<td>Exterior surface longwave radiation flux</td>
<td>W/m<sup>2</sup></td>
<td>-</td>
</tr>
<tr>
<td>h<sub>r</sub></td>
<td>Linearized radiative heat transfer  coefficient to air temperature</td>
<td>W/(m<sup>2</sup> K)</td>
<td>-</td>
</tr>
<tr>
<td>T<sub>surf</sub></td>
<td>Surface Outside face temperatures</td>
<td>K</td>
<td>-</td>
</tr>
<tr>
<td>T<sub>air</sub></td>
<td>Outside air temperature</td>
<td>K</td>
<td>-</td>
</tr>
<tr>
<td>T<sub>gnd</sub>
 </td>
<td>Environmental ground surface temperature</td>
<td>K</td>
<td>-</td>
</tr>
<tr>
<td>T<sub>sky</sub></td>
<td>Sky Effective temperature</td>
<td>K</td>
<td>-</td>
</tr>
<tr>
<td>F<sub>gnd</sub></td>
<td>view factor of wall surface to ground surface</td>
<td>-</td>
<td>0~1</td>
</tr>
<tr>
<td>F<sub>sky</sub></td>
<td>View factor of wall surface to sky</td>
<td>-</td>
<td>0~1</td>
</tr>
<tr>
<td>F<sub>air</sub></td>
<td>View factor of wall surface to air</td>
<td>-</td>
<td>0~1</td>
</tr>
<tr>
<td>e</td>
<td>Surface long-wave emissivity</td>
<td>-</td>
<td>0~1</td>
</tr>
<tr>
<td>s</td>
<td>Stefan-Boltzmann constant</td>
<td>W/m<sup>2</sup>-K<sup>4</sup></td>
<td>0.0000000567</td>
</tr>

</table>

Consider an enclosure consisting of building exterior surface, surrounding ground surface, and sky.  Using the assumptions above, we can determine the longwave radiative heat flux at the building exterior surface (Walton 1983; McClellan and Pedersen 1997).  The total longwave radiative heat flux is the sum of components due to radiation exchange with the ground, sky, and air.

<div>$$q_{LWR}^`` = {q''_{gnd}} + {q''_{sky}} + {q''_{air}}$$</div>

Applying the Stefan-Boltzmann Law to each component yields:

<div>$$q_{_{LWR}}^`` = \varepsilon \sigma {F_{gnd}}(T_{gnd}^4 - T_{surf}^4) + \varepsilon \sigma {F_{sky}}(T_{sky}^4 - T_{surf}^4) + \varepsilon \sigma {F_{air}}(T_{air}^4 - T_{surf}^4)$$</div>

where

e=long-wave emittance of the surface

s=Stefan-Boltzmann constant

F<sub>gnd</sub> = view factor of wall surface to ground surface temperature

F<sub>sky</sub> = view factor of wall surface to sky temperature

F<sub>air</sub> =view factor of wall surface to air temperature

T<sub>surf</sub> = outside surface temperature

T<sub>gnd</sub> = ground surface temperature

T<sub>sky</sub> = sky temperature

T<sub>air</sub> = air temperature

Linearized radiative heat transfer coefficients are introduced to render the above equation more compatible with the heat balance formulation,

<div>$$q_{_{LWR}}^`` = {h_{r,gnd}}({T_{gnd}} - {T_{surf}}) + {h_{r,sky}}({T_{sky}} - {T_{surf}}) + {h_{r,air}}({T_{air}} - {T_{surf}})$$</div>

where

<div>$${h_{r,gnd}} = \frac{{\varepsilon \sigma {F_{gnd}}(T_{surf}^4 - T_{gnd}^4)}}{{{T_{surf}} - {T_{gnd}}}}$$</div>

<div>$${h_{r,sky}} = \frac{{\varepsilon \sigma {F_{sky}}(T_{surf}^4 - T_{sky}^4)}}{{{T_{surf}} - {T_{sky}}}}$$</div>

<div>$${h_{r,air}} = \frac{{\varepsilon \sigma {F_{air}}(T_{surf}^4 - T_{air}^4)}}{{{T_{surf}} - {T_{air}}}}$$</div>

The longwave view factors to ground and sky are calculated with the following expressions (Walton 1983):

<div>$${F_{ground}} = 0.5(1 - \cos \phi )$$</div>

<div>$${F_{sky}} = 0.5(1 + \cos \phi )$$</div>

where *f* is the tilt angle of the surface.  The view factor to the sky is further split between sky and air radiation by:

<div>$$\beta  = \sqrt {0.5\left( {1 + \cos \phi } \right)} $$</div>

The ground surface temperature is assumed to be the same as the air temperature.  The final forms of the radiative heat transfer coefficients are shown here.

<div>$${h_{r,gnd}} = \frac{{\varepsilon \sigma {F_{gnd}}(T_{surf}^4 - T_{air}^4)}}{{{T_{surf}} - {T_{air}}}}$$</div>

<div>$${h_{r,sky}} = \frac{{\varepsilon \sigma {F_{sky}}\beta (T_{surf}^4 - T_{sky}^4)}}{{{T_{surf}} - {T_{sky}}}}$$</div>

<div>$${h_{r,air}} = \frac{{\varepsilon \sigma {F_{sky}}\left( {1 - \beta } \right)(T_{surf}^4 - T_{air}^4)}}{{{T_{surf}} - {T_{air}}}}$$</div>

### References

ASHRAE. 1993. 1993 ASHRAE Handbook – Fundamentals. Atlanta: American Society of Heating, Refrigerating, and Air-Conditioning Engineers, Inc.

Chapman, A. J. 1984. Heat Transfer, 4<sup>th</sup> Edition, New York: Macmillan Publishing Company.

Lienhard, J. H. 1981. A Heat Transfer Textbook, Englewood Cliffs, N.J.: Prentice-Hall, Inc.

McClellan, T. M., and C. O. Pedersen. 1997. Investigation of Outside Heat Balance Models for Use in a Heat Balance Cooling Load Calculation. ASHRAE Transactions, Vol. 103, Part 2, pp. 469-484.

Walton, G. N. 1983. Thermal Analysis Research Program Reference Manual. NBSSIR 83-2655. National Bureau of Standards.

### Atmospheric Variation

All buildings are located in the troposphere, the lowest layer of the atmosphere.  The troposphere extends from sea level to an altitude of 11 km.  Throughout the troposphere, air temperature decreases almost linearly with altitude at a rate of approximately 1°C per 150 m.  Barometric pressure decreases more slowly.  Wind speed, on the other hand, increases with altitude.

Because the atmosphere changes with altitude (defined as *height above ground* in this case), tall buildings can experience significant differences in local atmospheric properties between the ground floor and the top floor.  Buildings interact with the atmosphere through convective heat transfer between the outdoor air and the exterior surfaces of the building envelope, and through the exchange of air between the outside and inside of the building via infiltration and ventilation.

Impetus for using this modeling is illustrated in the next table.  Using a 70 story (284 meters) building as an example, the atmospheric variables are significant.

Table 4.  Atmospheric Variables at Two Different Altitudes above Ground Level

<table class="table table-striped">
<tr>
<th>Variable</th>
<th>1.5 Meters</th>
<th>284 meters</th>
<th>Absolute Diff</th>
<th>Percent Diff</th>
</tr>
<tr>
<td>Air Temperature</td>
<td>15°C</td>
<td>13.15°C</td>
<td>1.85°C</td>
<td>12.3%</td>
</tr>
<tr>
<td>Barometric Pressure</td>
<td>101,325 Pa</td>
<td>97,960 Pa</td>
<td>3,365 Pa</td>
<td>3.3%</td>
</tr>
<tr>
<td>Wind Speed</td>
<td>2.46 m/s</td>
<td>7.75 m/s</td>
<td>5.29 m/s</td>
<td>215%</td>
</tr>

</table>

Comparing the annual energy usage between 60 discretely modeled floors of a building, it turns out that the effect due to wind speed change is dominant over the first ten floors.  But at floor 25, surprisingly, the effect due to air temperature has caught up and is about equal to the effect of wind speed.  Above floor 25 the effect due to air temperature is now dominant.  Clearly it is desirable to model air temperature variation with altitude for high-rise buildings.

To accommodate atmospheric variation EnergyPlus automatically calculates the local outdoor air temperature and wind speed separately for each zone and surface that is exposed to the outdoor environment.  The zone centroid or surface centroid are used to determine the height above ground.  Only local outdoor air temperature and wind speed are currently calculated because they are important factors for the exterior convection calculation for surfaces (see Exterior Convection below) and can also be factors in the zone infiltration and ventilation calculations.  Variation in barometric pressure, however, is considered when using the Airflow Network objects.

#### Local Outdoor Air Temperature Calculation

Variation in outdoor air temperature is calculated using the U.S. Standard Atmosphere (1976).  According to this model, the relationship between air temperature and altitude in a given layer of the atmosphere is:

<div>$${T_z} = {T_b} + L\left( {{H_z} - {H_b}} \right)$$</div>

where

*T<sub>z</sub>* = air temperature at altitude *z*

*T<sub>b</sub>* = air temperature at the base of the layer, i.e., ground level for the troposphere

*L* = air temperature gradient, equal to –0.0065 K/m in the troposphere

*H<sub>b</sub>* = offset equal to zero for the troposphere

*H<sub>z</sub>* = geopotential altitude.

The variable *H<sub>z</sub>* is defined by:

<div>$${H_z} = \frac{{Ez}}{{\left( {E + z} \right)}}$$</div>

where

*E* = 6,356 km, the radius of the Earth

*z* = altitude.

For the purpose of modeling buildings in the troposphere, altitude *z* refers to the height above ground level, not the height above sea level.  The height above ground is calculated as the height of the centroid, or area-weighted center point, for each zone and surface.

The air temperature at ground level, *T<sub>b</sub>*, is derived from the weather file air temperature by inverting the equation above:

<div>$${T_b} = {T_{z,met}} - L\left( {\frac{{E{z_{met}}}}{{E + {z_{met}}}} - {H_b}} \right)$$</div>

where

*T<sub>z,met</sub>* = weather file air temperature (measured at the meteorological station)

*z<sub>met</sub>* = height above ground of the air temperature sensor at the meteorological station.

The default value for *z<sub>met</sub>* for air temperature measurement is 1.5 m above ground.  This value can be overridden by using the Site:WeatherStation object.

#### Local Wind Speed Calculation

Chapter 16 of the Handbook of Fundamentals (ASHRAE 2005).  The wind speed measured at a meteorological station is extrapolated to other altitudes with the equation:

<div>$${V_z} = {V_{met}}{\left( {\frac{{{\delta_{met}}}}{{{z_{met}}}}} \right)^{{\alpha_{met}}}}{\left( {\frac{z}{\delta }} \right)^\alpha }$$</div>

where

*z* = altitude, height above ground

*V<sub>z</sub>* = wind speed at altitude *z*

*a* = wind speed profile exponent at the site

*d* = wind speed profile boundary layer thickness at the site

*z<sub>met</sub>* = height above ground of the wind speed sensor at the meteorological station

*V<sub>met</sub>* = wind speed measured at the meteorological station

*a<sub>met</sub>* = wind speed profile exponent at the meteorological station

*d<sub>met</sub>* = wind speed profile boundary layer thickness at the meteorological station.

The wind speed profile coefficients *a*, *d*, *a<sub>met</sub>*, and *d<sub>met</sub>*, are variables that depend on the roughness characteristics of the surrounding terrain.  Typical values for *a* and d are shown in the following table:



Table 5. Wind Speed Profile Coefficients (ASHRAE Fundamentals 2005).

<table class="table table-striped">
<tr>
<th>Terrain Description</th>
<th>Exponent,a</th>
<th>Boundary Layer Thickness,d (m)</th>
</tr>
<tr>
<td>Flat, open country</td>
<td>0.14</td>
<td>270</td>
</tr>
<tr>
<td>Rough, wooded country</td>
<td>0.22</td>
<td>370</td>
</tr>
<tr>
<td>Towns and cities</td>
<td>0.33</td>
<td>460</td>
</tr>
<tr>
<td>Ocean</td>
<td>0.10</td>
<td>210</td>
</tr>
<tr>
<td>Urban, industrial, forest</td>
<td>0.22</td>
<td>370</td>
</tr>

</table>



The terrain types above map to the options in the *Terrain* field of the Building object.  The *Terrain* field can be overridden with specific values for *a* and d by using the Site:HeightVariation object.

The default value for *z<sub>met</sub>* for wind speed measurement is 10 m above ground.  The default values for *a<sub>met</sub>* and *d<sub>met</sub>* are 0.14 and 270 m, respectively, because most meteorological stations are located in an open field.  These values can be overridden by using the Site:WeatherStation object.

### Outdoor/Exterior Convection

Heat transfer from surface convection is modeled using the classical formulation:

<div>$${Q_c} = {h_{c,ext}}A\left( {{T_{surf}} - {T_{air}}} \right)$$</div>

where

*Q<sub>c</sub>* = rate of exterior convective heat transfer

*h<sub>c,ext</sub>* = exterior convection coefficient

*A* = surface area

*T<sub>surf</sub>* = surface temperature

*T<sub>air</sub>* = outdoor air temperature

Substantial research has gone into the formulation of models for estimating the exterior convection coefficient. Since the 1930's there have been many different methods published for calculating this coefficient, with much disparity between them (Cole and Sturrock 1977; Yazdanian and Klems 1994).  More recently Palyvos (2008) surveyed correlations cataloging some 91 different correlations into four categories based on functional form of the model equation.  EnergyPlus therefore offers a wide selection of different methods for determining values for *h<sub>c,ext</sub>*. The selection of model equations for *h<sub>c,ext</sub>* can be made at two different levels. The first is the set of options available in the input object SurfaceConvectionAlgorithm:Outside that provides a way of broadly selecting which model equations are applied throughout the model.  The input objects SurfaceProperty:ConvectionCoefficients and SurfaceProperty:ConvectionCoefficients:MultipleSurface also provide ways of selecting which model equations or values are applied for specific surfaces.  These basic options are identified by the key used for input and include:

* SimpleCombined

* TARP

* MoWiTT

* DOE-2

* AdaptiveConvectionAlgorithm

Note that when the outside environment indicates that it is raining, the exterior surfaces (exposed to wind) are assumed to be wet.  The convection coefficient is set to a very high number (1000) and the outside temperature used for the surface will be the wet-bulb temperature.  (If you choose to report this variable, you will see 1000 as its value.)

When the AdaptiveConvectionAlgorithm is used, there is a second, deeper level of control available for selecting among a larger variety of *h<sub>c,ext</sub>* equations and also defining custom equations using curve or table objects.  These options are described in this section.

In addition to the correlation choices described below, it is also possible to override the convection coefficients on the outside of any surface by other means:

* Use the SurfaceProperty:ConvectionCoefficients object in the input file to set the convection coefficient value on either side of any surface.

* Use the SurfaceProperty:OtherSideCoefficients object in the input file to set heat transfer coefficients and temperatures on surfaces.

* Use the EnergyManagementSystem Actuators that are available for overriding h<sub>c</sub> values.

These options can also use schedules to control values over time. Specific details are given in the Input Output Reference document.

#### Simple Combined

The simple algorithm uses surface roughness and local surface windspeed to calculate the exterior heat transfer coefficient (key:SimpleCombined).  The basic equation used is:

<div>$$h = D + E{V_z} + F{V_z}^2$$</div>

where

*h* = heat transfer coefficient

*V<sub>z</sub>* = local wind speed calculated at the height above ground of the surface centroid

D, E, F = material roughness coefficients

The roughness correlation is taken from Figure 1, Page 22.4, ASHRAE Handbook of Fundamentals (ASHRAE 1989).  The roughness coefficients are shown in the following table:

Table 6.  Roughness Coefficients D, E, and F.

<table class="table table-striped">
<tr>
<th>Roughness Index</th>
<th>D</th>
<th>E</th>
<th>F</th>
<th>Example Material</th>
</tr>
<tr>
<td>1 (Very Rough)</td>
<td>11.58</td>
<td>5.894</td>
<td>0.0</td>
<td>Stucco</td>
</tr>
<tr>
<td>2 (Rough)</td>
<td>12.49</td>
<td>4.065</td>
<td>0.028</td>
<td>Brick</td>
</tr>
<tr>
<td>3 (Medium Rough)</td>
<td>10.79</td>
<td>4.192</td>
<td>0.0</td>
<td>Concrete</td>
</tr>
<tr>
<td>4 (Medium Smooth)</td>
<td>8.23</td>
<td>4.0</td>
<td>-0.057</td>
<td>Clear pine</td>
</tr>
<tr>
<td>5 (Smooth)</td>
<td>10.22</td>
<td>3.1</td>
<td>0.0</td>
<td>Smooth Plaster</td>
</tr>
<tr>
<td>6 (Very Smooth)</td>
<td>8.23</td>
<td>3.33</td>
<td>-0.036</td>
<td>Glass</td>
</tr>

</table>

Note that the simple correlation yields a combined convection and radiation heat transfer coefficient.  Radiation to sky, ground, and air is included in the exterior convection coefficient for this algorithm.

All other algorithms yield a *convection only* heat transfer coefficient.  Radiation to sky, ground, and air is calculated automatically by the program.

#### TARP ALGORITHM

TARP, or Thermal Analysis Research Program, is an important predecessor of EnergyPlus (Walton 1983).  Walton developed a comprehensive model for exterior convection by blending correlations from ASHRAE and flat plate experiments by Sparrow et. al. In older versions of EnergyPlus, prior to version 6, the “TARP” model was called “Detailed.”  The model was reimplemented in version 6 to use Area and Perimeter values for the group of surfaces that make up a facade or roof, rather than the single surface being modeled.

Table 7. Nomenclature List of Variables.

<table class="table table-striped">
<tr>
<th>Variable</th>
<th>Description</th>
<th>Units</th>
<th>Range</th>
</tr>
<tr>
<td>A</td>
<td>Surface area of the surface</td>
<td>m<sup>2</sup></td>
<td>/= 0</td>
</tr>
<tr>
<td>h<sub>c</sub></td>
<td>Surface exterior convective heat transfer coefficient</td>
<td>W/(m<sup>2</sup>K)</td>
<td>-</td>
</tr>
<tr>
<td>h<sub>f</sub></td>
<td>Forced convective heat transfer coefficient</td>
<td>W/(m<sup>2</sup>K)</td>
<td>-</td>
</tr>
<tr>
<td>h<sub>n</sub></td>
<td>Natural convective heat transfer coefficient</td>
<td>W/(m<sup>2</sup>K)</td>
<td>-</td>
</tr>
<tr>
<td>P</td>
<td>Perimeter of surface</td>
<td>m</td>
<td>-</td>
</tr>
<tr>
<td>R<sub>f</sub></td>
<td>Surface roughness multiplier</td>
<td>-</td>
<td>-</td>
</tr>
<tr>
<td>T<sub>air</sub></td>
<td>Local outdoor air temperature calculated at the height above ground of the surface centroid</td>
<td>°C</td>
<td>-</td>
</tr>
<tr>
<td>T<sub>so</sub></td>
<td>Outside surface temperature</td>
<td>°C</td>
<td>-</td>
</tr>
<tr>
<td>DT</td>
<td>Temperature difference between the surface and air,</td>
<td>°C</td>
<td>-</td>
</tr>
<tr>
<td>V<sub>z</sub></td>
<td>Local wind speed calculated at the height above ground of the surface centroid</td>
<td>m/s</td>
<td>-</td>
</tr>
<tr>
<td>W<sub>f</sub></td>
<td>Wind direction modifier</td>
<td>-</td>
<td>-</td>
</tr>
<tr>
<td>f</td>
<td>Angle between the ground outward normal and the surface outward normal</td>
<td>degree</td>
<td>-</td>
</tr>
<tr>
<td>Roughness Index</td>
<td>Surface roughness index (6=very smooth, 5=smooth, 4=medium smooth, 3=medium rough, 2=rough, 1=very rough)</td>
<td>-</td>
<td>1~6</td>
</tr>

</table>



The Detailed, BLAST, and TARP convection models are very similar.  In all three models, convection is split into forced and natural components (Walton 1981).  The total convection coefficient is the sum of these components.

<div>$${h_c} = {h_f} + {h_n}$$</div>

The forced convection component is based on a correlation by Sparrow, Ramsey, and Mass (1979):

<div>$${h_f} = 2.537{W_f}{R_f}{\left( {\frac{{P{V_z}}}{A}} \right)^{1/2}}$$</div>

where

W<sub>f</sub>  =  1.0 for windward surfaces

or

W<sub>f</sub>  = 0.5 for leeward surfaces

Leeward is defined as greater than 100 degrees from normal incidence (Walton 1981).

The surface roughness multiplier Rf is based on the ASHRAE graph of surface conductance (ASHRAE 1981) and may be obtained from the following table:

Table 8.  Surface Roughness Multipliers (Walton 1981).

<table class="table table-striped">
<tr>
<th>Roughness Index</th>
<th>Rf</th>
<th>Example Material</th>
</tr>
<tr>
<td>1 (Very Rough)</td>
<td>2.17</td>
<td>Stucco</td>
</tr>
<tr>
<td>2 (Rough)</td>
<td>1.67</td>
<td>Brick</td>
</tr>
<tr>
<td>3 (Medium Rough)</td>
<td>1.52</td>
<td>Concrete</td>
</tr>
<tr>
<td>4 (Medium Smooth)</td>
<td>1.13</td>
<td>Clear pine</td>
</tr>
<tr>
<td>5 (Smooth)</td>
<td>1.11</td>
<td>Smooth Plaster</td>
</tr>
<tr>
<td>6 (Very Smooth)</td>
<td>1.00</td>
<td>Glass</td>
</tr>

</table>

The natural convection component *h<sub>n</sub>* is calculated in the same way as the interior "Detailed" model.  The detailed natural convection model correlates the convective heat transfer coefficient to the surface orientation and the difference between the surface and zone air temperatures (where DT = Air Temperature - Surface Temperature).  The algorithm is taken directly from Walton (1983).  Walton derived his algorithm from the ASHRAE Handbook (2001), Table 5 on p. 3.12, which gives equations for natural convection heat transfer coefficients in the turbulent range for large, vertical plates and for large, horizontal plates facing upward when heated (or downward when cooled).   A note in the text also gives an approximation for large, horizontal plates facing downward when heated (or upward when cooled) recommending that it should be half of the facing upward value.  Walton adds a curve fit as a function of the cosine of the tilt angle to provide intermediate values between vertical and horizontal.  The curve fit values at the extremes match the ASHRAE values very well.

For no temperature difference OR a vertical surface the following correlation is used:

<div>$$h = 1.31{\left| {\Delta T} \right|^{\frac{1}{3}}}$$</div>

For (DT &lt; 0.0 AND an upward facing surface)  OR  (DT &gt; 0.0 AND an downward facing surface) an enhanced convection correlation is used:

<div>$$h = \frac{{9.482{{\left| {\Delta T} \right|}^{\frac{1}{3}}}}}{{7.283 - \left| {\cos \Sigma } \right|}}$$</div>

where S is the surface tilt angle.

For (DT &gt; 0.0 AND an upward facing surface)  OR  (DT &lt; 0.0 AND an downward facing surface) a reduced convection correlation is used:

<div>$$h = \frac{{1.810{{\left| {\Delta T} \right|}^{\frac{1}{3}}}}}{{1.382 + \left| {\cos \Sigma } \right|}}$$</div>

where S is the surface tilt angle.

#### MoWiTT Algorithm

Table 9.  Nomenclature List of Variables.

<table class="table table-striped">
<tr>
<th>Variable</th>
<th>Description</th>
<th>Units</th>
<th>Range</th>
</tr>
<tr>
<td>A</td>
<td>Constant</td>
<td>W/(m<sup>2</sup>K(m/s)<sup>b</sup></td>
<td>-</td>
</tr>
<tr>
<td>B</td>
<td>Constant</td>
<td>-</td>
<td>-</td>
</tr>
<tr>
<td>C<sub>t</sub></td>
<td>Turbulent natural convection constant</td>
<td>W/(m<sup>2</sup>K<sup>4/3</sup>)</td>
<td>-</td>
</tr>
<tr>
<td>h<sub>c</sub></td>
<td>Surface exterior convective heat transfer coefficient</td>
<td>W/(m<sup>2</sup>K)</td>
<td>-</td>
</tr>
<tr>
<td>T<sub>so</sub></td>
<td>Outside surface temperature</td>
<td>°C/K</td>
<td>-</td>
</tr>
<tr>
<td>DT</td>
<td>Temperature difference between the surface and air</td>
<td>°C/K</td>
<td>-</td>
</tr>

</table>



The MoWiTT model is based on measurements taken at the Mobile Window Thermal Test (MoWiTT) facility (Yazdanian and Klems 1994).  The correlation applies to very smooth, vertical surfaces (e.g. window glass) in low-rise buildings and has the form:

<div>$${h_c} = \sqrt {{{\left[ {{C_t}{{\left( {\Delta T} \right)}^{\frac{1}{3}}}} \right]}^2} + {{\left[ {aV_z^b} \right]}^2}} $$</div>

Constants a, b and turbulent natural convection constant C<sub>t</sub> are given in Table 10.  The original MoWiTT model has been modified for use in EnergyPlus so that it is sensitive to the local suface’s wind speed which varies with the height above ground.  The original MoWiTT model was formulated for use with the air velocity at the location of the weather station.  As of Version 7.2, EnergyPlus uses the “a” model coefficients derived by Booten et al. (2012) rather than the original values from Yazdanian and Klems (1994).

NOTE:  The MoWiTT algorithm may not be appropriate for rough surfaces, high-rise surfaces, or surfaces that employ movable insulation.

Table 10. MoWiTT Coefficients (Yazdanian and Klems 1994, Booten et al. 2012)

<table class="table table-striped">
<tr>
<th>Wind Direction</th>
<th>Ct</th>
<th>a</th>
<th>b</th>
</tr>
<tr>
<td>(Units)</td>
<td>W/m<sup>2</sup>K<sup>4/3</sup></td>
<td>W/m<sup>2</sup>K(m/s)<sup>b</sup></td>
<td>-</td>
</tr>
<tr>
<td>Windward</td>
<td>0.84</td>
<td>3.26</td>
<td>0.89</td>
</tr>
<tr>
<td>Leeward</td>
<td>0.84</td>
<td>3.55</td>
<td>0.617</td>
</tr>

</table>



#### DOE-2 Model

Table 11.  Nomenclature List of Variables.

<table class="table table-striped">
<tr>
<th>Variable</th>
<th>Description</th>
<th>Units</th>
<th>Range</th>
</tr>
<tr>
<td>a</td>
<td>Constant</td>
<td>W/(m<sup>2</sup>K(m/s)<sup>b</sup></td>
<td>-</td>
</tr>
<tr>
<td>b</td>
<td>Constant</td>
<td>-</td>
<td>-</td>
</tr>
<tr>
<td>h<sub>c</sub></td>
<td>Surface exterior convective heat transfer coefficient</td>
<td>W/(m<sup>2</sup>K)</td>
<td>-</td>
</tr>
<tr>
<td>h<sub>c,glass</sub></td>
<td>Convective heat transfer coefficient for very smooth surfaces (glass)</td>
<td>W/(m<sup>2</sup>K)</td>
<td>-</td>
</tr>
<tr>
<td>h<sub>n</sub></td>
<td>Natural convective heat transfer coefficient</td>
<td>W/(m<sup>2</sup>K)</td>
<td>-</td>
</tr>
<tr>
<td>R<sub>f</sub></td>
<td>Surface roughness multiplier</td>
<td>-</td>
<td>-</td>
</tr>
<tr>
<td>T<sub>so</sub></td>
<td>Outside surface temperature</td>
<td>°C/K</td>
<td>-</td>
</tr>
<tr>
<td>DT</td>
<td>Temperature difference between the surface and air,</td>
<td>°C/K</td>
<td>-</td>
</tr>
<tr>
<td>f</td>
<td>Angle between the ground outward normal and the surface outward normal</td>
<td>radian</td>
<td>-</td>
</tr>

</table>

##### 

The DOE-2 convection model is a combination of the MoWiTT and BLAST Detailed convection models (LBL 1994). The convection coefficient for very smooth surfaces (e.g. glass) is calculated as:

<div>$${h_{c,glass}} = \sqrt {h_n^2 + {{\left[ {aV_z^b} \right]}^2}} $$</div>

h<sub>n</sub> is calculated using Equation or Equation .  Constants a and b are given in Table 10.

For less smooth surfaces, the convection coefficient is modified according to the equation

<div>$${h_c} = {h_n} + {R_f}({h_{c,glass}} - {h_n})$$</div>

where R<sub>f</sub> is the roughness multiplier given by Table 8.

#### Adaptive Convection Algorithm

This algorithm has a structure that allows for finer control over the models used for particular surfaces.  The algorithm for the outside face was developed for EnergyPlus but it borrows concepts and its name from the research done by Beausoleil-Morrison (2000, 2002) for convection at the inside face (see the description below for interior convection).

The adaptive convection algorithm implemented in EnergyPlus for the outside face is much simpler than that for the inside face. The surface classification system has a total of 4 different categories for surfaces that depend on current wind direction and heat flow directions.  However it is more complex in that the *h<sub>c</sub>* equation is split into two parts and there are separate model equation selections for forced convection, *h<sub>f</sub>*, and natural convection, *h<sub>n</sub>*. The following table summarizes the categories and the default assignments for *h<sub>c</sub>* equations. The individual *h<sub>c</sub>* equations are documented below.

Table 12. Adaptive Convection Algorithm Details

<table class="table table-striped">
<tr>
<th>#</th>
<th>Surface Classifi-cation</th>
<th>Heat Flow Direction</th>
<th>Wind Direct-ion</th>
<th>h<sub>f</sub>  Models</th>
<th>h<sub>n</sub> Models</th>
</tr>
<tr>
<td>1</td>
<td>Roof Stable</td>
<td>Down</td>
<td>Any</td>
<td>TARPWindward MoWiTTWindward DOE2Windward NusseltJurges BlockenWindward EmmelRoof ClearRoof</td>
<td>WaltonStableHorizontalOrTilt AlamdariStableHorizontal</td>
</tr>
<tr>
<td>2</td>
<td>Roof Unstable</td>
<td>Up</td>
<td>Any</td>
<td>TARPWindward MoWiTTWindward DOE2Windward NusseltJurges BlockenWindward EmmelRoof ClearRoof</td>
<td>WaltonUnstableHorizontalOrTilt AlamdariUnstableHorizontal</td>
</tr>
<tr>
<td>3</td>
<td>Vertical Wall Windward</td>
<td>Any</td>
<td>Windward</td>
<td>TARPWindward DOE2Windward MoWiTTWindward NusseltJurges McAdams Mitchell BlockenWindward EmmelVertical</td>
<td>ASHRAEVerticalWall AlamdariHammondVerticalWall FohannoPolidoriVerticalWall ISO15099Windows</td>
</tr>
<tr>
<td>4</td>
<td>Vertical Wall Leeward</td>
<td>Any</td>
<td>Leeward</td>
<td>TARPLeeward MoWiTTLeeward DOE2Leeward EmmelVertical NusseltJurges McAdams Mitchell</td>
<td>ASHRAEVerticalWall AlamdariHammondVerticalWall FohannoPolidoriVerticalWall ISO15099Windows</td>
</tr>

</table>



##### Outside Face Surface Classification

During an initial setup phase, all the heat transfer surfaces in the input file are analyzed in groups to determine appropriate values for geometry scales used in many of the convection correlations.  Eight separate groups are assembled for nominally vertical exterior surfaces for eight bins of azimuth: north, northeast, east, southeast, south, southwest, west, northwest.  Surfaces with the same range of azimuth are grouped together and analyzed for overall geometry parameters.  A ninth group is assembled for nominally horizontal exterior surfaces for a roof bin that is also analyzed for geometry.  These geometry routines find bounds and limits of all the surfaces in the group and then model geometric parameters from these limits.

##### Sparrow Windward

As discussed above for the TARP algorithm, a Sparrow et al. (1979) conducted flat plate measurements and develop the following correlation for finite-size flat plates oriented to windward.

<div>$${h_f} = 2.53{R_f}{\left( {\frac{{P{V_z}}}{A}} \right)^{{\raise0.7ex\hbox{$1$} \!\mathord{\left/ {\vphantom {1 2}}\right.}\!\lower0.7ex\hbox{$2$}}}}$$</div>

##### Sparrow Leeward

Sparrow et al. (1979) conducted flat plate measurements and develop the following correlation for finite-size flat plates oriented to leeward.

<div>$${h_f} = \frac{{2.53}}{2}{R_f}{\left( {\frac{{P{V_z}}}{A}} \right)^{{\raise0.7ex\hbox{$1$} \!\mathord{\left/ {\vphantom {1 2}}\right.}\!\lower0.7ex\hbox{$2$}}}}$$</div>

##### MoWITT Windward

As discussed above, Yazdanian and Klems (1994) used outdoor laboratory measurements to develop the following correlation for smooth surfaces oriented to windward. Booten et al. (2012) developed revised coefficients for use with local surface wind speeds.

<div>$${h_c} = \sqrt {{{\left[ {0.84{{\left| {\Delta T} \right|}^{{\raise0.7ex\hbox{$1$} \!\mathord{\left/ {\vphantom {1 3}}\right.}\!\lower0.7ex\hbox{$3$}}}}} \right]}^2} + {{\left[ {2.38{\kern 1pt} {\kern 1pt} V_z^{0.89}} \right]}^2}} $$</div>

This model equation is for the total film coefficient and includes the natural convection portion. Therefore it should not be used in conjunction with a second natural convection model equation.

##### MoWITT Leeward

Yazdanian and Klems (1994) used outdoor laboratory measurements to develop the following correlation for smooth surfaces oriented to leeward. Booten et al. (2012) developed revised coefficients for use with local surface wind speeds.

<div>$${h_c} = \sqrt {{{\left[ {0.84{{\left| {\Delta T} \right|}^{{\raise0.7ex\hbox{$1$} \!\mathord{\left/ {\vphantom {1 3}}\right.}\!\lower0.7ex\hbox{$3$}}}}} \right]}^2} + {{\left[ {2.86{\kern 1pt} {\kern 1pt} V_z^{0.617}} \right]}^2}} $$</div>

This model equation is for the total film coefficient and includes the natural convection portion. Therefore it should not be used in conjunction with a second natural convection model equation.

##### Blocken

Blocken et al. (2009) developed a set of correlations for windward facing outdoor surfaces using numerical methods (key: BlockenWindward).

<div>$$
  \begin{array}{lcl}
    h_f = 4.6V^{0.89}_{10m} & : & \theta \leq 11.25 \\
    h_f = 5.0V^{0.80}_{10m} & : & 11.25 &lt; \theta \leq 33.75 \\
    h_f = 4.6V^{0.84}_{10m} & : & 33.75 &lt; \theta \leq 56.25 \\
    h_f = 4.5V^{0.81}_{10m} & : & 56.25 &lt; \theta \leq 100.0
  \end{array}
$$</div>

Where *V<sub>10m</sub>* is the air velocity at the location of the weather station and θ is the angle of incidence between the wind and the surface in degrees.  This model is only applicable to windward surfaces and lacks a natural convection component and therefore cannot be used on its own but only within the adaptive convection algorithm for the outside face.

##### Clear

Clear et al. (2003) developed correlations from measurements for horizontal roofs on two commercial buildings. In EnergyPlus the implementation uses the model for natural convection plus turbulent forced convection (eq. 8A in the reference) and applies it to the center point of each surface section that makes up the roof.

<div>$${h_c} = \eta \frac{k}{{{L_n}}}0.15Ra_{{L_n}}^{{\raise0.7ex\hbox{$1$} \!\mathord{\left/ {\vphantom {1 3}}\right.}\!\lower0.7ex\hbox{$3$}}} + \frac{k}{x}{R_f}0.0296{\mathop{\rm Re}\nolimits}_x^{{\raise0.7ex\hbox{$4$} \!\mathord{\left/ {\vphantom {4 5}}\right.}\!\lower0.7ex\hbox{$5$}}}{\Pr ^{{\raise0.7ex\hbox{$1$} \!\mathord{\left/ {\vphantom {1 3}}\right.}\!\lower0.7ex\hbox{$3$}}}}$$</div>

Where

* *x* is the distance to the surface centroid from where the wind begins to intersect the roof. In EnergyPlus this is currently simplified to half the square root of the roof surface.

* <span>\({L_n} = \frac{{Area}}{{Perimeter}}\)</span> of overall roof

* <span>\(k\)</span> is the thermal conductivity of air

* <span>\(\eta  = \frac{ln\left(1+\frac{Gr_{L,x}}{Re_x^2}\right)} {1+ln\left(1+\frac{Gr_{L,x}}{Re_x^2}\right)} is the weighting factor for natural convection (suppressed at high forced convection rates)

* <span>\(R{a_{{L_n}}} = G{r_{{L_n}}}\Pr \)</span> is the Rayleigh number

* <span>\(G{r_{{L_n}}} = \frac{{g{\rho ^2}{L_n}^3\Delta T}}{{{T_f}{\mu ^2}}}\)</span>is the Grashof number

* <span>\({{\mathop{\rm Re}\nolimits}_x} = \frac{{{V_z}\rho x}}{\mu }\)</span>is the Reynolds number at x

* Pr is the Prandtl number

This model only claims to be applicable to