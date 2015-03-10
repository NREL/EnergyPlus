# Coupling EnergyPlus with the Building Controls Virtual Test Bed

## Algorithm for data exchange

The process in which at least two simulators solve initial-value differential equations that are coupled to each other is called co-simulation. Various algorithms are possible for the data exchange. In the BCVTB, data are exchanged between its client programs, including EnergyPlus, using a fixed synchronization time step. There is no iteration between the clients. In the co-simulation literature, this coupling scheme is referred to as *quasi-dynamic coupling*, *loose coupling* or *ping-pong coupling* (Hensen 1999, Zhai and Chen 2005).

The algorithm for exchanging data is as follows: Suppose we have a system with two clients, with client 1 being EnergyPlus and client 2 being, for example, the Simulink program from Mathworks. Suppose each client solves an initial-value ordinary differential equation that is coupled to the differential equation of the other client. Let ![](media/image1.png)  denote the number of time steps and let ![](media/image2.png)  denote the time steps. We will use the subscripts *1* and *2* to denote the state variable and the function that computes the next state variable of the simulator *1* and *2*, respectively.

The simulator *1* computes, for ![](media/image3.png) the sequence

*x~1~(k+1) = f~1~(x~1~(k), x~2~(k))*

and, similarly, the simulator *2* computes the sequence

*x~2~(k+1) = f~2~(x~2~(k), x~1~(k))*

with initial conditions *x~1~(0) = x~1,0~* and *x~2~(0) = x~2,0~*.

To advance from time *k* to *k+1*, each simulator uses its own time integration algorithm. At the end of the time step, the simulator *1* sends the new state *x~1~(k+1)* to the BCVTB and it receives the state *x~2~(k+1)* from the BCVTB. The same procedure is done with the simulator *2*. The BCVTB synchronizes the data in such a way that it does not matter which of the two simulators is called first.

In comparison to numerical methods of differential equations, this scheme is identical to an explicit Euler integration, which is an integration algorithm that computes for an ordinary differential equation with specified initial values,

*dx/dt = h(x),*

*x(0)  = x~0~,*

on the time interval *t  [0, 1]*, the following sequence:

**Step 0:**|Initialize counter *k=0* and number of steps ![](media/image4.png) .
------------------------|---------------------------------------------------------------------------
|Set initial state *x(k) = x~0~* and set time step *t = 1/N*.
**Step 1:**|Compute new state *x(k+1) = x(k) + h(x(k)) t*.
|*Replace k* by *k+1.*
**Step 2:**|If *k=N* stop, else go to Step 1.

In the situation where the differential equation is solved using co-simulation, the above algorithm becomes

**Step 0:**|Initialize counter *k=0* and number of steps ![](media/image5.png) .
------------------------|---------------------------------------------------------------------------
|Set initial state *x~1~(k) = x~1,0~* and *x~2~(k) = x~2,0~*. Set the time step *t = 1/N*.
**Step 1:**|Compute new states|  *x~1~(k+1) = x~1~(k) + f~1~(x~1~(k), x~2~(k)) t*, and|  *x~2~(k+1) = x~2~(k) + f~2~(x~2~(k), x~1~(k)) t*.
|Replace *k* by *k+1*.
**Step 2:**|If *k=N* stop, else go to Step 1.

This algorithm is implemented in the BCVTB. Note that there is no iteration between the two simulators.