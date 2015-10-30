Conduction Finite Difference Node Heat Flux Reporting
================

**M.J. Witte, GARD Analytics, Inc.**

 - October 30, 2015
 - *Revision Date*
 

## Justification for New Feature ##

The primary motivation for this work is to support experimental testing where heat flux meters are embedded within a wall. It is also useful for other work to show the flow of heat within material layers, especially for phase change material (PCM) applications.

## E-mail and  Conference Call Conclusions ##

*insert text*

## Overview ##

Currently, the conduction finite difference algorithm (HeatBalanceAlgorithm,ConductionFiniteDifference) reports the temperature at each node within a surface.  This will add reporting of the heat flux between nodes.

## Approach ##

At the end of the conduction finite difference (CondFD) solution for each timestep, the surface inside face conduction heat flux and the node temperatures will be used to calculate the heat flux between nodes.  The figure below illustrates the conduction finite difference node layout (from the Engineering Reference).


![CondFD Nodes adapted from Engineering Reference](CondFDNodeFluxOutput-CondFDNodes-image176.png)

Figure 1. Node depiction for Conduction Finite Difference Model (Nodes are numbered from outside to outside face to inside face.  External surface node = 1, Interior surface node = 8)

For every CondFD surface, the Surface Inside Face Conduction Heat Transfer Rate per Area is available as an output. Starting with this flux, the heat flux from the Interior surface node 8 to the next interior node 7 is the Surface Inside Face Conduction Heat Transfer Rate per Area plus any change in heat storage in the material thickness associated with node 8.  Once the flux from node 8 to node 7 is determined, then the next flux can be calculated, and so on.

## Design ##

### Relevant variables ###

*Existing*
HeatBalFiniteDiffManager::SurfaceFD(surf).TDT(node) = new node temperature

HeatBalFiniteDiffManager::SurfaceFD(surf).TDReport(node) = old node temperature from previous timestep (at the time it's used here, then it gets updated)

DataHeatBalSurface::OpaqSurfInsFaceConductionFlux

*New*
HeatBalFiniteDiffManager::SurfaceFD(surf).CpDelXRhoS(n) = Node heat capacitance of Node n, Cp \* Rho \* delX (where delX is the thickness of the node)

HeatBalFiniteDiffManager::SurfaceFD(surf).QDreport(n) = Heat flux from Node(n) to Node(n+1)

### Relevant Functions ###
HeatBalFiniteDiffManager::
*Existing*
ExteriorBCEqns - Calculate outside surface face node
InteriorNodeEqns - Calculate inner nodes which are not at the interface between two materials
IntInterfaeNodeEqns - Calculate material interface nodes
InteriorBCEqns - Caclculate inside surface face node

*New*
CalcInterNodeHeatFlux - Calculate the node-to-node heat fluxes

### Method ###
1. There are multiple functions (listed above) which solve for the temperatures of the nodes.  These are within a time loop that subdivides the zone time step. Each one determines the properties for the materials in the various nodes, which could be just regular material, r-layer, phase change material, or a combination of two materials.
2. Within the various calc routines, store the applicable values for SurfaceFD(surf).CpDelXRhoS(n).
3. After the new temperatures have been solved, call CalcInterNodeHeatFlux to compute the heat fluxes for reporting.

```
		// surfaceFD.QDreport( 1 ) is the flux from node 1 to node 2
        // surfaceFD.QDreport( 2 ) is the flux from node 2 to node 3; etc.
		// when this is called TDT( NodeNum ) is the new temp and TDreport( NodeNum ) is still the previous temp
		// For the TDT and TDReport arrays, Node 1 is the outside face, and Node TotNodes+1 is the inside face
		// For the QDreport array, QDreport(1) is from Node 1 to Node 2, etc.

		// Last node is always the surface inside face (use this because outside face is not defined for all surfaces)
		// Note that TotNodes is the number of nodes in the surface including the outside face node,
        // but not the inside face node so the arrays are all allocated to Totodes+1

		// Heat flux from the last interior node(TotNodes) to the inside face node (TotNodes+1)
		surfaceFD.QDreport( TotNodes ) = OpaqSurfInsFaceConductionFlux( Surf ) + surfaceFD.CpDelXRhoS( TotNodes + 1) * ( surfaceFD.TDT( TotNodes + 1) - surfaceFD.TDreport( TotNodes + 1 )) / TimeStepZoneSec;

		// Heat flux for remaining nodes.
		for ( node = TotNodes - 1; node >= 1; --node ) {
				// Start with inside face and work outward, positive value is flowing towards the inside face
			surfaceFD.QDreport( node ) = surfaceFD.QDreport( node + 1 ) + surfaceFD.CpDelXRhoS( node + 1 )  * ( surfaceFD.TDT( node + 1 ) - surfaceFD.TDreport( node + 1 )) / TimeStepZoneSec;
		}
```

## Testing/Validation/Data Sources ##

Steady-state fluxes should all be equal.

Other fluxes should balance over time.

## Input Output Reference Documentation ##



## Input Description ##

*No changes to input.*

### Conduction Finite Difference (CondFD) Outputs

#### CondFD Surface Heat Flux From Node &lt;X&gt; To Node &lt;X+1&gt; [W/m2]

This will output heat flux from one node to the next in surfaces being simulated with ConductionFiniteDifference. The key values for this output variable are the surface name. The nodes are numbered from outside to inside of the surface. The full listing will appear in the RDD file.

#### CondFD Surface Heat Capacitance Node &lt;X&gt; [W/m2-K]

This will output the node heat capacitance in surfaces being simulated with ConductionFiniteDifference. The key values for this output variable are the surface name. The nodes are numbered from outside to inside of the surface. The full listing will appear in the RDD file. For this output, the heat capacitance is defined as the product of specific heat, density, and node thickness.

## Engineering Reference ##

*See actual engineering ref doc changes.*

## Example File and Transition Changes ##

No transition required.  New outputs will be added to example file CondFD1ZonePurchAirAutoSizeWithPCM.

## References ##

*None.*
