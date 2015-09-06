# Spatial Data Structures in EnergyPlus

## Acknowledgments

Simon Vidanovic (@vidanovic) provided valuable help on daylighting, surfaces, and how best to do the octree integration.

## Introduction

EnergyPlus performs a number of performance-critical computations using *O*(*N*<sup>2</sup>) or higher computational complexity algorithms and data structures. These create significant barriers to the use of EnergyPlus for large buildings and fine-grained models.

Spatial data structures, such as kd-trees and octrees, are a widely used tool for reducing the complexity of queries about objects in 2D or 3D space. They work by binning objects by their position so that a subset of candidate objects can be checked against the query rather than all objects. Such queries might be asking for all the objects within a given distance of some point, asking for the objects that a given ray can hit, or finding object collisions. Modern 3D application performance is heavily dependent on spatial data structures.

EnergyPlus does these types of spatial queries in a number of subsystems including daylighting, solar shading, and solar reflection. Based on the potential for large performance and scalability gains NREL contracted with Objexx to develop a prototype spatial sorting system and evaluate its potential for use in EnergyPlus, starting in this phase with daylighting, where the performance is a known problem and the integration seemed straightforward.

When a spatial data structure is applicable there is no question that it will become dramatically faster due to its *O*(*N* log *N*) complexity when the number of objects reaches some level compared to the *O*(*N*<sup>2</sup>) of a brute force implementation. The question for EnergyPlus and any application is whether the spatial sort performance can be fast enough that size level where it begins to dominate is small enough to be of interest.

The results for EnergyPlus make it clear that spatial data structures are significantly faster at real-world model sizes, even with the non-optimized prototype implementation.

## Daylighting

Daylighting computations are slow for even moderate-sized buildings and heavily performed-limited by a large number of PierceSurface calls. For each illumination reference point these algorithms loop over all surfaces looking for surfaces that a line segment or ray can intersect. With surfaces in a spatial data structure a much smaller number of candidate surfaces (those in the sub-regions that the segment/ray hits) can be evaluated. This makes daylighting a good candidate for initial spatial data structure experiments.

## Design

An octree cube design was selected for the prototype development for the efficiency of queries that cubes provide. An octree cube subdivides a cube (starting from a cube enclosing the bounding box around all the objects) into 8 sub-cubes, each of exactly half the side length of the parent cube. Only sub-cubes containing objects are populated so it is more of a sparse tree-like structure than a uniform 3D array grid.

There are octree parameters that can be tuned for a specific problem domain, such as rules for when a cube is populated enough to warrant subdivision and maximum tree depth. There are also octree variants, such as "loose" octrees, that can be worth evaluating with non-point objects. These can improve the performance by a factor but don't alter the fundamental complexity of the octree.

The SurfaceOctree.hh/.cc files provide the prototype surface octree system for EnergyPlus. The `SurfaceOctreeCube` class is the fundamental component of the octree, serving both as the type of the outer octree cube and its sub-cubes.

For daylighting use the prototype octree needed to handle queries to find all (populated) cubes that a line segment or ray can intersect. Two variants were developed with different specificity--performance tradeoffs:
* Fast lookups using cube enclosing spheres so they include some cubes (and thus surfaces) that are not actually hit. For our purposes, providing some extra candidates to check can affect performance but is not a correctness issue.
* Slower but precise lookups that find the exact cubes intersected.
It was anticipated that the exact lookups would be substantially slower such that the enclosing sphere methods could give a faster solution but they were actually only moderately slower (although this varied quite a bit across compilers) so their greater specificity appears to make them a better choice in testing so far.

## Integration

The prototype surface octree was set up as a global object and gets created right after the surfaces are configured by the call to `GetHeatBalanceInput` in `HeatBalanceManager::ManageHeatBalance`.

Based on profiling of a Hotel daylighting model provided by Simon Vidanovic (@vidanovic) the octree was integrated into queries in three of the `Hit` functions, two of which were responsible for the bulk of the `PierceSurface` calls that dominated the profile. Other places in `DaylightingManager` call `PierceSurface` and these should be evaluated to see if they would benefit from octree queries.

## Results

The results exceeded expectations and strongly show that spatial data structures can be a big win for EnergyPlus performance and scalability.

For the Hotel test case used in testing an overall performance improvement of 48% was obtained with the prototype octree system. The number of `PierceSurface` calls was reduced from 138 billion to 50 billion. Once the switch to the faster query call system is implemented the results for this case should be better than a 2X speedup.

Due to the computational complexity difference the performance gain grows dramatically with building size (more specifically in this instance the number of surfaces).

Larger buildings with daylighting-dominated performance were not found during this phase but a method for easily generating buildings of various sizes with OpenStudio Ruby scripts was provided by Daniel Macumber () and, once embellished to directly generate runnable models with the needed attributes, are expected to be part of future spatial sorting development work.

Profiling showed that construction of octrees is fast enough to be insignificant in real-world cases.

## Future

Bringing the octree system to a production-ready and optimized state and extending its integration to other parts of EnergyPlus are recommended follow-on activities for FY16. Some specific activities toward these ends are detailed below.

* A series of increasing size building models should be generated (using OpenStudio scripting) to get a full performance profile of the octree and tune the linear--octree cutoff size (or determine that the octree is fast enough for small models that such a crossover isn't necessary).

* The prototype octree has the queries collect and return a `std::vector` collection of the candidate cubes' surfaces. For production use a faster approach should be developed that passes the function (wrapping the PierceSurface call and following operations) to avoid the heap allocation and to allow early short-circuiting of the queries.

* Octree parameters should be tuned based on a series of relevant cases to further improve performance.

* PierceSurface can be further refined for greater speed.

* Octree creation should be enabled only for models that use it. There may be multiple octrees for different purposes as this work progresses. The one-time octree creation time and memory cost is very modest.

## Conclusions

While more trials are needed to get the full picture, spatial sorting clearly has a place in moving EnergyPlus to be more scalable. A well-optimized octree system should be able to provide dramatic speedups for large buildings in models that use some of the currently expensive computations including daylighting and solar effects. The use of an extended "spatial" data structure for lowering the complexity of radiant heat transfer computations, while farther "out there" is also worth exploring.

Objexx looks forward to bringing the octree system to production readiness for upcoming FY16 EnergyPlus releases
