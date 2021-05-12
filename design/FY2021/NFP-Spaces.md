# Adding "Spaces" to EnergyPlus #

**Michael J. Witte, GARD Analytics, Inc.**  
**Jason W. DeGraw, ORNL**  
**With input from many more . . .**  

 -  Original, November 22, 2019
 -  Revised, April 1, 2020
    * Keep the current definition of Zone
    * Add new objects for Space, SpaceType, CompoundSpaceType, and Enclosure
 -  Revised, November 25, 2020
    * Condensed and focused on final proposal somewhat agreed to back in April 2020
    * Revised to reflect simplified Construction:AirBoundary object (radiant and solar enclosures are now the same)
 -  Revised, December 11, 2020
    * Simplify transition rules by allowing Zone and Space to share names
    * Per comments, decide that load assignments like internal gains, infiltration and ventilation, should be at the Space level
    * Add language to clarify concerns raised in comments
    * Add daylighting and thermal comfort considerations

## Justification for New Feature ##

Thermal zones (HVAC zones) are often composed of a variety of spaces grouped together to be served by
a single thermostat. For example, an office building core zone may include private offices, conference rooms, restrooms
and corridors. Each of these spaces (rooms) has different internal gains and may or may not be fully enclosed by solid surfaces.
Currently, the smallest building element is a zone. This requires the user (or interface) to blend these spaces together for surfaces,
internal gains, and other specifications. The option to specify each space explicitly would greatly simplify input in data managemenent,
especially for space-based interfaces (such as OpenStudio) and for codes and standards modeling.

Other advantages include better surface groupings for radiant and solar exchange. For example, a private office which is part of a large 
HVAC zone could be modeled as a space so that it does not pass solar or radiant to other surfaces in the larger zone. 
There are also potential computing performance advantages to dividing zones into smaller enclosures for radiant exchange and solar
distribution, especially for large HVAC zones with many surfaces.

## Overview ##
As proposed, Zones remain the same entity as before. Space is added as a new layer for assembling surfaces and gains into a Zone. 
HVAC connections and controls remain at the Zone level. 

### Current EnergyPlus model ###
EnergyPlus input currently has the following (very flat) heierarchy:

    Zone
        Surfaces (references a zone name)
        People, Lights, etc. (references a zone name or zone list name)
        Thermostat (one per zone)
        HVAC equipment (attaches to a zone)

Zones represent an air mass (air node) that exchanges heat with surfaces, internal loads, HVAC equipment, etc. 
A Zone is essentially the "atomic unit" of the building. By default a zone has a uniform air temperature, but there are room air models 
that allow modeling of stratification and other non-uniform temperature effects. Thermostats and HVAC equipment are assigned per Zone.

The internal data model adds another layer:

    Enclosure
        Zone
            Surfaces
            People, Lights, etc.
            Thermostat
            HVAC equipment
        Zone
            Surfaces
            People, Lights, etc.
            Thermostat
            HVAC equipment

Enclosures are used for radiant and solar/daylighting exchange. Enclosures are primarily concerned with Surfaces, but they
are also related to internal gains (which cast radiant and visible energy into the enclosure) and thermal comfort (MRT). 
By default there is one Enclosure for each Zone.  By using Construction:AirBoundary, multiple zones may be grouped into a single larger Enclosure. 
Enclosures are assigned automatically based on the zones connected by an air boundary surface. 


## New Space Object

### Relationship assumptions ##

If we keep the current definition of "Zone" and add the concept of
"Space" (<= Zone), then this would add a new layer to the data model:

    Zone
        Thermostat
        HVAC equipment
        Space 2
            Surfaces
            Internal Gains (People, Lights, etc.)
        Space 3
            Surfaces
            Internal Gains (People, Lights, etc.)

    Enclosure
        Space 1
            Surfaces
            Internal Gains (People, Lights, etc.)
        Space 2
            Surfaces
            Internal Gains (People, Lights, etc.)
    

### Definitions

 * Surface - A geometric plane which is attached to a Space. 
   * A Surface can be opaque, transparent, or an air boundary.
   * Each Surface belongs to one Space.
   * Inter-Space Surfaces (adjacent to another Space) are modeled the same as current inter-Zone surfaces (two linked surfaces).
   * Inter-Space surfaces connecting spaces that are part of the same Zone will see the same air temperature but may be in 
     different enclosures. If they connect spaces that are in different Zones, then they will see different air temperatures as well.
   * Air boundary surfaces will combine one or more spaces into an enclosure.

 * Space - A collection of one or more Surfaces and internal gains.
   * Each Space belongs to one Zone (explicitly user-assigned).
   * The Spaces in a Zone are lumped together for the Zone heat balance.
   * There is no heat balance at the Space level.
   * Each Space belongs to one Enclosure (implicitly assigned).

 * Zone - An air mass connecting Surfaces, internal gains, and HVAC equipment for heat balance and HVAC control.
   * Each Zone is comprised of one or more Spaces.
   * All Surfaces and internal gains associated with the Spaces are included in the Zone.
   * The Zone heat balance does not change: it has an air node (or a Room Air Model) and includes all Surfaces and 
   internal gains from its Spaces plus any HVAC which is attached to the Zone.

 * Enclosure - A continuous volume connecting Surfaces for radiant, solar, and daylighting exchange.
   * Each Enclosure is comprised of one or more Spaces.
   * There is one Enclosure for each Space unless there are air boundary surfaces 
   which group multiple Spaces into a single Enclosure.
   * All Surfaces and internal radiant gains associated with the Spaces are included in the Enclosure.
   * Enclosures only distribute radiant (thermal), solar, and visible energy to and from the Surfaces.
   * There is no full heat balance at the Enclosure level. Each Enclosure only balances the radiant/solar flux on each 
   Surface. These fluxes then become part of the Surface inside heat balance.

## Proposed Input Approach ##
Very minimal changes to current inputs.

 * Old Zone object becomes Space object plus some optional tags.
 * New Zone object has a name and a list of Spaces (allow Zone and Space names to be the same, if desired, to
   simplify backwards compatibility).
 * Surfaces will reference a Space instead of a Zone.
 * ZoneHVAC and Thermostats remain as-is and continue to reference a Zone or ZoneList.
 * New SpaceList object (equivalent to ZoneList, but for Spaces).
 * Internal gains reference Space or Spacelist (instead of Zone or ZoneList).
 * ZoneInfiltration:* and ZoneVentilation:* are changed to Infiltration:* and Ventilation:* . 
   These will reference Space or SpaceList (instead of Zone or ZoneList).
 * Daylighting controls will reference a Space. (Daylighting reference points see all windows in an enclosure).

### Proposed objects:

    Space,  !- Same fields as old Zone object plus new optional tags at the end
      Name,
      Origin,
      Multiplier,
      Ceiling Height,
      Volume,
      Floor Area,
      Convection algorithms,
      Part of Total Floor Area,
      User-defined Tags for Space Types (for reporting purposes only at this point)
      
    Zone, !- Just a list of Spaces
      Name,
      Space Name 1,
      Space Name 2,
      ...
      
    Lighting/People/Equipment
      Name,
      Space or SpaceList Name, !- Replace current Zone or ZoneList Name
      Schedule Name,
      ...
    
    Infiltration:* and Ventilation:*
      Name,
      Space or SpaceList Name, !- Replace current Zone or ZoneList Name
      ...
    
    Surface,
      Name,
      ...
      Space Name,  !- Replace current Zone Name

## Data Structure Considerations ##

 * Add fields to `Surface` to track the Space name and index along with the existing Zone and Enclosure fields.
 
 * Proposed Surface ordering (same as current): 
   - All shading surfaces are first
   - All air boundary surfaces are next (once PR8370 merges).
   - Group by Zone
   - Group by surface type within each Zone

e.g., Zone1 contains Space1 and Space3, Zone2 contains Space2.

      Shading Surfaces
      Air Boundary Surfaces
      Space1-OpaqueSurface1 (begin surfaces in Zone1)
      Space1-OpaqueSurface2
      Space3-OpaqueSurface1
      Space3-OpaqueSurface2
      Space1-Window1
      Space1-Window2
      Space2-OpaqueSurface1 (begin surfaces in Zone2)
      Space2-OpaqueSurface2
      Space2-Window1

 * Alternate Surface ordering: 
   - All shading surfaces are first
   - All air boundary surfaces are next
   - Group by Enclosure (because enclosure radiant exchange is more computationally intensive than zone heat balance?)
   - Group by Space within each Enclosure.
   - Group by surface type within each Space
   - Modify heat balance Zone loops to be Space loops(?)

e.g., Enclosure1 contains Space1 and Space3, Enclosure2 contains Space2.

      Shading Surfaces
      Air Boundary Surfaces
      Space1-OpaqueSurface1 (begin surfaces in Enclosure1)
      Space1-OpaqueSurface2
      Space1-Window1
      Space1-Window2
      Space3-OpaqueSurface1
      Space3-OpaqueSurface2
      Space2-OpaqueSurface1 (begin surfaces in Enclosure2)
      Space2-OpaqueSurface2
      Space2-Window1

## Options/Questions for Discussion
 * Keep ZoneVentilation and ZoneInfiltration at the Zone level or move to the Space level (and rename)?

   *Move to Space level and rename to Ventilation:* and Infiltration:*.
   
 * Allow Zone Names to be the same as Space Names or force all names to be unique across Spaces and Zones?
 
   *Allowing the same names would simplify backward compatibility.*
 * Zone-based or Enclosure-based surface grouping?
 
   *This is a design question, somewhat premature at this point.*

## Testing/Validation/Data Sources ##

There should be no substantive diffs (possibly some small diffs due to change in computational order). 
For the regression tests which are one-to-one Space-To-Zone, all numeric results
should stay exactly the same, but output variable names and table headings may change.

## Input Output Reference Documentation ##

The I/O Reference section for Zone becomes Space with some explanatory text at the top.
The new Zone becomes a list of Spaces.

## Outputs Description ##

Output variables which are zone-based will remain the same.
Some space-level output variables may be added.
Table reports summarizing inputs at the space level will be added.
Table reports allocating energy at the space level *may* be added.

## Engineering Reference ##

Calulations won't change, so doc changes will be minimal to clarify when Space, Zone, and Enclosure.

## Example File and Transition Changes ##

* Convert Zone objects to Space objects (one-to-one, just change the object class name).
* Insert new Zone objects (one for each original Zone, no name change).
