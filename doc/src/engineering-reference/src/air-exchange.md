# Air Exchange

Air exchange and interchange between zones is treated as a convective gain. Temperature-difference-controlled or constant air mixing can be specified as a one-way or cross-zone phenomenon modeled using the ZoneMixing or ZoneCrossMixing objects. Air exchange through doorways between refrigerated spaces can be modeled using the ZoneRefrigerationMixing object.

For one-way mixing (using ZoneMixing object(s)), the mixing air flow is only used for the energy and mass balance for the receiving zone. The mass referred in this section includes air, water vapor and CO~2~. The source zone energy and mass balance are not effected, although the user may choose to enter complementary pairs of one-way mixing objects. Multiple mixing flows can be defined for any receiving zone. For cross-zone mixing (using ZoneCrossMixing object(s)), the mixing air flow impacts the mass and energy balances in both the source and receiving zones. No more than one ZoneCrossMixing object can be used for any receiving zone. A separate ZoneCrossMixing object must be used for each of the two zones exchanging air if the mixing flow is bi-directional and based on a temperature difference greater than zero.

For refrigerated space air exchange (using ZoneRefrigerationDoorMixing object(s)), the mixing air flow impacts the mass and energy balances in both the source and receiving zones. A single object accounts for the two-way air flow with the energy and mass exchanges determined by  the air density difference between the two zones.

## Temperature Difference Controlled Air Exchange

The volume of air flow into the receiving zone is specified by the user with a number of control parameters and schedules listed in the Input Output Guide. The user can turn this one-way flow on or off based on the temperature difference between the source and receiving zones, or it may be independent of the temperature difference. The density and specific heat of the air flowing into the receiving zone are determined using the average temperature and average humidity ratio in the source and receiving zones. The humidity ratio of the air flowing into the receiving zone is set equal to the humidity ratio of the source zone. The mass, moisture,and energy terms are then used as described in two previous sections, Basis for the Zone and Air System Integration, and Moisture Predictor-Corrector.

![](media/image1844.png)\


where:

c~P,Avg~= Average specific heat of air within the two zones (J/kg.K)

![](media/image1845.png) = Mass of moist air flowing into the receiving zone (kg~air~/s)

Moisture~MixingFlowToReceivingZone~= Moisture mass flow rate into the receiving zone (kg~H2O~/s)

P~Barometric~= Outside barometric pressure (Pa)

ρ~Avg~= Average density of air within the two zones (kg/s)

![](media/image1846.png) = Energy added to receiving zone air by mixing mass flow (W)

T~ReceivingZone~= Temperature in the Receiving Zone (^o^C)

T~SourceZone~ = Temperature in the Source Zone (^o^C)

![](media/image1847.png) = Volume rate of air flow defined by the user (m^3^/s)

W~ReceivingZone~ = Humidity Ratio in the Receiving Zone (kg~H~~2~~O~/kg~dry air~)

W~SourceZone~= Humidity Ratio in the Source Zone (kg~H2O~/kg~dry air~)

For cross-mixing, the mass of moist air exchanged between the two zones is assumed to be equal.  Again, the density and specific heat are based on the average conditions in the two zones. Note that the temperature and humidity ratio differences ensure that when the energy and  moisture terms are used in the Moisture Predictor-Corrector, they correctly reflect a loss or gain in each zone.

![](media/image1848.png)\


where:

![](media/image1849.png) = Mass of moist air flowing into the source zone (kg~air~/s)

Moisture~MixingFlow~~ToSource~~Zone~= Moisture mass flow rate into the source zone (kg~H2O~/s)

![](media/image1850.png) = Sensible energy added to source zone air by mixing mass flow (W)

![](media/image1851.png) = Sensible energy added to receiving zone air by mixing mass flow, W

![](media/image1852.png) = Latent load added to source zone air by mixing mass flow (kg~H2O~/s)

![](media/image1853.png) = Latent load added to receiving zone air by mixing mass flow (kg~H2O~/s)

## Density Difference Controlled Air Exchange

When closed refrigerated spaces exchange air with other closed spaces, the air flow is determined by the difference in air density between the two spaces. The fundamental assumption for this case is that the mass of dry air exchanged between the two spaces is the same.(Gosney and Olama,  1975] This assumption applies to situations where the colder of the two spaces is essentially sealed to other air flows, that is, there are no open doors or exhaust air flows. Multiple refrigeration door mixing objects can be used for the zone, but if there are multiple doors open at the same time for any significant amount of time, the model will not give results appropriate for that condition.

The sensible and latent energy loads are modeled according to the guidance specified in (ASHRAE 2006d, ASHRAE 2009, and Gosney and Olama, 1975).  Equal dry air exchange is assumed, that is, the mass of dry air infiltrating into the receiving zone is assumed to equal the mass of dry air infiltrating out of the source zone.

![](media/image1854.png)\


where:

A~door~=  Area of door between Zones A and B (m^2^)

F~Flow~ =  Doorway flow factor, = 0.8 if ΔT > 11^o^C; =1.1 if ΔT <= 11^o^C

F~Protection~ = Doorway protection factor, = 0 for no protection; =  0.5 for an air curtain; and 0.9 for a strip curtain (dimensionless)

g=  Gravitational constant (m/s^2^)

h~ZoneA~=  enthalpy of the air within Zone A (J/kg)

h~ZoneB~=  enthalpy of the air within Zone B (J/kg)

H~door~=  Height of door between source and receiving zones (m)

Q~FullFlow~ = Sensible and latent refrigeration load (on Zone A) for fully established flow (W)

Q~Mixing~ = Sensible and latent mixing refrigeration load on Zone A for the time step (W)

m~DryAir~~ZoneAB~ = Mass of dry air exchanged between zones A and B (kg~air~/s)

Schedule~DoorOpen~ = Value scheduled by user, fraction of time door open during time step (dimensionless)

W~ZoneA~=  Humidity ratio of the air within Zone A (kg~H2O~/kg~air~)

W~ZoneB~ =  Humidity ratio of air within Zone B (kg~H2O~/kg~air~)

Ρ~ZoneA~=  Density of air within Zone A (kg/m^3^)

ρ~ZoneB~ =  Density of air within Zone B (kg/m^3^)

## References

ASHRAE. 2006d. *Refrigeration Handbook*, Chapter 13. Atlanta: American Society of Heating,

Refrigerating and Air-Conditioning Engineers, Inc.

ASHRAE. 2009. *Fundamentals Handbook*, Chapter 1. Atlanta: American Society of Heating, Refrigerating and Air-Conditioning Engineers, Inc.

Gosney, W.B., Olama, G.A.-L. 1975. Heat and Enthalpy Gains through Cold Room Doorways,  Proceedings of the Institute of Refrigeration, vol. 72, pp 31-41