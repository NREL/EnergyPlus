# -*- coding: utf-8 -*-
#
# Configuration file for the Sphinx documentation builder.
#
# This file does only contain a selection of the most common options. For a
# full list see the documentation:
# http://www.sphinx-doc.org/en/master/config

from json import load
import os
from shutil import copytree, rmtree
from subprocess import check_call, CalledProcessError, DEVNULL

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
from pathlib import Path
import sys
this_file_path = Path(__file__)
api_source_dir = this_file_path.parent.parent.parent.parent / 'src' / 'EnergyPlus' / 'api'
print(f"**Adding api_source_dir: {api_source_dir}")
sys.path.insert(0, str(api_source_dir))

# add a mock version of pyenergyplus
autodoc_mock_imports = ["pyenergyplus"]

# # build the C docs # #

# assuming doxygen is on PATH, but alter this locally if you need to point to a specific binary
DOXYGEN_BINARY = 'doxygen'

# set up some file paths for convenience
this_file_path = Path(__file__)
repo_root = this_file_path.parent.parent.parent.parent
rtd_dir = this_file_path.parent.parent
doxygen_dir = rtd_dir / 'doxygen'
doxygen_html_output_dir = doxygen_dir / '_build' / 'html'
sphinx_dir = rtd_dir / 'sphinx'
target_c_prebuilt_dir = sphinx_dir / '_build_c'

# test a file path to make sure we are in the right spot before trying to run
if doxygen_dir.exists():
    print("* Directory validation completed successfully")
else:
    raise Exception(f"Expected doxygen config dir to exist at \"{doxygen_dir}\" but it does not; aborting!")

# now try to run doxygen:
try:
    check_call([DOXYGEN_BINARY], cwd=doxygen_dir, stdout=DEVNULL, stderr=DEVNULL)
    print("* Doxygen completed successfully")
except CalledProcessError as e:
    raise Exception(f"Doxygen failed! Exception string: {str(e)}") from None
except FileNotFoundError as e:
    raise Exception(
        f"Doxygen binary not found, was it on path?  Looked for it at: {DOXYGEN_BINARY}; error = {str(e)}"
    ) from None

# ok, so doxygen should've run, validate the output directory exists
if doxygen_html_output_dir.exists():
    print("* Doxygen html output directory existence verified")
else:
    raise Exception(
        f"Although Doxygen appeared to run, the output directory is missing at {doxygen_html_output_dir}"
    ) from None

# alright, it exists, time to clean up the previous version, if it exists
if target_c_prebuilt_dir.exists():
    try:
        rmtree(target_c_prebuilt_dir)
        print("* Successfully deleted previous _build_c html directory")
    except Exception as e:
        raise Exception(f"Could not delete existing _build_c html directory") from None
else:
    print("* No _build_c directory to remove, skipping this step")

# ok, now just copy it
try:
    copytree(doxygen_html_output_dir, target_c_prebuilt_dir)
    print("* Successfully copied doxygen output directory to c_prebuilt directory")
except Exception as e:
    raise Exception(
        f"Could not copy doxygen output from '{doxygen_html_output_dir}' to '{target_c_prebuilt_dir}'"
    ) from None

# rename the root c file to index_c.html so it doesn't override the sphinx index.html file
os.rename(target_c_prebuilt_dir / 'index.html', target_c_prebuilt_dir / 'index_c.html')

print("* C Docs Complete!")

print("* Generating epJSON schema")
# # OK, now we need to make sure the epJSON schema is generated so we can process it
# Since this will primarily just be run by readthedocs, I'm just going to re-run the schema generator
try:
    check_call(['python3', 'scripts/dev/generate_epJSON_schema/generate_epJSON_schema.py', '.'], cwd=repo_root)
except CalledProcessError as e:
    raise Exception(f"Schema Generation failed! Exception string: {str(e)}") from None
except FileNotFoundError as e:
    raise Exception(
        f"python3 binary not found, what?  Looked for it at: `python3'; error = {str(e)}"
    ) from None

generated_schema_file = repo_root / 'idd' / 'Energy+.schema.epJSON.in'  # I know this will have CMake placeholders
if not generated_schema_file.exists():
    raise Exception("Generated schema file did not exist, aborting.")
print("* Generated schema existence confirmed")
with generated_schema_file.open() as f:
    o = load(f)


class Group:
    SimulationParameters = 'Simulation Parameters'
    ComplianceObjects = 'Compliance Objects'
    LocationAndClimate = 'Location and Climate'
    Schedules = 'Schedules'
    SurfaceConstructionElements = 'Surface Construction Elements'
    ThermalZonesAndSurfaces = 'Thermal Zones and Surfaces'
    AdvancedConstructionSurfaceZoneConcepts = 'Advanced Construction, Surface, Zone Concepts'
    DetailedGroundHeatTransfer = 'Detailed Ground Heat Transfer'
    RoomAirModels = 'Room Air Models'
    InternalGains = 'Internal Gains'
    Daylighting = 'Daylighting'
    ZoneAirflow = 'Zone Airflow'
    NaturalVentilationAndDuctLeakage = 'Natural Ventilation and Duct Leakage'
    ExteriorEquipment = 'Exterior Equipment'
    HVACTemplates = 'HVAC Templates'
    HVACDesignObjects = 'HVAC Design Objects'
    ZoneHVACControlsAndThermostats = 'Zone HVAC Controls and Thermostats'
    ZoneHVACForcedAirUnits = 'Zone HVAC Forced Air Units'
    ZoneHVACRadiativeConvectiveUnits = 'Zone HVAC Radiative/Convective Units'
    ZoneHVACAirLoopTerminalUnits = 'Zone HVAC Air Loop Terminal Units'
    ZoneHVACEquipmentConnections = 'Zone HVAC Equipment Connections'
    Fans = 'Fans'
    Coils = 'Coils'
    EvaporativeCoolers = 'Evaporative Coolers'
    HumidifiersAndDehumidifiers = 'Humidifiers and Dehumidifiers'
    HeatRecovery = 'Heat Recovery'
    UnitaryEquipment = 'Unitary Equipment'
    VariableRefrigerantFlowEquipment = 'Variable Refrigerant Flow Equipment'
    Controllers = 'Controllers'
    AirDistribution = 'Air Distribution'
    NodeBranchManagement = 'Node-Branch Management'
    Pumps = 'Pumps'
    PlantCondenserFlowControl = 'Plant-Condenser Flow Control'
    NonZoneEquipment = 'Non-Zone Equipment'
    SolarCollectors = 'Solar Collectors'
    PlantHeatingAndCoolingEquipment = 'Plant Heating and Cooling Equipment'
    CondenserEquipmentAndHeatExchangers = 'Condenser Equipment and Heat Exchangers'
    WaterHeatersAndThermalStorage = 'Water Heaters and Thermal Storage'
    PlantCondenserLoops = 'Plant-Condenser Loops'
    PlantCondenserControl = 'Plant-Condenser Control'
    EnergyManagementSystem = 'Energy Management System (EMS)'
    ExternalInterface = 'External Interface'
    UserDefinedHVACAndPlantComponentModels = 'User Defined HVAC and Plant Component Models'
    SystemAvailabilityManagers = 'System Availability Managers'
    SetpointManagers = 'Setpoint Managers'
    Refrigeration = 'Refrigeration'
    DemandLimitingControls = 'Demand Limiting Controls'
    ElectricLoadCenterGeneratorSpecifications = 'Electric Load Center-Generator Specifications'
    WaterSystems = 'Water Systems'
    OperationalFaults = 'Operational Faults'
    GeneralDataEntry = 'General Data Entry'
    HybridModel = 'Hybrid Model'
    PerformanceCurves = 'Performance Curves'
    PerformanceTables = 'Performance Tables'
    FluidProperties = 'Fluid Properties'
    Economics = 'Economics'
    Parametrics = 'Parametrics'
    OutputReporting = 'Output Reporting'
    PythonPluginSystem = 'Python Plugin System'
    Unknown = '*UnknownGroup*'  # indicates something went wrong here

    @staticmethod
    def all_group_names() -> list:
        return [
            Group.Unknown,
            Group.SimulationParameters,
            Group.ComplianceObjects,
            Group.LocationAndClimate,
            Group.Schedules,
            Group.SurfaceConstructionElements,
            Group.ThermalZonesAndSurfaces,
            Group.AdvancedConstructionSurfaceZoneConcepts,
            Group.DetailedGroundHeatTransfer,
            Group.RoomAirModels,
            Group.InternalGains,
            Group.Daylighting,
            Group.ZoneAirflow,
            Group.NaturalVentilationAndDuctLeakage,
            Group.ExteriorEquipment,
            Group.HVACTemplates,
            Group.HVACDesignObjects,
            Group.ZoneHVACControlsAndThermostats,
            Group.ZoneHVACForcedAirUnits,
            Group.ZoneHVACRadiativeConvectiveUnits,
            Group.ZoneHVACAirLoopTerminalUnits,
            Group.ZoneHVACEquipmentConnections,
            Group.Fans,
            Group.Coils,
            Group.EvaporativeCoolers,
            Group.HumidifiersAndDehumidifiers,
            Group.HeatRecovery,
            Group.UnitaryEquipment,
            Group.VariableRefrigerantFlowEquipment,
            Group.Controllers,
            Group.AirDistribution,
            Group.NodeBranchManagement,
            Group.Pumps,
            Group.PlantCondenserFlowControl,
            Group.NonZoneEquipment,
            Group.SolarCollectors,
            Group.PlantHeatingAndCoolingEquipment,
            Group.CondenserEquipmentAndHeatExchangers,
            Group.WaterHeatersAndThermalStorage,
            Group.PlantCondenserLoops,
            Group.PlantCondenserControl,
            Group.EnergyManagementSystem,
            Group.ExternalInterface,
            Group.UserDefinedHVACAndPlantComponentModels,
            Group.SystemAvailabilityManagers,
            Group.SetpointManagers,
            Group.Refrigeration,
            Group.DemandLimitingControls,
            Group.ElectricLoadCenterGeneratorSpecifications,
            Group.WaterSystems,
            Group.OperationalFaults,
            Group.GeneralDataEntry,
            Group.HybridModel,
            Group.PerformanceCurves,
            Group.PerformanceTables,
            Group.FluidProperties,
            Group.Economics,
            Group.Parametrics,
            Group.OutputReporting,
            Group.PythonPluginSystem,
        ]


idd_object_map = {
    Group.SimulationParameters: [
        "Version",
        "SimulationControl",
        "PerformancePrecisionTradeoffs",
        "Building",
        "ShadowCalculation",
        "SurfaceConvectionAlgorithm:Inside",
        "SurfaceConvectionAlgorithm:Outside",
        "HeatBalanceAlgorithm",
        "HeatBalanceSettings:ConductionFiniteDifference",
        "ZoneAirHeatBalanceAlgorithm",
        "ZoneAirContaminantBalance",
        "ZoneAirMassFlowConservation",
        "ZoneCapacitanceMultiplier:ResearchSpecial",
        "Timestep",
        "ConvergenceLimits",
        "HVACSystemRootFindingAlgorithm",
    ],
    Group.ComplianceObjects: [
        "Compliance:Building",
    ],
    Group.LocationAndClimate: [
        "Site:Location",
        "Site:VariableLocation",
        "SizingPeriod:DesignDay",
        "SizingPeriod:WeatherFileDays",
        "SizingPeriod:WeatherFileConditionType",
        "RunPeriod",
        "RunPeriodControl:SpecialDays",
        "RunPeriodControl:DaylightSavingTime",
        "WeatherProperty:SkyTemperature",
        "Site:WeatherStation",
        "Site:HeightVariation",
        "Site:GroundTemperature:BuildingSurface",
        "Site:GroundTemperature:FCfactorMethod",
        "Site:GroundTemperature:Shallow",
        "Site:GroundTemperature:Deep",
        "Site:GroundTemperature:Undisturbed:FiniteDifference",
        "Site:GroundTemperature:Undisturbed:KusudaAchenbach",
        "Site:GroundTemperature:Undisturbed:Xing",
        "Site:GroundDomain:Slab",
        "Site:GroundDomain:Basement",
        "Site:GroundReflectance",
        "Site:GroundReflectance:SnowModifier",
        "Site:WaterMainsTemperature",
        "Site:Precipitation",
        "RoofIrrigation",
        "Site:SolarAndVisibleSpectrum",
        "Site:SpectrumData",
    ],
    Group.Schedules: [
        "ScheduleTypeLimits",
        "Schedule:Day:Hourly",
        "Schedule:Day:Interval",
        "Schedule:Day:List",
        "Schedule:Week:Daily",
        "Schedule:Week:Compact",
        "Schedule:Year",
        "Schedule:Compact",
        "Schedule:Constant",
        "Schedule:File:Shading",
        "Schedule:File",
    ],
    Group.SurfaceConstructionElements: [
        "Material",
        "Material:NoMass",
        "Material:InfraredTransparent",
        "Material:AirGap",
        "Material:RoofVegetation",
        "WindowMaterial:SimpleGlazingSystem",
        "WindowMaterial:Glazing",
        "WindowMaterial:GlazingGroup:Thermochromic",
        "WindowMaterial:Glazing:RefractionExtinctionMethod",
        "WindowMaterial:Gas",
        "WindowGap:SupportPillar",
        "WindowGap:DeflectionState",
        "WindowMaterial:GasMixture",
        "WindowMaterial:Gap",
        "WindowMaterial:Shade",
        "WindowMaterial:ComplexShade",
        "WindowMaterial:Blind",
        "WindowMaterial:Screen",
        "WindowMaterial:Shade:EquivalentLayer",
        "WindowMaterial:Drape:EquivalentLayer",
        "WindowMaterial:Blind:EquivalentLayer",
        "WindowMaterial:Screen:EquivalentLayer",
        "WindowMaterial:Glazing:EquivalentLayer",
        "WindowMaterial:Gap:EquivalentLayer",
        "MaterialProperty:MoisturePenetrationDepth:Settings",
        "MaterialProperty:PhaseChange",
        "MaterialProperty:PhaseChangeHysteresis",
        "MaterialProperty:VariableThermalConductivity",
        "MaterialProperty:HeatAndMoistureTransfer:Settings",
        "MaterialProperty:HeatAndMoistureTransfer:SorptionIsotherm",
        "MaterialProperty:HeatAndMoistureTransfer:Suction",
        "MaterialProperty:HeatAndMoistureTransfer:Redistribution",
        "MaterialProperty:HeatAndMoistureTransfer:Diffusion",
        "MaterialProperty:HeatAndMoistureTransfer:ThermalConductivity",
        "MaterialProperty:GlazingSpectralData",
        "Construction",
        "Construction:CfactorUndergroundWall",
        "Construction:FfactorGroundFloor",
        "Construction:InternalSource",
        "Construction:AirBoundary",
        "WindowThermalModel:Params",
        "WindowsCalculationEngine",
        "Construction:ComplexFenestrationState",
        "Construction:WindowEquivalentLayer",
        "Construction:WindowDataFile",
    ],
    Group.ThermalZonesAndSurfaces: [
        "GlobalGeometryRules",
        "GeometryTransform",
        "Zone",
        "ZoneList",
        "ZoneGroup",
        "BuildingSurface:Detailed",
        "Wall:Detailed",
        "RoofCeiling:Detailed",
        "Floor:Detailed",
        "Wall:Exterior",
        "Wall:Adiabatic",
        "Wall:Underground",
        "Wall:Interzone",
        "Roof",
        "Ceiling:Adiabatic",
        "Ceiling:Interzone",
        "Floor:GroundContact",
        "Floor:Adiabatic",
        "Floor:Interzone",
        "FenestrationSurface:Detailed",
        "Window",
        "Door",
        "GlazedDoor",
        "Window:Interzone",
        "Door:Interzone",
        "GlazedDoor:Interzone",
        "WindowShadingControl",
        "WindowProperty:FrameAndDivider",
        "WindowProperty:AirflowControl",
        "WindowProperty:StormWindow",
        "InternalMass",
        "Shading:Site",
        "Shading:Building",
        "Shading:Site:Detailed",
        "Shading:Building:Detailed",
        "Shading:Overhang",
        "Shading:Overhang:Projection",
        "Shading:Fin",
        "Shading:Fin:Projection",
        "Shading:Zone:Detailed",
        "ShadingProperty:Reflectance",
    ],
    Group.AdvancedConstructionSurfaceZoneConcepts: [
        "SurfaceProperty:HeatTransferAlgorithm",
        "SurfaceProperty:HeatTransferAlgorithm:MultipleSurface",
        "SurfaceProperty:HeatTransferAlgorithm:SurfaceList",
        "SurfaceProperty:HeatTransferAlgorithm:Construction",
        "SurfaceProperty:HeatBalanceSourceTerm",
        "SurfaceControl:MovableInsulation",
        "SurfaceProperty:OtherSideCoefficients",
        "SurfaceProperty:OtherSideConditionsModel",
        "SurfaceProperty:Underwater",
        "Foundation:Kiva",
        "Foundation:Kiva:Settings",
        "SurfaceProperty:ExposedFoundationPerimeter",
        "SurfaceConvectionAlgorithm:Inside:AdaptiveModelSelections",
        "SurfaceConvectionAlgorithm:Outside:AdaptiveModelSelections",
        "SurfaceConvectionAlgorithm:Inside:UserCurve",
        "SurfaceConvectionAlgorithm:Outside:UserCurve",
        "SurfaceProperty:ConvectionCoefficients",
        "SurfaceProperty:ConvectionCoefficients:MultipleSurface",
        "SurfaceProperties:VaporCoefficients",
        "SurfaceProperty:ExteriorNaturalVentedCavity",
        "SurfaceProperty:SolarIncidentInside",
        "SurfaceProperty:LocalEnvironment",
        "ZoneProperty:LocalEnvironment",
        "SurfaceProperty:SurroundingSurfaces",
        "ComplexFenestrationProperty:SolarAbsorbedLayers",
        "ZoneProperty:UserViewFactors:BySurfaceName",
    ],
    Group.DetailedGroundHeatTransfer: [
        "GroundHeatTransfer:Control",
        "GroundHeatTransfer:Slab:Materials",
        "GroundHeatTransfer:Slab:MatlProps",
        "GroundHeatTransfer:Slab:BoundConds",
        "GroundHeatTransfer:Slab:BldgProps",
        "GroundHeatTransfer:Slab:Insulation",
        "GroundHeatTransfer:Slab:EquivalentSlab",
        "GroundHeatTransfer:Slab:AutoGrid",
        "GroundHeatTransfer:Slab:ManualGrid",
        "GroundHeatTransfer:Slab:XFACE",
        "GroundHeatTransfer:Slab:YFACE",
        "GroundHeatTransfer:Slab:ZFACE",
        "GroundHeatTransfer:Basement:SimParameters",
        "GroundHeatTransfer:Basement:MatlProps",
        "GroundHeatTransfer:Basement:Insulation",
        "GroundHeatTransfer:Basement:SurfaceProps",
        "GroundHeatTransfer:Basement:BldgData",
        "GroundHeatTransfer:Basement:Interior",
        "GroundHeatTransfer:Basement:ComBldg",
        "GroundHeatTransfer:Basement:EquivSlab",
        "GroundHeatTransfer:Basement:EquivAutoGrid",
        "GroundHeatTransfer:Basement:AutoGrid",
        "GroundHeatTransfer:Basement:ManualGrid",
        "GroundHeatTransfer:Basement:XFACE",
        "GroundHeatTransfer:Basement:YFACE",
        "GroundHeatTransfer:Basement:ZFACE",
    ],
    Group.RoomAirModels: [
        "RoomAirModelType",
        "RoomAir:TemperaturePattern:UserDefined",
        "RoomAir:TemperaturePattern:ConstantGradient",
        "RoomAir:TemperaturePattern:TwoGradient",
        "RoomAir:TemperaturePattern:NondimensionalHeight",
        "RoomAir:TemperaturePattern:SurfaceMapping",
        "RoomAir:Node",
        "RoomAirSettings:OneNodeDisplacementVentilation",
        "RoomAirSettings:ThreeNodeDisplacementVentilation",
        "RoomAirSettings:CrossVentilation",
        "RoomAirSettings:UnderFloorAirDistributionInterior",
        "RoomAirSettings:UnderFloorAirDistributionExterior",
        "RoomAir:Node:AirflowNetwork",
        "RoomAir:Node:AirflowNetwork:AdjacentSurfaceList",
        "RoomAir:Node:AirflowNetwork:InternalGains",
        "RoomAir:Node:AirflowNetwork:HVACEquipment",
        "RoomAirSettings:AirflowNetwork",
    ],
    Group.InternalGains: [
        "People",
        "ComfortViewFactorAngles",
        "Lights",
        "ElectricEquipment",
        "GasEquipment",
        "HotWaterEquipment",
        "SteamEquipment",
        "OtherEquipment",
        "ElectricEquipment:ITE:AirCooled",
        "ZoneBaseboard:OutdoorTemperatureControlled",
        "SwimmingPool:Indoor",
        "ZoneContaminantSourceAndSink:CarbonDioxide",
        "ZoneContaminantSourceAndSink:Generic:Constant",
        "SurfaceContaminantSourceAndSink:Generic:PressureDriven",
        "ZoneContaminantSourceAndSink:Generic:CutoffModel",
        "ZoneContaminantSourceAndSink:Generic:DecaySource",
        "SurfaceContaminantSourceAndSink:Generic:BoundaryLayerDiffusion",
        "SurfaceContaminantSourceAndSink:Generic:DepositionVelocitySink",
        "ZoneContaminantSourceAndSink:Generic:DepositionRateSink",
    ],
    Group.Daylighting: [
        "Daylighting:Controls",
        "Daylighting:ReferencePoint",
        "Daylighting:DELight:ComplexFenestration",
        "DaylightingDevice:Tubular",
        "DaylightingDevice:Shelf",
        "DaylightingDevice:LightWell",
        "Output:DaylightFactors",
        "Output:IlluminanceMap",
        "OutputControl:IlluminanceMap:Style",
    ],
    Group.ZoneAirflow: [
        "ZoneInfiltration:DesignFlowRate",
        "ZoneInfiltration:EffectiveLeakageArea",
        "ZoneInfiltration:FlowCoefficient",
        "ZoneVentilation:DesignFlowRate",
        "ZoneVentilation:WindandStackOpenArea",
        "ZoneAirBalance:OutdoorAir",
        "ZoneMixing",
        "ZoneCrossMixing",
        "ZoneRefrigerationDoorMixing",
        "ZoneEarthtube",
        "ZoneCoolTower:Shower",
        "ZoneThermalChimney",
    ],
    Group.NaturalVentilationAndDuctLeakage: [
        "AirflowNetwork:SimulationControl",
        "AirflowNetwork:MultiZone:Zone",
        "AirflowNetwork:MultiZone:Surface",
        "AirflowNetwork:MultiZone:ReferenceCrackConditions",
        "AirflowNetwork:MultiZone:Surface:Crack",
        "AirflowNetwork:MultiZone:Surface:EffectiveLeakageArea",
        "AirflowNetwork:MultiZone:Component:DetailedOpening",
        "AirflowNetwork:MultiZone:Component:SimpleOpening",
        "AirflowNetwork:MultiZone:Component:HorizontalOpening",
        "AirflowNetwork:MultiZone:Component:ZoneExhaustFan",
        "AirflowNetwork:MultiZone:ExternalNode",
        "AirflowNetwork:MultiZone:WindPressureCoefficientArray",
        "AirflowNetwork:MultiZone:WindPressureCoefficientValues",
        "AirflowNetwork:ZoneControl:PressureController",
        "AirflowNetwork:Distribution:Node",
        "AirflowNetwork:Distribution:Component:Leak",
        "AirflowNetwork:Distribution:Component:LeakageRatio",
        "AirflowNetwork:Distribution:Component:Duct",
        "AirflowNetwork:Distribution:Component:Fan",
        "AirflowNetwork:Distribution:Component:Coil",
        "AirflowNetwork:Distribution:Component:HeatExchanger",
        "AirflowNetwork:Distribution:Component:TerminalUnit",
        "AirflowNetwork:Distribution:Component:ConstantPressureDrop",
        "AirflowNetwork:Distribution:Component:OutdoorAirFlow",
        "AirflowNetwork:Distribution:Component:ReliefAirFlow",
        "AirflowNetwork:Distribution:Linkage",
        "AirflowNetwork:Distribution:DuctViewFactors",
        "AirflowNetwork:OccupantVentilationControl",
        "AirflowNetwork:IntraZone:Node",
        "AirflowNetwork:IntraZone:Linkage",
    ],
    Group.ExteriorEquipment: [
        "Exterior:Lights",
        "Exterior:FuelEquipment",
        "Exterior:WaterEquipment",
    ],
    Group.HVACTemplates: [
        "HVACTemplate:Thermostat",
        "HVACTemplate:Zone:IdealLoadsAirSystem",
        "HVACTemplate:Zone:BaseboardHeat",
        "HVACTemplate:Zone:FanCoil",
        "HVACTemplate:Zone:PTAC",
        "HVACTemplate:Zone:PTHP",
        "HVACTemplate:Zone:WaterToAirHeatPump",
        "HVACTemplate:Zone:VRF",
        "HVACTemplate:Zone:Unitary",
        "HVACTemplate:Zone:VAV",
        "HVACTemplate:Zone:VAV:FanPowered",
        "HVACTemplate:Zone:VAV:HeatAndCool",
        "HVACTemplate:Zone:ConstantVolume",
        "HVACTemplate:Zone:DualDuct",
        "HVACTemplate:System:VRF",
        "HVACTemplate:System:Unitary",
        "HVACTemplate:System:UnitaryHeatPump:AirToAir",
        "HVACTemplate:System:UnitarySystem",
        "HVACTemplate:System:VAV",
        "HVACTemplate:System:PackagedVAV",
        "HVACTemplate:System:ConstantVolume",
        "HVACTemplate:System:DualDuct",
        "HVACTemplate:System:DedicatedOutdoorAir",
        "HVACTemplate:Plant:ChilledWaterLoop",
        "HVACTemplate:Plant:Chiller",
        "HVACTemplate:Plant:Chiller:ObjectReference",
        "HVACTemplate:Plant:Tower",
        "HVACTemplate:Plant:Tower:ObjectReference",
        "HVACTemplate:Plant:HotWaterLoop",
        "HVACTemplate:Plant:Boiler",
        "HVACTemplate:Plant:Boiler:ObjectReference",
        "HVACTemplate:Plant:MixedWaterLoop",
    ],
    Group.HVACDesignObjects: [
        "DesignSpecification:OutdoorAir",
        "DesignSpecification:ZoneAirDistribution",
        "Sizing:Parameters",
        "Sizing:Zone",
        "DesignSpecification:ZoneHVAC:Sizing",
        "DesignSpecification:AirTerminal:Sizing",
        "Sizing:System",
        "Sizing:Plant",
        "OutputControl:Sizing:Style",
    ],
    Group.ZoneHVACControlsAndThermostats: [
        "ZoneControl:Humidistat",
        "ZoneControl:Thermostat",
        "ZoneControl:Thermostat:OperativeTemperature",
        "ZoneControl:Thermostat:ThermalComfort",
        "ZoneControl:Thermostat:TemperatureAndHumidity",
        "ThermostatSetpoint:SingleHeating",
        "ThermostatSetpoint:SingleCooling",
        "ThermostatSetpoint:SingleHeatingOrCooling",
        "ThermostatSetpoint:DualSetpoint",
        "ThermostatSetpoint:ThermalComfort:Fanger:SingleHeating",
        "ThermostatSetpoint:ThermalComfort:Fanger:SingleCooling",
        "ThermostatSetpoint:ThermalComfort:Fanger:SingleHeatingOrCooling",
        "ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint",
        "ZoneControl:Thermostat:StagedDualSetpoint",
        "ZoneControl:ContaminantController",
    ],
    Group.ZoneHVACForcedAirUnits: [
        "ZoneHVAC:IdealLoadsAirSystem",
        "ZoneHVAC:FourPipeFanCoil",
        "ZoneHVAC:WindowAirConditioner",
        "ZoneHVAC:PackagedTerminalAirConditioner",
        "ZoneHVAC:PackagedTerminalHeatPump",
        "ZoneHVAC:WaterToAirHeatPump",
        "ZoneHVAC:Dehumidifier:DX",
        "ZoneHVAC:EnergyRecoveryVentilator",
        "ZoneHVAC:EnergyRecoveryVentilator:Controller",
        "ZoneHVAC:UnitVentilator",
        "ZoneHVAC:UnitHeater",
        "ZoneHVAC:EvaporativeCoolerUnit",
        "ZoneHVAC:HybridUnitaryHVAC",
        "ZoneHVAC:OutdoorAirUnit",
        "ZoneHVAC:OutdoorAirUnit:EquipmentList",
        "ZoneHVAC:TerminalUnit:VariableRefrigerantFlow",
    ],
    Group.ZoneHVACRadiativeConvectiveUnits: [
        "ZoneHVAC:Baseboard:RadiantConvective:Water",
        "ZoneHVAC:Baseboard:RadiantConvective:Steam",
        "ZoneHVAC:Baseboard:RadiantConvective:Electric",
        "ZoneHVAC:CoolingPanel:RadiantConvective:Water",
        "ZoneHVAC:Baseboard:Convective:Water",
        "ZoneHVAC:Baseboard:Convective:Electric",
        "ZoneHVAC:LowTemperatureRadiant:VariableFlow",
        "ZoneHVAC:LowTemperatureRadiant:ConstantFlow",
        "ZoneHVAC:LowTemperatureRadiant:Electric",
        "ZoneHVAC:LowTemperatureRadiant:SurfaceGroup",
        "ZoneHVAC:HighTemperatureRadiant",
        "ZoneHVAC:VentilatedSlab",
        "ZoneHVAC:VentilatedSlab:SlabGroup",
    ],
    Group.ZoneHVACAirLoopTerminalUnits: [
        "AirTerminal:SingleDuct:ConstantVolume:Reheat",
        "AirTerminal:SingleDuct:ConstantVolume:NoReheat",
        "AirTerminal:SingleDuct:VAV:NoReheat",
        "AirTerminal:SingleDuct:VAV:Reheat",
        "AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan",
        "AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat",
        "AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat",
        "AirTerminal:SingleDuct:SeriesPIU:Reheat",
        "AirTerminal:SingleDuct:ParallelPIU:Reheat",
        "AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction",
        "AirTerminal:SingleDuct:ConstantVolume:FourPipeBeam",
        "AirTerminal:SingleDuct:ConstantVolume:CooledBeam",
        "AirTerminal:SingleDuct:Mixer",
        "AirTerminal:DualDuct:ConstantVolume",
        "AirTerminal:DualDuct:VAV",
        "AirTerminal:DualDuct:VAV:OutdoorAir",
        "ZoneHVAC:AirDistributionUnit",
    ],
    Group.ZoneHVACEquipmentConnections: [
        "ZoneHVAC:EquipmentList",
        "ZoneHVAC:EquipmentConnections",
    ],
    Group.Fans: [
        "Fan:SystemModel",
        "Fan:ConstantVolume",
        "Fan:VariableVolume",
        "Fan:OnOff",
        "Fan:ZoneExhaust",
        "FanPerformance:NightVentilation",
        "Fan:ComponentModel",
    ],
    Group.Coils: [
        "Coil:Cooling:Water",
        "Coil:Cooling:Water:DetailedGeometry",
        "Coil:Cooling:DX",
        "Coil:Cooling:DX:CurveFit:Performance",
        "Coil:Cooling:DX:CurveFit:OperatingMode",
        "Coil:Cooling:DX:CurveFit:Speed",
        "Coil:Cooling:DX:SingleSpeed",
        "Coil:Cooling:DX:TwoSpeed",
        "Coil:Cooling:DX:MultiSpeed",
        "Coil:Cooling:DX:VariableSpeed",
        "Coil:Cooling:DX:TwoStageWithHumidityControlMode",
        "CoilPerformance:DX:Cooling",
        "Coil:Cooling:DX:VariableRefrigerantFlow",
        "Coil:Heating:DX:VariableRefrigerantFlow",
        "Coil:Cooling:DX:VariableRefrigerantFlow:FluidTemperatureControl",
        "Coil:Heating:DX:VariableRefrigerantFlow:FluidTemperatureControl",
        "Coil:Heating:Water",
        "Coil:Heating:Steam",
        "Coil:Heating:Electric",
        "Coil:Heating:Electric:MultiStage",
        "Coil:Heating:Fuel",
        "Coil:Heating:Gas:MultiStage",
        "Coil:Heating:Desuperheater",
        "Coil:Heating:DX:SingleSpeed",
        "Coil:Heating:DX:MultiSpeed",
        "Coil:Heating:DX:VariableSpeed",
        "Coil:Cooling:WaterToAirHeatPump:ParameterEstimation",
        "Coil:Heating:WaterToAirHeatPump:ParameterEstimation",
        "Coil:Cooling:WaterToAirHeatPump:EquationFit",
        "Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit",
        "Coil:Heating:WaterToAirHeatPump:EquationFit",
        "Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit",
        "Coil:WaterHeating:AirToWaterHeatPump:Pumped",
        "Coil:WaterHeating:AirToWaterHeatPump:Wrapped",
        "Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed",
        "Coil:WaterHeating:Desuperheater",
        "CoilSystem:Cooling:DX",
        "CoilSystem:Heating:DX",
        "CoilSystem:Cooling:Water:HeatExchangerAssisted",
        "CoilSystem:Cooling:DX:HeatExchangerAssisted",
        "CoilSystem:IntegratedHeatPump:AirSource",
        "Coil:Cooling:DX:SingleSpeed:ThermalStorage",
    ],
    Group.EvaporativeCoolers: [
        "EvaporativeCooler:Direct:CelDekPad",
        "EvaporativeCooler:Indirect:CelDekPad",
        "EvaporativeCooler:Indirect:WetCoil",
        "EvaporativeCooler:Indirect:ResearchSpecial",
        "EvaporativeCooler:Direct:ResearchSpecial",
    ],
    Group.HumidifiersAndDehumidifiers: [
        "Humidifier:Steam:Electric",
        "Humidifier:Steam:Gas",
        "Dehumidifier:Desiccant:NoFans",
        "Dehumidifier:Desiccant:System",
    ],
    Group.HeatRecovery: [
        "HeatExchanger:AirToAir:FlatPlate",
        "HeatExchanger:AirToAir:SensibleAndLatent",
        "HeatExchanger:Desiccant:BalancedFlow",
        "HeatExchanger:Desiccant:BalancedFlow:PerformanceDataType1",
    ],
    Group.UnitaryEquipment: [
        "AirLoopHVAC:UnitarySystem",
        "UnitarySystemPerformance:Multispeed",
        "AirLoopHVAC:Unitary:Furnace:HeatOnly",
        "AirLoopHVAC:Unitary:Furnace:HeatCool",
        "AirLoopHVAC:UnitaryHeatOnly",
        "AirLoopHVAC:UnitaryHeatCool",
        "AirLoopHVAC:UnitaryHeatPump:AirToAir",
        "AirLoopHVAC:UnitaryHeatPump:WaterToAir",
        "AirLoopHVAC:UnitaryHeatCool:VAVChangeoverBypass",
        "AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed",
    ],
    Group.VariableRefrigerantFlowEquipment: [
        "AirConditioner:VariableRefrigerantFlow",
        "AirConditioner:VariableRefrigerantFlow:FluidTemperatureControl",
        "AirConditioner:VariableRefrigerantFlow:FluidTemperatureControl:HR",
        "ZoneTerminalUnitList",
    ],
    Group.Controllers: [
        "Controller:WaterCoil",
        "Controller:OutdoorAir",
        "Controller:MechanicalVentilation",
        "AirLoopHVAC:ControllerList",
    ],
    Group.AirDistribution: [
        "AirLoopHVAC",
        "AirLoopHVAC:OutdoorAirSystem:EquipmentList",
        "AirLoopHVAC:OutdoorAirSystem",
        "OutdoorAir:Mixer",
        "AirLoopHVAC:ZoneSplitter",
        "AirLoopHVAC:SupplyPlenum",
        "AirLoopHVAC:SupplyPath",
        "AirLoopHVAC:ZoneMixer",
        "AirLoopHVAC:ReturnPlenum",
        "AirLoopHVAC:ReturnPath",
        "AirLoopHVAC:DedicatedOutdoorAirSystem",
        "AirLoopHVAC:Mixer",
        "AirLoopHVAC:Splitter",
    ],
    Group.NodeBranchManagement: [
        "Branch",
        "BranchList",
        "Connector:Splitter",
        "Connector:Mixer",
        "ConnectorList",
        "NodeList",
        "OutdoorAir:Node",
        "OutdoorAir:NodeList",
        "Pipe:Adiabatic",
        "Pipe:Adiabatic:Steam",
        "Pipe:Indoor",
        "Pipe:Outdoor",
        "Pipe:Underground",
        "PipingSystem:Underground:Domain",
        "PipingSystem:Underground:PipeCircuit",
        "PipingSystem:Underground:PipeSegment",
        "Duct",
    ],
    Group.Pumps: [
        "Pump:VariableSpeed",
        "Pump:ConstantSpeed",
        "Pump:VariableSpeed:Condensate",
        "HeaderedPumps:ConstantSpeed",
        "HeaderedPumps:VariableSpeed",
    ],
    Group.PlantCondenserFlowControl: [
        "TemperingValve",
    ],
    Group.NonZoneEquipment: [
        "LoadProfile:Plant",
    ],
    Group.SolarCollectors: [
        "SolarCollectorPerformance:FlatPlate",
        "SolarCollector:FlatPlate:Water",
        "SolarCollector:FlatPlate:PhotovoltaicThermal",
        "SolarCollectorPerformance:PhotovoltaicThermal:Simple",
        "SolarCollector:IntegralCollectorStorage",
        "SolarCollectorPerformance:IntegralCollectorStorage",
        "SolarCollector:UnglazedTranspired",
        "SolarCollector:UnglazedTranspired:Multisystem",
    ],
    Group.PlantHeatingAndCoolingEquipment: [
        "Boiler:HotWater",
        "Boiler:Steam",
        "Chiller:Electric:EIR",
        "Chiller:Electric:ReformulatedEIR",
        "Chiller:Electric",
        "Chiller:Absorption:Indirect",
        "Chiller:Absorption",
        "Chiller:ConstantCOP",
        "Chiller:EngineDriven",
        "Chiller:CombustionTurbine",
        "ChillerHeater:Absorption:DirectFired",
        "ChillerHeater:Absorption:DoubleEffect",
        "HeatPump:PlantLoop:EIR:Cooling",
        "HeatPump:PlantLoop:EIR:Heating",
        "HeatPump:WaterToWater:EquationFit:Heating",
        "HeatPump:WaterToWater:EquationFit:Cooling",
        "HeatPump:WaterToWater:ParameterEstimation:Cooling",
        "HeatPump:WaterToWater:ParameterEstimation:Heating",
        "DistrictCooling",
        "DistrictHeating",
        "PlantComponent:TemperatureSource",
        "CentralHeatPumpSystem",
        "ChillerHeaterPerformance:Electric:EIR",
    ],
    Group.CondenserEquipmentAndHeatExchangers: [
        "CoolingTower:SingleSpeed",
        "CoolingTower:TwoSpeed",
        "CoolingTower:VariableSpeed:Merkel",
        "CoolingTower:VariableSpeed",
        "CoolingTowerPerformance:CoolTools",
        "CoolingTowerPerformance:YorkCalc",
        "EvaporativeFluidCooler:SingleSpeed",
        "EvaporativeFluidCooler:TwoSpeed",
        "FluidCooler:SingleSpeed",
        "FluidCooler:TwoSpeed",
        "GroundHeatExchanger:System",
        "GroundHeatExchanger:Vertical:Properties",
        "GroundHeatExchanger:Vertical:Array",
        "GroundHeatExchanger:Vertical:Single",
        "GroundHeatExchanger:ResponseFactors",
        "GroundHeatExchanger:Pond",
        "GroundHeatExchanger:Surface",
        "GroundHeatExchanger:HorizontalTrench",
        "GroundHeatExchanger:Slinky",
        "HeatExchanger:FluidToFluid",
    ],
    Group.WaterHeatersAndThermalStorage: [
        "WaterHeater:Mixed",
        "WaterHeater:Stratified",
        "WaterHeater:Sizing",
        "WaterHeater:HeatPump:PumpedCondenser",
        "WaterHeater:HeatPump:WrappedCondenser",
        "ThermalStorage:Ice:Simple",
        "ThermalStorage:Ice:Detailed",
        "ThermalStorage:ChilledWater:Mixed",
        "ThermalStorage:ChilledWater:Stratified",
    ],
    Group.PlantCondenserLoops: [
        "PlantLoop",
        "CondenserLoop",
    ],
    Group.PlantCondenserControl: [
        "PlantEquipmentList",
        "CondenserEquipmentList",
        "PlantEquipmentOperation:Uncontrolled",
        "PlantEquipmentOperation:CoolingLoad",
        "PlantEquipmentOperation:HeatingLoad",
        "PlantEquipmentOperation:OutdoorDryBulb",
        "PlantEquipmentOperation:OutdoorWetBulb",
        "PlantEquipmentOperation:OutdoorRelativeHumidity",
        "PlantEquipmentOperation:OutdoorDewpoint",
        "PlantEquipmentOperation:ComponentSetpoint",
        "PlantEquipmentOperation:ThermalEnergyStorage",
        "PlantEquipmentOperation:OutdoorDryBulbDifference",
        "PlantEquipmentOperation:OutdoorWetBulbDifference",
        "PlantEquipmentOperation:OutdoorDewpointDifference",
        "PlantEquipmentOperationSchemes",
        "CondenserEquipmentOperationSchemes",
    ],
    Group.EnergyManagementSystem: [
        "EnergyManagementSystem:Sensor",
        "EnergyManagementSystem:Actuator",
        "EnergyManagementSystem:ProgramCallingManager",
        "EnergyManagementSystem:Program",
        "EnergyManagementSystem:Subroutine",
        "EnergyManagementSystem:GlobalVariable",
        "EnergyManagementSystem:OutputVariable",
        "EnergyManagementSystem:MeteredOutputVariable",
        "EnergyManagementSystem:TrendVariable",
        "EnergyManagementSystem:InternalVariable",
        "EnergyManagementSystem:CurveOrTableIndexVariable",
        "EnergyManagementSystem:ConstructionIndexVariable",
    ],
    Group.ExternalInterface: [
        "ExternalInterface",
        "ExternalInterface:Schedule",
        "ExternalInterface:Variable",
        "ExternalInterface:Actuator",
        "ExternalInterface:FunctionalMockupUnitImport",
        "ExternalInterface:FunctionalMockupUnitImport:From:Variable",
        "ExternalInterface:FunctionalMockupUnitImport:To:Schedule",
        "ExternalInterface:FunctionalMockupUnitImport:To:Actuator",
        "ExternalInterface:FunctionalMockupUnitImport:To:Variable",
        "ExternalInterface:FunctionalMockupUnitExport:From:Variable",
        "ExternalInterface:FunctionalMockupUnitExport:To:Schedule",
        "ExternalInterface:FunctionalMockupUnitExport:To:Actuator",
        "ExternalInterface:FunctionalMockupUnitExport:To:Variable",
    ],
    Group.UserDefinedHVACAndPlantComponentModels: [
        "ZoneHVAC:ForcedAir:UserDefined",
        "PlantEquipmentOperation:UserDefined",
        "AirTerminal:SingleDuct:UserDefined",
        "Coil:UserDefined",
        "PlantComponent:UserDefined",
    ],
    Group.SystemAvailabilityManagers: [
        "AvailabilityManager:Scheduled",
        "AvailabilityManager:ScheduledOn",
        "AvailabilityManager:ScheduledOff",
        "AvailabilityManager:OptimumStart",
        "AvailabilityManager:NightCycle",
        "AvailabilityManager:DifferentialThermostat",
        "AvailabilityManager:HighTemperatureTurnOff",
        "AvailabilityManager:HighTemperatureTurnOn",
        "AvailabilityManager:LowTemperatureTurnOff",
        "AvailabilityManager:LowTemperatureTurnOn",
        "AvailabilityManager:NightVentilation",
        "AvailabilityManager:HybridVentilation",
        "AvailabilityManagerAssignmentList",
    ],
    Group.SetpointManagers: [
        "SetpointManager:Scheduled",
        "SetpointManager:Scheduled:DualSetpoint",
        "SetpointManager:OutdoorAirReset",
        "SetpointManager:SingleZone:Reheat",
        "SetpointManager:SingleZone:Heating",
        "SetpointManager:SingleZone:Cooling",
        "SetpointManager:SingleZone:Humidity:Minimum",
        "SetpointManager:SingleZone:Humidity:Maximum",
        "SetpointManager:MixedAir",
        "SetpointManager:OutdoorAirPretreat",
        "SetpointManager:Warmest",
        "SetpointManager:Coldest",
        "SetpointManager:ReturnAirBypassFlow",
        "SetpointManager:WarmestTemperatureFlow",
        "SetpointManager:MultiZone:Heating:Average",
        "SetpointManager:MultiZone:Cooling:Average",
        "SetpointManager:MultiZone:MinimumHumidity:Average",
        "SetpointManager:MultiZone:MaximumHumidity:Average",
        "SetpointManager:MultiZone:Humidity:Minimum",
        "SetpointManager:MultiZone:Humidity:Maximum",
        "SetpointManager:FollowOutdoorAirTemperature",
        "SetpointManager:FollowSystemNodeTemperature",
        "SetpointManager:FollowGroundTemperature",
        "SetpointManager:CondenserEnteringReset",
        "SetpointManager:CondenserEnteringReset:Ideal",
        "SetpointManager:SingleZone:OneStageCooling",
        "SetpointManager:SingleZone:OneStageHeating",
        "SetpointManager:ReturnTemperature:ChilledWater",
        "SetpointManager:ReturnTemperature:HotWater",
    ],
    Group.Refrigeration: [
        "Refrigeration:Case",
        "Refrigeration:CompressorRack",
        "Refrigeration:CaseAndWalkInList",
        "Refrigeration:Condenser:AirCooled",
        "Refrigeration:Condenser:EvaporativeCooled",
        "Refrigeration:Condenser:WaterCooled",
        "Refrigeration:Condenser:Cascade",
        "Refrigeration:GasCooler:AirCooled",
        "Refrigeration:TransferLoadList",
        "Refrigeration:Subcooler",
        "Refrigeration:Compressor",
        "Refrigeration:CompressorList",
        "Refrigeration:System",
        "Refrigeration:TranscriticalSystem",
        "Refrigeration:SecondarySystem",
        "Refrigeration:WalkIn",
        "Refrigeration:AirChiller",
        "ZoneHVAC:RefrigerationChillerSet",
    ],
    Group.DemandLimitingControls: [
        "DemandManagerAssignmentList",
        "DemandManager:ExteriorLights",
        "DemandManager:Lights",
        "DemandManager:ElectricEquipment",
        "DemandManager:Thermostats",
        "DemandManager:Ventilation",
    ],
    Group.ElectricLoadCenterGeneratorSpecifications: [
        "Generator:InternalCombustionEngine",
        "Generator:CombustionTurbine",
        "Generator:MicroTurbine",
        "Generator:Photovoltaic",
        "PhotovoltaicPerformance:Simple",
        "PhotovoltaicPerformance:EquivalentOne-Diode",
        "PhotovoltaicPerformance:Sandia",
        "Generator:PVWatts",
        "ElectricLoadCenter:Inverter:PVWatts",
        "Generator:FuelCell",
        "Generator:FuelCell:PowerModule",
        "Generator:FuelCell:AirSupply",
        "Generator:FuelCell:WaterSupply",
        "Generator:FuelCell:AuxiliaryHeater",
        "Generator:FuelCell:ExhaustGasToWaterHeatExchanger",
        "Generator:FuelCell:ElectricalStorage",
        "Generator:FuelCell:Inverter",
        "Generator:FuelCell:StackCooler",
        "Generator:MicroCHP",
        "Generator:MicroCHP:NonNormalizedParameters",
        "Generator:FuelSupply",
        "Generator:WindTurbine",
        "ElectricLoadCenter:Generators",
        "ElectricLoadCenter:Inverter:Simple",
        "ElectricLoadCenter:Inverter:FunctionOfPower",
        "ElectricLoadCenter:Inverter:LookUpTable",
        "ElectricLoadCenter:Storage:Simple",
        "ElectricLoadCenter:Storage:Battery",
        "ElectricLoadCenter:Transformer",
        "ElectricLoadCenter:Distribution",
        "ElectricLoadCenter:Storage:Converter",
    ],
    Group.WaterSystems: [
        "WaterUse:Equipment",
        "WaterUse:Connections",
        "WaterUse:Storage",
        "WaterUse:Well",
        "WaterUse:RainCollector",
    ],
    Group.OperationalFaults: [
        "FaultModel:TemperatureSensorOffset:OutdoorAir",
        "FaultModel:HumiditySensorOffset:OutdoorAir",
        "FaultModel:EnthalpySensorOffset:OutdoorAir",
        "FaultModel:TemperatureSensorOffset:ReturnAir",
        "FaultModel:EnthalpySensorOffset:ReturnAir",
        "FaultModel:TemperatureSensorOffset:ChillerSupplyWater",
        "FaultModel:TemperatureSensorOffset:CoilSupplyAir",
        "FaultModel:TemperatureSensorOffset:CondenserSupplyWater",
        "FaultModel:ThermostatOffset",
        "FaultModel:HumidistatOffset",
        "FaultModel:Fouling:AirFilter",
        "FaultModel:Fouling:Boiler",
        "FaultModel:Fouling:EvaporativeCooler",
        "FaultModel:Fouling:Chiller",
        "FaultModel:Fouling:CoolingTower",
        "FaultModel:Fouling:Coil",
    ],
    Group.GeneralDataEntry: [
        "Matrix:TwoDimension",
    ],
    Group.HybridModel: [
        "HybridModel:Zone",
    ],
    Group.PerformanceCurves: [
        "Curve:Linear",
        "Curve:QuadLinear",
        "Curve:Quadratic",
        "Curve:Cubic",
        "Curve:Quartic",
        "Curve:Exponent",
        "Curve:Bicubic",
        "Curve:Biquadratic",
        "Curve:QuadraticLinear",
        "Curve:CubicLinear",
        "Curve:Triquadratic",
        "Curve:Functional:PressureDrop",
        "Curve:FanPressureRise",
        "Curve:ExponentialSkewNormal",
        "Curve:Sigmoid",
        "Curve:RectangularHyperbola1",
        "Curve:RectangularHyperbola2",
        "Curve:ExponentialDecay",
        "Curve:DoubleExponentialDecay",
        "Curve:ChillerPartLoadWithLift",
    ],
    Group.PerformanceTables: [
        "Table:IndependentVariable",
        "Table:IndependentVariableList",
        "Table:Lookup",
    ],
    Group.FluidProperties: [
        "FluidProperties:Name",
        "FluidProperties:GlycolConcentration",
        "FluidProperties:Temperatures",
        "FluidProperties:Saturated",
        "FluidProperties:Superheated",
        "FluidProperties:Concentration",
    ],
    Group.Economics: [
        "CurrencyType",
        "ComponentCost:Adjustments",
        "ComponentCost:Reference",
        "ComponentCost:LineItem",
        "UtilityCost:Tariff",
        "UtilityCost:Qualify",
        "UtilityCost:Charge:Simple",
        "UtilityCost:Charge:Block",
        "UtilityCost:Ratchet",
        "UtilityCost:Variable",
        "UtilityCost:Computation",
        "LifeCycleCost:Parameters",
        "LifeCycleCost:RecurringCosts",
        "LifeCycleCost:NonrecurringCost",
        "LifeCycleCost:UsePriceEscalation",
        "LifeCycleCost:UseAdjustment",
    ],
    Group.Parametrics: [
        "Parametric:SetValueForRun",
        "Parametric:Logic",
        "Parametric:RunControl",
        "Parametric:FileNameSuffix",
    ],
    Group.OutputReporting: [
        "Output:VariableDictionary",
        "Output:Surfaces:List",
        "Output:Surfaces:Drawing",
        "Output:Schedules",
        "Output:Constructions",
        "Output:EnergyManagementSystem",
        "OutputControl:SurfaceColorScheme",
        "Output:Table:SummaryReports",
        "Output:Table:TimeBins",
        "Output:Table:Monthly",
        "Output:Table:Annual",
        "OutputControl:Table:Style",
        "OutputControl:ReportingTolerances",
        "Output:Variable",
        "Output:Meter",
        "Output:Meter:MeterFileOnly",
        "Output:Meter:Cumulative",
        "Output:Meter:Cumulative:MeterFileOnly",
        "Meter:Custom",
        "Meter:CustomDecrement",
        "OutputControl:Files",
        "Output:JSON",
        "Output:SQLite",
        "Output:EnvironmentalImpactFactors",
        "EnvironmentalImpactFactors",
        "FuelFactors",
        "Output:Diagnostics",
        "Output:DebuggingData",
        "Output:PreprocessorMessage",
    ],
    Group.PythonPluginSystem: [
        "PythonPlugin:SearchPaths",
        "PythonPlugin:Instance",
        "PythonPlugin:Variables",
        "PythonPlugin:TrendVariable",
        "PythonPlugin:OutputVariable",
    ]
}


def get_group_name_for_idd_object(idd_object_name):
    for group_name, object_list in idd_object_map.items():
        if idd_object_name in object_list:
            return group_name
    return Group.Unknown


print("* Processing schema into RTD contents")
rtd_out = ""
# schema_version_number = o["epJSON_schema_version"]
# schema_version_sha = o["epJSON_schema_build"]
# h.write(f"<h1>Schema {schema_version_number} - {schema_version_sha}</h1>")
idf_objects: dict = o["properties"]
rtd_out += f"{len(idf_objects)} objects in all.\n\n"

object_entries_by_group = dict()

for obj_name, data in idf_objects.items():
    group_name = get_group_name_for_idd_object(obj_name)
    details = ""
    details += f"{obj_name}\n{'=' * len(obj_name)}\n\n"
    if 'memo' in data:
        memo = data['memo'].replace('*', '\\*').replace('|', '\\|')
        details += f"{memo}\n"
    pattern_props = data['patternProperties']
    value_with_unknown_key = next(iter(pattern_props.values()))  # only key could be '.*' or something else
    fields: dict = value_with_unknown_key['properties']
    details += "\n* Fields\n\n"
    for field_name, field_data in fields.items():
        default_string = ''
        if 'default' in field_data:
            if obj_name == 'Version':
                default_string = ' (Current Version)'
            else:
                default_data = str(field_data['default']).replace('*', '\\*')
                default_string = f" (Default: {default_data})"
        field_type = field_data.get('type', 'unknown field type')
        this_field_type = field_type
        if field_type == 'array' and 'items' in field_data:
            this_field_type = 'Array of {'
            for i, variable_name in enumerate(field_data['items']['properties']):
                if i == 0:
                    this_field_type += variable_name
                else:
                    this_field_type += ', ' + variable_name
            this_field_type += '}'
        details += f"    - `{field_name}` [{this_field_type}]{default_string}\n"
    final_object_snippet = details + '\n'
    if group_name in object_entries_by_group:
        object_entries_by_group[group_name].append(final_object_snippet)
    else:
        object_entries_by_group[group_name] = [final_object_snippet]

for group_name in Group.all_group_names():
    if group_name not in object_entries_by_group:
        continue
    rtd_out += f"{group_name}\n{'*' * len(group_name)}\n\n"
    for obj_snippet in object_entries_by_group[group_name]:
        rtd_out += obj_snippet

print("* Writing schema contents into template, saving at schema.rst")
schema_template = sphinx_dir / 'in.schema.rst.in'
with open(schema_template) as template:
    template_text = template.read()

output_schema_file = sphinx_dir / 'schema.rst'
with open(output_schema_file, 'w') as out:
    out.write(template_text.replace('{REPLACE_ME}', rtd_out))

print("* Schema docs complete!")

# then add the folder to the sphinx extra paths so the objects get included
html_extra_path = ['_build_c']

# -- Project information -----------------------------------------------------

project = u'EnergyPlus Live Documentation'
copyright = u'2020, National Renewable Energy Laboratory for the United States Department of Energy'
author = u'National Renewable Energy Laboratory for the United States Department of Energy'

# The short X.Y version
version = u''
# The full version, including alpha/beta/rc tags
release = u'0.2'

extra_nav_links = {
    'Blah': 'https://energyplus.net'
}

# -- General configuration ---------------------------------------------------

# If your documentation needs a minimal Sphinx version, state it here.
#
# needs_sphinx = '1.0'

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
sys.path.insert(0, os.path.abspath('.'))
extensions = [
    'sphinx.ext.autodoc',
    'sphinx.ext.coverage',
    'sphinx.ext.mathjax',
    'sphinx.ext.viewcode',
]

# Add any paths that contain templates here, relative to this directory.
templates_path = ['.templates']

# The suffix(es) of source filenames.
# You can specify multiple suffix as a list of string:
#
# source_suffix = ['.rst', '.md']
source_suffix = '.rst'

# The master toctree document.
master_doc = 'index'

# The language for content autogenerated by Sphinx. Refer to documentation
# for a list of supported languages.
#
# This is also used if you do content translation via gettext catalogs.
# Usually you set "language" from the command line for these cases.
language = None

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = [u'_build', 'Thumbs.db', '.DS_Store']

# The name of the Pygments (syntax highlighting) style to use.
pygments_style = None


# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
html_theme = 'haiku'
html_logo = '../../../release/favicon.png'
html_show_sphinx = False
# html_show_copyright = False

# Theme options are theme-specific and customize the look and feel of a theme
# further.  For a list of options available for each theme, see the
# documentation.
#
# html_theme_options = {}

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
# html_static_path = ['_static']

# Custom sidebar templates, must be a dictionary that maps document names
# to template names.
#
# The default sidebars (for documents that don't match any pattern) are
# defined by theme itself.  Builtin themes are using these templates by
# default: ``['localtoc.html', 'relations.html', 'sourcelink.html',
# 'searchbox.html']``.
#
html_sidebars = {
    '**': [
        'about.html',
        'navigation.html',
        'relations.html',
        'searchbox.html',
    ]
}

# -- Options for HTMLHelp output ---------------------------------------------

# Output file base name for HTML help builder.
htmlhelp_basename = 'EnergyPlusPythonAPIDocumentationdoc'


# -- Options for LaTeX output ------------------------------------------------

latex_elements = {
    # The paper size ('letterpaper' or 'a4paper').
    #
    # 'papersize': 'letterpaper',

    # The font size ('10pt', '11pt' or '12pt').
    #
    # 'pointsize': '10pt',

    # Additional stuff for the LaTeX preamble.
    #
    # 'preamble': '',

    # Latex figure (float) alignment
    #
    # 'figure_align': 'htbp',
}

# Grouping the document tree into LaTeX files. List of tuples
# (source start file, target name, title,
#  author, documentclass [howto, manual, or own class]).
latex_documents = [
    (master_doc, 'EnergyPlusPythonAPIDocumentation.tex', u'EnergyPlus Python API Documentation Documentation',
     u'National Renewable Energy Laboratory for the United States Department of Energy', 'manual'),
]


# -- Options for manual page output ------------------------------------------

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).
man_pages = [
    (master_doc, 'energypluspythonapidocumentation', u'EnergyPlus Python API Documentation Documentation',
     [author], 1)
]


# -- Options for Texinfo output ----------------------------------------------

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
texinfo_documents = [
    (master_doc, 'EnergyPlusPythonAPIDocumentation', u'EnergyPlus Python API Documentation Documentation',
     author, 'EnergyPlusPythonAPIDocumentation', 'One line description of project.',
     'Miscellaneous'),
]


# -- Options for Epub output -------------------------------------------------

# Bibliographic Dublin Core info.
epub_title = project

# The unique identifier of the text. This can be a ISBN number
# or the project homepage.
#
# epub_identifier = ''

# A unique identification for the text.
#
# epub_uid = ''

# A list of files that should not be packed into the epub file.
epub_exclude_files = ['search.html']


# -- Extension configuration -------------------------------------------------
