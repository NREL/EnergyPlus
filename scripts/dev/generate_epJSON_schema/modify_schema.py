def anyOf():
    return [
        {
            "type": "number",
        },
        {
            "type": "string"
        }
    ]


def isInt(s):
    try:
        int(s)
        return True
    except ValueError:
        return False

extension_renaming = {
    'LifeCycleCost:UseAdjustment': 'multipliers',
    'LifeCycleCost:UsePriceEscalation': 'escalations',
    'ElectricLoadCenter:Transformer': 'meters',
    'ElectricLoadCenter:Generators': 'generator_outputs',
    'Generator:FuelCell:AirSupply': 'constituent_fractions',
    'DemandManager:ExteriorLights': 'lights',
    'DemandManager:Ventilation': 'controllers',
    'DemandManagerAssignmentList': 'manager_data',
    'DemandManager:Lights': 'lights',
    'DemandManager:Thermostats': 'thermostats',
    'DemandManager:ElectricEquipment': 'equipment',
    'DaylightingDevice:Tubular': 'transition_lengths',
    'Daylighting:Controls': 'control_data',
    'ZoneHVAC:Baseboard:RadiantConvective:Steam': 'surface_fractions',
    'ZoneHVAC:Baseboard:RadiantConvective:Electric': 'surface_fractions',
    'ZoneHVAC:HighTemperatureRadiant': 'surface_fractions',
    'ZoneHVAC:LowTemperatureRadiant:SurfaceGroup': 'surface_fractions',
    'ZoneHVAC:Baseboard:RadiantConvective:Water': 'surface_fractions',
    'ZoneHVAC:VentilatedSlab:SlabGroup': 'data',
    'ZoneHVAC:CoolingPanel:RadiantConvective:Water': 'surface_fractions',
    'AirConditioner:VariableRefrigerantFlow:FluidTemperatureControl:HR': 'loading_indices',
    'AirConditioner:VariableRefrigerantFlow:FluidTemperatureControl': 'loading_indices',
    'ZoneTerminalUnitList': 'terminal_units',
    'RoomAir:TemperaturePattern:NondimensionalHeight': 'pairs',
    'RoomAir:Node:AirflowNetwork:InternalGains': 'gains',
    'RoomAirSettings:AirflowNetwork': 'nodes',
    'RoomAir:Node:AirflowNetwork:HVACEquipment': 'equipment_fractions',
    'RoomAir:TemperaturePattern:SurfaceMapping': 'surface_deltas',
    'RoomAir:Node:AirflowNetwork:AdjacentSurfaceList': 'surfaces',
    'Controller:MechanicalVentilation': 'zone_specifications',
    'WaterUse:Connections': 'connections',
    'WaterUse:RainCollector': 'surfaces',
    'Output:Table:Annual': 'variable_details',
    'Meter:CustomDecrement': 'variable_details',
    'Output:Table:Monthly': 'variable_details',
    'Output:Table:SummaryReports': 'reports',
    'Meter:Custom': 'variable_details',
    'UnitarySystemPerformance:Multispeed': 'flow_ratios',
    'SurfaceProperty:ExteriorNaturalVentedCavity': 'surface',
    'ZoneProperty:UserViewFactors:bySurfaceName': 'view_factors',
    'SurfaceProperty:HeatTransferAlgorithm:SurfaceList': 'surface',
    'AirLoopHVAC:ZoneSplitter': 'nodes',
    'AirLoopHVAC:SupplyPath': 'components',
    'AirLoopHVAC:ReturnPath': 'components',
    'AirLoopHVAC:ReturnPlenum': 'nodes',
    'AirLoopHVAC:ZoneMixer': 'nodes',
    'AirLoopHVAC:SupplyPlenum': 'nodes',
    'BuildingSurface:Detailed': 'vertices',
    'Shading:Zone:Detailed': 'vertices',
    'RoofCeiling:Detailed': 'vertices',
    'Shading:Site:Detailed': 'vertices',
    'Wall:Detailed': 'vertices',
    'ZoneList': 'zones',
    'Floor:Detailed': 'vertices',
    'Shading:Building:Detailed': 'vertices',
    'SolarCollector:UnglazedTranspired:Multisystem': 'systems',
    'SolarCollector:UnglazedTranspired': 'surfaces',
    'Parametric:SetValueForRun': 'values',
    'Parametric:Logic': 'lines',
    'Parametric:FileNameSuffix': 'suffixes',
    'Parametric:RunControl': 'runs',
    'ZoneHVAC:EquipmentList': 'equipment',
    'AvailabilityManagerAssignmentList': 'managers',
    'Table:MultiVariableLookup': 'values',
    'Table:OneIndependentVariable': 'values',
    'Table:TwoIndependentVariables': 'values',
    'Matrix:TwoDimension': 'values',
    'WindowMaterial:GlazingGroup:Thermochromic': 'temperature_data',
    'Schedule:Compact': 'data',
    'Schedule:Day:Interval': 'data',
    'Schedule:Week:Compact': 'data',
    'EnergyManagementSystem:GlobalVariable': 'variables',
    'EnergyManagementSystem:ProgramCallingManager': 'programs',
    'EnergyManagementSystem:Program': 'lines',
    'EnergyManagementSystem:Subroutine': 'lines',
    'Refrigeration:CaseAndWalkInList': 'cases_and_walkins',
    'Refrigeration:CompressorList': 'compressors',
    'ZoneHVAC:RefrigerationChillerSet': 'chillers',
    'Refrigeration:WalkIn': 'zone_data',
    'Refrigeration:TransferLoadList': 'transfer_loads',
    'Branch': 'components',
    'PipingSystem:Underground:Domain': 'pipe_circuits',
    'Connector:Splitter': 'branches',
    'Connector:Mixer': 'branches',
    'BranchList': 'branches',
    'PipingSystem:Underground:PipeCircuit': 'pipe_segments',
    'NodeList': 'nodes',
    'OutdoorAir:NodeList': 'nodes',
    'Fan:SystemModel': 'speed_fractions',
    'AirflowNetwork:Distribution:DuctViewFactors': 'surfaces',
    'GroundHeatExchanger:System': 'vertical_well_locations',
    'GroundHeatExchanger:ResponseFactors': 'g_functions',
    'Foundation:Kiva': 'blocks',
    'SurfaceProperty:ExposedFoundationPerimeter': 'surfaces',
    'SurfaceProperty:SurroundingSurfaces': 'surfaces',
    'ZoneHVAC:HybridUnitaryHVAC': 'modes',
    'ShadowCalculation': 'shading_zone_groups',
    'Schedule:Year': 'schedule_weeks',
    'WindowShadingControl': 'fenestration_surfaces'
}
remaining_objects = [
    'Site:SpectrumData',
    'Schedule:Day:List',
    'MaterialProperty:GlazingSpectralData'
]

def change_version(schema):
    schema["epJSON_schema_version"] = "${CMAKE_VERSION_MAJOR}.${CMAKE_VERSION_MINOR}.${CMAKE_VERSION_PATCH}"
    schema["epJSON_schema_build"] = "${CMAKE_VERSION_BUILD}"
    loc = schema['properties']['Version']['patternProperties']['.*']['properties']['version_identifier']
    loc['default'] = "${CMAKE_VERSION_MAJOR}.${CMAKE_VERSION_MINOR}"
    loc['type'] = "string"


def change_schedule_compact(schema):
    loc = schema['properties']['Schedule:Compact']['patternProperties']['.*']['properties']['extensions']['items']['properties']['field']
    loc.pop('type')
    loc['anyOf'] = anyOf()


def change_special_cased_enums(schema):
    loc = schema['properties']['GroundHeatTransfer:Slab:Insulation']['patternProperties']['.*']['properties']['ivins_flag_is_there_vertical_insulation']
    loc.pop('type')
    newAnyOf = anyOf()
    newAnyOf[0]['enum'] = [int(i) for i in loc['enum'][:] if isInt(i)]
    newAnyOf[1]['enum'] = ['']
    loc['anyOf'] = newAnyOf
    loc.pop('enum')

    loc = schema['properties']['WindowMaterial:Screen']['patternProperties']['.*']['properties']['angle_of_resolution_for_screen_transmittance_output_map']
    loc.pop('type')
    newAnyOf = anyOf()
    newAnyOf[0]['enum'] = [int(i) for i in loc['enum'][:] if isInt(i)]
    newAnyOf[1]['enum'] = ['']
    loc['anyOf'] = newAnyOf
    loc.pop('enum')

    loc = schema['properties']['Refrigeration:System']['patternProperties']['.*']['properties']['number_of_compressor_stages']
    loc.pop('type')
    newAnyOf = anyOf()
    newAnyOf[0]['enum'] = [int(i) for i in loc['enum'][:] if isInt(i)]
    newAnyOf[1]['enum'] = ['']
    loc['anyOf'] = newAnyOf
    loc.pop('enum')

    loc = schema['properties']['ElectricLoadCenter:Transformer']['patternProperties']['.*']['properties']['phase']
    loc.pop('type')
    newAnyOf = anyOf()
    newAnyOf[0]['enum'] = [int(i) for i in loc['enum'][:] if isInt(i)]
    newAnyOf[1]['enum'] = ['']
    loc['anyOf'] = newAnyOf
    loc.pop('enum')

    loc = schema['properties']['Zone']['patternProperties']['.*']['properties']['zone_outside_convection_algorithm']['enum']
    loc.insert(0, '')

    loc = schema['properties']['Zone']['patternProperties']['.*']['properties']['zone_inside_convection_algorithm']['enum']
    loc.insert(0, '')


def change_utility_cost(schema):
    loc = schema['properties']['UtilityCost:Charge:Block']
    legacy_idd = loc['legacy_idd']['fields']
    loc = loc['patternProperties']['.*']['properties']
    for i in range(6, len(legacy_idd)):
        field = legacy_idd[i]
        loc[field].pop('type')
        loc[field]['anyOf'] = anyOf()

    loc = schema['properties']['UtilityCost:Ratchet']['patternProperties']['.*']['properties']
    loc['offset_value_or_variable_name'].pop('type')
    loc['offset_value_or_variable_name']['anyOf'] = anyOf()
    loc['multiplier_value_or_variable_name'].pop('type')
    loc['multiplier_value_or_variable_name']['anyOf'] = anyOf()

    loc = schema['properties']['UtilityCost:Charge:Simple']['patternProperties']['.*']['properties']
    loc['cost_per_unit_value_or_variable_name'].pop('type')
    loc['cost_per_unit_value_or_variable_name']['anyOf'] = anyOf()

    loc = schema['properties']['UtilityCost:Qualify']['patternProperties']['.*']['properties']
    loc['threshold_value_or_variable_name'].pop('type')
    loc['threshold_value_or_variable_name']['anyOf'] = anyOf()

    loc = schema['properties']['UtilityCost:Tariff']['patternProperties']['.*']['properties']
    loc['minimum_monthly_charge_or_variable_name'].pop('type')
    loc['minimum_monthly_charge_or_variable_name']['anyOf'] = anyOf()
    loc['monthly_charge_or_variable_name'].pop('type')
    loc['monthly_charge_or_variable_name']['anyOf'] = anyOf()


def add_explicit_extensible_bounds(schema):
    # Schedule:Year
    loc = schema['properties']['Schedule:Year']['patternProperties']['.*']['properties']['schedule_weeks']
    loc['minItems'] = 1
    loc['maxItems'] = 53

    # EnergyManagementSystem:Program
    loc = schema['properties']['EnergyManagementSystem:Program']['patternProperties']['.*']
    if 'required' in loc and 'lines' not in loc['required']:
        loc['required'].append('lines')
    if 'required' not in loc:
        loc['required'] = ['lines']
    loc['properties']['lines']['minItems'] = 1


def change_special_cased_name_fields(schema):
    original_name = schema['properties']['ZoneHVAC:TerminalUnit:VariableRefrigerantFlow']['legacy_idd']['field_info'].pop('zone_terminal_unit_name')
    schema['properties']['ZoneHVAC:TerminalUnit:VariableRefrigerantFlow']['legacy_idd']['field_info']['name'] = original_name
    schema['properties']['ZoneHVAC:TerminalUnit:VariableRefrigerantFlow']['legacy_idd']['fields'][0] = 'name'
    schema['properties']['ZoneHVAC:TerminalUnit:VariableRefrigerantFlow']['legacy_idd']['alphas']['fields'][0] = 'name'
    del schema['properties']['ZoneHVAC:TerminalUnit:VariableRefrigerantFlow']['patternProperties']['.*']['required'][0]
    schema['properties']['ZoneHVAC:TerminalUnit:VariableRefrigerantFlow']['name'] = \
         schema['properties']['ZoneHVAC:TerminalUnit:VariableRefrigerantFlow']['patternProperties']['.*']['properties'].pop('zone_terminal_unit_name')

    original_name = schema['properties']['AirConditioner:VariableRefrigerantFlow']['legacy_idd']['field_info'].pop('heat_pump_name')
    schema['properties']['AirConditioner:VariableRefrigerantFlow']['legacy_idd']['field_info']['name'] = original_name
    schema['properties']['AirConditioner:VariableRefrigerantFlow']['legacy_idd']['fields'][0] = 'name'
    schema['properties']['AirConditioner:VariableRefrigerantFlow']['legacy_idd']['alphas']['fields'][0] = 'name'
    del schema['properties']['AirConditioner:VariableRefrigerantFlow']['patternProperties']['.*']['required'][0]
    schema['properties']['AirConditioner:VariableRefrigerantFlow']['name'] = \
        schema['properties']['AirConditioner:VariableRefrigerantFlow']['patternProperties']['.*']['properties'].pop('heat_pump_name')


def change_extensions_name(schema):
    for key, value in extension_renaming.items():
        schema['properties'][key]['patternProperties']['.*']['properties'][value] = schema['properties'][key]['patternProperties']['.*']['properties']['extensions']
        loc = schema['properties'][key]['patternProperties']['.*']['properties']
        del loc['extensions']
        schema['properties'][key]['legacy_idd']['extension'] = value

    for key in remaining_objects:
        schema['properties'][key]['legacy_idd']['extension'] = 'extensions'

def change_89_release_issues(schema):
    curves = [
        'Curve:Linear', 'Curve:Quadratic', 'Curve:Cubic', 'Curve:Quartic', 'Curve:Exponent',
        'Curve:Bicubic', 'Curve:Biquadratic', 'Curve:QuadraticLinear', 'Curve:CubicLinear', 'Curve:Triquadratic',
        'Curve:ExponentialSkewNormal', 'Curve:Sigmoid', 'Curve:RectangularHyperbola1', 'Curve:RectangularHyperbola2', 'Curve:ExponentialDecay',
        'Curve:DoubleExponentialDecay', 'Curve:ChillerPartLoadWithLift', 'Table:OneIndependentVariable', 'Table:TwoIndependentVariables', 'Table:MultiVariableLookup'
    ]
    for curve in curves:
        schema['properties'][curve]['patternProperties']['.*']['properties']['output_unit_type']['enum'] = [
            '',
            'Capacity',
            'Dimensionless',
            'Power',
            'Pressure',
            'Temperature'
        ]
    schema['properties']['OtherEquipment']['patternProperties']['.*']['properties']['fuel_type']['enum'].append('Water')
    schema['properties']['WindowMaterial:Glazing:EquivalentLayer']['patternProperties']['.*']['properties']['optical_data_type']['enum'].append('SpectralAverage')
    schema['properties']['ZoneHVAC:CoolingPanel:RadiantConvective:Water']['patternProperties']['.*']['properties']['control_type']['enum'].append('ZoneTotalLoad')
    schema['properties']['ZoneHVAC:CoolingPanel:RadiantConvective:Water']['patternProperties']['.*']['properties']['control_type']['enum'].append('ZoneConvectiveLoad')

    schema['properties']['FuelFactors']['patternProperties']['.*']['properties']['existing_fuel_resource_name'].pop('enum')
    schema['properties']['LifeCycleCost:UsePriceEscalation']['patternProperties']['.*']['properties']['resource'].pop('enum')
    schema['properties']['AirConditioner:VariableRefrigerantFlow']['patternProperties']['.*']['properties']['fuel_type'].pop('enum')
    schema['properties']['GlobalGeometryRules']['patternProperties']['.*']['properties']['starting_vertex_position'].pop('enum')
    schema['properties']['GlobalGeometryRules']['patternProperties']['.*']['properties']['vertex_entry_direction'].pop('enum')
    schema['properties']['GlobalGeometryRules']['patternProperties']['.*']['properties']['coordinate_system'].pop('enum')
    schema['properties']['WaterHeater:Mixed']['patternProperties']['.*']['properties']['heater_fuel_type'].pop('enum')
    schema['properties']['Boiler:HotWater']['patternProperties']['.*']['properties']['fuel_type'].pop('enum')
    schema['properties']['Schedule:Week:Compact']['patternProperties']['.*']['properties']['data']['items']['properties']['daytype_list'].pop('enum')
    schema['properties']['Output:Table:SummaryReports']['patternProperties']['.*']['properties']['reports']['items']['properties']['report_name'].pop('enum')


