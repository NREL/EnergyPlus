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
    'WindowShadingControl': 'fenestration_surfaces',
    'PlantEquipmentList': 'equipment',
    'CondenserEquipmentList': 'equipment'
}
remaining_objects = [
    'Site:SpectrumData',
    'Schedule:Day:List',
    'MaterialProperty:GlazingSpectralData'
]


def get_schema_object(schema, object_key):
    if '.*' in schema['properties'][object_key]['patternProperties']:
        return schema['properties'][object_key]['patternProperties']['.*']
    if R'^.*\S.*$' in schema['properties'][object_key]['patternProperties']:
        return schema['properties'][object_key]['patternProperties'][R'^.*\S.*$']
    raise KeyError(R'The patternProperties value is not a valid choice (".*", "^.*\S.*$")')


def change_version(schema):
    schema["epJSON_schema_version"] = "${CMAKE_VERSION_MAJOR}.${CMAKE_VERSION_MINOR}.${CMAKE_VERSION_PATCH}"
    schema["epJSON_schema_build"] = "${CMAKE_VERSION_BUILD}"
    loc = get_schema_object(schema, 'Version')['properties']['version_identifier']
    loc['default'] = "${CMAKE_VERSION_MAJOR}.${CMAKE_VERSION_MINOR}"
    loc['type'] = "string"


def change_schedule_compact(schema):
    loc = get_schema_object(schema, 'Schedule:Compact')['properties']['extensions']['items']['properties']['field']
    loc.pop('type')
    loc['anyOf'] = anyOf()


def change_special_cased_enums(schema):
    loc = get_schema_object(schema, 'GroundHeatTransfer:Slab:Insulation')['properties']['ivins_flag_is_there_vertical_insulation']
    loc.pop('type')
    newAnyOf = anyOf()
    newAnyOf[0]['enum'] = [int(i) for i in loc['enum'][:] if isInt(i)]
    newAnyOf[1]['enum'] = ['']
    loc['anyOf'] = newAnyOf
    loc.pop('enum')

    loc = get_schema_object(schema, 'WindowMaterial:Screen')['properties']['angle_of_resolution_for_screen_transmittance_output_map']
    loc.pop('type')
    newAnyOf = anyOf()
    newAnyOf[0]['enum'] = [int(i) for i in loc['enum'][:] if isInt(i)]
    newAnyOf[1]['enum'] = ['']
    loc['anyOf'] = newAnyOf
    loc.pop('enum')

    loc = get_schema_object(schema, 'Refrigeration:System')['properties']['number_of_compressor_stages']
    loc.pop('type')
    newAnyOf = anyOf()
    newAnyOf[0]['enum'] = [int(i) for i in loc['enum'][:] if isInt(i)]
    newAnyOf[1]['enum'] = ['']
    loc['anyOf'] = newAnyOf
    loc.pop('enum')

    loc = get_schema_object(schema, 'ElectricLoadCenter:Transformer')['properties']['phase']
    loc.pop('type')
    newAnyOf = anyOf()
    newAnyOf[0]['enum'] = [int(i) for i in loc['enum'][:] if isInt(i)]
    newAnyOf[1]['enum'] = ['']
    loc['anyOf'] = newAnyOf
    loc.pop('enum')

    loc = get_schema_object(schema, 'Zone')['properties']['zone_outside_convection_algorithm']['enum']
    loc.insert(0, '')

    loc = get_schema_object(schema, 'Zone')['properties']['zone_inside_convection_algorithm']['enum']
    loc.insert(0, '')


def change_utility_cost(schema):
    legacy_idd = schema['properties']['UtilityCost:Charge:Block']['legacy_idd']['fields']
    loc = get_schema_object(schema, 'UtilityCost:Charge:Block')['properties']
    for i in range(6, len(legacy_idd)):
        field = legacy_idd[i]
        loc[field].pop('type')
        loc[field]['anyOf'] = anyOf()

    loc = get_schema_object(schema, 'UtilityCost:Ratchet')['properties']
    loc['offset_value_or_variable_name'].pop('type')
    loc['offset_value_or_variable_name']['anyOf'] = anyOf()
    loc['multiplier_value_or_variable_name'].pop('type')
    loc['multiplier_value_or_variable_name']['anyOf'] = anyOf()

    loc = get_schema_object(schema, 'UtilityCost:Charge:Simple')['properties']
    loc['cost_per_unit_value_or_variable_name'].pop('type')
    loc['cost_per_unit_value_or_variable_name']['anyOf'] = anyOf()

    loc = get_schema_object(schema, 'UtilityCost:Qualify')['properties']
    loc['threshold_value_or_variable_name'].pop('type')
    loc['threshold_value_or_variable_name']['anyOf'] = anyOf()

    loc = get_schema_object(schema, 'UtilityCost:Tariff')['properties']
    loc['minimum_monthly_charge_or_variable_name'].pop('type')
    loc['minimum_monthly_charge_or_variable_name']['anyOf'] = anyOf()
    loc['monthly_charge_or_variable_name'].pop('type')
    loc['monthly_charge_or_variable_name']['anyOf'] = anyOf()


def add_explicit_extensible_bounds(schema):
    # Schedule:Year
    loc = get_schema_object(schema, 'Schedule:Year')['properties']['schedule_weeks']
    loc['minItems'] = 1
    loc['maxItems'] = 53

    # EnergyManagementSystem:Program
    loc = get_schema_object(schema, 'EnergyManagementSystem:Program')
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
    del get_schema_object(schema, 'ZoneHVAC:TerminalUnit:VariableRefrigerantFlow')['required'][0]
    schema['properties']['ZoneHVAC:TerminalUnit:VariableRefrigerantFlow']['name'] = \
        get_schema_object(schema, 'ZoneHVAC:TerminalUnit:VariableRefrigerantFlow')['properties'].pop('zone_terminal_unit_name')

    original_name = schema['properties']['AirConditioner:VariableRefrigerantFlow']['legacy_idd']['field_info'].pop('heat_pump_name')
    schema['properties']['AirConditioner:VariableRefrigerantFlow']['legacy_idd']['field_info']['name'] = original_name
    schema['properties']['AirConditioner:VariableRefrigerantFlow']['legacy_idd']['fields'][0] = 'name'
    schema['properties']['AirConditioner:VariableRefrigerantFlow']['legacy_idd']['alphas']['fields'][0] = 'name'
    del get_schema_object(schema, 'AirConditioner:VariableRefrigerantFlow')['required'][0]
    schema['properties']['AirConditioner:VariableRefrigerantFlow']['name'] = \
        get_schema_object(schema, 'AirConditioner:VariableRefrigerantFlow')['properties'].pop('heat_pump_name')


def change_extensions_name(schema):
    for key, value in extension_renaming.items():
        get_schema_object(schema, key)['properties'][value] = get_schema_object(schema, key)['properties']['extensions']
        loc = get_schema_object(schema, key)['properties']
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
        get_schema_object(schema, curve)['properties']['output_unit_type']['enum'] = [
            '',
            'Capacity',
            'Dimensionless',
            'Power',
            'Pressure',
            'Temperature'
        ]
    get_schema_object(schema, 'OtherEquipment')['properties']['fuel_type']['enum'].append('Water')
    get_schema_object(schema, 'WindowMaterial:Glazing:EquivalentLayer')['properties']['optical_data_type']['enum'].append('SpectralAverage')
    get_schema_object(schema, 'ZoneHVAC:CoolingPanel:RadiantConvective:Water')['properties']['control_type']['enum'].append('ZoneTotalLoad')
    get_schema_object(schema, 'ZoneHVAC:CoolingPanel:RadiantConvective:Water')['properties']['control_type']['enum'].append('ZoneConvectiveLoad')

    get_schema_object(schema, 'FuelFactors')['properties']['existing_fuel_resource_name'].pop('enum')
    get_schema_object(schema, 'LifeCycleCost:UsePriceEscalation')['properties']['resource'].pop('enum')
    get_schema_object(schema, 'AirConditioner:VariableRefrigerantFlow')['properties']['fuel_type'].pop('enum')
    get_schema_object(schema, 'GlobalGeometryRules')['properties']['starting_vertex_position'].pop('enum')
    get_schema_object(schema, 'GlobalGeometryRules')['properties']['vertex_entry_direction'].pop('enum')
    get_schema_object(schema, 'GlobalGeometryRules')['properties']['coordinate_system'].pop('enum')
    get_schema_object(schema, 'WaterHeater:Mixed')['properties']['heater_fuel_type'].pop('enum')
    get_schema_object(schema, 'Boiler:HotWater')['properties']['fuel_type'].pop('enum')
    get_schema_object(schema, 'Schedule:Week:Compact')['properties']['data']['items']['properties']['daytype_list'].pop('enum')
    get_schema_object(schema, 'Output:Table:SummaryReports')['properties']['reports']['items']['properties']['report_name'].pop('enum')


