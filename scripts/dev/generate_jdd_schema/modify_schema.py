anyOf = [
    {
        "type": "number",
    },
    {
        "type": "string"
    }
]


def change_version(schema):
    schema["jdd_version"] = "${CMAKE_VERSION_MAJOR}.${CMAKE_VERSION_MINOR}.${CMAKE_VERSION_PATCH}"
    schema["jdd_build"] = "${CMAKE_VERSION_BUILD}"
    loc = schema['properties']['Version']['properties']['version_identifier']
    loc['default'] = "${CMAKE_VERSION_MAJOR}.${CMAKE_VERSION_MINOR}"
    loc['type'] = "string"


def change_schedule_compact(schema):
    loc = schema['properties']['Schedule:Compact']['patternProperties']['.*']['properties']['extensions']['items']['properties']['field']
    loc.pop('type')
    loc['anyOf'] = anyOf


def change_special_cased_enums(schema):
    loc = schema['properties']['GroundHeatTransfer:Slab:Insulation']['patternProperties']['.*']['properties']['ivins_flag_is_there_vertical_insulation']
    loc['type'] = "number"
    loc = loc['enum']
    for i in range(len(loc)):
        loc[i] = int(loc[i])

    loc = schema['properties']['WindowMaterial:Screen']['patternProperties']['.*']['properties']['angle_of_resolution_for_screen_transmittance_output_map']
    loc['type'] = "number"
    loc = loc['enum']
    for i in range(len(loc)):
        loc[i] = int(loc[i])

    loc = schema['properties']['Refrigeration:System']['patternProperties']['.*']['properties']['number_of_compressor_stages']
    loc['type'] = "number"
    loc = loc['enum']
    for i in range(len(loc)):
        loc[i] = int(loc[i])

    loc = schema['properties']['ElectricLoadCenter:Transformer']['patternProperties']['.*']['properties']['phase']
    loc['type'] = "number"
    loc = loc['enum']
    for i in range(len(loc)):
        loc[i] = int(loc[i])


def change_utility_cost(schema):
    loc = schema['properties']['UtilityCost:Charge:Block']
    legacy_idd = loc['legacy_idd']['fields']
    loc = loc['patternProperties']['.*']['properties']
    for i in range(6, len(legacy_idd)):
        field = legacy_idd[i]
        loc[field].pop('type')
        loc[field]['anyOf'] = anyOf

    loc = schema['properties']['UtilityCost:Ratchet']['patternProperties']['.*']['properties']
    loc['offset_value_or_variable_name'].pop('type')
    loc['offset_value_or_variable_name']['anyOf'] = anyOf
    loc['multiplier_value_or_variable_name'].pop('type')
    loc['multiplier_value_or_variable_name']['anyOf'] = anyOf

    loc = schema['properties']['UtilityCost:Charge:Simple']['patternProperties']['.*']['properties']
    loc['cost_per_unit_value_or_variable_name'].pop('type')
    loc['cost_per_unit_value_or_variable_name']['anyOf'] = anyOf

    loc = schema['properties']['UtilityCost:Qualify']['patternProperties']['.*']['properties']
    loc['threshold_value_or_variable_name'].pop('type')
    loc['threshold_value_or_variable_name']['anyOf'] = anyOf

    loc = schema['properties']['UtilityCost:Tariff']['patternProperties']['.*']['properties']
    loc['minimum_monthly_charge_or_variable_name'].pop('type')
    loc['minimum_monthly_charge_or_variable_name']['anyOf'] = anyOf
    loc['monthly_charge_or_variable_name'].pop('type')
    loc['monthly_charge_or_variable_name']['anyOf'] = anyOf


def change_special_cased_name_fields(schema):
    schema['properties']['ZoneHVAC:TerminalUnit:VariableRefrigerantFlow']['legacy_idd']['fields'][0] = 'name'
    schema['properties']['ZoneHVAC:TerminalUnit:VariableRefrigerantFlow']['legacy_idd']['alphas']['fields'][0] = 'name'
    del schema['properties']['ZoneHVAC:TerminalUnit:VariableRefrigerantFlow']['patternProperties']['.*']['required'][0]
    schema['properties']['ZoneHVAC:TerminalUnit:VariableRefrigerantFlow']['name'] = \
         schema['properties']['ZoneHVAC:TerminalUnit:VariableRefrigerantFlow']['patternProperties']['.*']['properties'].pop('zone_terminal_unit_name')

    schema['properties']['AirConditioner:VariableRefrigerantFlow']['legacy_idd']['fields'][0] = 'name'
    schema['properties']['AirConditioner:VariableRefrigerantFlow']['legacy_idd']['alphas']['fields'][0] = 'name'
    del schema['properties']['AirConditioner:VariableRefrigerantFlow']['patternProperties']['.*']['required'][0]
    schema['properties']['AirConditioner:VariableRefrigerantFlow']['name'] = \
        schema['properties']['AirConditioner:VariableRefrigerantFlow']['patternProperties']['.*']['properties'].pop('heat_pump_name')
