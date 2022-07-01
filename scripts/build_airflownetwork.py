import json
import argparse
import math

if __name__ == '__main__':
    
    parser = argparse.ArgumentParser(description='.')
    parser.add_argument('epJSON', metavar='EPJSON')
    parser.add_argument('-o', '--output', dest='output_file', action='store',
                        default='afn.epJSON', help='file to store output in')
    parser.add_argument('-v', '--verbose', dest='verbose', action='store_true',
                        help='operate verbosely')
    args = parser.parse_args()

    fp = open(args.epJSON, 'r')
    model = json.load(fp)
    fp.close()

    window_objects = ['Window', 'Window:Interzone', 'FenestrationSurface:Detailed']
    door_objects = ['Door', 'GlazedDoor', 'Door:Interzone', 'GlazedDoor:Interzone']

    simple_window_objects = ['Window',
                             'Window:Interzone']
    simple_door_objects = ['Door',
                           'GlazedDoor',
                           'Door:Interzone',
                           'GlazedDoor:Interzone']
    fenestration_objects = ['FenestrationSurface:Detailed']
    simple_wall_objects = ['Wall:Exterior',
                           'Wall:Interzone',
                           #'Wall:Underground',
                           'Wall:Adiabatic']
    simple_envelope_objects = ['Wall:Exterior']
    simple_partition_objects = ['Wall:Interzone',
                                'Wall:Adiabatic']
    simple_roofceiling_objects = ['Roof',
                                  'Ceiling:Adiabatic',
                                  'Ceiling:Interzone']
    simple_roof_objects = ['Roof']
    simple_ceiling_objects = ['Ceiling:Adiabatic',
                              'Ceiling:Interzone']
    simple_floor_objects = ['Floor:Adiabatic',
                            #'Floor:GroundContact',
                            'Floor:Interzone']   
    detailed_wall_objects = ['BuildingSurface:Detailed',
                             'Wall:Detailed']
    detailed_roofceiling_objects = ['RoofCeiling:Detailed']
    detailed_floor_objects = ['Floor:Detailed']

    surface_handler = {'Window': Surface.from_envelope,
                       'Window:Interzone': Surface.from_partition,
                       'Door': Surface.from_envelope,
                       'GlazedDoor': Surface.from_envelope,
                       'Door:Interzone': Surface.from_partition,
                       'GlazedDoor:Interzone': Surface.from_partition,
                       'FenestrationSurface:Detailed': Surface.from_detailed_fenestration}

    windows = {}
    for el in window_objects:
        number = 0
        try:
            these_windows = model[el]
            number = len(these_windows)
            windows[el] = these_windows
        except:
            pass
        if args.verbose:
            print("Found %d '%s' window-type objects" % (number, el))

    doors = {}
    for el in door_objects:
        number = 0
        try:
            these_doors = model[el]
            number = len(these_doors)
            doors[el] = these_doors
        except:
            pass
        if args.verbose:
            print("Found %d '%s' door-type objects" % (number, el))

    n_zones = len(model['Zone'])
    interzonal_area = []
    keys = list(model['Zone'].keys())
    zone_lookup = {}
    for i in range(n_zones):
        interzonal_area.append(n_zones*[0.0])
        zone_lookup[keys[i]] = i

    # Handle opaque surfaces
    surfaces = []

    for type in simple_envelope_objects:
        for name,obj in model.get(type, {}).items():
            surfaces.append(Surface.from_envelope(model, type, name, obj, None))

    for type in simple_partition_objects:
        objects = model.get(type, {})
        keys = list(objects.keys())
        for name in keys:
            obj = model[type][name]
            result = Surface.from_detailed(model, type, name, obj, None)
            surfaces.append(result)
            if result.other is not None:
                keys.remove(result.other.surface_name)

    for type in detailed_wall_objects:
        objects = model.get(type, {})
        keys = list(objects.keys())
        for name in keys:
            obj = model[type][name]
            result = Surface.from_detailed(model, type, name, obj, None)
            if result is not None:
                surfaces.append(result)
                if result.other is not None:
                    keys.remove(result.other.surface_name)

    # Split into internal/external surfaces
    envelope_leakage_name = 'Envelope Leakage'
    interzone_leakage_name = 'Interzone Leakage'
    envelope_surfaces = []
    interzone_surfaces = []
    for surf in surfaces:
        if surf.other is not None:
            surf.component_name = interzone_leakage_name
            interzone_surfaces.append(surf)
        else:
            surf.component_name = envelope_leakage_name
            envelope_surfaces.append(surf)

    # Handle window and door surfaces
    if 'FenestrationSurface:Detailed' in windows:
        for obj in windows['FenestrationSurface:Detailed'].values():
            repair_fenestration_surface_detailed(obj)
    envelope_opening_name = 'Envelope Opening'
    interzone_opening_name = 'Interzone Opening'
    interzone_doors_and_windows = []
    envelope_doors_and_windows = []
    allowed_parents = envelope_surfaces + interzone_surfaces
    for type, objects in windows.items():
        for name, obj in objects.items():
            result = surface_handler[type](model, type, name, obj, allowed_parents)
            if result is not None:
                if result.other is not None:
                    result.component_name = interzone_opening_name
                    interzone_doors_and_windows.append(result)
                else:
                    result.component_name = envelope_opening_name
                    envelope_doors_and_windows.append(result)

    # Account for surface areas of openings in parent surfaces
    for opening_surface in interzone_doors_and_windows + envelope_doors_and_windows:
        if opening_surface.parent is not None:
            new_area = opening_surface.parent.area - opening_surface.area
            assert new_area > 0.0
            opening_surface.parent.area = new_area
        else:
            # Is this an error? Need to verify
            pass


    # Sum up interzone areas
    for surf in interzone_surfaces:
        i = zone_lookup[surf.zone]
        j = zone_lookup[surf.other.zone]
        #print(surf.name,i,j, surf.zone, surf.other.zone)
        interzonal_area[i][j] += surf.area
        interzonal_area[j][i] += surf.area

    max_interzonal_area = 0.0
    for i in range(n_zones):
        for j in range(i+1, n_zones):
            max_interzonal_area = max(max_interzonal_area, interzonal_area[i][j])

    if verbose:
        print(interzonal_area)
        print(max_interzonal_area)

    max_envelope_area = 0.0
    for surf in envelope_surfaces:
        max_envelope_area = max(max_envelope_area, surf.area)

    # Set up the components
    coefficient = 0.01
    envelope_leakage = Crack(name=envelope_leakage_name, coef=coefficient * max_envelope_area)
    interzone_leakage = Crack(name=interzone_leakage_name, coef=2*coefficient * max_interzonal_area)
    envelope_opening = SimpleOpening(name=envelope_opening_name, coef=coefficient)
    interzone_opening = SimpleOpening(name=envelope_opening_name, coef=coefficient)

    # Set up the factors
    for surf in envelope_surfaces:
        surf.window_door_opening_factor_or_crack_factor = surf.area/max_envelope_area

    # Set up the factors
    for surf in interzone_surfaces:
        surf.window_door_opening_factor_or_crack_factor = surf.area/max_interzonal_area

    # Add to the model
    model['AirflowNetwork:MultiZone:Surface'] = {}
    for obj in envelope_surfaces + interzone_surfaces + interzone_doors_and_windows + envelope_doors_and_windows:
        model['AirflowNetwork:MultiZone:Surface'][obj.name] = obj.to_json()

    model['AirflowNetwork:MultiZone:Surface:Crack'] = {
        envelope_leakage.name: envelope_leakage.to_json(),
        interzone_leakage.name: interzone_leakage.to_json()
        }

    model['AirflowNetwork:MultiZone:Component:SimpleOpening'] = {
        envelope_opening.name: envelope_opening.to_json(),
        interzone_opening.name: interzone_opening.to_json()
        }

    model['AirflowNetwork:MultiZone:Zone'] = {}
    for name in model['Zone'].keys():
        model['AirflowNetwork:MultiZone:Zone'][name+'_AFN'] = {'zone_name': 'name'}

    fp = open(args.output_file, 'w')
    json.dump(model, fp)
    fp.close()
    
