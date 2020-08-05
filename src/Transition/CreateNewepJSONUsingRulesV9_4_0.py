import json
import re
import sys
import os


def process_fenestrationsurface_detailed(epJSON: dict) -> bool:
    """
    This object now has extensible vertices

    Args:
    -----
    epJSON (dict): loaded json file

    Returns:
    --------
    has_changes (bool): whether changes have been made or not
    """

    has_changes = False

    re_vertex = re.compile(r'vertex_(\d+)_([xyz])_coordinate')
    if 'FenestrationSurface:Detailed' in epJSON:
        loc = epJSON['FenestrationSurface:Detailed']
        for obj_name in loc.keys():
            loc[obj_name]['vertices'] = []
            vertices = {}
            topop = []
            for field in loc[obj_name].keys():
                m = re_vertex.match(field)
                if m:
                    group = int(m.groups()[0])
                    coordinate = "vertex_{}_coordinate".format(m.groups()[1])
                    if group not in vertices:
                        vertices[group] = {}
                    vertices[group][coordinate] = loc[obj_name][field]
                    topop.append(field)
                    has_changes = True
            for k, v in vertices.items():
                missing_keys = set(['vertex_x_coordinate',
                                    'vertex_y_coordinate',
                                    'vertex_z_coordinate']) - set(v.keys())
                if missing_keys:
                    raise ValueError("{o} has invalid vertices, missing {k}"
                                     .format(o=obj_name, k=missing_keys))
                loc[obj_name]['vertices'].append(v)
            for field in topop:
                loc[obj_name].pop(field)

    return has_changes

def process_people_detailed(epJSON: dict) -> bool:
    """
    This object now has extensible vertices

    Args:
    -----
    epJSON (dict): loaded json file

    Returns:
    --------
    has_changes (bool): whether changes have been made or not
    """

    has_changes = False

    re_comfort = re.compile(r'thermal_comfort_model_(\d+)_type')
    if 'People' in epJSON:
        loc = epJSON['People']
        for obj_name in loc.keys():
            loc[obj_name]['thermal_comfort_models'] = []
            topop = []
            for field in loc[obj_name].keys():
                m = re_comfort.match(field)
                if m:
                    (loc[obj_name]['thermal_comfort_models']
                     .append(
                         {"thermal_comfort_model_type": loc[obj_name][field]}
                     ))
                    has_changes = True
            for field in topop:
                loc[obj_name].pop(field)

    return has_changes


if __name__ == '__main__':
    has_changes = False

    if len(sys.argv) < 2:
        print("  Error: Specify the epJSON file name to convert.\n")
        print("   e.g.,")
        print("     'python CreateNewepJSONUsingRulesV9_4_0.py myfile.epJSON'")
        exit()

    target_file = sys.argv[1]

    if not os.path.exists(target_file):
        print(f"  Error: Could not find input file: {target_file}.")
        exit()

    with open(target_file, 'r') as json_file:
        epJSON = json.load(json_file)

    # Process object changes
    fenestration_changes = process_fenestrationsurface_detailed(epJSON)
    has_changes = has_changes or fenestration_changes

    people_changes = process_people_detailed(epJSON)
    has_changes = has_changes or people_changes

    if has_changes:

        fname_no_ext, _ = os.path.splitext(os.path.basename(target_file))
        output_path = "{}-New.epJSON".format(fname_no_ext)
        output_path = os.path.abspath(output_path)
        print(f"Saving as {output_path}")

        with open(output_path, 'w') as f:
            f.write(json.dumps(epJSON, indent=4))
