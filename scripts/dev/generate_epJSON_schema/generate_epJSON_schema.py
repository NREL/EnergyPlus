import idd_parser
import modify_schema
import json
import sys


source_dir_path = sys.argv[1]
data = idd_parser.Data()
with open(source_dir_path + '/idd/Energy+.idd.in', 'r') as f:
    data.file = f.read()

idd_parser.parse_idd(data)
modify_schema.change_version(data.schema)
modify_schema.change_schedule_compact(data.schema)
modify_schema.change_utility_cost(data.schema)
modify_schema.change_special_cased_enums(data.schema)
modify_schema.change_special_cased_name_fields(data.schema)
modify_schema.change_extensions_name(data.schema)
modify_schema.change_89_release_issues(data.schema)
modify_schema.add_explicit_extensible_bounds(data.schema)

with open(source_dir_path + '/idd/Energy+.schema.epJSON.in', 'w') as f2:
    f2.write(json.dumps(data.schema, indent=4))
