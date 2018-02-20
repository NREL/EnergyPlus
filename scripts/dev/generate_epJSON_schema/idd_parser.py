import re

# Preston Shires

# tokens
TOKEN_NONE = 0
TOKEN_END = 1
TOKEN_EXCLAMATION = 2
TOKEN_COMMA = 3
TOKEN_SEMICOLON = 4
TOKEN_STRING = 5
TOKEN_NUMBER = 6
TOKEN_FIELD = 7
TOKEN_A = 8
TOKEN_N = 9
TOKEN_GROUP = 100

# object level tokens
TOKEN_MEMO = 10
TOKEN_MIN_FIELDS = 11
TOKEN_EXTENSIBLE = 12
TOKEN_FORMAT = 13
TOKEN_UNIQUE_OBJ = 14
TOKEN_REQUIRED_OBJ = 15
TOKEN_REFERENCE_CLASS_NAME = 17
TOKEN_OBSOLETE = 18

# field level tokens
TOKEN_KEY = 19
TOKEN_NOTE = 20
TOKEN_OBJ_LIST = 21
TOKEN_DEFAULT = 22
TOKEN_AUTOCALCULATABLE = 23
TOKEN_AUTOSIZABLE = 24
TOKEN_MIN = 25
TOKEN_MIN_EXCLUSIVE = 26
TOKEN_MAX = 27
TOKEN_MAX_EXCLUSIVE = 28
TOKEN_BEGIN_EXTENSIBLE = 29
TOKEN_UNITS = 30
TOKEN_TYPE = 31
TOKEN_REQUIRED_FIELD = 32
TOKEN_REFERENCE = 33
TOKEN_IP_UNITS = 34
TOKEN_UNITS_BASED_ON_FIELD = 35
TOKEN_DEPRECATED = 36
TOKEN_EXTERNAL_LIST = 37
TOKEN_RETAIN_CASE = 38

# above object level group string
GROUP_STR = 'group'

# object level strings
EXTENSIBLE_STR = 'extensible:'
MEMO_STR = 'memo'
MIN_FIELDS_STR = 'min-fields'
FORMAT_STR = 'format'
FIELD_STR = 'field'
UNIQUE_OBJ_STR = 'unique-object'
REQUIRED_OBJ_STR = 'required-object'
OBSOLETE_STR = 'obsolete'

# field-level strings
REQUIRED_FIELD_STR = 'required-field'
TYPE_STR = 'type'
REFERENCE_STR = 'reference'
REFERENCE_CLASS_NAME_STR = '-class-name'
KEY_STR = 'key'
NOTE_STR = 'note'
DEFAULT_STR = 'default'
AUTOCALCULATABLE_STR = 'autocalculatable'
AUTOSIZABLE_STR = 'autosizable'
MINIMUM_STR = 'minimum'
MAXIMUM_STR = 'maximum'
BEGIN_EXTENSIBLE_STR = 'begin-extensible'
UNITS_STR = 'units'
IP_UNITS_STR = 'ip-units'
BASED_ON_FIELD_STR = 'BasedOnField'
DEPRECATED_STR = 'deprecated'
RETAIN_CASE_STR = 'retaincase'

# field-level type strings
ALPHA_STR = 'alpha'
CHOICE_STR = 'choice'
OBJECT_LIST_STR = 'object-list'
EXTERNAL_LIST_STR = 'external-list'
REAL_STR = 'real'
INTEGER_STR = 'integer'
NODE_STR = 'node'


class Data:
    index = 0
    file = None
    file_size = 0
    schema = None


def parse_idd(data):
    root = {'$schema': "http://json-schema.org/draft-04/schema#", 'properties': {}}
    data.file_size = len(data.file)

    while data.index < data.file_size:
        token = look_ahead(data)
        if token == TOKEN_END:
            break
        elif token == TOKEN_NONE:
            print("TOKEN_NONE returned in parse_idd at index ", data.index, data.file[data.index - 50: data.index])
            break
        elif token == TOKEN_EXCLAMATION:
            eat_comment(data)
        elif token == TOKEN_GROUP:
            next_token(data)
            eat_comment(data)
        else:
            obj_name = parse_string(data)
            if obj_name is None or obj_name == "":
                return root
            obj_data = parse_obj(data)
            root['properties'][obj_name] = {}
            root['properties'][obj_name]['patternProperties'] = {}
            root['properties'][obj_name]['patternProperties']['.*'] = obj_data
            if 'name' in obj_data:
                root['properties'][obj_name]['name'] = obj_data.pop('name')

            root['properties'][obj_name]['legacy_idd'] = obj_data.pop('legacy_idd')
            root['properties'][obj_name]['type'] = 'object'
            if 'is_required_obj' in obj_data:
                obj_data.pop('is_required_obj')
                root['properties'][obj_name]['minProperties'] = 1
                if 'required' not in root:
                    root['required'] = []
                root['required'].append(obj_name)
            if 'is_unique_obj' in obj_data:
                obj_data.pop('is_unique_obj')
                root['properties'][obj_name]['maxProperties'] = 1
            if 'memo' in obj_data:
                root['properties'][obj_name]['memo'] = obj_data.pop('memo')
            if 'min_fields' in obj_data:
                root['properties'][obj_name]['min_fields'] = obj_data.pop('min_fields')
            if 'extensible_size' in obj_data:
                root['properties'][obj_name]['extensible_size'] = obj_data.pop('extensible_size')
            if 'format' in obj_data:
                root['properties'][obj_name]['format'] = obj_data.pop('format')

            for name, field in obj_data['properties'].items():
                if 'unitsBasedOnField' in field:
                    based_on_field = field['unitsBasedOnField']
                    index = int(based_on_field[1]) - 1
                    if based_on_field[0].lower() == 'a':
                        field['unitsBasedOnField'] = root['properties'][obj_name]['legacy_idd']['alphas']['fields'][index]
                    else:
                        field['unitsBasedOnField'] = root['properties'][obj_name]['legacy_idd']['numerics']['fields'][index]

    data.schema = root


def parse_obj(data):
    root = {'type': 'object', 'properties': {}, 'legacy_idd': {'fields': [], 'alphas': { 'fields': [] }, 'numerics': { 'fields': [] } } }
    extensible_count = 0
    duplicate_field_count = 0

    while True:
        token = look_ahead(data)
        if token == TOKEN_NONE:
            raise RuntimeError("TOKEN_NONE returned")

        elif token == TOKEN_END:
            return root

        elif token == TOKEN_MEMO:
            next_token(data)
            memo = parse_line(data)
            if 'memo' in root:
                root['memo'] += " " + memo
            else:
                root['memo'] = memo

        elif token == TOKEN_EXCLAMATION:
            eat_comment(data)

        elif token == TOKEN_REQUIRED_OBJ:
            next_token(data)
            root['is_required_obj'] = True

        elif token == TOKEN_UNIQUE_OBJ:
            next_token(data)
            root['is_unique_obj'] = True

        elif token == TOKEN_EXTENSIBLE:
            next_token(data)
            if look_ahead(data) != TOKEN_NUMBER:
                raise RuntimeError("expected number after extensible:")
            num = parse_number(data)
            if num is None:
                raise RuntimeError("parse number returned None")
            root['extensible_size'] = num
            eat_comment(data)  # eat useless extensible string

        elif token == TOKEN_MIN_FIELDS:
            next_token(data)
            if look_ahead(data) != TOKEN_NUMBER:
                raise RuntimeError("expected number after /min-fields")
            num = parse_number(data)
            if num is None:
                raise RuntimeError("parse number returned None")
            if 'min_fields' in root:
                raise KeyError("min_fields already exists, must define only one per object")
            root['min_fields'] = num

        elif token == TOKEN_FORMAT:
            next_token(data)
            if look_ahead(data) != TOKEN_STRING:
                raise RuntimeError("expected string after /format")
            if 'format' in root:
                raise RuntimeError("cannot have duplicate formats")
            root['format'] = parse_line(data)

        elif token == TOKEN_OBSOLETE:
            next_token(data)
            if look_ahead(data) != TOKEN_STRING:
                raise RuntimeError("expected string after /obsolete")
            if 'obsolete' in root:
                raise RuntimeError("cannot have duplicate obsolete")
            root['obsolete'] = parse_line(data)

        elif token == TOKEN_A or token == TOKEN_N:  # past all object level comments, should be field now
            token_a_or_n = token
            eat_whitespace(data)
            field_number = data.file[data.index]
            data.index += 1
            field_number += str(int(parse_number(data)))
            comma_or_semicolon = look_ahead(data)
            if comma_or_semicolon != TOKEN_COMMA and comma_or_semicolon != TOKEN_SEMICOLON:
                raise RuntimeError("No comma or semicolon after A field")
            next_token(data)
            token = look_ahead(data)

            if token == TOKEN_A or token == TOKEN_N:
                next_token(data)
                comma_or_semicolon = next_token(data)
                token = look_ahead(data)
                if comma_or_semicolon == TOKEN_SEMICOLON:
                    eat_comment(data)
                    return root
                if token != TOKEN_A and token != TOKEN_N:
                    eat_comment(data)
                continue

            if token == TOKEN_NOTE:
                eat_comment(data)
                if comma_or_semicolon == TOKEN_SEMICOLON:
                    return root
                continue

            if token != TOKEN_FIELD:
                raise RuntimeError("expected /field after , or ;")
            next_token(data)
            field_name = parse_line(data)
            field_name = field_name.lower()
            field_data = parse_field(data, token_a_or_n)

            if 'begin-extensible' in field_data:
                field_data.pop('begin-extensible')
                field_name = re.sub('[ ]+[0-9]+', '', field_name)
                field_name = re.sub('[^0-9a-zA-Z]+', '_', field_name)
                append_field_number(root, field_number, field_name, True)
                root['properties']['extensions'] = {'type': 'array', 'items': {'properties': {}, 'type': 'object'}}
                root['legacy_idd']['extensibles'] = [field_name]
                parse_extensibles(root['properties']['extensions']['items'], field_data, field_name)
                extensible_count += 1
                continue
            elif 'extensible_size' in root and extensible_count >= root['extensible_size'] \
                    and comma_or_semicolon != TOKEN_SEMICOLON:
                continue
            elif extensible_count > 0:
                if extensible_count < root['extensible_size']:
                    field_name = re.sub('[ ]+[0-9]+', '', field_name)
                    field_name = re.sub('[^0-9a-zA-Z]+', '_', field_name)
                    append_field_number(root, field_number, field_name, True)
                    root['legacy_idd']['extensibles'].append(field_name)
                    parse_extensibles(root['properties']['extensions']['items'], field_data, field_name)
                    extensible_count += 1
                if comma_or_semicolon == TOKEN_SEMICOLON:
                    return root
                continue

            field_name = re.sub('[^0-9a-zA-Z]+', '_', field_name)
            if field_name in root['properties']:
                duplicate_field_count += 1
                field_name += "_" + str(duplicate_field_count)
            append_field_number(root, field_number, field_name, False)
            root['legacy_idd']['fields'].append(field_name)
            if field_name != 'name':
                root['properties'][field_name] = field_data
                is_required = field_data.get('is_required', False)
                if is_required:
                    field_data.pop('is_required')
                    if 'default' not in field_data:
                        if 'required' not in root:
                            root['required'] = []
                        root['required'].append(field_name)

            else:
                root['name'] = field_data

            if comma_or_semicolon == TOKEN_SEMICOLON:
                return root


def parse_field(data, token):
    root = {}
    is_autocalculatable = False
    is_autosizable = False

    if token == TOKEN_A:
        root['type'] = 'string'
    else:
        root['type'] = 'number'

    while True:
        token = look_ahead(data)
        if token == TOKEN_NONE:
            raise RuntimeError("token none returned in parse field")

        elif token == TOKEN_EXCLAMATION:
            next_token(data)
            eat_comment(data)

        elif token == TOKEN_REQUIRED_FIELD:
            next_token(data)
            root['is_required'] = True

        elif token == TOKEN_EXCLAMATION:
            next_token(data)
            eat_comment(data)

        elif token == TOKEN_TYPE:  # default is string, so only change if type: array etc?
            next_token(data)
            if match_string(data, ALPHA_STR) or match_string(data, CHOICE_STR):
                if 'type' not in root or root['type'] != 'string':
                    root['type'] = 'string'
            elif match_string(data, OBJECT_LIST_STR):
                if 'data_type' not in root:
                    root['data_type'] = 'object_list'
                else:
                    raise RuntimeError("two object-lists?")
            elif match_string(data, EXTERNAL_LIST_STR):
                if 'data_type' not in root:
                    root['data_type'] = 'external_list'
                else:
                    raise RuntimeError("Two external-lists?")

            elif match_string(data, REAL_STR) or match_string(data, INTEGER_STR):
                if 'type' not in root or 'type' != 'number':
                    root['type'] = 'number'

            elif match_string(data, NODE_STR):
                root['type'] = 'string'

        elif token == TOKEN_OBJ_LIST:
            next_token(data)
            if 'object_list' not in root:
                root['object_list'] = []
            root['object_list'].append(parse_line(data))
            root['object_list'].sort()

        elif token == TOKEN_EXTERNAL_LIST:
            next_token(data)
            if 'external_list' not in root:
                root['external_list'] = []
            root['external_list'].append(parse_line(data))
            root['external_list'].sort()

        elif token == TOKEN_DEFAULT:
            next_token(data)
            default = parse_number(data)
            if default is None:
                default = parse_line(data)
                if default.lower() == 'autocalculate':
                    default = 'Autocalculate'
                elif default.lower() == 'autosize':
                    default = 'Autosize'
            if 'default' in root:
                raise RuntimeError("cannot have two defaults at " + default)
            else:
                root['default'] = default

        elif token == TOKEN_NOTE or token == TOKEN_MEMO:
            next_token(data)
            note = parse_line(data)
            if 'note' in root:
                root['note'] += ' ' + note
            else:
                root['note'] = note

        elif token == TOKEN_KEY:
            next_token(data)
            if 'enum' not in root:
                root['enum'] = []
            root['enum'].append(parse_line(data))
            root['enum'].sort()

        elif token == TOKEN_REFERENCE or token == TOKEN_REFERENCE_CLASS_NAME:
            token_str = 'reference' if token == TOKEN_REFERENCE else 'reference-class-name'
            next_token(data)
            token = look_ahead(data)
            if token == TOKEN_STRING:
                reference = parse_line(data)
            elif token == TOKEN_NUMBER:
                reference = parse_number(data)
            else:
                raise RuntimeError("Expected string or number after \\" + token_str)
            if token_str not in root:
                root[token_str] = []
            root[token_str].append(reference)
            root[token_str].sort()

        elif token == TOKEN_AUTOCALCULATABLE:
            next_token(data)
            is_autocalculatable = True

        elif token == TOKEN_AUTOSIZABLE:
            next_token(data)
            is_autosizable = True

        elif token == TOKEN_MAX or token == TOKEN_MAX_EXCLUSIVE:
            next_token(data)
            root['maximum'] = parse_number(data)
            if token == TOKEN_MAX_EXCLUSIVE:
                root['exclusiveMaximum'] = True

        elif token == TOKEN_MIN or token == TOKEN_MIN_EXCLUSIVE:
            next_token(data)
            root['minimum'] = parse_number(data)
            if token == TOKEN_MIN_EXCLUSIVE:
                root['exclusiveMinimum'] = True

        elif token == TOKEN_BEGIN_EXTENSIBLE:
            next_token(data)
            root['begin-extensible'] = True

        elif token == TOKEN_UNITS:
            next_token(data)
            root['units'] = parse_line(data)  # WHITESPACE TRAILING!!!

        elif token == TOKEN_UNITS_BASED_ON_FIELD:
            next_token(data)
            root['unitsBasedOnField'] = parse_line(data)

        elif token == TOKEN_IP_UNITS:
            next_token(data)
            root['ip-units'] = parse_line(data)  # WHITESPACE TRAILING!!!

        elif token == TOKEN_DEPRECATED:
            next_token(data)
            root['deprecated'] = True

        elif token == TOKEN_RETAIN_CASE:
            next_token(data)
            root['retaincase'] = True

        elif token == TOKEN_GROUP:
            next_token(data)
            eat_comment(data)

        elif token == TOKEN_A or token == TOKEN_N or token == TOKEN_END or token == TOKEN_STRING:
            has_default = 'default' in root
            if is_autocalculatable:
                create_any_of(root, TOKEN_AUTOCALCULATABLE, has_default)
            elif is_autosizable:
                create_any_of(root, TOKEN_AUTOSIZABLE, has_default)
            if 'enum' in root and has_default:
                root['enum'].insert(0, '')
                root['enum'].sort()
            return root


def append_field_number(root, field_number, field_name, is_in_extensibles):
    if not is_in_extensibles:
        if field_number[0].lower() == 'a':
            root['legacy_idd']['alphas']['fields'].append(field_name)
        elif field_number[0].lower() == 'n':
            root['legacy_idd']['numerics']['fields'].append(field_name)
    else:
        if field_number[0].lower() == 'a':
            if 'extensions' not in root['legacy_idd']['alphas']:
                root['legacy_idd']['alphas']['extensions'] = []
            root['legacy_idd']['alphas']['extensions'].append(field_name)
        elif field_number[0].lower() == 'n':
            if 'extensions' not in root['legacy_idd']['numerics']:
                root['legacy_idd']['numerics']['extensions'] = []
            root['legacy_idd']['numerics']['extensions'].append(field_name)


def parse_extensibles(items, field_data, field_name):
    if 'is_required' in field_data:
        field_data.pop('is_required')
        if 'required' not in items:
            items['required'] = []
        items['required'].append(field_name)
        items['required'].sort()
    items['properties'][field_name] = field_data


def create_any_of(root, token, has_default):
    root['anyOf'] = [{}, {}]

    if 'type' in root:
        root['anyOf'][0]['type'] = root.pop('type')
    else:
        raise RuntimeError("shouldn't be in create_any_of, type was string")
    if 'minimum' in root:
        root['anyOf'][0]['minimum'] = root.pop('minimum')
    if 'exclusiveMinimum' in root:
        root['anyOf'][0]['exclusiveMinimum'] = root.pop('exclusiveMinimum')
    if 'maximum' in root:
        root['anyOf'][0]['maximum'] = root.pop('maximum')
    if 'exclusiveMaximum' in root:
        root['anyOf'][0]['exclusiveMaximum'] = root.pop('exclusiveMaximum')

    enum_list = []
    if has_default:
        enum_list.append('')
    if token == TOKEN_AUTOCALCULATABLE:
        enum_list.append('Autocalculate')
    else:
        enum_list.append('Autosize')
    root['anyOf'][1] = {'type': 'string', 'enum': enum_list}


def look_ahead(data):
    save_index = data.index
    token = next_token(data)
    data.index = save_index
    return token


def next_token(data):
    eat_whitespace(data)
    if data.index == data.file_size:
        return TOKEN_END

    c = data.file[data.index]
    data.index += 1
    if c == '!':
        return TOKEN_EXCLAMATION
    elif c == ',':
        return TOKEN_COMMA
    elif c == ';':
        return TOKEN_SEMICOLON
    elif c == '\\':
        if match_string(data, MEMO_STR):
            return TOKEN_MEMO
        if match_string(data, FIELD_STR):
            return TOKEN_FIELD
        if match_string(data, FORMAT_STR):
            return TOKEN_FORMAT
        if match_string(data, MIN_FIELDS_STR):
            return TOKEN_MIN_FIELDS
        if match_string(data, REQUIRED_OBJ_STR):
            return TOKEN_REQUIRED_OBJ
        if match_string(data, UNIQUE_OBJ_STR):
            return TOKEN_UNIQUE_OBJ
        if match_string(data, EXTENSIBLE_STR):
            return TOKEN_EXTENSIBLE
        if match_string(data, TYPE_STR):
            return TOKEN_TYPE
        if match_string(data, REFERENCE_STR):
            if match_string(data, REFERENCE_CLASS_NAME_STR):
                return TOKEN_REFERENCE_CLASS_NAME
            return TOKEN_REFERENCE
        if match_string(data, REQUIRED_FIELD_STR):
            return TOKEN_REQUIRED_FIELD
        if match_string(data, KEY_STR):
            return TOKEN_KEY
        if match_string(data, NOTE_STR):
            return TOKEN_NOTE
        if match_string(data, OBJECT_LIST_STR):
            return TOKEN_OBJ_LIST
        if match_string(data, EXTERNAL_LIST_STR):
            return TOKEN_EXTERNAL_LIST
        if match_string(data, DEFAULT_STR):
            return TOKEN_DEFAULT
        if match_string(data, AUTOCALCULATABLE_STR):
            return TOKEN_AUTOCALCULATABLE
        if match_string(data, AUTOSIZABLE_STR):
            return TOKEN_AUTOSIZABLE
        if match_string(data, BEGIN_EXTENSIBLE_STR):
            return TOKEN_BEGIN_EXTENSIBLE
        if match_string(data, DEPRECATED_STR):
            return TOKEN_DEPRECATED
        if match_string(data, OBSOLETE_STR):
            return TOKEN_OBSOLETE
        if match_string(data, RETAIN_CASE_STR):
            return TOKEN_RETAIN_CASE
        if match_string(data, GROUP_STR):
            return TOKEN_GROUP
        if match_string(data, MINIMUM_STR):
            if data.file[data.index] == '>':
                data.index += 1
                return TOKEN_MIN_EXCLUSIVE
            elif data.file[data.index] == ' ' and data.file[data.index + 1] == '>':
                data.index += 2
                return TOKEN_MIN_EXCLUSIVE
            else:
                return TOKEN_MIN
        if match_string(data, MAXIMUM_STR):
            if data.file[data.index] == '<':
                data.index += 1
                return TOKEN_MAX_EXCLUSIVE
            elif data.file[data.index] == ' ' and data.file[data.index + 1] == '<':
                data.index += 2
                return TOKEN_MAX_EXCLUSIVE
            else:
                return TOKEN_MAX
        if match_string(data, IP_UNITS_STR):
            return TOKEN_IP_UNITS
        if match_string(data, UNITS_STR):
            if match_string(data, BASED_ON_FIELD_STR):
                return TOKEN_UNITS_BASED_ON_FIELD
            return TOKEN_UNITS

        return TOKEN_NONE
    elif c.isalpha() or c in '-:.#/\[]{}_@$%^&*()|+=<>?\'"~':
        if c == 'A':
            if parse_number(data) is not None:
                return TOKEN_A
        elif c == 'N':
            if parse_number(data) is not None:
                return TOKEN_N
        return TOKEN_STRING
    elif c.isdigit():
        return TOKEN_NUMBER
    else:
        data.index -= 1
        return TOKEN_NONE


def parse_line(data):
    string = ""
    if data.file[data.index] == '\n':
        return string
    eat_whitespace(data)
    while data.index < data.file_size and data.file[data.index] not in '\n\r':
        string += data.file[data.index]
        data.index += 1
    return string


def parse_number(data):
    eat_whitespace(data)
    save_index = data.index
    if data.file[save_index] not in '-+0123456789.':
        return None
    num = ""
    while data.file[save_index] not in ' \n\t\r,;':
        if data.file[save_index] not in '-+0123456789.eE':
            return None
        num += data.file[save_index]
        save_index += 1

    data.index += len(num)
    return float(num)


def match_string(data, string):
    eat_whitespace(data)
    save_index = data.index
    if save_index + len(string) > data.file_size:
        return False
    for j in range(len(string)):
        if data.file[save_index].lower() != string[j].lower():
            return False
        save_index += 1

    data.index += len(string)  # if matched, move data.index to end of string
    return True


def parse_string(data):
    eat_whitespace(data)
    s = ""
    while data.index < data.file_size:
        c = data.file[data.index]
        if c == ',' or c == ';' or c == '!':
            data.index += 1
            return s
        s += c
        data.index += 1


def eat_whitespace(data):
    while data.index < data.file_size:
        if data.file[data.index] in ' \t\n\r':
            data.index += 1
        else:
            break


def eat_comment(data):
    if data.file[data.index] == '\n':
        data.index += 1
        return
    while True:
        data.index += 1
        if data.index == data.file_size or data.file[data.index] == '\n':
            return

