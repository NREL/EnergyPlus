import re
import os


def update_other_equipment_in_file(filename):
    assert os.path.isfile(filename)
    with open(filename, 'rU') as f:
        input_file_lines = f.readlines()

    linere = re.compile(r'^(\s*)(.*)([,;])\s*(?:!-\s*(.*))?')

    reading_otherequip_block = False
    with open(filename, 'w') as f:
        for line in input_file_lines:

            blockm = re.search(r'^(\s*)OtherEquipment,', line)
            if blockm:
                block_whitespace = blockm.group(1)
                reading_otherequip_block = True
                whitespaces = []
                fields = []
                comments = []
                continue

            if not reading_otherequip_block:
                f.write(line)
                continue

            m = linere.search(line)

            if m is None:
                continue

            whitespace, value, delimeter, comment = m.groups()
            whitespaces.append(whitespace)
            fields.append(value)
            comments.append(comment)

            if delimeter == ';':
                reading_otherequip_block = False
                whitespaces.insert(1, whitespaces[0])
                fields.insert(1, 'None')
                comments.insert(1, 'Fuel Type')
                newblock = '{}OtherEquipment,\n'.format(block_whitespace)
                max_field_width = max([len(field) + len(whitespace) for field, whitespace in zip(fields, whitespaces)]) + 8
                for i, (field, whitespace, comment) in enumerate(zip(fields, whitespaces, comments)[:-1]):
                    if i == len(fields) - 1:
                        delim = ';'
                    else:
                        delim = ','
                    newblock += '{}{}{}'.format(
                        whitespace,
                        field,
                        delim,
                    )
                    if comment is not None:
                        newblock += '{}!- {}'.format(
                            ' ' * (max_field_width - (len(field) + len(whitespace))),
                            comment
                        )
                    newblock += '\n'
                f.write(newblock)







def main():
    this_dir = os.path.dirname(os.path.abspath(__file__))
    example_file_dir = os.path.abspath(os.path.join(this_dir, '..', '..', 'testfiles'))
    for dirpath, dirnames, filenames in os.walk(example_file_dir):
        print dirpath
        for filename in filenames:
            if filename.endswith('.idf'):
                print '  {}'.format(filename)
                update_other_equipment_in_file(os.path.join(dirpath, filename))


if __name__ == '__main__':
    main()
