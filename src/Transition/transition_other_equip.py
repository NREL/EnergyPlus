import re
import os


def update_other_equipment_in_file(filename):
    assert os.path.isfile(filename)
    with open(filename, 'rU') as f:
        input_file_lines = f.readlines()

    linere = re.compile(r'^(\s*)(.*)([,;])(\s*)(?:!-\s*(.*))?')

    reading_otherequip_block = False
    with open(filename, 'w') as f:
        for line in input_file_lines:

            blockm = re.search(r'^(\s*)OtherEquipment,', line)
            if blockm:
                block_whitespace = blockm.group(1)
                reading_otherequip_block = True
                max_comment_col = 0
                leading_whitespaces = []
                fields = []
                comments = []
                continue

            if not reading_otherequip_block:
                f.write(line)
                continue

            m = linere.search(line)

            if m is None:
                continue

            leading_whitespace, value, delimeter, comment_whitespace, comment = m.groups()
            max_comment_col = max(max_comment_col, sum(map(len, (leading_whitespace, value, delimeter, comment_whitespace))))
            leading_whitespaces.append(leading_whitespace)
            fields.append(value)
            comments.append(comment)

            if delimeter == ';':
                reading_otherequip_block = False
                leading_whitespaces.insert(1, leading_whitespaces[0])
                fields.insert(1, 'None')
                comments.insert(1, 'Fuel Type')
                newblock = '{}OtherEquipment,\n'.format(block_whitespace)
                for i, (field, leading_whitespace, comment) in enumerate(zip(fields, leading_whitespaces, comments)):
                    if i == len(fields) - 1:
                        delim = ';'
                    else:
                        delim = ','
                    newblock += '{}{}{}'.format(
                        leading_whitespace,
                        field,
                        delim,
                    )
                    if comment is not None:
                        newblock += '{}!- {}'.format(
                            ' ' * (max_comment_col - (len(field) + len(leading_whitespace) + 1)),
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
