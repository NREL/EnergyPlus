import re
import os


def add_fuel_type(filename):
    assert os.path.isfile(filename)
    with open(filename, 'rU') as f:
        filecontents = f.read()
        
    def repl(m):
        if m.group(2) == 'Coil:Heating:Gas':
            try:
                itemlist = m.group(3).split(',')
                pre_whitespace = re.match(r'\s*', itemlist[1].split('\n')[1]).group()
                width = len(itemlist[1].split('\n')[1]) + 1 + len(re.match(r'\s*', itemlist[2]).group())
                width -= len(pre_whitespace) + len('NaturalGas,')
                post_whitespace = ' ' * width
                pre, post = itemlist[2].split('\n')
                itemlist[2] = '{}\n{}NaturalGas,{}!- FuelType\n{}'.format(pre, pre_whitespace, post_whitespace, post)
                
                group3 = ','.join(itemlist)
                return m.group(1) + m.group(2) + ',' + group3 + ';'
            except:
                return m.group()
        else:
            return m.group()
    
    newfilecontents = re.sub(r'(\s*)([\w:]+),(.*?);', repl, filecontents, flags=re.DOTALL)

    with open(filename, 'w') as f:
        f.write(newfilecontents)
    
def main():
    this_dir = os.path.dirname(os.path.abspath(__file__))
    example_file_dir = os.path.abspath(os.path.join(this_dir, '..', '..', 'testfiles'))
    for dirpath, dirnames, filenames in os.walk(example_file_dir):
        print dirpath
        for filename in filenames:
            if filename.endswith('.idf'):
                print '  {}'.format(filename)
                add_fuel_type(os.path.join(dirpath, filename))


if __name__ == '__main__':
    main()