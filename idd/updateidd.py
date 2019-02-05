import re
import os
import shutil


def repl_cooling_sequence(m):
    eqnum = int(m.group(1))
    fieldnum = (eqnum - 1) * 4 + 1
    return 'N{:<2d}, \\field Zone Equipment {:d} Cooling Sequence'.format(fieldnum, eqnum)


def repl_heating_sequence(m):
    eqnum = int(m.group(2))
    fieldnum = (eqnum - 1) * 4 + 2
    return 'N{:<2d}, \\field Zone Equipment {:d} Heating or No-Load Sequence'.format(fieldnum, eqnum)


def main():
    newitems = r'''  N{n1:<2d}, \field Zone Equipment {eqnum} Sequential Cooling Fraction
       \note The fraction of the remaining cooling load this equipment will attempt to serve
       \note if the load distribution scheme is SequentialLoad, otherwise ignored.
       \type real
       \minimum 0
       \maximum 1
       \default 1
  N{n2:<2d}{delim2} \field Zone Equipment {eqnum} Sequential Heating or No-Load Fraction
       \note The fraction of the remaining heating load this equipment will attempt to serve
       \note if the load distribution scheme is SequentialLoad, otherwise ignored.
       \type real
       \minimum 0
       \maximum 1
       \default 1
'''
    here = os.path.dirname(os.path.abspath(__file__))
    iniddobj = False
    in_idd = os.path.join(here, 'Energy+.idd.in')
    out_idd = os.path.join(here, 'Energy+.idd.in_new')
    eqnum = None
    delim = ','
    with open(in_idd, 'r') as f_in, open(out_idd, 'w') as f_out:
        for line in f_in:
            if not iniddobj:
                if line == 'ZoneHVAC:EquipmentList,\n':
                    iniddobj = True
                f_out.write(line)
                continue
            if line == 'ZoneHVAC:EquipmentConnections,\n':
                iniddobj = False
                f_out.write(line)
                continue
            m = re.search(r'A\d+\s*,\s*\\field Zone Equipment (\d+) Object Type', line)
            if m:
                eqnum = int(m.group(1))
            line = re.sub(r'N\d+\s*,\s*\\field Zone Equipment (\d+) Cooling Sequence', repl_cooling_sequence, line)
            heating_re = r'N\d+\s*([,;])\s*\\field Zone Equipment (\d+) Heating or No-Load Sequence'
            m = re.search(heating_re, line)
            if m:
                delim = m.group(1)
            line = re.sub(heating_re, repl_heating_sequence, line)
            f_out.write(line)
            if line == '       \\note when the zone thermostat requests heating or no load\n':
                f_out.write(newitems.format(n1=(eqnum-1)*4+3, n2=(eqnum-1)*4+4, delim2=delim, eqnum=eqnum))

    os.remove(in_idd)
    shutil.move(out_idd, in_idd)


if __name__ == '__main__':
    main()