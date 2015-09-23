# This is a quick and dirty python script to transition the EMPD model.
# You'll want to use this math in the official transition program.
# Once you're done that, feel free to delete this.

import os
import re
import sys
import math

def Sat_press(Tdb):

    ''' Function to compute saturation vapor pressure in [kPa]
        ASHRAE Fundamentals handbood (2005) p 6.2, equation 5 and 6
            Tdb = Dry bulb temperature [degC]
            Valid from -100C to 200 C
    '''

    C1 = -5674.5359
    C2 = 6.3925247
    C3 = -0.009677843
    C4 = 0.00000062215701
    C5 = 2.0747825E-09
    C6 = -9.484024E-13
    C7 = 4.1635019
    C8 = -5800.2206
    C9 = 1.3914993
    C10 = -0.048640239
    C11 = 0.000041764768
    C12 = -0.000000014452093
    C13 = 6.5459673

    TK = Tdb + 273.15                     # Converts from degC to degK

    if TK <= 273.15:
        result = math.exp(C1/TK + C2 + C3*TK + C4*TK**2 + C5*TK**3 +
                          C6*TK**4 + C7*math.log(TK)) / 1000
    else:
        result = math.exp(C8/TK + C9 + C10*TK + C11*TK**2 + C12*TK**3 +
                          C13*math.log(TK)) / 1000
    return result


def convert_empd_inputs(name, d_empd, a, b, c, d, density_matl):
    """
    Convert the inputs from the 8.4 EMPD model to the 8.5 EMPD model

    :param name: original A1 field
    :param d_empd: original N1 field
    :param a: original N2 field
    :param b: original N3 field
    :param c: original N4 field
    :param d: original N5 field
    :param density_matl: density of the material, looked up from the Material object.
    :return: The fields in the order they need to be for the new model.
    """

    # Assume T, RH, and P
    T = 24  # degC
    RH = 0.45
    P_ambient = 101325

    # Assume time interval of 24 hours
    t_p = 24 * 60 * 60  # seconds

    slope_MC = a * b * RH ** (b - 1) + c * d * RH ** (d - 1)
    PV_sat = Sat_press(T) * 1000  # kPa -> Pa

    diffusivity_EMPD = d_empd ** 2 * math.pi * slope_MC * density_matl / (t_p * PV_sat)

    diffusivity_air = 2.0e-7 * (T + 273.15) ** 0.81 / P_ambient

    mu_EMPD = diffusivity_air / diffusivity_EMPD

    return name, mu_EMPD, a, b, c, d, d_empd, 0, 0, 0


def transition_empd(input_idf, output_idf=None):
    """
    Transition the MaterialProperty:MoisturePenetrationDepth:Settings block.

    :param input_idf: path to the idf file to transition
    :param output_idf: path to the output idf file, will overwrite input if omitted
    :return: None
    """
    if output_idf is None:
        output_idf = input_idf

    assert os.path.isfile(input_idf)

    with open(input_idf, 'rU') as f:
        input_file_lines = f.readlines()

    # Get a dict of all the material densities because we'll need them later.
    reading_mat_block = False
    matblockre = re.compile(r'^\s*Material,\s*$')
    linere = re.compile(r'\s*(.*)([,;])')
    material_densities = {}
    for line in input_file_lines:

        # Determine if we're starting the material block
        matm = matblockre.search(line)
        if matm:
            reading_mat_block = True
            lineno = 0
            continue

        # Move along if we're not reading the material block
        if not reading_mat_block:
            continue

        # See if the line looks like a field value
        m = linere.search(line)

        # If not, move to next line
        if m is None:
            continue

        # Note what line we're on
        lineno += 1

        if lineno == 1:
            # Material Name is the 1st field
            mat_name = m.group(1)
        elif lineno == 5:
            # Density is the 5th field
            material_densities[mat_name] = float(m.group(1))
            reading_mat_block = False

    newblock = """  MaterialProperty:MoisturePenetrationDepth:Settings,
    {},                   !- Name
    {},                   !- Water Vapor Diffusion Resistance Factor {{dimensionless}}
    {},                   !- Moisture Equation Coefficient a {{dimensionless}}
    {},                   !- Moisture Equation Coefficient b {{dimensionless}}
    {},                   !- Moisture Equation Coefficient c {{dimensionless}}
    {},                   !- Moisture Equation Coefficient d {{dimensionless}}
    {},                   !- Surface-layer penetration depth {{m}}
    {},                   !- Deep-layer penetration depth {{m}}
    {},                   !- Coating layer thickness {{m}}
    {};                   !- Coating layer water vapor diffusion resistance factor {{m}}
    """

    # Find all the EMPD blocks and convert them.
    reading_empd_block = False
    with open(output_idf, 'w') as f:
        for line in input_file_lines:

            # Determine if we're starting the block
            if line.find('MaterialProperty:MoisturePenetrationDepth:Settings') > -1:
                reading_empd_block = True
                fields = []
                continue

            if not reading_empd_block:
                f.write(line)
                continue

            # See if the line looks like a field value
            m = linere.search(line)

            # If not, move to next line
            if m is None:
                continue

            # Get the value of the field and store it
            value, delimeter = m.groups()
            fields.append(value)

            # If this is the last field, close out the block and do the transition
            if delimeter == ';':
                reading_empd_block = False
                fields = [fields[0]] + map(float, fields[1:])
                fields.append(material_densities[fields[0]])
                newfields = convert_empd_inputs(*fields)
                f.write(newblock.format(*newfields))

if __name__ == '__main__':
    transition_empd(*sys.argv[1:])
