import os
import shutil
from eppy.modeleditor import IDF


def parse_encost_file(encost_path):
    """Parses the NIST encostYY.txt file into individual files by region-sector.
    Files are saved to a sub-directory with the same name as the file."""
    
    read_dir = os.path.dirname(encost_path)
    read_name = os.path.basename(encost_path)
    read_path = os.path.normpath(encost_path)
    
    sub_dir_name = read_name.strip('.txt')
    sub_dir_path = os.path.join(read_dir, sub_dir_name)        
    
    if not os.path.exists(sub_dir_path):
        os.makedirs(sub_dir_path)
        
    with open(read_path, 'r') as read_file:

        counter = 0

        for line in read_file:
        
            # lines without data and end of file
            if len(line.strip()) == 0:

                counter = 0
                write_file.close
                next

            # lines with data
            else:
                
                # header
                if counter == 0:

                    # remove newline
                    region_sector = line.strip('\n')
                    
                    # remove trailing spaces
                    region_sector = region_sector.rstrip()
                    
                    # open write file and write line
                    write_name = region_sector + '.txt'
                    write_path = os.path.join(sub_dir_path, write_name)
                    write_file = open(write_path, 'w')
                    write_file.write(line)
                    
                    counter += 1

                # data
                else:

                    # remove indentation if present 
                    # encost17.txt had a single leading space, encost18.txt did not
                    data = line.lstrip()
                    write_file.write(data)
                    counter += 1

    write_file.close()
    
    print 'Writing EnCost files to: %s' % sub_dir_path
    return sub_dir_path

  
def encost_to_idf(idd_dir, sub_dir_path, del_dir=None):
    """Read parsed encost files and make an IDF of 
    LifeCycleCost:UsePriceEscalation objects for each region-sector-fuel."""

    # eppy setup
    idd_name = r'Energy+.idd'
    idd_path = os.path.join(idd_dir, idd_name)
    IDF.setiddname(idd_path)

    # create a blank idf, add Version object
    idf_dir = os.path.dirname(sub_dir_path)
    idf_name = r'LCCusePriceEscalationDataSet.idf'
    idf_path = os.path.join(idf_dir, idf_name)
    open(idf_path, 'w').close()
    idf = IDF(idf_path)
    idf.newidfobject('VERSION')
    idf.save()

    print 'Reading EnCost files from: %s' % sub_dir_path
    encost_year = '20' + sub_dir_path[-2:]
    
    for read_file in os.listdir(sub_dir_path):

        read_path = os.path.join(sub_dir_path, read_file)

        # dict of dicts to hold data for making IDF
        # { 'region - sector - fuel': { 'fuel': [escalation_rate_year1, ...] } }
        obj_dict = {}

        with open(read_path, 'r') as encost_file:

            # get file name (region and sector) from file path 
            file_name = os.path.basename(read_path).strip('.txt')
            
            years = []

            for idx, line in enumerate(encost_file):

                # check that file name == file header
                if idx == 0:
                  
                    if file_name == line.rstrip():
                      
                        # remove periods from file name, e.g. 'U.S. Avg' becomes 'USAvg'
                        region_sector = file_name.replace('. ', '.').replace('.', '')
                                                
                    else:
                      
                        print 'ERROR: Parsed EnCost file name does not match file header.'
                        return

                # years
                elif idx == 1:

                    # split line by spaces into list
                    years = line.split(' ')
                    escalation_start_year = years[1]
                    
                    # check length
                    if len(years) != 31:
                        print 'ERROR: length of years list does not equal 31'
                    
                # fuel, non-zero even indices
                elif idx % 2 == 0:

                    # strip newline
                    nist_fuel = line.strip('\n')

                    # separate region, sector, and fuel with hyphen for obj_dict key and IDF object name
                    region_sector_fuel = region_sector.replace(' ', ' - ') + ' - ' + nist_fuel

                    # assign EnergyPlus fuels
                    if nist_fuel == 'Electricity':
                        # TODO https://github.com/NREL/EnergyPlus/issues/6144
                        eplus_fuel = 'ElectricityPurchased'
                    elif nist_fuel == 'Distillate Oil':
                        eplus_fuel = 'FuelOil#1'
                    elif nist_fuel == 'Natural gas':
                        eplus_fuel = 'NaturalGas'
                    elif nist_fuel == 'LPG':
                        eplus_fuel = 'Propane'
                    elif nist_fuel == 'Residual Oil':
                        eplus_fuel = 'FuelOil#2'
                    elif nist_fuel == 'Coal':
                        eplus_fuel = nist_fuel
                        
                # fuel prices, odd indices > 1
                else:

                    prices = []
                    escalation = []
                    fuel_escalation_dict = {}

                    # strip newline and split by spaces into list
                    prices = line.strip('\n').split(' ')
                    
                    # check length
                    if len(prices) != 31:
                        print 'ERROR: length of price list is not equal to 31'
                    
                    # get year 0 price for denominator of escalation calculation and delete
                    price_0 = float(prices[0])
                    del prices[0]
                    
                    # convert prices to floats
                    prices = [float(item) for item in prices]

                    # calculate escalation rate and add to list
                    # these should match the Annual Supplement to NIST Handbook 135, Tables Ca-1 to Ca-5
                    [escalation.append(item / price_0) for item in prices]
  
                    # round values
                    escalation = [round(item, 4) for item in escalation]    

                    # assign list of escalation rates to EnergyPlus fuel name
                    fuel_escalation_dict[eplus_fuel] = escalation
                    
                    # assign fuel escalation dict to region-sector-fuel name
                    obj_dict[region_sector_fuel] = fuel_escalation_dict

        # make an IDF using eppy
        for region_sector_fuel, fuel_escalation_dict in obj_dict.iteritems():

            # make a new object for each key in obj_dict and set name
            obj = idf.newidfobject('LifeCycleCost:UsePriceEscalation'.upper())
            obj.LCC_Price_Escalation_Name = region_sector_fuel

            # set non-extensible fields
            for fuel, escalation in fuel_escalation_dict.iteritems():

                obj.Resource = fuel
                obj.Escalation_Start_Year = escalation_start_year

                # set extensible fields
                for idx, item in enumerate(escalation):

                    year_num = idx + 1
                    attribute = 'Year_%s_Escalation' % year_num
                    setattr(obj, attribute, escalation[idx])
                
                # TODO remove Version object
#                 idf.removeidfobject(idf.getobject('VERSION'), name?)
                
                # save as new IDF with start year appended
                idf.saveas(os.path.join(idf_dir, idf_name.replace('.idf', '%s.idf' % encost_year)))
    
    # delete original idf
    os.remove(idf_name)
    
    # delete encost directory
    if del_dir == True:
        shutil.rmtree(sub_dir_path)
        print 'Deleting EnCost directory: %s' % sub_dir_path   
        
    print 'IDF file saved to: %s' % idf_dir
    