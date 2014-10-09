#!/usr/bin/python

# generates a csv summary of the output variables in E+
# main assumption: function calls are single line -- **NOT** wrapped
# can call 1 of 2 ways:
#  Method 1: NO ARGUMENTS, in this case, the defaults below are used
#  Method 2: 2 ARGUMENTS, the first is the source dir to find *.cc files, and the second is the csv output file name

import sys
import os
import glob
import csv

# defaults here
source_dir = "C:\ResearchProjects\EnergyPlus\GitHub\EnergyPlusTeamBranch\src\EnergyPlus" #"/home/elee/EnergyPlus/GitHub/EnergyPlusTeam/src/EnergyPlus"
output_file = "SetupOutputVariableCalls.csv"

if len(sys.argv) == 1:
	pass
elif len(sys.argv) == 3:
	source_dir = sys.argv[1]
	output_file = sys.argv[2]
else:
	print("Bad usage, either zero or two command line arguments are required")
	print("If using zero arguments, the defaults hardwired in the script are used")
	print("If using two arguments, the first is the source dir to find .cc files, and the second is the output csv file")
	sys.exit(1)
	
def do_initial_line_trimming(working_line):
	# trim leading/trailing whitespace
	working_line = working_line.strip()
	# cut the call to SetupOutputVariable off
	sov_index = working_line.index("SetupOutputVariable")
	working_line = working_line[sov_index + 21:]
	# cut the last 3 characters off
	working_line = working_line[:-3]
	return working_line

def get_level_zero_comma_locations(line):
	current_char_location = -1
	current_parentheses_level = 0
	level_zero_comma_locations = []
	for char in line:
		current_char_location += 1
		if char == ',' and current_parentheses_level == 0:
			level_zero_comma_locations.append(current_char_location)
		elif char == '(':
			current_parentheses_level += 1
		elif char == ')':
			current_parentheses_level -= 1
#		print "Character: " + char + "; paren_level: " + str(current_parentheses_level)
	return level_zero_comma_locations

def get_arguments_based_on_comma_locations(line, comma_locations):
	arguments = []
	for i in range(len(comma_locations)+1):
		if i == 0:
			arguments.append(working_line[0:comma_locations[i]].strip())
		elif i == len(comma_locations):
			arguments.append(working_line[comma_locations[i-1]+1:].strip())
		else:
			arguments.append(working_line[comma_locations[i-1]+1:comma_locations[i]].strip())
	return arguments

def process_variable_name_and_units(argument):
	# parse out data from the variable name first
	first_bracket = argument.find('[')
	variable_name = ""
	variable_units = ""
	if first_bracket == -1:
		variable_name = argument.strip()
	else:
		variable_name = argument[0:first_bracket].strip() + '"'
		variable_units = argument[first_bracket+1:-2].strip()
	return variable_name, variable_units

class OutputVariableCall:
	
	def __init__(self, file_name, line_number, variable_name, units, variable_itself, index_type_key, variable_type_key, keyed_value):
		self.file_name = file_name
		self.line_number = line_number
		self.variable_name = variable_name
		self.units = units
		self.variable = variable_itself
		self.index_type_key = index_type_key
		self.variable_type_key = variable_type_key
		self.keyed_value = keyed_value
	
	@staticmethod
	def spew_header_to_csv(csv_file_object):
		csv_file_object.writerow(["Filename","Line number","Variable name","Units","Variable reference","Index type key","Variable type key","Keyed value"])
	
	def spew_to_csv(self, csv_file_object):
		csv_file_object.writerow([self.file_name, self.line_number, self.variable_name, self.units, self.variable, self.index_type_key, self.variable_type_key, self.keyed_value])

# this is the master list of calls
output_variables = []

files = sorted(glob.glob(os.path.join(source_dir,"*.cc")))

for this_file in files:

	file_name = os.path.basename(this_file)
	if file_name == "OutputProcessor.cc":
		continue

	with open(this_file,'r') as f:
		
		line_number = 0
		
		for line in f.readlines():
		
			line_number += 1
			
			# create a copy of line to work on
			working_line = line
			#print working_line
			
			# trim off comments first
			if "//" in working_line:
				working_line = working_line[0:working_line.index("//")]
			
			if 'SetupOutputVariable' in working_line:
				
				# trim off the leading/trailing stuff to just get args
				working_line = do_initial_line_trimming(working_line)
				
				# get all the zero parentheses level comma locations to parse actual arguments, even if the arguments have embedded commas
				comma_locations = get_level_zero_comma_locations(working_line)
				
				# get a list of the actual arguments
				arguments = get_arguments_based_on_comma_locations(working_line, comma_locations)
				
				# parse out data from the variable name first
				variable_name, variable_units = process_variable_name_and_units(arguments[0])
				
				# the second can be taken as-is
				actual_variable = arguments[1]
				
				# the third should be processed for quotes
				index_type_key = arguments[2]
				
				# same for the fourth
				variable_type_key = arguments[3]
				
				# same for the fifth I guess
				keyed_value = arguments[4]

				# add to the array
				output_variables.append(OutputVariableCall(file_name, line_number, variable_name, variable_units, actual_variable, index_type_key, variable_type_key, keyed_value))
	
	print "Finished with file %s; %i calls processed so far" % (file_name, len(output_variables))

# output the data to csv
with open(output_file,"w") as csvfile:
	csv_writer = csv.writer(csvfile)
	OutputVariableCall.spew_header_to_csv(csv_writer)
	for call in output_variables:
		call.spew_to_csv(csv_writer)

print "DONE"
