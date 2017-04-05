#!/usr/bin/env python

final_output = ''
string = ''
start_num = 22
end_num = 3100
eol_comment = '\\note fields as indicated'
max_string_length = 100  # before adding eol_comment
for i in range(start_num, end_num+1):
	string += "N{0},".format(i)
	if (len(string) >= max_string_length) or (i == end_num):
		final_output += string + '  ' + eol_comment + '\n'
		string = ''
# now replace the very last comma with a semicolon
reversed_split = final_output.rsplit(',', 1)
good_output = ';'.join(reversed_split)
print(good_output)

