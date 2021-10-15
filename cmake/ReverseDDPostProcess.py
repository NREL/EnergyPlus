import os
import shutil

# find out ow many DD's there are
num_DDs = 0
with open("in.idf") as f:
    for line in f:
        if "SizingPeriod:DesignDay," in line:
            num_DDs += 1

# read all lines from csv
csv_contents = []
with open("eplusout.csv") as f:
    for line in f:
        csv_contents.append(line)

meter_exists = os.path.exists("eplusmtr.csv")
mtr_contents = []
if meter_exists:
    with open("eplusmtr.csv") as f:
        for line in f:
            mtr_contents.append(line)

# now find out how many lines of data there are in each file and each environment
csv_data_rows = len(csv_contents) - 1
csv_data_rows_per_envrn = int(csv_data_rows / num_DDs)
mtr_data_rows = 0
mtr_data_rows_per_envrn = 0
if meter_exists:
    mtr_data_rows = len(mtr_contents) - 1
    mtr_data_rows_per_envrn = mtr_data_rows / num_DDs

# write the files back out in the appropriate order
shutil.copy("eplusout.csv", "eplusout-before_revDD_swapback.csv")
with open("eplusout.csv", "w") as f:
    f.write("%s" % csv_contents[0])
    for row_num in range(csv_data_rows_per_envrn+1, 2*csv_data_rows_per_envrn+1):
        f.write("%s" % csv_contents[row_num])
    for row_num in range(1, csv_data_rows_per_envrn+1):
        f.write("%s" % csv_contents[row_num])
    if num_DDs > 2:
        for row_num in range(2*csv_data_rows_per_envrn+1, csv_data_rows+1):
            f.write("%s" % csv_contents[row_num])
if meter_exists:
    shutil.copy("eplusmtr.csv", "eplusmtr-before_revDD_swapback.csv")
    with open("eplusmtr.csv", "w") as f:
        f.write("%s" % mtr_contents[0])
        for row_num in range(mtr_data_rows_per_envrn+1, 2*mtr_data_rows_per_envrn+1):
            f.write("%s" % mtr_contents[row_num])
        for row_num in range(1, mtr_data_rows_per_envrn+1):
            f.write("%s" % mtr_contents[row_num])
        if num_DDs > 2:
            for row_num in range(2*mtr_data_rows_per_envrn+1, mtr_data_rows+1):
                f.write("%s" % mtr_contents[row_num])
