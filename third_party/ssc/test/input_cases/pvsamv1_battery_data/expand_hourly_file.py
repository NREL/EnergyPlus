import csv
import sys

if __name__ == "__main__":
    filename = sys.argv[1]
    outfilename = sys.argv[2]
    steps_per_hour = int(sys.argv[3])
    with open(filename, "r", newline='') as datafile:
        with open(outfilename, "w", newline='') as outfile:
            data = csv.reader(datafile, delimiter=',', quotechar='\"')
            output = csv.writer(outfile, delimiter=',', quotechar='\"')
            for row in data:
                for i in range(0, steps_per_hour):
                    output.writerow(row)
            