#require
require 'rubygems'
require 'fileutils'
require 'pathname'
require 'zip/zip'
#get build directory name from user
print "Build directory name: "
bld_dir = gets.chomp
#make temporary directory
puts "Making temporary directory"
FileUtils.mkdir("tmp")
#copy IDD files
puts "Copying IDD files"
idds = Pathname.glob("../../IDD/V*.idd")
idds.each {|idd| FileUtils.cp(idd,"tmp")}
#copy report variable CSV files and version 3 object renames
puts "Copying report variable CSVs"
rptvars = Pathname.glob("ReportVars/*")
rptvars.each {|rptvar| FileUtils.cp(rptvar,"tmp")}
#copy executable files
puts "Copying executable files"
bins = Pathname.glob("../../#{bld_dir}/SourceCode/Transition/Release/Transition*.exe")
bins.each {|bin| FileUtils.cp(bin,"tmp")}
#make zip file
puts "Generating zip file"
#zip results
p = Pathname.new("tmp")
Zip::ZipFile::open("multipletransitionidfversionupdater-win.zip",true) do |zf|
  p.each_entry {|f| zf.add(f.basename,"tmp/" + f)}
end
#remove temporary directory
puts "Removing temporary directory"
FileUtils.rm_rf("tmp")
puts "Done"
