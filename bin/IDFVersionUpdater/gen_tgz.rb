#require
require 'fileutils'
require 'pathname'
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
bins = Pathname.glob("../../#{bld_dir}/SourceCode/Transition/Transition*")
bins.each {|bin| FileUtils.cp(bin,"tmp")}
#change to temporary directory
FileUtils.cd("tmp")
#determine platform
if RUBY_PLATFORM.downcase.include?("darwin")
  os = "mac"
elsif RUBY_PLATFORM.downcase.include?("linux")
  os = "lin"
end
#make tarball
puts "Generating tar.gz file"
system("tar -czf multipletransitionidfversionupdater-#{os}.tar.gz *")
#move tarball
FileUtils.mv("multipletransitionidfversionupdater-#{os}.tar.gz","../")
#remove temporary directory
puts "Removing temporary directory"
FileUtils.cd("../")
FileUtils.rm_rf("tmp")
puts "Done"
