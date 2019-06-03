set(CPACK_PACKAGE_VENDOR "US Department of Energy" )
set(CPACK_PACKAGE_CONTACT "Edwin Lee <edwin.lee@nrel.gov>")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "EnergyPlus is a whole building energy simulation program that engineers, architects, and researchers use to model both energy consumption and water use in buildings.")

list(APPEND CMAKE_MODULE_PATH "${CMAKE_BINARY_DIR}/Modules")

set(CPACK_PACKAGE_VERSION_MAJOR "${CMAKE_VERSION_MAJOR}" )
set(CPACK_PACKAGE_VERSION_MINOR "${CMAKE_VERSION_MINOR}" )
set(CPACK_PACKAGE_VERSION_PATCH "${CMAKE_VERSION_PATCH}" )
set(CPACK_PACKAGE_VERSION_BUILD "${CMAKE_VERSION_BUILD}" )

set(CPACK_PACKAGE_VERSION "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}-${CPACK_PACKAGE_VERSION_BUILD}")
# Default the debian package name to include version to allow several versions to be installed concurrently instead of overwriting any existing one
# set(CPACK_DEBIAN_PACKAGE_NAME "energyplus-${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
# set(CPACK_DEBIAN_PACKAGE_HOMEPAGE "https://www.energyplus.net")

include(cmake/TargetArch.cmake)
target_architecture(TARGET_ARCH)

if ( "${CMAKE_BUILD_TYPE}" STREQUAL "" OR "${CMAKE_BUILD_TYPE}" STREQUAL "Release" )
  set(CPACK_PACKAGE_FILE_NAME "${CMAKE_PROJECT_NAME}-${CPACK_PACKAGE_VERSION}-${CMAKE_SYSTEM_NAME}-${TARGET_ARCH}")
else()
  set(CPACK_PACKAGE_FILE_NAME "${CMAKE_PROJECT_NAME}-${CPACK_PACKAGE_VERSION}-${CMAKE_SYSTEM_NAME}-${TARGET_ARCH}-${CMAKE_BUILD_TYPE}")
endif()

if( WIN32 AND NOT UNIX )
  set(CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS_SKIP TRUE)
  include(InstallRequiredSystemLibraries)
  if(CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS)
  install(PROGRAMS ${CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS} DESTINATION "./")
  endif()
endif()

install(FILES "${CMAKE_SOURCE_DIR}/LICENSE.txt" DESTINATION "./")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/LICENSE.txt")

install( FILES "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Energy+.idd" DESTINATION ./ )
install( FILES "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Energy+.schema.epJSON" DESTINATION ./ )

# Some docs are generated on the fly here, create a dir for the 'built' files
set( DOCS_OUT "${CMAKE_BINARY_DIR}/autodocs" )
install(CODE "execute_process(COMMAND \"${CMAKE_COMMAND}\" -E make_directory \"${DOCS_OUT}\")")

# the output variables listing
install(CODE "execute_process(COMMAND \"${PYTHON_EXECUTABLE}\" \"${CMAKE_SOURCE_DIR}/doc/tools/parse_output_variables.py\" \"${CMAKE_SOURCE_DIR}/src/EnergyPlus\" \"${DOCS_OUT}/SetupOutputVariables.csv\" \"${DOCS_OUT}/SetupOutputVariables.md\")")
install(FILES "${CMAKE_BINARY_DIR}/autodocs/SetupOutputVariables.csv" DESTINATION "./")

# the example file summary
install(CODE "execute_process(COMMAND \"${PYTHON_EXECUTABLE}\" \"${CMAKE_SOURCE_DIR}/doc/tools/example_file_summary.py\" \"${CMAKE_SOURCE_DIR}/testfiles\" \"${DOCS_OUT}/ExampleFiles.html\")"
   COMPONENT ExampleFiles)
install(FILES "${DOCS_OUT}/ExampleFiles.html" DESTINATION "./ExampleFiles/" COMPONENT ExampleFiles)

# the example file objects link
install(CODE "execute_process(COMMAND \"${PYTHON_EXECUTABLE}\" \"${CMAKE_SOURCE_DIR}/doc/tools/example_file_objects.py\"
\"${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Energy+.idd\" \"${CMAKE_SOURCE_DIR}/testfiles\" \"${DOCS_OUT}/ExampleFiles-ObjectsLink.html\")"
  COMPONENT ExampleFiles)
install(FILES "${DOCS_OUT}/ExampleFiles-ObjectsLink.html" DESTINATION "./ExampleFiles/" COMPONENT ExampleFiles)

# the change log, only if we do have a github token in the environment
# Watch out! GITHUB_TOKEN could go out of scope by the time install target is run.
# Better to move this condition into the install CODE.
if(NOT "$ENV{GITHUB_TOKEN}" STREQUAL "")
  install(CODE "execute_process(COMMAND \"${PYTHON_EXECUTABLE}\" \"${CMAKE_SOURCE_DIR}/doc/tools/create_changelog.py\" \"${CMAKE_SOURCE_DIR}\" \"${DOCS_OUT}/changelog.md\" \"${DOCS_OUT}/changelog.html\" \"${GIT_EXECUTABLE}\" \"$ENV{GITHUB_TOKEN}\" \"${PREV_RELEASE_SHA}\" \"${CPACK_PACKAGE_VERSION}\")")
  install(FILES "${DOCS_OUT}/changelog.html" DESTINATION "./" OPTIONAL)
else()
  message(WARNING "No GITHUB_TOKEN found in environment; package won't include the change log")
endif()

# Install files that are in the current repo
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/AirCooledChiller.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/ASHRAE_2005_HOF_Materials.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/Boilers.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/California_Title_24-2008.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/Chillers.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/CompositeWallConstructions.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/DXCoolingCoil.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/ElectricGenerators.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/ElectricityUSAEnvironmentalImpactFactors.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/ElectronicEnthalpyEconomizerCurves.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/ExhaustFiredChiller.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/FluidPropertiesRefData.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/FossilFuelEnvironmentalImpactFactors.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/GLHERefData.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/GlycolPropertiesRefData.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/LCCusePriceEscalationDataSet2012.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/LCCusePriceEscalationDataSet2013.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/LCCusePriceEscalationDataSet2014.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/LCCusePriceEscalationDataSet2015.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/LCCusePriceEscalationDataSet2016.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/LCCusePriceEscalationDataSet2017.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/LCCusePriceEscalationDataSet2018.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/MoistureMaterials.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/PerfCurves.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/PrecipitationSchedulesUSA.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/RefrigerationCasesDataSet.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/RefrigerationCompressorCurves.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/ResidentialACsAndHPsPerfCurves.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/RooftopPackagedHeatPump.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/SandiaPVdata.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/Schedules.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/SolarCollectors.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/StandardReports.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/SurfaceColorSchemes.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/USHolidays-DST.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/Window5DataFile.dat" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/WindowBlindMaterials.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/WindowConstructs.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/WindowGasMaterials.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/WindowGlassMaterials.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/WindowScreenMaterials.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/WindowShadeMaterials.idf" DESTINATION "./DataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/FMUs/MoistAir.fmu" DESTINATION "./DataSets/FMUs" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/FMUs/ShadingController.fmu" DESTINATION "./DataSets/FMUs" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/TDV/TDV_2008_kBtu_CTZ06.csv" DESTINATION "./DataSets/TDV" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/TDV/TDV_read_me.txt" DESTINATION "./DataSets/TDV" COMPONENT Datasets)

INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/Macro/Locations-DesignDays.xls" DESTINATION "./MacroDataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/Macro/SandiaPVdata.imf" DESTINATION "./MacroDataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/Macro/SolarCollectors.imf" DESTINATION "./MacroDataSets" COMPONENT Datasets)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/datasets/Macro/UtilityTariffObjects.imf" DESTINATION "./MacroDataSets" COMPONENT Datasets)

# weather files
INSTALL(FILES "${CMAKE_SOURCE_DIR}/weather/USA_CA_San.Francisco.Intl.AP.724940_TMY3.ddy" DESTINATION "./WeatherData" COMPONENT WeatherData)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/weather/USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw" DESTINATION "./WeatherData" COMPONENT WeatherData)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/weather/USA_CA_San.Francisco.Intl.AP.724940_TMY3.stat" DESTINATION "./WeatherData" COMPONENT WeatherData)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/weather/USA_CO_Golden-NREL.724666_TMY3.ddy" DESTINATION "./WeatherData" COMPONENT WeatherData)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/weather/USA_CO_Golden-NREL.724666_TMY3.epw" DESTINATION "./WeatherData" COMPONENT WeatherData)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/weather/USA_CO_Golden-NREL.724666_TMY3.stat" DESTINATION "./WeatherData" COMPONENT WeatherData)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/weather/USA_FL_Tampa.Intl.AP.722110_TMY3.ddy" DESTINATION "./WeatherData" COMPONENT WeatherData)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/weather/USA_FL_Tampa.Intl.AP.722110_TMY3.epw" DESTINATION "./WeatherData" COMPONENT WeatherData)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/weather/USA_FL_Tampa.Intl.AP.722110_TMY3.stat" DESTINATION "./WeatherData" COMPONENT WeatherData)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/weather/USA_IL_Chicago-OHare.Intl.AP.725300_TMY3.ddy" DESTINATION "./WeatherData" COMPONENT WeatherData)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/weather/USA_IL_Chicago-OHare.Intl.AP.725300_TMY3.epw" DESTINATION "./WeatherData" COMPONENT WeatherData)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/weather/USA_IL_Chicago-OHare.Intl.AP.725300_TMY3.stat" DESTINATION "./WeatherData" COMPONENT WeatherData)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/weather/USA_VA_Sterling-Washington.Dulles.Intl.AP.724030_TMY3.ddy" DESTINATION "./WeatherData" COMPONENT WeatherData)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/weather/USA_VA_Sterling-Washington.Dulles.Intl.AP.724030_TMY3.epw" DESTINATION "./WeatherData" COMPONENT WeatherData)
INSTALL(FILES "${CMAKE_SOURCE_DIR}/weather/USA_VA_Sterling-Washington.Dulles.Intl.AP.724030_TMY3.stat" DESTINATION "./WeatherData" COMPONENT WeatherData)

INSTALL( DIRECTORY testfiles/ DESTINATION ExampleFiles/
  COMPONENT ExampleFiles
  PATTERN _* EXCLUDE
  PATTERN *.ddy EXCLUDE
  PATTERN CMakeLists.txt EXCLUDE
  PATTERN performance EXCLUDE
)

# TODO Remove version from file name or generate
# These files names are stored in variables because they also appear as start menu shortcuts later.
set( RULES_XLS Rules9-1-0-to-9-2-0.md )
install(FILES "${CMAKE_SOURCE_DIR}/release/Bugreprt.txt" DESTINATION "./")
install(FILES "${CMAKE_SOURCE_DIR}/release/ep.gif" DESTINATION "./")
install(FILES "${CMAKE_SOURCE_DIR}/release/readme.html" DESTINATION "./")
install(FILES "${CMAKE_SOURCE_DIR}/src/Transition/InputRulesFiles/${RULES_XLS}" DESTINATION "./")
install(FILES "${CMAKE_SOURCE_DIR}/src/Transition/OutputRulesFiles/OutputChanges9-1-0-to-9-2-0.md" DESTINATION "./")
install(FILES "${CMAKE_SOURCE_DIR}/bin/CurveFitTools/IceStorageCurveFitTool.xlsm" DESTINATION "PreProcess/HVACCurveFitTool/")
install(FILES "${CMAKE_SOURCE_DIR}/src/Transition/SupportFiles/Report Variables 9-1-0 to 9-2-0.csv" DESTINATION "PreProcess/IDFVersionUpdater/")
install(FILES "${CMAKE_SOURCE_DIR}/idd/V9-1-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater/")
install(FILES "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater/" RENAME "V9-2-0-Energy+.idd" )

# Workflow stuff, takes about 40KB, so not worth it proposing to not install it
install(FILES "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/workflows/app_g_postprocess.py" DESTINATION "workflows/") # COMPONENT Workflows)
install(FILES "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/workflows/calc_soil_surface_temp.py" DESTINATION "workflows/") # COMPONENT Workflows)
install(FILES "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/workflows/coeff_check.py" DESTINATION "workflows/") # COMPONENT Workflows)
install(FILES "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/workflows/coeff_conv.py" DESTINATION "workflows/") # COMPONENT Workflows)
install(FILES "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/workflows/energyplus.py" DESTINATION "workflows/") # COMPONENT Workflows)
install(FILES "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/workflows/transition.py" DESTINATION "workflows/") # COMPONENT Workflows)

if( WIN32 )
  # calcsoilsurftemp is now built from source, just need to install the batch run script
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/src/CalcSoilSurfTemp/RunCalcSoilSurfTemp.bat" DESTINATION "PreProcess/CalcSoilSurfTemp/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/EP-Launch/EP-Launch.exe" DESTINATION "./")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/scripts/Epl-run.bat" DESTINATION "./")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/scripts/RunDirMulti.bat" DESTINATION "./")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/release/RunEP.ico" DESTINATION "./")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/scripts/RunEPlus.bat" DESTINATION "./")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/scripts/RunReadESO.bat" DESTINATION "./")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/release/Runep.pif" DESTINATION "./")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/CSVProc/CSVproc.exe" DESTINATION "PostProcess/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/scripts/RunReadESO.bat" DESTINATION "PostProcess/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/CoeffConv/CoeffCheck.exe" DESTINATION "PreProcess/CoeffConv/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/CoeffConv/CoeffCheckExample.cci" DESTINATION "PreProcess/CoeffConv/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/CoeffConv/CoeffConv.exe" DESTINATION "PreProcess/CoeffConv/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/CoeffConv/CoeffConvExample.coi" DESTINATION "PreProcess/CoeffConv/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/CoeffConv/EPL-Check.BAT" DESTINATION "PreProcess/CoeffConv/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/CoeffConv/EPL-Conv.BAT" DESTINATION "PreProcess/CoeffConv/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/CoeffConv/ReadMe.txt" DESTINATION "PreProcess/CoeffConv/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/src/Basement/RunBasement.bat" DESTINATION "PreProcess/GrndTempCalc/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/src/Slab/RunSlab.bat" DESTINATION "PreProcess/GrndTempCalc/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/CurveFitTools/CurveFitTool.xlsm" DESTINATION "PreProcess/HVACCurveFitTool/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/IDFEditor/IDFEditor.exe" DESTINATION "PreProcess/IDFEditor/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/src/ParametricPreprocessor/RunParam.bat" DESTINATION "PreProcess/ParametricPreProcessor/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/ViewFactorCalculation/readme.txt" DESTINATION "PreProcess/ViewFactorCalculation/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/ViewFactorCalculation/View3D.exe" DESTINATION "PreProcess/ViewFactorCalculation/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/ViewFactorCalculation/View3D32.pdf" DESTINATION "PreProcess/ViewFactorCalculation/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/ViewFactorCalculation/ViewFactorInterface.xls" DESTINATION "PreProcess/ViewFactorCalculation/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/WeatherConverter/Abbreviations.csv" DESTINATION "PreProcess/WeatherConverter/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/WeatherConverter/ASHRAE_2013_Monthly_DesignConditions.csv" DESTINATION "PreProcess/WeatherConverter/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/WeatherConverter/ASHRAE_2013_OtherMonthly_DesignConditions.csv" DESTINATION "PreProcess/WeatherConverter/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/WeatherConverter/ASHRAE_2013_Yearly_DesignConditions.csv" DESTINATION "PreProcess/WeatherConverter/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/WeatherConverter/Cal Climate Zone Lat Long data.csv" DESTINATION "PreProcess/WeatherConverter/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/WeatherConverter/CountryCodes.txt" DESTINATION "PreProcess/WeatherConverter/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/WeatherConverter/EPlusWth.dll" DESTINATION "PreProcess/WeatherConverter/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/WeatherConverter/libifcoremd.dll" DESTINATION "PreProcess/WeatherConverter/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/WeatherConverter/libifportmd.dll" DESTINATION "PreProcess/WeatherConverter/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/WeatherConverter/libmmd.dll" DESTINATION "PreProcess/WeatherConverter/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/WeatherConverter/svml_dispmd.dll" DESTINATION "PreProcess/WeatherConverter/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/WeatherConverter/TimeZoneCodes.txt" DESTINATION "PreProcess/WeatherConverter/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/WeatherConverter/WBANLocations.csv" DESTINATION "PreProcess/WeatherConverter/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/WeatherConverter/Weather.exe" DESTINATION "PreProcess/WeatherConverter/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/EP-Compare/Run-Win/EP-Compare Libs/Appearance Pak.dll" DESTINATION "PostProcess/EP-Compare/EP-Compare Libs/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/EP-Compare/Run-Win/EP-Compare Libs/EHInterfaces5001.dll" DESTINATION "PostProcess/EP-Compare/EP-Compare Libs/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/EP-Compare/Run-Win/EP-Compare Libs/EHObjectArray5001.dll" DESTINATION "PostProcess/EP-Compare/EP-Compare Libs/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/EP-Compare/Run-Win/EP-Compare Libs/EHObjectCollection5001.dll" DESTINATION "PostProcess/EP-Compare/EP-Compare Libs/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/EP-Compare/Run-Win/EP-Compare Libs/EHTreeView4301.DLL" DESTINATION "PostProcess/EP-Compare/EP-Compare Libs/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/EP-Compare/Run-Win/EP-Compare Libs/MBSChartDirector5Plugin16042.dll" DESTINATION "PostProcess/EP-Compare/EP-Compare Libs/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/EP-Compare/Run-Win/EP-Compare.exe" DESTINATION "PostProcess/EP-Compare/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/EP-Compare/GraphHints.csv" DESTINATION "PostProcess/EP-Compare/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/EPDraw/Run-Win/EPDrawGUI Libs/Appearance Pak.dll" DESTINATION "PreProcess/EPDraw/EPDrawGUI Libs/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/EPDraw/Run-Win/EPDrawGUI Libs/Shell.dll" DESTINATION "PreProcess/EPDraw/EPDrawGUI Libs/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/EPDraw/Run-Win/EPDrawGUI.exe" DESTINATION "PreProcess/EPDraw/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/EPDraw/Run-Win/EPlusDrw.dll" DESTINATION "PreProcess/EPDraw/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/EPDraw/Run-Win/libifcoremd.dll" DESTINATION "PreProcess/EPDraw/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/EPDraw/Run-Win/libifportmd.dll" DESTINATION "PreProcess/EPDraw/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/EPDraw/Run-Win/libmmd.dll" DESTINATION "PreProcess/EPDraw/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/EPDraw/Run-Win/svml_dispmd.dll" DESTINATION "PreProcess/EPDraw/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/src/Basement/basementexample.audit" DESTINATION "PreProcess/GrndTempCalc/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/src/Basement/basementexample.csv" DESTINATION "PreProcess/GrndTempCalc/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/src/Basement/BasementExample.idf" DESTINATION "PreProcess/GrndTempCalc/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/src/Basement/basementexample.out" DESTINATION "PreProcess/GrndTempCalc/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/src/Basement/basementexample_out.idf" DESTINATION "PreProcess/GrndTempCalc/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/src/Slab/slabexample.ger" DESTINATION "PreProcess/GrndTempCalc/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/src/Slab/slabexample.gtp" DESTINATION "PreProcess/GrndTempCalc/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/src/Slab/SlabExample.idf" DESTINATION "PreProcess/GrndTempCalc/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/IDFVersionUpdater/Run-Win/IDFVersionUpdater Libs/Appearance Pak.dll" DESTINATION "PreProcess/IDFVersionUpdater/IDFVersionUpdater Libs/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/IDFVersionUpdater/Run-Win/IDFVersionUpdater Libs/RBGUIFramework.dll" DESTINATION "PreProcess/IDFVersionUpdater/IDFVersionUpdater Libs/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/IDFVersionUpdater/Run-Win/IDFVersionUpdater Libs/msvcp120.dll" DESTINATION "PreProcess/IDFVersionUpdater/IDFVersionUpdater Libs/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/IDFVersionUpdater/Run-Win/IDFVersionUpdater Libs/msvcr120.dll" DESTINATION "PreProcess/IDFVersionUpdater/IDFVersionUpdater Libs/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/IDFVersionUpdater/Run-Win/IDFVersionUpdater Libs/Shell.dll" DESTINATION "PreProcess/IDFVersionUpdater/IDFVersionUpdater Libs/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/IDFVersionUpdater/Run-Win/IDFVersionUpdater.exe" DESTINATION "PreProcess/IDFVersionUpdater/")
  INSTALL(PROGRAMS "${CMAKE_SOURCE_DIR}/bin/EPMacro/Windows/EPMacro.exe" DESTINATION "./")
  INSTALL(PROGRAMS "${CMAKE_SOURCE_DIR}/bin/System/ComDlg32.OCX" DESTINATION "./temp/")
  INSTALL(PROGRAMS "${CMAKE_SOURCE_DIR}/bin/System/Dforrt.dll" DESTINATION "./temp/")
  INSTALL(PROGRAMS "${CMAKE_SOURCE_DIR}/bin/System/Graph32.ocx" DESTINATION "./temp/")
  INSTALL(PROGRAMS "${CMAKE_SOURCE_DIR}/bin/System/Gsw32.exe" DESTINATION "./temp/")
  INSTALL(PROGRAMS "${CMAKE_SOURCE_DIR}/bin/System/Gswdll32.dll" DESTINATION "./temp/")
  INSTALL(PROGRAMS "${CMAKE_SOURCE_DIR}/bin/System/MSCOMCTL.OCX" DESTINATION "./temp/")
  INSTALL(PROGRAMS "${CMAKE_SOURCE_DIR}/bin/System/Msflxgrd.ocx" DESTINATION "./temp/")
  INSTALL(PROGRAMS "${CMAKE_SOURCE_DIR}/bin/System/MSINET.OCX" DESTINATION "./temp/")
  INSTALL(PROGRAMS "${CMAKE_SOURCE_DIR}/bin/System/Msvcrtd.dll" DESTINATION "./temp/")
  INSTALL(PROGRAMS "${CMAKE_SOURCE_DIR}/bin/System/Vsflex7L.ocx" DESTINATION "./temp/")
endif()

if( APPLE )
  set(CPACK_PACKAGE_DEFAULT_LOCATION "/Applications")
  set(CPACK_PACKAGE_VERSION "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
  set(CPACK_IFW_TARGET_DIRECTORY "/Applications/${CMAKE_PROJECT_NAME}-${CPACK_PACKAGE_VERSION_MAJOR}-${CPACK_PACKAGE_VERSION_MINOR}-${CPACK_PACKAGE_VERSION_PATCH}")

  install(DIRECTORY "${CMAKE_SOURCE_DIR}/bin/EP-Launch-Lite/EP-Launch-Lite.app" DESTINATION "PreProcess")
  install(DIRECTORY "${CMAKE_SOURCE_DIR}/bin/IDFVersionUpdater/Run-Mac/IDFVersionUpdater.app" DESTINATION "PreProcess/IDFVersionUpdater")
  install(DIRECTORY "${CMAKE_SOURCE_DIR}/bin/Mac/Uninstall EnergyPlus.app" DESTINATION "./")
  install(DIRECTORY "${CMAKE_SOURCE_DIR}/bin/EP-Compare/Run-Mac/EP-Compare.app" DESTINATION "PostProcess/EP-Compare")
  install(FILES "${CMAKE_SOURCE_DIR}/bin/EP-Compare/GraphHints.csv" DESTINATION "PostProcess/EP-Compare/")
  install(PROGRAMS "${CMAKE_SOURCE_DIR}/bin/EPMacro/Mac/EPMacro" DESTINATION "./")

  configure_file(scripts/runenergyplus.in "${CMAKE_BINARY_DIR}/scripts/runenergyplus" @ONLY)
  install(PROGRAMS "${CMAKE_BINARY_DIR}/scripts/runenergyplus" DESTINATION "./")
  install(PROGRAMS scripts/runepmacro DESTINATION "./")
  install(PROGRAMS scripts/runreadvars DESTINATION "./")

  install(CODE "MESSAGE(\"Creating symlinks.\")" COMPONENT Symlinks)

endif()

if(UNIX)
  install(FILES doc/man/energyplus.1 DESTINATION "./")
endif()

if( UNIX AND NOT APPLE )
  INSTALL(PROGRAMS "${CMAKE_SOURCE_DIR}/bin/EP-Compare/Run-Linux/EP-Compare" DESTINATION "PostProcess/EP-Compare/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/EP-Compare/Run-Linux/EP-Compare Libs/EHInterfaces5001.so" DESTINATION "PostProcess/EP-Compare/EP-Compare Libs/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/EP-Compare/Run-Linux/EP-Compare Libs/EHObjectArray5001.so" DESTINATION "PostProcess/EP-Compare/EP-Compare Libs/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/EP-Compare/Run-Linux/EP-Compare Libs/EHObjectCollection5001.so" DESTINATION "PostProcess/EP-Compare/EP-Compare Libs/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/EP-Compare/Run-Linux/EP-Compare Libs/EHTreeView4301.so" DESTINATION "PostProcess/EP-Compare/EP-Compare Libs/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/EP-Compare/Run-Linux/EP-Compare Libs/libMBSChartDirector5Plugin16042.so" DESTINATION "PostProcess/EP-Compare/EP-Compare Libs/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/EP-Compare/Run-Linux/EP-Compare Libs/libRBAppearancePak.so" DESTINATION "PostProcess/EP-Compare/EP-Compare Libs/")

  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/IDFVersionUpdater/Run-Linux/IDFVersionUpdater Libs/libRBAppearancePak.so" DESTINATION "PreProcess/IDFVersionUpdater/IDFVersionUpdater Libs/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/IDFVersionUpdater/Run-Linux/IDFVersionUpdater Libs/libRBShell.so" DESTINATION "PreProcess/IDFVersionUpdater/IDFVersionUpdater Libs/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/IDFVersionUpdater/Run-Linux/IDFVersionUpdater Libs/RBGUIFramework.so" DESTINATION "PreProcess/IDFVersionUpdater/IDFVersionUpdater Libs/")
  INSTALL(FILES "${CMAKE_SOURCE_DIR}/bin/IDFVersionUpdater/Run-Linux/IDFVersionUpdater Libs/libc++.so.1" DESTINATION "PreProcess/IDFVersionUpdater/IDFVersionUpdater Libs/")
  INSTALL(PROGRAMS "${CMAKE_SOURCE_DIR}/bin/IDFVersionUpdater/Run-Linux/IDFVersionUpdater" DESTINATION "PreProcess/IDFVersionUpdater/")

  INSTALL(PROGRAMS "${CMAKE_SOURCE_DIR}/bin/EPMacro/Linux/EPMacro" DESTINATION "./")

  configure_file(scripts/runenergyplus.in "${CMAKE_BINARY_DIR}/scripts/runenergyplus" @ONLY)
  install(PROGRAMS "${CMAKE_BINARY_DIR}/scripts/runenergyplus" DESTINATION "./")
  install(PROGRAMS scripts/runepmacro DESTINATION "./")
  install(PROGRAMS scripts/runreadvars DESTINATION "./")
endif()

configure_file("${CMAKE_SOURCE_DIR}/cmake/CMakeCPackOptions.cmake.in"
  "${CMAKE_BINARY_DIR}/CMakeCPackOptions.cmake" @ONLY)
set(CPACK_PROJECT_CONFIG_FILE "${CMAKE_BINARY_DIR}/CMakeCPackOptions.cmake")

if ( BUILD_DOCS )
  install(FILES "${CMAKE_BINARY_DIR}/doc-pdf/Acknowledgments.pdf" DESTINATION "./Documentation" COMPONENT Documentation)
  install(FILES "${CMAKE_BINARY_DIR}/doc-pdf/AuxiliaryPrograms.pdf" DESTINATION "./Documentation" COMPONENT Documentation)
  install(FILES "${CMAKE_BINARY_DIR}/doc-pdf/EMSApplicationGuide.pdf" DESTINATION "./Documentation" COMPONENT Documentation)
  install(FILES "${CMAKE_BINARY_DIR}/doc-pdf/EngineeringReference.pdf" DESTINATION "./Documentation" COMPONENT Documentation)
  install(FILES "${CMAKE_BINARY_DIR}/doc-pdf/EnergyPlusEssentials.pdf" DESTINATION "./Documentation" COMPONENT Documentation)
  install(FILES "${CMAKE_BINARY_DIR}/doc-pdf/ExternalInterfacesApplicationGuide.pdf" DESTINATION "./Documentation" COMPONENT Documentation)
  install(FILES "${CMAKE_BINARY_DIR}/doc-pdf/GettingStarted.pdf" DESTINATION "./Documentation" COMPONENT Documentation)
  install(FILES "${CMAKE_BINARY_DIR}/doc-pdf/InputOutputReference.pdf" DESTINATION "./Documentation" COMPONENT Documentation)
  install(FILES "${CMAKE_BINARY_DIR}/doc-pdf/InterfaceDeveloper.pdf" DESTINATION "./Documentation" COMPONENT Documentation)
  install(FILES "${CMAKE_BINARY_DIR}/doc-pdf/ModuleDeveloper.pdf" DESTINATION "./Documentation" COMPONENT Documentation)
  install(FILES "${CMAKE_BINARY_DIR}/doc-pdf/OutputDetailsAndExamples.pdf" DESTINATION "./Documentation" COMPONENT Documentation)
  install(FILES "${CMAKE_BINARY_DIR}/doc-pdf/PlantApplicationGuide.pdf" DESTINATION "./Documentation" COMPONENT Documentation)
  install(FILES "${CMAKE_BINARY_DIR}/doc-pdf/TipsAndTricksUsingEnergyPlus.pdf" DESTINATION "./Documentation" COMPONENT Documentation)
  install(FILES "${CMAKE_BINARY_DIR}/doc-pdf/UsingEnergyPlusForCompliance.pdf" DESTINATION "./Documentation" COMPONENT Documentation)
endif ()

include(CPack)
include(CPackIFW)

#cpack_add_component(EnergyPlus
  #DISPLAY_NAME "EnergyPlus"
  #DESCRIPTION "The EnergyPlus program itself"
  #REQUIRED
#)

#cpack_add_component(AuxiliaryPrograms
  #DISPLAY_NAME "Auxiliary Programs"
  #DESCRIPTION "The suite of Fortran auxiliary programs such as ReadVarsESO, ExpandObjects, etc"
  #REQUIRED
#)

cpack_add_component(Documentation
  DISPLAY_NAME "Documentation"
  DESCRIPTION "EnergyPlus documentation in PDF format"
)

cpack_add_component(Datasets
  DISPLAY_NAME "Datasets"
  DESCRIPTION "Useful resources such as material and equipment performance data"
)

cpack_add_component(ExampleFiles
  DISPLAY_NAME "Example Files"
  DESCRIPTION "IDF Example Files"
)

cpack_add_component(WeatherData
  DISPLAY_NAME "Weather Data"
  DESCRIPTION "A few EPW files"
)

# This stuff actually requires admin privileges since touched system locations
cpack_add_component(Symlinks
  DISPLAY_NAME "Create Symlinks - requires admin"
  DESCRIPTION "This will symlink the executable to /usr/local/bin and copy the man page"
)

# Regular stuff, like chmod +x
cpack_ifw_configure_component(Unspecified
    SCRIPT cmake/install_operations.qs
)

cpack_ifw_configure_component(Symlinks
    SCRIPT cmake/create_symlinks.qs
    REQUIRES_ADMIN_RIGHTS
)

SET(CMAKE_INSTALL_UCRT_LIBRARIES TRUE)

INCLUDE(InstallRequiredSystemLibraries)

INSTALL(FILES ${CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS} DESTINATION "./" COMPONENT Libraries)

