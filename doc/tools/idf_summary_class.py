#!/usr/bin/python

class IDFSummary:
	
	def __init__(self, idfFileName):
		self.filename = idfFileName
		self.location = ""
		self.weatherFileName = "" #no
		self.numSizingPeriods = -1
		self.floorArea = -1
		self.numberFloors = -1
		self.numberZones = -1
		self.internalMass = False
		self.people = False
		self.lights = False
		self.windows = False
		self.detachedShading = False
		self.daylighting = False
		self.compactSchedules = False
		self.zoneAutoSize = False
		self.idealLoads = False
		self.zoneEquipment = False
		self.centralAirHandlingEquipment = False
		self.systemAutoSize = False
		self.districtCooling = False
		self.districtHeating = False
		self.coils = False
		self.pumps = False
		self.boilers = False
		self.chillers = False
		self.towers = False
		self.plantAutosize = False
		self.standardReports = False
		self.hourlyTimeSeries = False
		self.timeBinsReport = False
		self.htmlReport = False
		self.SQLReport = False
		self.environmentEmissions = False
		self.utilityTariffs = False
		self.lifeCycleCosts = False
		self.costEstimates = False
		self.description = ""

	@staticmethod
	def summarize_header():
		return ",".join(["Filename",
		                "Location",
		                "NumSizingPeriods",
		                "FloorArea-m2",
		                "NumStories",
		                "NumZones",
		                "InternalMass?",
		                "People?",
		                "Lights?",
		                "Windows?",
		                "DetachedShading?",
		                "Daylighting?",
		                "CompactSchedules?",
		                "ZoneAutoSized?",
		                "IdealLoads?",
		                "ZoneEquipment?",
		                "AirLoopEquipment?",
		                "SystemAutoSized?",
		                "DistrictCooling?",
		                "DistrictHeating?",
		                "Coils?",
		                "Pumps?",
		                "Boilers?",
		                "Chillers?",
		                "CoolingTowers?",
		                "PlantAutoSized?",
		                "StandardReports?",
		                "HourlyTimeSeries?",
		                "TimeBinsReport?",
		                "HTMLReport?",
		                "SQLReport?",
		                "Emissions?",
		                "UtilityTariffs?",
		                "LifeCycleCosts?",
		                "CostEstimates?",
		                "Description"]) + '\n'

	@staticmethod
	def summarize_header_html():
		outString = ' <tr>\n'
		for headerItem in [
				"Filename",
				"Location",
				"NumSizingPeriods",
				"FloorArea-m2",
				"NumStories",
				"NumZones",
				"InternalMass?",
				"People?",
				"Lights?",
				"Windows?",
				"DetachedShading?",
				"Daylighting?",
				"CompactSchedules?",
				"ZoneAutoSized?",
				"IdealLoads?",
				"ZoneEquipment?",
				"AirLoopEquipment?",
				"SystemAutoSized?",
				"DistrictCooling?",
				"DistrictHeating?",
				"Coils?",
				"Pumps?",
				"Boilers?",
				"Chillers?",
				"CoolingTowers?",
				"PlantAutoSized?",
				"StandardReports?",
				"HourlyTimeSeries?",
				"TimeBinsReport?",
				"HTMLReport?",
				"SQLReport?",
				"Emissions?",
				"UtilityTariffs?",
				"LifeCycleCosts?",
				"CostEstimates?",
				"Description"]:
			outString += '  <th>' + headerItem + '</th>\n'
		outString += ' </tr>\n'
		return outString

	def summarize(self):
		return ",".join([str(x) for x in 
		               [self.filename,
		                self.location,
		                self.numSizingPeriods,
		                self.floorArea,
		                self.numberFloors,
		                self.numberZones,
		                self.internalMass,
		                self.people,
		                self.lights,
		                self.windows,
		                self.detachedShading,
		                self.daylighting,
		                self.compactSchedules,
		                self.zoneAutoSize,
		                self.idealLoads,
		                self.zoneEquipment,
		                self.centralAirHandlingEquipment,
		                self.systemAutoSize,
		                self.districtCooling,
		                self.districtHeating,
		                self.coils,
		                self.pumps,
		                self.boilers,
		                self.chillers,
		                self.towers,
		                self.plantAutosize,
		                self.standardReports,
		                self.hourlyTimeSeries,
		                self.timeBinsReport,
		                self.htmlReport,
		                self.SQLReport,
		                self.environmentEmissions,
		                self.utilityTariffs,
		                self.lifeCycleCosts,
		                self.costEstimates,
		                self.description]]) + '\n'

	def summarize_html(self):
		outString = ' <tr>\n'
		for value in [
					self.filename,
					self.location,
					self.numSizingPeriods,
					self.floorArea,
					self.numberFloors,
					self.numberZones,
					self.internalMass,
					self.people,
					self.lights,
					self.windows,
					self.detachedShading,
					self.daylighting,
					self.compactSchedules,
					self.zoneAutoSize,
					self.idealLoads,
					self.zoneEquipment,
					self.centralAirHandlingEquipment,
					self.systemAutoSize,
					self.districtCooling,
					self.districtHeating,
					self.coils,
					self.pumps,
					self.boilers,
					self.chillers,
					self.towers,
					self.plantAutosize,
					self.standardReports,
					self.hourlyTimeSeries,
					self.timeBinsReport,
					self.htmlReport,
					self.SQLReport,
					self.environmentEmissions,
					self.utilityTariffs,
					self.lifeCycleCosts,
					self.costEstimates,
					self.description]:
			outString += '  <td>' + str(value) + '</td>\n'
		outString += ' </tr>\n'
		return outString
