#include <rs0003.h>
#include <loadobject_205.h>

namespace tk205  {

	namespace rs0003_ns  {
	
		void from_json(const nlohmann::json& j, ProductInformation& x) {
			a205_json_get<std::string>(j, "manufacturer", x.manufacturer, x.manufacturer_is_set, false);
			a205_json_get<ashrae205_ns::Pattern>(j, "model_number", x.model_number, x.model_number_is_set, false);
			a205_json_get<rs0003_ns::ImpellerType>(j, "impeller_type", x.impeller_type, x.impeller_type_is_set, false);
			a205_json_get<int>(j, "number_of_impellers", x.number_of_impellers, x.number_of_impellers_is_set, false);
		}
		const std::string_view ProductInformation::manufacturer_units = "";

		const std::string_view ProductInformation::model_number_units = "";

		const std::string_view ProductInformation::impeller_type_units = "";

		const std::string_view ProductInformation::number_of_impellers_units = "";

		const std::string_view ProductInformation::manufacturer_description = "Assembly/unit manufacturer name";

		const std::string_view ProductInformation::model_number_description = "Assembly/unit model number";

		const std::string_view ProductInformation::impeller_type_description = "Type of impeller in fan assembly";

		const std::string_view ProductInformation::number_of_impellers_description = "Number of impellers included in the fan assembly";

		const std::string_view ProductInformation::manufacturer_name = "manufacturer";

		const std::string_view ProductInformation::model_number_name = "model_number";

		const std::string_view ProductInformation::impeller_type_name = "impeller_type";

		const std::string_view ProductInformation::number_of_impellers_name = "number_of_impellers";

		void from_json(const nlohmann::json& j, Description& x) {
			a205_json_get<rs0003_ns::ProductInformation>(j, "product_information", x.product_information, x.product_information_is_set, false);
		}
		const std::string_view Description::product_information_units = "";

		const std::string_view Description::product_information_description = "Data group describing product information";

		const std::string_view Description::product_information_name = "product_information";

		void from_json(const nlohmann::json& j, AssemblyComponent& x) {
			a205_json_get<rs0003_ns::ComponentType>(j, "component_type", x.component_type, x.component_type_is_set, true);
			a205_json_get<std::string>(j, "component_description", x.component_description, x.component_description_is_set, false);
			a205_json_get<ashrae205_ns::UUID>(j, "component_id", x.component_id, x.component_id_is_set, false);
			a205_json_get<double>(j, "wet_pressure_difference", x.wet_pressure_difference, x.wet_pressure_difference_is_set, true);
		}
		const std::string_view AssemblyComponent::component_type_units = "";

		const std::string_view AssemblyComponent::component_description_units = "";

		const std::string_view AssemblyComponent::component_id_units = "";

		const std::string_view AssemblyComponent::wet_pressure_difference_units = "Pa";

		const std::string_view AssemblyComponent::component_type_description = "Type of component";

		const std::string_view AssemblyComponent::component_description_description = "Informative description of the component";

		const std::string_view AssemblyComponent::component_id_description = "Identifier of the corresponding Standard 205 representation";

		const std::string_view AssemblyComponent::wet_pressure_difference_description = "Additional static pressure difference if the component is wet (e.g., because of condensate collection or wetting evaporative media)";

		const std::string_view AssemblyComponent::component_type_name = "component_type";

		const std::string_view AssemblyComponent::component_description_name = "component_description";

		const std::string_view AssemblyComponent::component_id_name = "component_id";

		const std::string_view AssemblyComponent::wet_pressure_difference_name = "wet_pressure_difference";

		void from_json(const nlohmann::json& j, SystemCurve& x) {
			a205_json_get<std::vector<double>>(j, "standard_air_volumetric_flow_rate", x.standard_air_volumetric_flow_rate, x.standard_air_volumetric_flow_rate_is_set, true);
			a205_json_get<std::vector<double>>(j, "static_pressure_difference", x.static_pressure_difference, x.static_pressure_difference_is_set, true);
		}
		const std::string_view SystemCurve::standard_air_volumetric_flow_rate_units = "m3/s";

		const std::string_view SystemCurve::static_pressure_difference_units = "Pa";

		const std::string_view SystemCurve::standard_air_volumetric_flow_rate_description = "Volumetric air flow rate through an air distribution system at standard air conditions";

		const std::string_view SystemCurve::static_pressure_difference_description = "Static pressure difference of an air distribution system";

		const std::string_view SystemCurve::standard_air_volumetric_flow_rate_name = "standard_air_volumetric_flow_rate";

		const std::string_view SystemCurve::static_pressure_difference_name = "static_pressure_difference";

		void from_json(const nlohmann::json& j, Performance& x) {
			a205_json_get<double>(j, "nominal_standard_air_volumetric_flow_rate", x.nominal_standard_air_volumetric_flow_rate, x.nominal_standard_air_volumetric_flow_rate_is_set, true);
			a205_json_get<bool>(j, "is_enclosed", x.is_enclosed, x.is_enclosed_is_set, true);
			a205_json_get<std::vector<rs0003_ns::AssemblyComponent>>(j, "assembly_components", x.assembly_components, x.assembly_components_is_set, true);
			a205_json_get<double>(j, "heat_loss_fraction", x.heat_loss_fraction, x.heat_loss_fraction_is_set, true);
			a205_json_get<double>(j, "maximum_impeller_rotational_speed", x.maximum_impeller_rotational_speed, x.maximum_impeller_rotational_speed_is_set, true);
			a205_json_get<double>(j, "minimum_impeller_rotational_speed", x.minimum_impeller_rotational_speed, x.minimum_impeller_rotational_speed_is_set, true);
			a205_json_get<rs0003_ns::SystemCurve>(j, "stability_curve", x.stability_curve, x.stability_curve_is_set, false);
			a205_json_get<rs0003_ns::OperationSpeedControlType>(j, "operation_speed_control_type", x.operation_speed_control_type, x.operation_speed_control_type_is_set, true);
			a205_json_get<rs0003_ns::InstallationSpeedControlType>(j, "installation_speed_control_type", x.installation_speed_control_type, x.installation_speed_control_type_is_set, true);
			a205_json_get<rs0005_ns::RS0005>(j, "motor_representation", x.motor_representation, x.motor_representation_is_set, false);
			a205_json_get<rs0007_ns::RS0007>(j, "mechanical_drive_representation", x.mechanical_drive_representation, x.mechanical_drive_representation_is_set, false);
			if (x.operation_speed_control_type == OperationSpeedControlType::CONTINUOUS) {
				x.performance_map = std::make_unique<rs0003_ns::PerformanceMapContinuous>();
				if (x.performance_map) {
					x.performance_map->initialize(j.at("performance_map"));
				}
			}
			if (x.operation_speed_control_type == OperationSpeedControlType::DISCRETE) {
				x.performance_map = std::make_unique<rs0003_ns::PerformanceMapDiscrete>();
				if (x.performance_map) {
					x.performance_map->initialize(j.at("performance_map"));
				}
			}
		}
		const std::string_view Performance::nominal_standard_air_volumetric_flow_rate_units = "m3/s";

		const std::string_view Performance::is_enclosed_units = "";

		const std::string_view Performance::assembly_components_units = "";

		const std::string_view Performance::heat_loss_fraction_units = "-";

		const std::string_view Performance::maximum_impeller_rotational_speed_units = "rev/s";

		const std::string_view Performance::minimum_impeller_rotational_speed_units = "rev/s";

		const std::string_view Performance::stability_curve_units = "";

		const std::string_view Performance::operation_speed_control_type_units = "";

		const std::string_view Performance::installation_speed_control_type_units = "";

		const std::string_view Performance::motor_representation_units = "";

		const std::string_view Performance::mechanical_drive_representation_units = "";

		const std::string_view Performance::performance_map_units = "";

		const std::string_view Performance::nominal_standard_air_volumetric_flow_rate_description = "Nominal or rated air flow rate at standard air conditions";

		const std::string_view Performance::is_enclosed_description = "Fan assembly is enclosed";

		const std::string_view Performance::assembly_components_description = "An array of components included in the fan assembly air stream, not including any fans";

		const std::string_view Performance::heat_loss_fraction_description = "Fraction of efficiency losses transferred into the air stream";

		const std::string_view Performance::maximum_impeller_rotational_speed_description = "Maximum impeller rotational speed";

		const std::string_view Performance::minimum_impeller_rotational_speed_description = "Minimum impeller rotational speed";

		const std::string_view Performance::stability_curve_description = "The system curve defining the stability area for system selection";

		const std::string_view Performance::operation_speed_control_type_description = "Type of performance map";

		const std::string_view Performance::installation_speed_control_type_description = "Type of fan impeller speed control";

		const std::string_view Performance::motor_representation_description = "The corresponding Standard 205 motor representation";

		const std::string_view Performance::mechanical_drive_representation_description = "The corresponding Standard 205 mechanical drive representation";

		const std::string_view Performance::performance_map_description = "Data group describing fan assembly performance when operating";

		const std::string_view Performance::nominal_standard_air_volumetric_flow_rate_name = "nominal_standard_air_volumetric_flow_rate";

		const std::string_view Performance::is_enclosed_name = "is_enclosed";

		const std::string_view Performance::assembly_components_name = "assembly_components";

		const std::string_view Performance::heat_loss_fraction_name = "heat_loss_fraction";

		const std::string_view Performance::maximum_impeller_rotational_speed_name = "maximum_impeller_rotational_speed";

		const std::string_view Performance::minimum_impeller_rotational_speed_name = "minimum_impeller_rotational_speed";

		const std::string_view Performance::stability_curve_name = "stability_curve";

		const std::string_view Performance::operation_speed_control_type_name = "operation_speed_control_type";

		const std::string_view Performance::installation_speed_control_type_name = "installation_speed_control_type";

		const std::string_view Performance::motor_representation_name = "motor_representation";

		const std::string_view Performance::mechanical_drive_representation_name = "mechanical_drive_representation";

		const std::string_view Performance::performance_map_name = "performance_map";

		void from_json(const nlohmann::json& j, RS0003& x) {
			a205_json_get<ashrae205_ns::Metadata>(j, "metadata", x.metadata, x.metadata_is_set, true);
			a205_json_get<rs0003_ns::Description>(j, "description", x.description, x.description_is_set, false);
			a205_json_get<rs0003_ns::Performance>(j, "performance", x.performance, x.performance_is_set, true);
		}
		void RS0003::initialize(const nlohmann::json& j) {
			a205_json_get<ashrae205_ns::Metadata>(j, "metadata", metadata, metadata_is_set, true);
			a205_json_get<rs0003_ns::Description>(j, "description", description, description_is_set, false);
			a205_json_get<rs0003_ns::Performance>(j, "performance", performance, performance_is_set, true);
		}
		const std::string_view RS0003::metadata_units = "";

		const std::string_view RS0003::description_units = "";

		const std::string_view RS0003::performance_units = "";

		const std::string_view RS0003::metadata_description = "Metadata data group";

		const std::string_view RS0003::description_description = "Data group describing product and rating information";

		const std::string_view RS0003::performance_description = "Data group containing performance information";

		const std::string_view RS0003::metadata_name = "metadata";

		const std::string_view RS0003::description_name = "description";

		const std::string_view RS0003::performance_name = "performance";

		void from_json(const nlohmann::json& j, GridVariablesContinuous& x) {
			a205_json_get<std::vector<double>>(j, "standard_air_volumetric_flow_rate", x.standard_air_volumetric_flow_rate, x.standard_air_volumetric_flow_rate_is_set, true);
			a205_json_get<std::vector<double>>(j, "static_pressure_difference", x.static_pressure_difference, x.static_pressure_difference_is_set, true);
		}
		void GridVariablesContinuous::populate_performance_map(PerformanceMapBase* performance_map) {
			add_grid_axis(performance_map, standard_air_volumetric_flow_rate);
			add_grid_axis(performance_map, static_pressure_difference);
			performance_map->finalize_grid();
		}
		const std::string_view GridVariablesContinuous::standard_air_volumetric_flow_rate_units = "m3/s";

		const std::string_view GridVariablesContinuous::static_pressure_difference_units = "Pa";

		const std::string_view GridVariablesContinuous::standard_air_volumetric_flow_rate_description = "Volumetric air flow rate through fan assembly at standard air conditions";

		const std::string_view GridVariablesContinuous::static_pressure_difference_description = "External static pressure across fan assembly at dry coil conditions";

		const std::string_view GridVariablesContinuous::standard_air_volumetric_flow_rate_name = "standard_air_volumetric_flow_rate";

		const std::string_view GridVariablesContinuous::static_pressure_difference_name = "static_pressure_difference";

		void from_json(const nlohmann::json& j, LookupVariablesContinuous& x) {
			a205_json_get<std::vector<double>>(j, "impeller_rotational_speed", x.impeller_rotational_speed, x.impeller_rotational_speed_is_set, true);
			a205_json_get<std::vector<double>>(j, "shaft_power", x.shaft_power, x.shaft_power_is_set, true);
		}
		void LookupVariablesContinuous::populate_performance_map(PerformanceMapBase* performance_map) {
			add_data_table(performance_map, impeller_rotational_speed);
			add_data_table(performance_map, shaft_power);
		}
		const std::string_view LookupVariablesContinuous::impeller_rotational_speed_units = "rev/s";

		const std::string_view LookupVariablesContinuous::shaft_power_units = "W";

		const std::string_view LookupVariablesContinuous::impeller_rotational_speed_description = "Rotational speed of fan impeller";

		const std::string_view LookupVariablesContinuous::shaft_power_description = "Mechanical shaft power input to fan assembly";

		const std::string_view LookupVariablesContinuous::impeller_rotational_speed_name = "impeller_rotational_speed";

		const std::string_view LookupVariablesContinuous::shaft_power_name = "shaft_power";

		void from_json(const nlohmann::json& j, PerformanceMapContinuous& x) {
			a205_json_get<rs0003_ns::GridVariablesContinuous>(j, "grid_variables", x.grid_variables, x.grid_variables_is_set, true);
			x.grid_variables.populate_performance_map(&x);
			a205_json_get<rs0003_ns::LookupVariablesContinuous>(j, "lookup_variables", x.lookup_variables, x.lookup_variables_is_set, true);
			x.lookup_variables.populate_performance_map(&x);
		}
		void PerformanceMapContinuous::initialize(const nlohmann::json& j) {
			a205_json_get<rs0003_ns::GridVariablesContinuous>(j, "grid_variables", grid_variables, grid_variables_is_set, true);
			grid_variables.populate_performance_map(this);
			a205_json_get<rs0003_ns::LookupVariablesContinuous>(j, "lookup_variables", lookup_variables, lookup_variables_is_set, true);
			lookup_variables.populate_performance_map(this);
		}
		const std::string_view PerformanceMapContinuous::grid_variables_units = "";

		const std::string_view PerformanceMapContinuous::lookup_variables_units = "";

		const std::string_view PerformanceMapContinuous::grid_variables_description = "Data group describing grid variables for continuous fan performance";

		const std::string_view PerformanceMapContinuous::lookup_variables_description = "Data group describing lookup variables for continuous fan performance";

		const std::string_view PerformanceMapContinuous::grid_variables_name = "grid_variables";

		const std::string_view PerformanceMapContinuous::lookup_variables_name = "lookup_variables";

		LookupVariablesContinuousStruct PerformanceMapContinuous::calculate_performance(double standard_air_volumetric_flow_rate, double static_pressure_difference) {
			std::vector<double> target {standard_air_volumetric_flow_rate, static_pressure_difference};
			auto v = PerformanceMapBase::calculate_performance(target);
			LookupVariablesContinuousStruct s {v[0], v[1], };
			return s;
		}
		void from_json(const nlohmann::json& j, GridVariablesDiscrete& x) {
			a205_json_get<std::vector<int>>(j, "speed_number", x.speed_number, x.speed_number_is_set, true);
			a205_json_get<std::vector<double>>(j, "static_pressure_difference", x.static_pressure_difference, x.static_pressure_difference_is_set, true);
		}
		void GridVariablesDiscrete::populate_performance_map(PerformanceMapBase* performance_map) {
			add_grid_axis(performance_map, speed_number);
			add_grid_axis(performance_map, static_pressure_difference);
			performance_map->finalize_grid();
		}
		const std::string_view GridVariablesDiscrete::speed_number_units = "-";

		const std::string_view GridVariablesDiscrete::static_pressure_difference_units = "Pa";

		const std::string_view GridVariablesDiscrete::speed_number_description = "Number indicating discrete speed of fan impeller in rank order (with 1 being the lowest speed)";

		const std::string_view GridVariablesDiscrete::static_pressure_difference_description = "External static pressure across fan assembly at dry coil conditions";

		const std::string_view GridVariablesDiscrete::speed_number_name = "speed_number";

		const std::string_view GridVariablesDiscrete::static_pressure_difference_name = "static_pressure_difference";

		void from_json(const nlohmann::json& j, LookupVariablesDiscrete& x) {
			a205_json_get<std::vector<double>>(j, "standard_air_volumetric_flow_rate", x.standard_air_volumetric_flow_rate, x.standard_air_volumetric_flow_rate_is_set, true);
			a205_json_get<std::vector<double>>(j, "shaft_power", x.shaft_power, x.shaft_power_is_set, true);
			a205_json_get<std::vector<double>>(j, "impeller_rotational_speed", x.impeller_rotational_speed, x.impeller_rotational_speed_is_set, true);
		}
		void LookupVariablesDiscrete::populate_performance_map(PerformanceMapBase* performance_map) {
			add_data_table(performance_map, standard_air_volumetric_flow_rate);
			add_data_table(performance_map, shaft_power);
			add_data_table(performance_map, impeller_rotational_speed);
		}
		const std::string_view LookupVariablesDiscrete::standard_air_volumetric_flow_rate_units = "m3/s";

		const std::string_view LookupVariablesDiscrete::shaft_power_units = "W";

		const std::string_view LookupVariablesDiscrete::impeller_rotational_speed_units = "rev/s";

		const std::string_view LookupVariablesDiscrete::standard_air_volumetric_flow_rate_description = "Volumetric air flow rate through fan assembly at standard air conditions";

		const std::string_view LookupVariablesDiscrete::shaft_power_description = "Mechanical shaft power input to fan assembly";

		const std::string_view LookupVariablesDiscrete::impeller_rotational_speed_description = "Rotational speed of fan impeller";

		const std::string_view LookupVariablesDiscrete::standard_air_volumetric_flow_rate_name = "standard_air_volumetric_flow_rate";

		const std::string_view LookupVariablesDiscrete::shaft_power_name = "shaft_power";

		const std::string_view LookupVariablesDiscrete::impeller_rotational_speed_name = "impeller_rotational_speed";

		void from_json(const nlohmann::json& j, PerformanceMapDiscrete& x) {
			a205_json_get<rs0003_ns::GridVariablesDiscrete>(j, "grid_variables", x.grid_variables, x.grid_variables_is_set, true);
			x.grid_variables.populate_performance_map(&x);
			a205_json_get<rs0003_ns::LookupVariablesDiscrete>(j, "lookup_variables", x.lookup_variables, x.lookup_variables_is_set, true);
			x.lookup_variables.populate_performance_map(&x);
		}
		void PerformanceMapDiscrete::initialize(const nlohmann::json& j) {
			a205_json_get<rs0003_ns::GridVariablesDiscrete>(j, "grid_variables", grid_variables, grid_variables_is_set, true);
			grid_variables.populate_performance_map(this);
			a205_json_get<rs0003_ns::LookupVariablesDiscrete>(j, "lookup_variables", lookup_variables, lookup_variables_is_set, true);
			lookup_variables.populate_performance_map(this);
		}
		const std::string_view PerformanceMapDiscrete::grid_variables_units = "";

		const std::string_view PerformanceMapDiscrete::lookup_variables_units = "";

		const std::string_view PerformanceMapDiscrete::grid_variables_description = "Data group describing grid variables for discrete fan performance";

		const std::string_view PerformanceMapDiscrete::lookup_variables_description = "Data group describing lookup variables for discrete fan performance";

		const std::string_view PerformanceMapDiscrete::grid_variables_name = "grid_variables";

		const std::string_view PerformanceMapDiscrete::lookup_variables_name = "lookup_variables";

		LookupVariablesDiscreteStruct PerformanceMapDiscrete::calculate_performance(double speed_number, double static_pressure_difference) {
			std::vector<double> target {speed_number, static_pressure_difference};
			auto v = PerformanceMapBase::calculate_performance(target);
			LookupVariablesDiscreteStruct s {v[0], v[1], v[2], };
			return s;
		}
	}
}

