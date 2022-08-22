#include <rs0006.h>
#include <loadobject_205.h>

namespace tk205  {

	namespace rs0006_ns  {
	
		void from_json(const nlohmann::json& j, ProductInformation& x) {
			a205_json_get<std::string>(j, "manufacturer", x.manufacturer, x.manufacturer_is_set, false);
			a205_json_get<ashrae205_ns::Pattern>(j, "model_number", x.model_number, x.model_number_is_set, false);
		}
		const std::string_view ProductInformation::manufacturer_units = "";

		const std::string_view ProductInformation::model_number_units = "";

		const std::string_view ProductInformation::manufacturer_description = "Manufacturer name";

		const std::string_view ProductInformation::model_number_description = "Model number";

		const std::string_view ProductInformation::manufacturer_name = "manufacturer";

		const std::string_view ProductInformation::model_number_name = "model_number";

		void from_json(const nlohmann::json& j, Description& x) {
			a205_json_get<rs0006_ns::ProductInformation>(j, "product_information", x.product_information, x.product_information_is_set, false);
		}
		const std::string_view Description::product_information_units = "";

		const std::string_view Description::product_information_description = "Data group describing product information";

		const std::string_view Description::product_information_name = "product_information";

		void from_json(const nlohmann::json& j, GridVariables& x) {
			a205_json_get<std::vector<double>>(j, "output_power", x.output_power, x.output_power_is_set, true);
			a205_json_get<std::vector<double>>(j, "output_frequency", x.output_frequency, x.output_frequency_is_set, true);
		}
		void GridVariables::populate_performance_map(PerformanceMapBase* performance_map) {
			add_grid_axis(performance_map, output_power);
			add_grid_axis(performance_map, output_frequency);
			performance_map->finalize_grid();
		}
		const std::string_view GridVariables::output_power_units = "W";

		const std::string_view GridVariables::output_frequency_units = "Hz";

		const std::string_view GridVariables::output_power_description = "Power delivered to the motor";

		const std::string_view GridVariables::output_frequency_description = "Frequency delivered to the motor";

		const std::string_view GridVariables::output_power_name = "output_power";

		const std::string_view GridVariables::output_frequency_name = "output_frequency";

		void from_json(const nlohmann::json& j, LookupVariables& x) {
			a205_json_get<std::vector<double>>(j, "efficiency", x.efficiency, x.efficiency_is_set, true);
		}
		void LookupVariables::populate_performance_map(PerformanceMapBase* performance_map) {
			add_data_table(performance_map, efficiency);
		}
		const std::string_view LookupVariables::efficiency_units = "-";

		const std::string_view LookupVariables::efficiency_description = "Efficiency of drive";

		const std::string_view LookupVariables::efficiency_name = "efficiency";

		void from_json(const nlohmann::json& j, PerformanceMap& x) {
			a205_json_get<rs0006_ns::GridVariables>(j, "grid_variables", x.grid_variables, x.grid_variables_is_set, true);
			x.grid_variables.populate_performance_map(&x);
			a205_json_get<rs0006_ns::LookupVariables>(j, "lookup_variables", x.lookup_variables, x.lookup_variables_is_set, true);
			x.lookup_variables.populate_performance_map(&x);
		}
		void PerformanceMap::initialize(const nlohmann::json& j) {
			a205_json_get<rs0006_ns::GridVariables>(j, "grid_variables", grid_variables, grid_variables_is_set, true);
			grid_variables.populate_performance_map(this);
			a205_json_get<rs0006_ns::LookupVariables>(j, "lookup_variables", lookup_variables, lookup_variables_is_set, true);
			lookup_variables.populate_performance_map(this);
		}
		const std::string_view PerformanceMap::grid_variables_units = "";

		const std::string_view PerformanceMap::lookup_variables_units = "";

		const std::string_view PerformanceMap::grid_variables_description = "Data group describing grid variables for drive performance";

		const std::string_view PerformanceMap::lookup_variables_description = "Data group describing lookup variables for drive performance";

		const std::string_view PerformanceMap::grid_variables_name = "grid_variables";

		const std::string_view PerformanceMap::lookup_variables_name = "lookup_variables";

		LookupVariablesStruct PerformanceMap::calculate_performance(double output_power, double output_frequency) {
			std::vector<double> target {output_power, output_frequency};
			auto v = PerformanceMapBase::calculate_performance(target);
			LookupVariablesStruct s {v[0], };
			return s;
		}
		void from_json(const nlohmann::json& j, Performance& x) {
			a205_json_get<double>(j, "maximum_power", x.maximum_power, x.maximum_power_is_set, true);
			a205_json_get<double>(j, "standby_power", x.standby_power, x.standby_power_is_set, true);
			a205_json_get<rs0006_ns::CoolingMethod>(j, "cooling_method", x.cooling_method, x.cooling_method_is_set, true);
			a205_json_get<rs0006_ns::PerformanceMap>(j, "performance_map", x.performance_map, x.performance_map_is_set, true);
		}
		const std::string_view Performance::maximum_power_units = "W";

		const std::string_view Performance::standby_power_units = "W";

		const std::string_view Performance::cooling_method_units = "";

		const std::string_view Performance::performance_map_units = "";

		const std::string_view Performance::maximum_power_description = "Maximum power draw of the drive";

		const std::string_view Performance::standby_power_description = "Power draw when the motor is not operating";

		const std::string_view Performance::cooling_method_description = "Method used to cool the drive";

		const std::string_view Performance::performance_map_description = "Data group describing drive performance when operating";

		const std::string_view Performance::maximum_power_name = "maximum_power";

		const std::string_view Performance::standby_power_name = "standby_power";

		const std::string_view Performance::cooling_method_name = "cooling_method";

		const std::string_view Performance::performance_map_name = "performance_map";

		void from_json(const nlohmann::json& j, RS0006& x) {
			a205_json_get<ashrae205_ns::Metadata>(j, "metadata", x.metadata, x.metadata_is_set, true);
			a205_json_get<rs0006_ns::Description>(j, "description", x.description, x.description_is_set, false);
			a205_json_get<rs0006_ns::Performance>(j, "performance", x.performance, x.performance_is_set, true);
		}
		void RS0006::initialize(const nlohmann::json& j) {
			a205_json_get<ashrae205_ns::Metadata>(j, "metadata", metadata, metadata_is_set, true);
			a205_json_get<rs0006_ns::Description>(j, "description", description, description_is_set, false);
			a205_json_get<rs0006_ns::Performance>(j, "performance", performance, performance_is_set, true);
		}
		const std::string_view RS0006::metadata_units = "";

		const std::string_view RS0006::description_units = "";

		const std::string_view RS0006::performance_units = "";

		const std::string_view RS0006::metadata_description = "Metadata data group";

		const std::string_view RS0006::description_description = "Data group describing product and rating information";

		const std::string_view RS0006::performance_description = "Data group containing performance information";

		const std::string_view RS0006::metadata_name = "metadata";

		const std::string_view RS0006::description_name = "description";

		const std::string_view RS0006::performance_name = "performance";

	}
}

