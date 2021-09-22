from pathlib import Path

from tests.simulations import BaseSimulationTest

test_dir = Path(__file__).parent.parent.parent


class TestSimulationsBase(BaseSimulationTest):
    def setUp(self):
        return

    def teardown(self):
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Full:Base:HVACTemplate-5ZoneBaseboardHeat")
    def test_5_zone_baseboard_heat(self):
        base_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZoneBaseboardHeat.idf')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Full:Base:HVACTemplate-5ZoneConstantVolumeChillerBoiler")
    def test_5_zone_constant_volume_chiller_boiler(self):
        base_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZoneConstantVolumeChillerBoiler.idf')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Full:Base:HVACTemplate-5ZoneDualDuct")
    def test_5_zone_dual_duct(self):
        base_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZoneDualDuct.idf')
        # Warnings turned off due to naming mismatch of output variables.
        self.perform_full_comparison(base_idf_file_path=base_file_path, warning_check=False)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Full:Base:HVACTemplate-5ZoneFanCoil")
    def test_5_zone_fan_coil(self):
        base_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZoneFanCoil.idf')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Full:Base:HVACTemplate-5ZoneFanCoil-DOAS")
    def test_5_zone_fan_coil_doas(self):
        base_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZoneFanCoil-DOAS.idf')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Full:Base:HVACTemplate-5ZoneFurnaceDX")
    def test_5_zone_furnace_dx(self):
        base_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZoneFurnaceDX.idf')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Full:Base:HVACTemplate-5ZonePacakagedVAV")
    def test_5_zone_packaged_vav(self):
        base_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZonePackagedVAV.idf')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Full:Base:HVACTemplate-5ZonePTAC")
    def test_5_zone_ptac(self):
        base_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZonePTAC.idf')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Full:Base:HVACTemplate-5ZonePTAC-DOAS")
    def test_5_zone_ptac_doas(self):
        base_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZonePTAC-DOAS.idf')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Full:Base:HVACTemplate-5ZonePTHP")
    def test_5_zone_pthp(self):
        base_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZonePTHP.idf')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Full:Base:HVACTemplate-5ZonePurchAir")
    def test_5_zone_purch_air(self):
        base_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZonePurchAir.idf')
        # Warnings turned off due to naming mismatch of output variables.
        self.perform_full_comparison(base_idf_file_path=base_file_path, warning_check=False)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Full:Base:HVACTemplate-5ZoneUnitaryHeatPump")
    def test_5_zone_unitary_heat_pump(self):
        # todo_eo: cooling_design_supply_air_temperature should affect minimum_supply_air_temperature, but does not
        #  currently in EO.
        base_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZoneUnitaryHeatPump.idf')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Full:Base:HVACTemplate-5ZoneUnitarySystem")
    def test_5_zone_unitary_system(self):
        base_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZoneUnitarySystem.idf')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Full:Base:HVACTemplate-5ZoneVAVFanPowered")
    def test_5_zone_vav_fan_powered(self):
        base_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZoneVAVFanPowered.idf')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Full:Base:HVACTemplate-5ZoneVAVWaterCooled")
    def test_5_zone_vav_water_cooled(self):
        base_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZoneVAVWaterCooled.idf')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Full:Base:HVACTemplate-5ZoneVAVWaterCooled-ObjectReference")
    def test_5_zone_vav_water_cooled_object_reference(self):
        base_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZoneVAVWaterCooled-ObjectReference.idf')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Full:Base:HVACTemplate-5ZoneVRF")
    def test_5_zone_vrf(self):
        base_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZoneVRF.idf')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Full:Base:HVACTemplate-5ZoneWaterToAirHeatPumpTowerBoiler")
    def test_5_zone_water_to_air_heat_pump_tower_boiler(self):
        base_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZoneWaterToAirHeatPumpTowerBoiler.idf')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return
