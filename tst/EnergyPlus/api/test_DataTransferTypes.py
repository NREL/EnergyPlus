import pytest

from pyenergyplus.api import EnergyPlusAPI
from pyenergyplus.common import EnergyPlusException

api = EnergyPlusAPI()

class TestPythonAPITypes():
    """
    py.test on EnergyPlusAPI.exchange functions to see type checking in action
    """

    def test_get_actuator_handle(self):

        assert (-1 == api.exchange.get_actuator_handle(
            u"Weather Data", u"Outdoor Dew Point", u"Environment"))

        # Wrong number of arguments
        with pytest.raises(TypeError) as e:
            api.exchange.get_actuator_handle(
                u"Weather Data", u"Outdoor Dew Point")
            assert ("get_actuator_handle() missing 1 required positional argument: 'actuator_key'" in str(e.value))

        with pytest.raises(EnergyPlusException) as e:
            api.exchange.get_actuator_handle(
                1, u"Outdoor Dew Point", u"Environment")
            assert ("`get_actuator_handle` expects `component_type` as a `str` or UTF-8 encoded `bytes`, not '1'" in str(e.value))

        with pytest.raises(EnergyPlusException) as e:
            api.exchange.get_actuator_handle(
                u"Weather Data", 2, u"Environment")
            assert ("`get_actuator_handle` expects `control_type` as a `str` or UTF-8 encoded `bytes`, not '2'" in str(e.value))

        with pytest.raises(EnergyPlusException) as e:
            api.exchange.get_actuator_handle(
                u"Weather Data", u"Outdoor Dew Point", 3)
            assert ("`get_actuator_handle` expects `actuator_key` as a `str` or UTF-8 encoded `bytes`, not '3'" in str(e.value))


    def test_get_variable_value(self):
        assert(0.0 == api.exchange.get_variable_value(-1))

        with pytest.raises(TypeError) as e:
            api.exchange.get_variable_value()
            assert ("get_variable_value() missing 1 required positional argument: 'variable_handle'" in str(e.value))

        with pytest.raises(EnergyPlusException) as e:
            api.exchange.get_variable_value(u"foo")
            assert ("`get_variable_value` expects `variable_handle` as a `str` or UTF-8 encoded `bytes`, not 'foo'" in str(e.value))
