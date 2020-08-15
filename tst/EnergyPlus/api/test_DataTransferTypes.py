import unittest

from pyenergyplus.api import EnergyPlusAPI
from pyenergyplus.common import EnergyPlusException

api = EnergyPlusAPI()


class TestPythonAPITypes(unittest.TestCase):
    """
    py.test on EnergyPlusAPI.exchange functions to see type checking in action
    """

    def setUp(self):
        self.state = api.runtime.new_state()

    def test_get_actuator_handle(self):

        self.assertEqual(api.exchange.get_actuator_handle(
            self.state, u"Weather Data", u"Outdoor Dew Point", u"Environment"), -1)

        # Wrong number of arguments
        with self.assertRaises(TypeError) as cm:
            api.exchange.get_actuator_handle(
                self.state, u"Weather Data", u"Outdoor Dew Point")
        self.assertIn("get_actuator_handle() missing 1 required positional argument: 'actuator_key'", str(cm.exception))

        with self.assertRaises(EnergyPlusException) as cm:
            api.exchange.get_actuator_handle(
                self.state, 1, u"Outdoor Dew Point", u"Environment")
            self.assertIn("`get_actuator_handle` expects `component_type` as a `str` or UTF-8 encoded `bytes`, not '1'", str(cm.exception))

        with self.assertRaises(EnergyPlusException) as cm:
            api.exchange.get_actuator_handle(
                self.state, u"Weather Data", 2, u"Environment")
            self.assertIn("`get_actuator_handle` expects `control_type` as a `str` or UTF-8 encoded `bytes`, not '2'", str(cm.exception))

        with self.assertRaises(EnergyPlusException) as cm:
            api.exchange.get_actuator_handle(
                self.state, u"Weather Data", u"Outdoor Dew Point", 3)
            self.assertIn("`get_actuator_handle` expects `actuator_key` as a `str` or UTF-8 encoded `bytes`, not '3'", str(cm.exception))

    def test_get_variable_value(self):
        self.assertEqual(api.exchange.get_variable_value(self.state, -1), 0.0)

        with self.assertRaises(TypeError) as cm:
            api.exchange.get_variable_value(self.state)
            self.assertIn("get_variable_value() missing 1 required positional argument: 'variable_handle'", str(cm.exception))

        with self.assertRaises(EnergyPlusException) as cm:
            api.exchange.get_variable_value(self.state, u"foo")
            self.assertIn("`get_variable_value` expects `variable_handle` as a `str` or UTF-8 encoded `bytes`, not 'foo'", str(cm.exception))


if __name__ == '__main__':
    unittest.main()
