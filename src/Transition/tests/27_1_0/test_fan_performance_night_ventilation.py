def test_fan_performance_night_ventilation_removes_maximum_flow_rate(prepare_and_run_transition):
    idf_text = """
  FanPerformance:NightVentilation,
    Explicit Maximum Flow,
    0.7,
    67.0,
    Autosize,
    0.9,
    1.0;

  FanPerformance:NightVentilation,
    Blank Maximum Flow,
    0.6,
    55.0,
    ,
    0.8;
    """

    expected_idf_text = """
  FanPerformance:NightVentilation,
    Explicit Maximum Flow,  !- Fan Name
    0.7,                     !- Fan Total Efficiency
    67.0,                    !- Pressure Rise {Pa}
    0.9,                     !- Motor Efficiency
    1.0;                     !- Motor in Airstream Fraction

  FanPerformance:NightVentilation,
    Blank Maximum Flow,      !- Fan Name
    0.6,                     !- Fan Total Efficiency
    55.0,                    !- Pressure Rise {Pa}
    0.8;                     !- Motor Efficiency
"""

    prepare_and_run_transition(
        ori_idf_text=idf_text,
        expected_idf_text=expected_idf_text,
        version_ori=(26, 2, 0),
        version_new=(27, 1, 0),
    )