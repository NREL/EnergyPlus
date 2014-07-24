set( target_name EnergyPlus_unit )

if(MSVC)
  ADD_DEFINITIONS(/Za) # Disables MS language extensions
endif()


set( test_src
  DataPlant.unit.cc
  SortAndStringUtilities.unit.cc
  Vectors.unit.cc
  Vector.unit.cc
)

ADD_EXECUTABLE( ${target_name} main.unit.cc ${tst_src} )

CREATE_TEST_TARGETS( ${target_name} "${test_src}" "" )
