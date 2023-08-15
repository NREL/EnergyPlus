import pytest
import idd_parser


# If this timeouts while you make modifications, you can do `pip install pytest-timeout` and use this mark here:
# @pytest.mark.timeout(2)
def test_ok_idd():
    data = idd_parser.Data()
    data.file = r"""
! Comment

\group test

OkObject,
  A1; \field Name
      \minimum 1

NotBrokenObject,
  A1, \field Name
  N2; \field Last Field
      \maximum 10

Obj2,
  A1; \field Name
"""
    idd_parser.parse_idd(data)
    assert data.schema["properties"].keys() == {"OkObject", "NotBrokenObject", "Obj2"}
    assert data.schema["properties"]["NotBrokenObject"] == {
        "patternProperties": {
            ".*": {"type": "object", "properties": {"last_field": {"type": "number", "maximum": 10.0}}}
        },
        "group": "test",
        "name": {"type": "string"},
        "legacy_idd": {
            "field_info": {
                "name": {"field_name": "Name", "field_type": "a"},
                "last_field": {"field_name": "Last Field", "field_type": "n"},
            },
            "fields": ["name", "last_field"],
            "alphas": {"fields": ["name"]},
            "numerics": {"fields": ["last_field"]},
        },
        "type": "object",
    }


def test_broken_idd():
    data = idd_parser.Data()
    data.file = r"""! Comment

\group test

OkObject,
  A1; \field Name
      \minimum 1

BrokenObject,
  A1, \field Name
  A2, \field Last Field
      \maximum 10

Obj2,
  A1; \field Name
"""
    with pytest.raises(idd_parser.MissingSemiColonException) as e:
        idd_parser.parse_idd(data)

    assert (
        str(e.value)
        == r"""In object 'BrokenObject', Missing semi-colon in field A2
Context:
  A1; \field Name
      \minimum 1

BrokenObject,
  A1, \field Name
  A2, \field Last Field
    ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      \maximum 10

Obj2,
  A1; \field Name"""
    )


def test_broken_idd_2():
    data = idd_parser.Data()
    data.file = r"""BrokenObject,
  A1 \field Name
  A2, \field Other
"""
    with pytest.raises(idd_parser.IddParsingError) as e:
        idd_parser.parse_idd(data)
    assert (
        str(e.value)
        == r"""In object 'BrokenObject', No comma or semicolon after field A1
Context:
BrokenObject,
  A1 \field Name
    ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  A2, \field Other"""
    )


def test_broken_idd_3():
    data = idd_parser.Data()
    data.file = r"""BrokenObject,
      \obsolete
  A1; \field Name

"""
    with pytest.raises(idd_parser.IddParsingError) as e:
        idd_parser.parse_idd(data)
    assert (
        str(e.value)
        == r"""In object 'BrokenObject', expected string after /obsolete
Context:
BrokenObject,
      \obsolete
                ^~~~~~~~~~~~~~~~~~~~~~~~~
  A1; \field Name
"""
    )


def test_duplicate_a_field_number():
    data = idd_parser.Data()
    data.file = r"""BrokenObject,
  A1, \field Name
  N1, \field Number 1
  A1, \field String Field
  N2; \field Number 1
"""
    with pytest.raises(idd_parser.IddParsingError) as e:
        idd_parser.parse_idd(data)
    assert (
        str(e.value)
        == r"""In object 'BrokenObject', duplicate field number A1
Context:
BrokenObject,
  A1, \field Name
  N1, \field Number 1
  A1, \field String Field
     ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  N2; \field Number 1"""
    )


def test_duplicate_n_field_number():
    data = idd_parser.Data()
    data.file = r"""BrokenObject,
  A1, \field Name
  N1, \field Number 1
  A2, \field String Field
  N1; \field Number 1
"""
    with pytest.raises(idd_parser.IddParsingError) as e:
        idd_parser.parse_idd(data)
    assert (
        str(e.value)
        == r"""In object 'BrokenObject', duplicate field number N1
Context:
BrokenObject,
  A1, \field Name
  N1, \field Number 1
  A2, \field String Field
  N1; \field Number 1
     ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"""
    )
