import pytest
from org_roam_sem.featurize import clean_link_context

@pytest.mark.parametrize("input, output", [
    ("+ [2024-06-05] The gnocchi was not very nice texture wise, also the sauce was too tangy. [[id:50cb08d1-a4b9-427d-9df6-378eee0e10ff][Syntax]] also said she found it too spicy.", "+ [2024-06-05] The gnocchi was not very nice texture wise, also the sauce was too tangy. Syntax also said she found it too spicy."),
    ("+ [2024-06-05] The gnocchi was not very nice texture wise, also the sauce was too tangy. [[id:50cb08d1-a4b9-427d-9df6-378eee0e10ff]] also said she found it too spicy.", "+ [2024-06-05] The gnocchi was not very nice texture wise, also the sauce was too tangy.  also said she found it too spicy."),
    ("+ [2024-06-05] The gnocchi was not very nice texture wise, also the sauce was too tangy. [[id:50cb08d1-a4b9-427d-9df6-378eee0e10ff][Synta", "+ [2024-06-05] The gnocchi was not very nice texture wise, also the sauce was too tangy. Synta"),
    ("+ [2024-06-05] The gnocchi was not very nice texture wise, also the sauce was too tangy. [[id:50cb08d1-a4b9-427d-9df6-378eee0e10", "+ [2024-06-05] The gnocchi was not very nice texture wise, also the sauce was too tangy. "),
    ("0cb08d1-a4b9-427d-9df6-378eee0e10ff][Syntax]] also said she found it too spicy.", "Syntax also said she found it too spicy.")
])
def test_link_context_cleaning(input, output):
    assert clean_link_context(input) == output
