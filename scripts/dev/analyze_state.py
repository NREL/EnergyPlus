from pathlib import Path
from typing import List, Set
from re import compile


class StateClass:
    def __init__(self, struct_name: str, instance_name: str):
        self.struct_name = struct_name
        self.instance_name = instance_name
        self.constructed = False
        self.construction_call_matches_struct = False
        self.clear_state_executed = False

    def validate(self) -> bool:
        """
        Checks all the identified conditions are met and if not issues warnings.

        :return: Returns True if the state class is validated or False if not
        """
        valid = True
        if not self.constructed:
            print("::warning file=EnergyPlusData.cc::State not constructed: " + self.instance_name)
            valid = False

        if not self.construction_call_matches_struct:
            print("::warning file=EnergyPlusData.cc::State constructed with different type: " + self.instance_name)
            valid = False

        if not self.clear_state_executed:
            print("::warning file=EnergyPlusData.cc::State clear_state() call missing: " + self.instance_name)
            valid = False
        return valid


class StateChecker:

    def __init__(self, repo_root_path: Path):
        self.repo_root = repo_root_path
        self.member_variables: Set[StateClass] = set()

        # read the state file contents -- if we ever split the data into files this will require modification
        header_file_path = self.repo_root / 'src' / 'EnergyPlus' / 'Data' / 'EnergyPlusData.hh'
        self.header_file_lines: List[str] = header_file_path.open().readlines()
        source_file_path = self.repo_root / 'src' / 'EnergyPlus' / 'Data' / 'EnergyPlusData.cc'
        self.source_file_lines: List[str] = source_file_path.open().readlines()

    def determine_member_variables(self) -> None:
        """
        Finds all member variables using a pretty simplistic clue, the unique_ptr declaration.
        This would change if we ever go to raw pointers or another storage method.
        Currently it looks for lines of the form:
             std::unique_ptr<AirflowNetworkBalanceManagerData> dataAirflowNetworkBalanceManager;
        """
        pattern = compile(r'\s*std::unique_ptr<(\w+)> (\w+);')
        for li in self.header_file_lines:
            m = pattern.match(li)
            if m:
                underlying_struct_name = m.group(1)
                member_var_name = m.group(2)
                self.member_variables.add(StateClass(underlying_struct_name, member_var_name))

    def validate_member_variable_construction(self):
        """
        Validates that the state member variables are constructed using a clue of the `make_unique` function.
        """
        pattern = compile(r'\s*this->(\w+) = std::make_unique<(\w+)>\(\);')
        for li in self.source_file_lines:
            if li.strip().startswith('#'):
                continue
            m = pattern.match(li)
            if m:
                member_var_name = m.group(1)
                underlying_struct_name = m.group(2)
                for v in self.member_variables:
                    if v.instance_name == member_var_name:
                        v.constructed = True
                        if v.struct_name == underlying_struct_name:
                            v.construction_call_matches_struct = True

    def validate_member_variable_clear_state(self):
        """
        Validates the member's clear_state call is made in an uncommented line
        """
        pattern = compile(r'\s*this->(\w+)->clear_state\(\);')
        for li in self.source_file_lines:
            if li.strip().startswith('#'):
                continue
            m = pattern.match(li)
            if m:
                member_var_name = m.group(1)
                for v in self.member_variables:
                    if v.instance_name == member_var_name:
                        v.clear_state_executed = True


if __name__ == '__main__':
    this_file_path = Path(__file__).resolve()  # should be in scripts/dev
    repo_root = this_file_path.parent.parent.parent  # dev, scripts, repo_root
    sc = StateChecker(repo_root)
    sc.determine_member_variables()
    sc.validate_member_variable_construction()
    sc.validate_member_variable_clear_state()
    all_good = True
    for mv in sc.member_variables:
        if not mv.validate():
            all_good = False
    if not all_good:
        print("::error file=EnergyPlusData.cc::Problems with State Variables!")
