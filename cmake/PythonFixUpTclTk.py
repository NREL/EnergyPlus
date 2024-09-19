import platform
import re
import shutil
import subprocess
import sys
from pathlib import Path


def locate_tk_so(python_dir: Path) -> Path:
    sos = list(python_dir.glob("lib-dynload/_tkinter*.so"))
    assert len(sos) == 1, "Unable to locate _tkinter so"
    return sos[0]


LINKED_RE = re.compile(
    r"(?P<libname>.*) \(compatibility version (?P<compat_version>\d+\.\d+\.\d+), "
    r"current version (?P<current_version>\d+\.\d+\.\d+)(?:, \w+)?\)"
)

LINKED_RE_ARM64 = re.compile(f"(?P<libname>.*) \(architecture arm64\)")


def get_linked_libraries(p: Path):
    linked_libs = []
    lines = subprocess.check_output(["otool", "-L", str(p)], encoding="utf-8", universal_newlines=True).splitlines()
    if "is not an object file" in lines[0]:
        return None
    lines = [x.strip() for x in lines[1:]]

    for line in lines:
        if m := LINKED_RE.match(line):
            linked_libs.append(m.groupdict())
        elif m := LINKED_RE_ARM64.match(line):
            linked_libs.append(m.groupdict())  # it will only have a libname key, I think that's fine
        else:
            raise ValueError(f"For {p}, cannot parse line: '{line}'")
    return linked_libs


if __name__ == "__main__":

    if platform.system() != "Darwin":
        sys.exit(0)

    print("PYTHON: Copying and fixing up Tcl/Tk")

    if len(sys.argv) == 2:
        python_dir = Path(sys.argv[1])
    else:
        print("Must call " + sys.argv[0] + "with one command line argument: the path to the python_lib directory")
        sys.exit(1)

    assert python_dir.is_dir()
    lib_dynload_dir = python_dir / "lib-dynload"

    tk_so = locate_tk_so(python_dir)
    tcl_tk_sos = [Path(t["libname"]) for t in get_linked_libraries(tk_so) if "libt" in t["libname"]]

    for tcl_tk_so in tcl_tk_sos:
        new_tcl_tk_so = lib_dynload_dir / tcl_tk_so.name
        shutil.copy(tcl_tk_so, new_tcl_tk_so)
        lines = subprocess.check_output(
            ["install_name_tool", "-change", str(tcl_tk_so), f"@loader_path/{new_tcl_tk_so.name}", str(tk_so)]
        )
        # Change the id that's the first line of the otool -L in this case and it's confusing
        lines = subprocess.check_output(["install_name_tool", "-id", str(new_tcl_tk_so.name), str(new_tcl_tk_so)])
