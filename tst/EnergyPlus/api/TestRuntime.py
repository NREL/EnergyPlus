import sys
from pyenergyplus.api import EnergyPlusAPI


def environment_handler() -> None:
    print("OH HAI ENVIRONMENT")
    sys.stdout.flush()


def progress_handler(progress: int) -> None:
    if 49 < progress < 51:
        print("HALFWAY THERE!!")
        sys.stdout.flush()


def error_handler(message: bytes) -> None:
    if b'Warning' in message:
        print("GOT A WARNING UH OH!")
        sys.stdout.flush()


api = EnergyPlusAPI()
api.runtime.callback_begin_new_environment(environment_handler)
api.runtime.callback_progress(progress_handler)
api.functional.callback_error(error_handler)
api.runtime.run_energyplus(sys.argv[1:])
