import os
import subprocess
from pathlib import Path
from typing import List, Set

import requests
from packaging.version import Version

ROOT_DIR = Path(__file__).parent.parent.parent
WORKFLOW_DIR = ROOT_DIR / ".github/workflows"


def get_github_token():
    if "GITHUB_TOKEN" in os.environ:
        return os.environ["GITHUB_TOKEN"]

    try:
        return subprocess.check_output(["gh", "auth", "token"], universal_newlines=True, encoding="utf-8").strip()
    except Exception:
        print("GITHUB_TOKEN not in ENV variable, and `gh` CLI not available")
        return None


def get_uses(workflows: List[Path]) -> Set[str]:
    uses = []
    for workflow_path in workflows:
        content = workflow_path.read_text()
        lines = content.splitlines()
        for i, line in enumerate(lines):
            if "uses" in line:
                action = line.split("uses: ")[1].split("#")[0].strip()
                uses.append(action)
    return set(uses)


def convert_to_version(tag_name):
    try:
        return Version(tag_name.replace("v", ""))
    except:
        return Version("0.0.0")


def check_latest(uses: Set[str]) -> dict[str, Version]:
    unique_actions = set([use.split("@")[0] for use in set(uses)])

    headers = None
    gha_token = get_github_token()
    if gha_token is not None:
        headers = {"Authorization": f"token {gha_token}"}
    latests = {}
    for action in unique_actions:
        r = requests.get(f"https://api.github.com/repos/{action}/releases", headers=headers)
        r.raise_for_status()
        latests[action] = next(reversed(sorted([convert_to_version(x["tag_name"]) for x in r.json()])))
    return latests


def get_replacements(uses: Set[str], latests: dict[str, Version]) -> dict[str, str]:
    replacements = {}
    for use in uses:
        action, version = use.split("@")
        assert action in latests, f"{action} not found in latests: {latests}"
        replacement_v = None
        if "." in version:
            replacement_v = latests[action]
        else:
            replacement_v = latests[action].major
        replacement = f"{action}@v{replacement_v}"
        if replacement == use:
            print(f"No updates found for {use}")
            continue
        replacements[use] = replacement
    return replacements


if __name__ == "__main__":
    workflows = list(WORKFLOW_DIR.glob("*.yml"))
    uses = get_uses(workflows=workflows)
    latests = check_latest(uses=uses)
    replacements = get_replacements(uses=uses, latests=latests)
    for workflow_path in workflows:
        content = workflow_path.read_text()
        for ori, new in replacements.items():
            content = content.replace(ori, new)
        workflow_path.write_text(content)
