from ofxtools.Parser import OFXTree
from ofxtools.header import make_header
from rich import print
import xml.etree.ElementTree as ET
import sys
import os

FALLBACK_PATH = ""

INT_LIST = [
    {"id": "PCRF", "name": "PCRF"},
    {"id": "CONSERVATION", "name": "Conservation"},
    {"id": "PROJECT FOR AWESOM", "name": "P4A"},
    {"id": "WATER.ORG", "name": "Water.org"},
    {"id": "GOOD-STORE-DFTB", "name": "Good Store"},
    {"id": "STEAM", "name": "Steam"},
    {"id": "Patreon", "name": "Patreon"},
]

MISC_LIST = [{"id": "Patreon", "name": "Patreon"}]


def trace(s):
    print(f"[bright_black]{s}[/bright_black]")


def warn(s):
    print(f"[yellow]WARN: {s}[/yellow]")


def error(s):
    print(f"[red]ERROR: {s}[/red]")


def process(ofx):
    print("\n----------")
    print("Processing now\n")

    int_count = 0
    swap_count = 0
    misc_count = 0
    for stmt in ofx.statements:
        for tx in stmt.transactions:
            if "INT&#" in tx.name:
                rules = [
                    rule for rule in INT_LIST if rule["id"].lower() in tx.memo.lower()
                ]
                if len(rules) == 0:
                    warn(
                        f"No rule found for {tx.memo} ({tx.name}). Swapping memo and name."
                    )
                    temp = tx.name
                    tx.name = tx.memo
                    tx.memo = temp
                    swap_count += 1
                elif len(rules) > 1:
                    error(f"{len(rules)} rules found for {tx.memo} ({tx.name})")
                else:
                    trace(f"Applying {rules[0]["name"]} rule to {tx.memo}")
                    tx.name = rules[0]["name"]
                    int_count += 1
            else:
                rules = [
                    rule
                    for rule in MISC_LIST
                    if rule["id"].lower() in tx.name.lower()
                    and rule["name"] != tx.name  # Would be no-op
                ]
                if len(rules) > 1:
                    error(
                        f"{len(rules)} rules found for name: {tx.name}, memo: {tx.memo}"
                    )
                elif len(rules) == 1:
                    print(
                        f"Applying misc {rules[0]["name"]} rule to name: {tx.name}, memo: {tx.memo}"
                    )
                    tx.name = rules[0]["name"]
                    misc_count += 1
    print(
        f"\nApplied a total of {int_count + misc_count} rules ({int_count} int, {misc_count} misc) and {swap_count} swaps"
    )
    print("----------\n")


if __name__ == "__main__":
    if len(sys.argv) < 2:
        if FALLBACK_PATH == "":
            error("No path given and no fallback path set")
            sys.exit(2)
        file_path = FALLBACK_PATH
        warn(f"Falling back to path: {FALLBACK_PATH}")
    else:
        file_path = sys.argv[1]
    if not os.path.exists(file_path):
        error(f"File {file_path} does not exist.")
        sys.exit(1)

    print(f"Reading file from: {file_path}")
    parser = OFXTree()
    with open(file_path, "rb") as f:
        parser.parse(f)
    ofx = parser.convert()

    process(ofx)

    root = ofx.to_etree()
    ET.indent(root, space="  ")
    final_ofx_data = (
        str(make_header(version=220))
        + "\n"
        + ET.tostring(root, encoding="utf-8").decode("utf-8")
    )

    with open(file_path, "w", encoding="utf-8") as f:
        f.write(final_ofx_data)
    print(f"Successfully wrote to file: {file_path}")

    print("Done")

"""
TODO:
- Automatically filter old transactions?
"""
