import re, json, sys, pathlib
ROOT = pathlib.Path(__file__).resolve().parent.parent
def guess(instr):
    s = instr.lower()
    if "csv" in s or "comma" in s: return "csv_ints"
    if re.search(r"\bn\b.*\bthen\b.*\b n integers\b", s) or "read n then n integers" in s: return "ints:5"
    if re.search(r"\barray\b|\blist\b|\bsequence\b", s): return "ints:5"
    if re.search(r"\binteger\b|\bnumber\b", s): return "int"
    return "line"
if __name__ == "__main__":
    for fp in sorted((ROOT/"benchmark").glob("*.jsonl")):
        for ln in open(fp, encoding="utf-8"):
            if not ln.strip(): continue
            obj = json.loads(ln)
            print(obj["task_id"], guess(obj["instruction"]))
