# scripts/add_task_manual.py  (REPLACE)
import argparse, json, re, pathlib, sys
from utils import sanitize_code, task_fingerprint

ROOT = pathlib.Path(__file__).resolve().parent.parent
BENCH = ROOT / "data" / "incoming"
BENCH.mkdir(parents=True, exist_ok=True)

def next_id(lang, fp):
    fp_path = BENCH / f"{lang}.jsonl"
    n = 0
    if fp_path.exists():
        for line in fp_path.read_text(encoding="utf-8").splitlines():
            try:
                obj = json.loads(line)
                if obj.get("language")==lang and "task_id" in obj:
                    m = re.search(r"/(\d+)$", obj["task_id"])
                    if m: n = max(n, int(m.group(1)))
                # 冲突检测：指纹一致视为重复
                if obj.get("fp")==fp: return None
            except: pass
    return n+1

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--lang", required=True, choices=["Python","Fortran","COBOL","Ada","Octave","Prolog"])
    ap.add_argument("--difficulty", required=True, choices=["easy","hard"])
    ap.add_argument("--instruction", required=True)
    ap.add_argument("--solution")
    ap.add_argument("--solution-file")
    ap.add_argument("--tests-json")
    ap.add_argument("--tests-file")
    ap.add_argument("--topic")
    ap.add_argument("--tags")
    args = ap.parse_args()

    fp_out = BENCH / f"{args.lang}.jsonl"
    sol = args.solution or pathlib.Path(args.solution_file).read_text(encoding="utf-8")
    tests_txt = args.tests_json or pathlib.Path(args.tests_file).read_text(encoding="utf-8")
    tests = json.loads(tests_txt)
    assert isinstance(tests, list) and all("input" in t and "output" in t for t in tests)

    code = sanitize_code(sol)
    fp = task_fingerprint(args.lang, args.instruction)
    k = next_id(args.lang, fp)
    if k is None:
        print(f"SKIP duplicate (fp={fp})", file=sys.stderr); sys.exit(0)

    item = {
        "task_id": f"{args.lang}/{k:04d}",
        "language": args.lang,
        "difficulty": args.difficulty,
        "instruction": args.instruction,
        "prompt": args.instruction,
        "mode": "io",
        "canonical_solution": code,
        "tests": tests,
        "raw_generation": [],
        "fp": fp
    }
    if args.topic: item["topic"] = args.topic
    if args.tags:  item["tags"]  = [t.strip() for t in args.tags.split(",") if t.strip()]

    with open(fp_out, "a", encoding="utf-8") as f:
        f.write(json.dumps(item, ensure_ascii=False) + "\n")
    print(f"Appended {item['task_id']} to {fp_out}")
if __name__ == "__main__":
    main()
