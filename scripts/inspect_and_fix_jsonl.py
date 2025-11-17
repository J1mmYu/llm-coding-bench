# scripts/inspect_and_fix_jsonl.py
import json, argparse, pathlib, sys
from scripts.utils import sanitize_code, eval_code_on_tests, task_fingerprint, ROOT
def load_jsonl(p):
    for line in pathlib.Path(p).read_text(encoding="utf-8").splitlines():
        if line.strip(): yield json.loads(line)
def save_jsonl(p, arr):
    p = pathlib.Path(p); p.parent.mkdir(parents=True, exist_ok=True)
    with p.open("w", encoding="utf-8") as f:
        for x in arr: f.write(json.dumps(x, ensure_ascii=False)+"\n")

def synth_inputs_like(inp):
    import random, string
    s = inp.strip("\n")
    if "," in s or ";" in s:
        k = max(3, min(20, len(s.split(","))+random.randint(-1,3)))
        return (", " if " " in s else ",").join(str(random.randint(-1000,1000)) for _ in range(k))+"\n"
    parts = s.split()
    if len(parts) >= 3 and all(p.lstrip("-").isdigit() for p in parts):
        k = max(3, min(15, len(parts)+random.randint(-1,3)))
        return " ".join(str(random.randint(-10**6,10**6)) for _ in range(k))+"\n"
    if s.lstrip("-").isdigit():
        import random; return f"{random.randint(-10**6,10**6)}\n"
    L = max(0, min(60, len(s)+3)); letters = string.ascii_letters+string.digits+"  ,;:_-"
    return "".join(random.choice(letters) for _ in range(L))+"\n"

def augment_tests(lang, code, tests, target=10):
    seen = set((t["input"], t["output"]) for t in tests)
    tries, added = 0, 0
    while len(tests) < target and tries < 200:
        base = tests[-1]["input"] if tests else "\n"
        new_inp = synth_inputs_like(base)
        # 单次执行拿输出
        from scripts.utils import LANG, _run
        import tempfile, pathlib as pl
        cfg = LANG[lang]
        with tempfile.TemporaryDirectory(dir=ROOT/".tmp_fix") as tdir:
            tdir = pl.Path(tdir)
            src = tdir / f"Main{cfg['ext'] if lang!='Ada' else '.adb'}"
            exe = tdir / "prog"
            src.write_text(code, encoding="utf-8")
            if cfg["compile"]:
                rc, out, err = _run(cfg["compile"].format(src=str(src), exe=str(exe)), timeout=20, cwd=tdir)
                if rc!=0: tries += 1; continue
            rc, out, err = _run(cfg["run"].format(src=str(src), exe=str(exe)), inp=new_inp, timeout=5, cwd=tdir)
            if rc==0:
                pair=(new_inp, out.strip())
                if pair not in seen:
                    tests.append({"input":new_inp, "output":out.strip()})
                    seen.add(pair); added += 1
        tries += 1
    return tests, added

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--in", dest="inp", required=True)
    ap.add_argument("--out", dest="outp", required=True)
    ap.add_argument("--lang", required=True)
    ap.add_argument("--min_tests", type=int, default=10)
    ap.add_argument("--keep_empty_inputs", action="store_true")
    args = ap.parse_args()

    cleaned=[]
    for obj in load_jsonl(args.inp):
        # 基本字段检查
        for k in ["task_id","language","difficulty","instruction","prompt","mode","canonical_solution","tests"]:
            if k not in obj: print("Missing", k, obj.get("task_id","?"), file=sys.stderr); continue
        obj["canonical_solution"] = sanitize_code(obj["canonical_solution"])
        if not args.keep_empty_inputs:
            obj["tests"] = [t for t in obj["tests"] if t.get("input","")!="" or t.get("output")!=""]
        if len(obj["tests"]) < args.min_tests:
            obj["tests"], _ = augment_tests(args.lang, obj["canonical_solution"], obj["tests"], target=args.min_tests)
        obj.setdefault("fp", task_fingerprint(obj["language"], obj["instruction"]))
        cleaned.append(obj)
    save_jsonl(args.outp, cleaned)
    print(f"Saved {len(cleaned)} tasks to {args.outp}")

if __name__ == "__main__":
    main()
