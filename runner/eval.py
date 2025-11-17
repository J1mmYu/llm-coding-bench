import json, subprocess, shlex, pathlib, argparse, os, re

ROOT = pathlib.Path(__file__).resolve().parent.parent
WORK = ROOT / ".workdir"; WORK.mkdir(exist_ok=True)
SKIP = set(s.strip() for s in os.environ.get("SKIP_LANGS","").split(",") if s.strip())

LANG_CFG = {
    "Python":  {"ext": ".py",  "compile": None,                                           "run": "python3 {src}"},
    "Fortran": {"ext": ".f90", "compile": "gfortran -O2 -o {exe} {src}",                  "run": "{exe}"},
    "COBOL":   {"ext": ".cob", "compile": "cobc -x -O2 -o {exe} {src}",                   "run": "{exe}"},
    "Ada":     {"ext": ".adb", "compile": "gnatmake -q -o {exe} {src}",                   "run": "{exe}"},
    "Octave":  {"ext": ".m",   "compile": None,                                           "run": "octave -qf {src}"},
    "Prolog":  {"ext": ".pl",  "compile": None,                                           "run": "swipl -q -s {src} -g main -t halt"}
}

def sanitize_code(code: str) -> str:
    s = code.lstrip("\ufeff")
    def strip_wrap(s, fence):
        s2 = s.lstrip()
        if not s2.startswith(fence): return None
        m = re.search(rf"{re.escape(fence)}\s*$", s2, flags=re.DOTALL)
        if not m: return None
        body = s2[len(fence):m.start()]
        body = re.sub(r"^\s*[A-Za-z0-9_+\-\.]*\s*\n", "", body, count=1)  # 去掉 ```python 语言标签行
        return body
    for fence in ("```", '"""', "'''"):
        out = strip_wrap(s, fence)
        if out is not None:
            s = out; break
    s = s.replace("\r\n","\n").replace("\r","\n")
    if not s.endswith("\n"): s += "\n"
    return s

def run_cmd(cmd, inp=None, timeout=10, cwd=None):
    try:
        p = subprocess.run(
            shlex.split(cmd),
            input=inp,
            text=True,
            encoding="utf-8",
            errors="replace",   # 关键：宽容解码，避免 UnicodeDecodeError
            capture_output=True,
            timeout=timeout,
            cwd=cwd,
        )
        return p.returncode, p.stdout, p.stderr
    except subprocess.TimeoutExpired:
        return -999, "", "TIMEOUT"

def show_diff(task_id, t_idx, inp, exp, got, rc, err):
    print(f"\n[FAIL] {task_id} test#{t_idx}")
    print(f"  return_code: {rc}")
    if err.strip():
        print(f"  stderr: {err.strip()[:300]}")
    print("  input   :", repr(inp))
    print("  expected:", repr(exp.strip()))
    print("  got     :", repr(got.strip()))
    if exp.strip() != got.strip():
        print("  hint    : whitespace/newline mismatch? lengths:",
              len(exp.strip()), "(exp) vs", len(got.strip()), "(got)")

def evaluate_file(jsonl_path, verbose=False):
    total = passed = 0
    for line in open(jsonl_path, 'r', encoding='utf-8'):
        if not line.strip(): continue
        task = json.loads(line)
        lang = task["language"]
        if lang in SKIP:
            total += len(task["tests"]); passed += len(task["tests"]);  # 跳过=不计入失败
            continue
        cfg = LANG_CFG[lang]
        tdir = WORK / (task["task_id"].replace("/","_")); tdir.mkdir(parents=True, exist_ok=True)
        for p in list(tdir.iterdir()): p.unlink()

        src = tdir / ("main.adb" if lang=="Ada" else f"Main{cfg['ext']}")
        src.write_text(sanitize_code(task["canonical_solution"]), encoding="utf-8")

        exe = tdir / "prog"
        if cfg["compile"]:
            code, out, err = run_cmd(cfg["compile"].format(src=str(src), exe=str(exe)), timeout=20)
            if code != 0:
                print(f"[COMPILE_ERROR] {task['task_id']} ({lang}) -> {err[:400]}")
                total += len(task["tests"])
                continue

        for i, t in enumerate(task["tests"], 1):
            code, out, err = run_cmd(cfg["run"].format(src=str(src), exe=str(exe)), inp=t["input"], timeout=10, cwd=tdir)
            ok = (code == 0 and out.strip() == t["output"].strip())
            if not ok and verbose:
                show_diff(task["task_id"], i, t["input"], t["output"], out, code, err)
            passed += int(ok); total += 1
    return total, passed

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("-v","--verbose",action="store_true")
    args = ap.parse_args()

    bench_dir = ROOT / "benchmark"
    jsonls = sorted([p for p in bench_dir.iterdir() if p.suffix==".jsonl"])
    gtot=gpas=0
    print("# Summary")
    for fp in jsonls:
        tot,pas = evaluate_file(fp, verbose=args.verbose)
        rate = 0.0 if tot==0 else 100.0*pas/tot
        print(f"{fp.name}: {pas}/{tot} passed ({rate:.1f}%)")
        gtot+=tot; gpas+=pas
    grate = 0.0 if gtot==0 else 100.0*gpas/gtot
    print(f"TOTAL: {gpas}/{gtot} passed ({grate:.1f}%)")

if __name__=="__main__": main()
