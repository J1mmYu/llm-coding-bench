import argparse, json, pathlib, random, subprocess, shlex, os, tempfile

ROOT = pathlib.Path(__file__).resolve().parent.parent
WORK = ROOT / ".work_fuzz"; WORK.mkdir(exist_ok=True)

LANG = {
 "Python":  { "ext": ".py",  "compile": None,                                          "run": "python3 {src}"},
 "Fortran": { "ext": ".f90", "compile": "gfortran -O2 -o {exe} {src}",                 "run": "{exe}"},
 "COBOL":   { "ext": ".cob", "compile": "cobc -x -O2 -o {exe} {src}",                  "run": "{exe}"},
 "Ada":     { "ext": ".adb", "compile": "gnatmake -q -o {exe} {src}",                  "run": "{exe}"},
 "Octave":  { "ext": ".m",   "compile": None,                                          "run": "octave -qf {src}"},
 "Prolog":  { "ext": ".pl",  "compile": None,                                          "run": "swipl -q -s {src} -g main -t halt"}
}

def run(cmd, inp=None, timeout=5, cwd=None):
    p = subprocess.run(shlex.split(cmd), input=inp, text=True, capture_output=True, timeout=timeout, cwd=cwd)
    return p.returncode, p.stdout

def gen_inp(mode):
    if mode=="line":
        L = random.randint(0,60)
        letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ  ,;:_-0123456789"
        s = "".join(random.choice(letters) for _ in range(L))
        return s + "\n"
    if mode=="int":
        return f"{random.randint(0, 10**6)}\n"
    if mode.startswith("ints:"):
        k = int(mode.split(":")[1])
        arr = [str(random.randint(-10**6,10**6)) for _ in range(k)]
        return " ".join(arr)+"\n"
    if mode=="csv_ints":
        k = random.randint(1,30)
        arr = [str(random.randint(-1000,1000)) for _ in range(k)]
        sep = random.choice([",", ", ", " , "])
        return sep.join(arr)+"\n"
    # 默认：整行
    return gen_inp("line")

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--lang", required=True, choices=list(LANG.keys()))
    ap.add_argument("--mode", required=True, help="line | int | ints:<k> | csv_ints")
    ap.add_argument("--n", type=int, default=20)
    args = ap.parse_args()

    fp = ROOT / "benchmark" / f"{args.lang}.jsonl"
    lines = fp.read_text(encoding="utf-8").splitlines()
    out_lines = []
    for line in lines:
        task = json.loads(line)
        cfg = LANG[task["language"]]
        tid = task["task_id"].replace("/","_")
        tdir = WORK / tid
        if tdir.exists():
            for p in tdir.iterdir(): p.unlink()
        else:
            tdir.mkdir(parents=True, exist_ok=True)
        src = tdir / ("Main"+cfg["ext"]); src.write_text(task["canonical_solution"], encoding="utf-8")
        exe = tdir / "prog"
        if cfg["compile"]:
            r = subprocess.run(shlex.split(cfg["compile"].format(src=str(src), exe=str(exe))), capture_output=True, text=True)
            if r.returncode!=0:  # 编译失败，跳过
                out_lines.append(line); continue
        # 生成并求 oracle 输出
        seen = set((t["input"], t["output"]) for t in task["tests"])
        added = 0
        for _ in range(args.n):
            inp = gen_inp(args.mode)
            rc, out = run(cfg["run"].format(src=str(src), exe=str(exe)), inp=inp, cwd=tdir)
            if rc==0:
                item = (inp, out.strip())
                if item not in seen:
                    task["tests"].append({"input":inp, "output":out.strip()})
                    seen.add(item); added += 1
        out_lines.append(json.dumps(task, ensure_ascii=False))
    fp.write_text("\n".join(out_lines)+"\n", encoding="utf-8")
    print(f"Augmented tests in {fp}")
if __name__ == "__main__":
    main()
