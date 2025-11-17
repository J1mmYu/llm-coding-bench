import json, csv, pathlib, subprocess, shlex, multiprocessing as mp, re, os, time

ROOT = pathlib.Path(__file__).resolve().parent.parent
WORK = ROOT / ".workdir_parallel"; WORK.mkdir(exist_ok=True)
REPORT = ROOT / "report"; REPORT.mkdir(exist_ok=True)
SKIP = set(s.strip() for s in os.environ.get("SKIP_LANGS","").split(",") if s.strip())

LANG = {
 "Python":  { "ext": ".py",  "compile": None,                                          "run": "python3 {src}"},
 "Fortran": { "ext": ".f90", "compile": "gfortran -O2 -o {exe} {src}",                 "run": "{exe}"},
 "COBOL":   { "ext": ".cob", "compile": "cobc -x -O2 -o {exe} {src}",                  "run": "{exe}"},  # compile 动态改
 "Ada":     { "ext": ".adb", "compile": "gnatmake -q -o {exe} {src}",                  "run": "{exe}"},
 "Octave":  { "ext": ".m",   "compile": None,                                          "run": "octave -qf {src}"},
 "Prolog":  { "ext": ".pl",  "compile": None,                                          "run": "swipl -q -s {src} -g main -t halt"}
}

def sanitize_code(code: str) -> str:
    s = code.lstrip("\ufeff")
    def strip_wrap(s, fence):
        s2 = s.lstrip()
        if not s2.startswith(fence): return None
        m = re.search(rf"{re.escape(fence)}\s*$", s2, flags=re.DOTALL)
        if not m: return None
        body = s2[len(fence):m.start()]
        body = re.sub(r"^\s*[A-Za-z0-9_+\-\.]*\s*\n", "", body, count=1)  # 去掉 ```python 等语言标签行
        return body
    for fence in ("```", '"""', "'''"):
        out = strip_wrap(s, fence)
        if out is not None:
            s = out; break
    s = s.replace("\r\n","\n").replace("\r","\n")
    if not s.endswith("\n"): s += "\n"
    return s

def run(cmd, inp=None, timeout=10, cwd=None):
    try:
        p = subprocess.run(
            shlex.split(cmd),
            input=inp,
            text=True,
            encoding="utf-8",
            errors="replace",   # 宽容解码，避免 UnicodeDecodeError
            capture_output=True,
            timeout=timeout,
            cwd=cwd
        )
        return p.returncode, p.stdout, p.stderr
    except subprocess.TimeoutExpired:
        return -999, "", "TIMEOUT"

def cobol_compile_cmd(src, exe):
    # 若源码指明 FREE 或看起来是 free-format，则加 -free
    code = pathlib.Path(src).read_text(encoding="utf-8", errors="ignore")
    head = "\n".join(code.splitlines()[:8]).lstrip()
    is_free = ">>SOURCE FORMAT FREE" in code or head.startswith("IDENTIFICATION DIVISION.") or head.startswith("PROGRAM-ID.")
    flag = "-free " if is_free else ""
    return f"cobc -x -O2 {flag}-o {exe} {src}"

def eval_task(task):
    lang = task["language"]
    if lang in SKIP:
        return (task["task_id"], lang, task["difficulty"], len(task["tests"]), len(task["tests"]), "SKIPPED", "")
    cfg = LANG[lang]
    tid = task["task_id"].replace("/","_")
    tdir = WORK / tid; tdir.mkdir(parents=True, exist_ok=True)
    for p in list(tdir.iterdir()): p.unlink()
    src = tdir / ("main.adb" if lang=="Ada" else ("Main"+cfg["ext"]))
    src.write_text(sanitize_code(task["canonical_solution"]), encoding="utf-8")
    exe = tdir / "prog"
    # 编译
    if cfg["compile"]:
        compile_cmd = cobol_compile_cmd(str(src), str(exe)) if lang=="COBOL" else cfg["compile"].format(src=str(src), exe=str(exe))
        code,out,err = run(compile_cmd, timeout=30)
        if code!=0: return (task["task_id"], lang, task["difficulty"], 0, len(task["tests"]), "COMPILE_ERROR", err[:200])
    # 测试
    passed = 0
    for t in task["tests"]:
        code,out,err = run(cfg["run"].format(src=str(src), exe=str(exe)), inp=t["input"], timeout=10, cwd=tdir)
        if code==0 and out.strip()==t["output"].strip(): passed += 1
    status = "OK" if passed==len(task["tests"]) else "FAIL"
    return (task["task_id"], lang, task["difficulty"], passed, len(task["tests"]), status, "")

def iter_tasks():
    bench = ROOT / "benchmark"
    for fp in sorted(bench.glob("*.jsonl")):
        for line in fp.read_text(encoding="utf-8", errors="replace").splitlines():
            if not line.strip(): continue
            yield json.loads(line)

if __name__=="__main__":
    tasks = list(iter_tasks())
    with mp.Pool() as pool:
        rows = list(pool.map(eval_task, tasks))
    # 写入 timestamped CSV，避免权限/锁冲突
    ts_name = f"summary_{os.getpid()}_{int(time.time())}.csv"
    ts_path = REPORT / ts_name
    with open(ts_path,"w",newline="",encoding="utf-8") as f:
        w = csv.writer(f); w.writerow(["task_id","language","difficulty","passed","total","status","note"]); w.writerows(rows)
    # 尝试更新 summary.csv；失败就忽略
    try:
        with open(REPORT/"summary.csv","w",newline="",encoding="utf-8") as f:
            w = csv.writer(f); w.writerow(["task_id","language","difficulty","passed","total","status","note"]); w.writerows(rows)
        print(f"CSV written -> {REPORT/'summary.csv'}")
    except PermissionError:
        print(f"CSV written -> {ts_path} (couldn't overwrite summary.csv)")
    # 控制台汇总
    totals = {}
    for _,lang,_,p,tot,_,_ in rows:
        a,b = totals.get(lang,(0,0)); totals[lang]=(a+p,b+tot)
    print("# Summary (parallel)")
    grand_p=grand_t=0
    for lang,(p,t) in sorted(totals.items()):
        rate=0 if t==0 else 100.0*p/t
        print(f"{lang}: {p}/{t} ({rate:.1f}%)"); grand_p+=p; grand_t+=t
    grate=0 if grand_t==0 else 100.0*grand_p/grand_t
    print(f"TOTAL: {grand_p}/{grand_t} ({grate:.1f}%)")
