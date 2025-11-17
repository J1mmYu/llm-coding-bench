import subprocess, shlex, tempfile, pathlib, os, json

ROOT = pathlib.Path(__file__).resolve().parent.parent
TMP  = ROOT / ".tmp_exec"; TMP.mkdir(exist_ok=True)

LANG = {
 "Python":  { "ext": ".py",  "compile": None,                                          "run": "python3 {src}"},
 "Fortran": { "ext": ".f90", "compile": "gfortran -O2 -o {exe} {src}",                 "run": "{exe}"},
 "COBOL":   { "ext": ".cob", "compile": "cobc -x -O2 -o {exe} {src}",                  "run": "{exe}"},
 "Ada":     { "ext": ".adb", "compile": "gnatmake -q -o {exe} {src}",                  "run": "{exe}"},
 "Octave":  { "ext": ".m",   "compile": None,                                          "run": "octave -qf {src}"},
 "Prolog":  { "ext": ".pl",  "compile": None,                                          "run": "swipl -q -s {src} -g main -t halt"},
}

def run(cmd, inp=None, timeout=10, cwd=None):
    p = subprocess.run(shlex.split(cmd), input=inp, text=True,
                       capture_output=True, timeout=timeout, cwd=cwd)
    return p.returncode, p.stdout, p.stderr

def eval_code_on_tests(lang:str, code:str, tests:list, timeout=10):
    cfg = LANG[lang]
    with tempfile.TemporaryDirectory(dir=TMP) as tdir:
        tdir = pathlib.Path(tdir)
        src = tdir / f"main{cfg['ext']}"
        exe = tdir / "prog"
        src.write_text(code, encoding="utf-8")
        # compile if needed
        if cfg["compile"]:
            rc, out, err = run(cfg["compile"].format(src=str(src), exe=str(exe)), timeout=20, cwd=tdir)
            if rc != 0:
                return {"ok": False, "passed": 0, "total": len(tests),
                        "status": "COMPILE_ERROR", "stderr": err[:400]}
        # run tests
        passed = 0
        last = {}
        for t in tests:
            rc, out, err = run(cfg["run"].format(src=str(src), exe=str(exe)), inp=t["input"], timeout=timeout, cwd=tdir)
            ok = (rc==0 and out.strip()==t["output"].strip())
            passed += int(ok)
            last = {"rc": rc, "out": out, "err": err}
            if not ok:
                return {"ok": False, "passed": passed, "total": len(tests),
                        "status": "FAIL", "last": last}
        return {"ok": True, "passed": passed, "total": len(tests), "status": "OK"}
