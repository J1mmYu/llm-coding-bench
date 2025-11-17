# scripts/utils.py
import re, hashlib, pathlib, subprocess, shlex, tempfile, os

ROOT = pathlib.Path(__file__).resolve().parent.parent
TMP = ROOT / ".tmp_utils"; TMP.mkdir(exist_ok=True)

LANG = {
 "Python":  { "ext": ".py",  "compile": None,                                          "run": "python3 {src}"},
 "Fortran": { "ext": ".f90", "compile": "gfortran -O2 -o {exe} {src}",                 "run": "{exe}"},
 "COBOL":   { "ext": ".cob", "compile": "cobc -x -O2 -o {exe} {src}",                  "run": "{exe}"},
 "Ada":     { "ext": ".adb", "compile": "gnatmake -q -o {exe} {src}",                  "run": "{exe}"},
 "Octave":  { "ext": ".m",   "compile": None,                                          "run": "octave -qf {src}"},
 "Prolog":  { "ext": ".pl",  "compile": None,                                          "run": "swipl -q -s {src} -g main -t halt"},
}

def sanitize_code(code:str)->str:
    s = code.lstrip("\ufeff")
    def strip_wrap(s, fence):
        s2 = s.lstrip()
        if not s2.startswith(fence): return None
        m = re.search(rf"{re.escape(fence)}\s*$", s2, flags=re.DOTALL)
        if not m: return None
        body = s2[len(fence):m.start()]
        body = re.sub(r"^\s*[A-Za-z0-9_+\-\.]*\s*\n", "", body, count=1)
        return body
    for fence in ("```", '"""', "'''"):
        out = strip_wrap(s, fence)
        if out is not None:
            s = out; break
    s = s.replace("\r\n","\n").replace("\r","\n")
    if not s.endswith("\n"): s += "\n"
    return s

def task_fingerprint(lang:str, instruction:str)->str:
    norm = re.sub(r"\s+", " ", instruction.lower()).strip()
    norm = re.sub(r"[^a-z0-9 ,;:_\-()\[\]{}]", "", norm)
    return hashlib.blake2b((lang+"|"+norm).encode("utf-8"), digest_size=12).hexdigest()

def _run(cmd, inp=None, timeout=10, cwd=None):
    try:
        p = subprocess.run(shlex.split(cmd), input=inp, text=True,
                           capture_output=True, timeout=timeout, cwd=cwd)
        return p.returncode, p.stdout, p.stderr
    except subprocess.TimeoutExpired:
        return -999, "", "TIMEOUT"

def eval_code_on_tests(lang:str, code:str, tests:list, timeout=10):
    cfg = LANG[lang]
    with tempfile.TemporaryDirectory(dir=TMP) as tdir:
        tdir = pathlib.Path(tdir)
        src = tdir / f"Main{cfg['ext'] if lang!='Ada' else '.adb'}"
        exe = tdir / "prog"
        src.write_text(sanitize_code(code), encoding="utf-8")
        if cfg["compile"]:
            rc, out, err = _run(cfg["compile"].format(src=str(src), exe=str(exe)), timeout=30, cwd=tdir)
            if rc!=0: return {"ok": False, "status":"COMPILE_ERROR", "stderr":err[:300]}
        passed = 0
        for t in tests:
            rc, out, err = _run(cfg["run"].format(src=str(src), exe=str(exe)), inp=t["input"], timeout=timeout, cwd=tdir)
            if rc==0 and out.strip()==t["output"].strip(): passed += 1
            else: return {"ok": False, "status":"FAIL", "last":{"rc":rc,"out":out,"err":err}}
        return {"ok": True, "status":"OK", "passed":passed, "total":len(tests)}
