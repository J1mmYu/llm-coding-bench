import json, pathlib, re, sys
ROOT = pathlib.Path(__file__).resolve().parent.parent
def sanitize(code:str)->str:
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
def process(fp):
    changed = 0
    lines = []
    for ln in open(fp,encoding="utf-8").read().splitlines():
        if not ln.strip(): continue
        obj = json.loads(ln)
        cs = obj.get("canonical_solution","")
        new = sanitize(cs)
        if new != cs:
            obj["canonical_solution"] = new; changed += 1
        lines.append(json.dumps(obj,ensure_ascii=False))
    fp.write_text("\n".join(lines)+"\n",encoding="utf-8")
    return changed
if __name__=="__main__":
    total=0
    for f in sorted((ROOT/"benchmark").glob("*.jsonl")):
        c = process(f); total += c
        print(f"{f.name}: fixed {c}")
    print("TOTAL fixed:", total)
