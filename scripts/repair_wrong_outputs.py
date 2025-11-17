# scripts/repair_wrong_outputs.py
import os, sys, json, argparse, urllib.request, pathlib, itertools, time, tempfile
from utils import sanitize_code, ROOT, eval_code_on_tests

API = "https://openrouter.ai/api/v1/chat/completions"
KEY = os.environ.get("OPENROUTER_API_KEY")

MODELS = [
 "anthropic/claude-sonnet-4.5",
 "openai/gpt-5",
 "google/gemini-2.5-flash",
]

TMP_REPAIR = ROOT / ".tmp_repair"
TMP_REPAIR.mkdir(parents=True, exist_ok=True)

def chat(model, messages, temperature=0.2):
    body = json.dumps({"model": model, "messages": messages, "temperature": temperature}).encode()
    req = urllib.request.Request(API, data=body, method="POST",
        headers={"Content-Type":"application/json","Authorization":f"Bearer {KEY}",
                 "HTTP-Referer":"http://localhost","X-Title":"llm-coding-bench"})
    with urllib.request.urlopen(req, timeout=90) as r:
        j = json.loads(r.read().decode())
    return j["choices"][0]["message"]["content"]

def gen_code(model, lang, instruction):
    sp = f"You are a {lang} expert. Return ONLY executable {lang} code that reads from stdin and writes to stdout. No comments, no markdown."
    return sanitize_code(chat(model, [{"role":"system","content":sp},{"role":"user","content":instruction}], 0.2))

def load_jsonl(p):
    for line in pathlib.Path(p).read_text(encoding="utf-8").splitlines():
        if line.strip(): yield json.loads(line)

def save_jsonl(p, arr):
    p = pathlib.Path(p); p.parent.mkdir(parents=True, exist_ok=True)
    with p.open("w", encoding="utf-8") as f:
        for x in arr: f.write(json.dumps(x, ensure_ascii=False)+"\n")

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--in", dest="inp", required=True)
    ap.add_argument("--out", dest="outp", required=True)
    ap.add_argument("--lang", required=True)
    ap.add_argument("--k_models", type=int, default=3)
    ap.add_argument("--agree", type=int, default=2, help="至少多少份实现一致才采纳")
    args = ap.parse_args()
    assert KEY, "Set OPENROUTER_API_KEY"

    tasks = list(load_jsonl(args.inp))
    fixed = []
    for t in tasks:
        # 先确认确实有错配
        code = t["canonical_solution"]
        for tt in t["tests"]:
            r = eval_code_on_tests(args.lang, code, [tt])
            if not r["ok"]:
                break
        else:
            fixed.append(t); continue

        # 生成多实现
        impls = [gen_code(m, args.lang, t["instruction"]) for m in MODELS[:args.k_models]]
        new_tests=[]
        for tt in t["tests"]:
            outputs=[]
            for code_i in impls:
                rr = eval_code_on_tests(args.lang, code_i, [tt])
                if rr["ok"]:
                    # 单样例执行时 ok 代表输出与 tt.output 匹配；我们需要真实输出值 → 重新跑一次单次执行取 stdout
                    from utils import LANG, _run
                    import tempfile, pathlib as pl
                    cfg = LANG[args.lang]
                    with tempfile.TemporaryDirectory(dir=TMP_REPAIR) as d:
                        d=pl.Path(d)
                        src=d/("Main"+cfg["ext"] if args.lang!="Ada" else "Main.adb")
                        exe=d/"prog"
                        src.write_text(code_i, encoding="utf-8")
                        if cfg["compile"]:
                            rc,o,e=_run(cfg["compile"].format(src=str(src), exe=str(exe)), timeout=20, cwd=d)
                            if rc!=0: continue
                        rc,o,e=_run(cfg["run"].format(src=str(src), exe=str(exe)), inp=tt["input"], timeout=5, cwd=d)
                        if rc==0: outputs.append(o.strip())
            # 取众数或达到 agree 阈值的值
            if not outputs:
                new_tests.append(tt)  # 无法修复，保留原样
            else:
                cand = max(set(outputs), key=outputs.count)
                if outputs.count(cand) >= args.agree:
                    new_tests.append({"input": tt["input"], "output": cand})
                else:
                    new_tests.append(tt)
        t["tests"] = new_tests
        fixed.append(t)

    save_jsonl(args.outp, fixed)
    print("Saved fixed ->", args.outp)

if __name__ == "__main__":
    main()
