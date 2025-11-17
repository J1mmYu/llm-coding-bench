# scripts/openrouter_ab.py  (REPLACE)
import os, sys, json, time, urllib.request, random
from utils import sanitize_code, eval_code_on_tests
from utils import ROOT
import argparse

API = "https://openrouter.ai/api/v1/chat/completions"
KEY = os.environ.get("OPENROUTER_API_KEY")

def chat(model, messages):
    body = json.dumps({"model": model, "messages": messages}).encode()
    req = urllib.request.Request(API, data=body, method="POST",
        headers={"Content-Type":"application/json","Authorization":f"Bearer {KEY}",
                 "HTTP-Referer":"http://localhost","X-Title":"llm-coding-bench"})
    with urllib.request.urlopen(req, timeout=90) as r:
        j = json.loads(r.read().decode())
    return j["choices"][0]["message"]["content"]

def gen_solution(model, lang, instruction):
    sp = f"You are a {lang} expert. Return ONLY executable {lang} code that reads from stdin and prints to stdout. No comments, no markdown."
    code = chat(model, [{"role":"system","content":sp},{"role":"user","content":instruction}])
    return sanitize_code(code)

def gen_tests(model, lang, instruction, min_cases=8):
    sp = (f"You are a senior QA for {lang}. Return ONLY a JSON array of IO tests: "
          f"each item has 'input' and 'output'. At least {min_cases} diverse cases. "
          f"Use exact outputs with trailing newlines if needed. No comments.")
    t = chat(model, [{"role":"system","content":sp},{"role":"user","content":instruction}])
    for _ in range(2):
        try:
            arr = json.loads(t)
            if isinstance(arr, list) and all("input" in x and "output" in x for x in arr):
                return arr
        except: pass
        # 严格 JSON 重试
        t = chat(model, [
            {"role":"system","content":"Return ONLY valid JSON like [{\"input\":\"...\",\"output\":\"...\"}] with no trailing text."},
            {"role":"user","content":instruction}
        ])
    raise RuntimeError("Test generation failed (JSON)")

def augment_tests_with_oracle(lang, code, tests, target=10):
    # 如果已有 tests < target，则用已有 tests 的输入格式去随机补齐
    if len(tests) >= target: return tests
    # 简单启发：从现有 input 中抽样“风格化”生成
    def synth_like(s):
        if "\n" in s.strip(): return s  # 多行维持
        if any(c in s for c in [",",";"]):  # csv 风格
            k = max(3, min(20, len(s.split(","))+random.randint(-1,3)))
            arr = [str(random.randint(-1000,1000)) for _ in range(k)]
            sep = random.choice([",", ", ", " , "])
            return sep.join(arr)+"\n"
        if len(s.split())>2:  # 多整数
            k = max(3, min(15, len(s.split())+random.randint(-1,3)))
            arr = [str(random.randint(-10**6,10**6)) for _ in range(k)]
            return " ".join(arr)+"\n"
        # 单整数或单行文本
        if s.strip().isdigit():
            return f"{random.randint(0,10**6)}\n"
        # 文本行
        letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ  ,;:_-0123456789"
        L = random.randint(0,60)
        return "".join(random.choice(letters) for _ in range(L))+"\n"

    seen = set((t["input"], t["output"]) for t in tests)
    tries = 0
    while len(tests) < target and tries < 200:
        tries += 1
        base = random.choice(tests)["input"]
        inp = synth_like(base)
        r = eval_code_on_tests(lang, code, [{"input":inp, "output":""}], timeout=5)
        if r["ok"]:  # eval_code_on_tests 只在全部匹配时返回 ok，但这里我们只想拿输出 → 单独跑一遍：
            pass
        # 单次执行拿输出
        from scripts.utils import LANG, _run
        cfg = LANG[lang]
        import tempfile, pathlib
        with tempfile.TemporaryDirectory(dir=ROOT/".tmp_oracle") as tdir:
            tdir = pathlib.Path(tdir)
            src = tdir / f"Main{cfg['ext'] if lang!='Ada' else '.adb'}"
            exe = tdir / "prog"
            src.write_text(code, encoding="utf-8")
            if cfg["compile"]:
                rc, out, err = _run(cfg["compile"].format(src=str(src), exe=str(exe)), timeout=20, cwd=tdir)
                if rc!=0: continue
            rc, out, err = _run(cfg["run"].format(src=str(src), exe=str(exe)), inp=inp, timeout=5, cwd=tdir)
            if rc==0:
                item = (inp, out.strip())
                if item not in seen:
                    tests.append({"input": inp, "output": out.strip()})
                    seen.add(item)
    return tests

if __name__ == "__main__":
    if not KEY:
        print("Set OPENROUTER_API_KEY first.", file=sys.stderr); sys.exit(2)
    ap = argparse.ArgumentParser()
    ap.add_argument("lang")
    ap.add_argument("difficulty")
    ap.add_argument("modelA")
    ap.add_argument("modelB")
    ap.add_argument("instruction", nargs="+")
    ap.add_argument("--min_cases", type=int, default=8)
    ap.add_argument("--target_cases", type=int, default=10)
    args = ap.parse_args()

    lang, diff = args.lang, args.difficulty
    modelA, modelB = args.modelA, args.modelB
    instruction = " ".join(args.instruction)

    sol = gen_solution(modelA, lang, instruction)
    tests = gen_tests(modelB, lang, instruction, min_cases=args.min_cases)
    tests = augment_tests_with_oracle(lang, sol, tests, target=args.target_cases)

    item = {"task_id": f"{lang}/auto", "language": lang, "difficulty": diff,
            "instruction": instruction, "prompt": instruction, "mode": "io",
            "canonical_solution": sol, "tests": tests, "raw_generation":[]}
    print(json.dumps(item, ensure_ascii=False))
