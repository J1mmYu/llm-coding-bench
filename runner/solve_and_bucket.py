# runner/solve_and_bucket.py  (REPLACE)
import os, json, time, pathlib, argparse, concurrent.futures
from collections import defaultdict
import sys


HERE = pathlib.Path(__file__).resolve()
PROJECT_ROOT = HERE.parents[1]
SCRIPTS_DIR  = PROJECT_ROOT / "scripts"
if str(SCRIPTS_DIR) not in sys.path:
    sys.path.insert(0, str(SCRIPTS_DIR))

from utils import eval_code_on_tests, sanitize_code, ROOT

API = "https://openrouter.ai/api/v1/chat/completions"
KEY = os.environ.get("OPENROUTER_API_KEY")


MODELS = [
    "openai/gpt-5.1",
    "google/gemini-3-pro-preview",
    "qwen/qwen3-next-80b-a3b-thinking", #qwen3-max
    "meta-llama/llama-4-maverick"
    "anthropic/claude-sonnet-4.5",
    "x-ai/grok-4.1-fast",
    "z-ai/glm-4.6",
    "deepseek/deepseek-v3.1-terminus"
]

def chat(model, messages, temperature=0.7):
    import urllib.request
    body = json.dumps(
        {"model": model, "messages": messages, "temperature": temperature}
    ).encode("utf-8")
    req = urllib.request.Request(
        API,
        data=body,
        method="POST",
        headers={
            "Content-Type": "application/json",
            "Authorization": f"Bearer {KEY}",
            "HTTP-Referer": "http://localhost",
            "X-Title": "llm-coding-bench",
        },
    )
    with urllib.request.urlopen(req, timeout=90) as r:
        j = json.loads(r.read().decode("utf-8", errors="replace"))
    return j["choices"][0]["message"]["content"]

def gen_solution(modelspec, lang, instruction):
    if "#t=" in modelspec:
        model, t = modelspec.split("#t=")
        t = float(t)
    else:
        model, t = modelspec, 0.7
    sp = (
        f"You are a {lang} expert. "
        "Return ONLY executable {lang} code that reads from stdin and writes to stdout. "
        "No comments, no markdown."
    )
    code = chat(
        model,
        [
            {"role": "system", "content": sp},
            {"role": "user", "content": instruction},
        ],
        temperature=t,
    )
    return sanitize_code(code)

def load_jsonl(fp):
    path = pathlib.Path(fp)
    if not path.exists():
        return []
    for line in path.read_text(encoding="utf-8", errors="replace").splitlines():
        if line.strip():
            yield json.loads(line)

def bucket(passes, total):
    # 这里可以以后再细调阈值，现在先和我们讨论的一致
    ratio = passes / total
    if ratio >= 0.8:
        return "Easy"
    if ratio <= 0.3:
        return "Hard"
    else:
        return "Medium"

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--lang", required=True)
    ap.add_argument(
        "--benchmark",
        default=str(ROOT / "benchmark"),
        help="目录下读取 <lang>.jsonl",
    )
    ap.add_argument("--max_workers", type=int, default=8)
    args = ap.parse_args()
    assert KEY, "Set OPENROUTER_API_KEY"

    src = pathlib.Path(args.benchmark) / f"{args.lang}.jsonl"
    tasks = list(load_jsonl(src))
    total_cfg = len(MODELS)
    results = defaultdict(int)

    def solve_one(cfg, t):
        try:
            code = gen_solution(cfg, args.lang, t["instruction"])
            r = eval_code_on_tests(args.lang, code, t["tests"])
            return (t["task_id"], 1 if r["ok"] else 0, cfg)
        except Exception:
            return (t["task_id"], 0, cfg)

    with concurrent.futures.ThreadPoolExecutor(
        max_workers=args.max_workers
    ) as ex:
        futs = [ex.submit(solve_one, cfg, t) for cfg in MODELS for t in tasks]
        for fut in concurrent.futures.as_completed(futs):
            tid, ok, cfg = fut.result()
            results[tid] += ok

    lines = ["task_id,lang,passes,total,bucket"]
    for t in tasks:
        p = results[t["task_id"]]
        lines.append(
            f'{t["task_id"]},{args.lang},{p},{total_cfg},{bucket(p,total_cfg)}'
        )

    outp = ROOT / "meta" / "runs" / f"bucket_{args.lang}_{int(time.time())}.csv"
    outp.parent.mkdir(parents=True, exist_ok=True)
    outp.write_text("\n".join(lines) + "\n", encoding="utf-8")
    print("Saved", outp)

if __name__ == "__main__":
    main()
