import os, sys, json, time, pathlib, argparse, random

HERE = pathlib.Path(__file__).resolve()
PROJECT_ROOT = HERE.parents[1]            # 仓库根目录
SCRIPTS_DIR  = PROJECT_ROOT / "scripts"   # scripts 目录

# 确保 scripts 在 import 路径里；之后统一 `from utils import ...`
if str(SCRIPTS_DIR) not in sys.path:
    sys.path.insert(0, str(SCRIPTS_DIR))

# 临时目录（用于编译/单样例运行）
TMP_VERIFY = PROJECT_ROOT / ".tmp_verify"
TMP_VERIFY.mkdir(parents=True, exist_ok=True)

# ---- 统一从 utils 引用工具函数（不要再用 from scripts.utils ...）----
from utils import sanitize_code, eval_code_on_tests, LANG, _run, ROOT
import urllib.request, urllib.error, http.client, time, json, os, pathlib, sys
# ... 其他 import 保留

API = "https://openrouter.ai/api/v1/chat/completions"
KEY = os.environ.get("OPENROUTER_API_KEY")

def chat(model, messages, temperature=0.7, max_retries=3):
    if not KEY:
        raise RuntimeError("OPENROUTER_API_KEY is not set")

    body = json.dumps({
        "model": model,
        "messages": messages,
        "temperature": temperature
    }).encode("utf-8")

    for attempt in range(max_retries):
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
        try:
            with urllib.request.urlopen(req, timeout=90) as r:
                raw = r.read()
            j = json.loads(raw.decode("utf-8", errors="replace"))
            return j["choices"][0]["message"]["content"]
        except (urllib.error.HTTPError,
                urllib.error.URLError,
                http.client.IncompleteRead) as e:
            # 最后一轮还失败就抛出去，让上层决定怎么处理
            if attempt == max_retries - 1:
                raise
            # 否则稍微等一会再重试
            wait = 2 * (attempt + 1)
            print(f"[chat] {model} attempt {attempt+1} failed: {e} (retry in {wait}s)",
                  file=sys.stderr)
            time.sleep(wait)


API = "https://openrouter.ai/api/v1/chat/completions"
KEY = os.environ.get("OPENROUTER_API_KEY")

def chat(model, messages, temperature=0.7, max_retries=3):
    if not KEY:
        raise RuntimeError("OPENROUTER_API_KEY is not set")

    body = json.dumps({
        "model": model,
        "messages": messages,
        "temperature": temperature
    }).encode("utf-8")

    for attempt in range(max_retries):
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
        try:
            with urllib.request.urlopen(req, timeout=90) as r:
                raw = r.read()
            j = json.loads(raw.decode("utf-8", errors="replace"))
            return j["choices"][0]["message"]["content"]
        except (urllib.error.HTTPError,
                urllib.error.URLError,
                http.client.IncompleteRead) as e:
            # 最后一轮还失败就抛出去，让上层决定怎么处理
            if attempt == max_retries - 1:
                raise
            # 否则稍微等一会再重试
            wait = 2 * (attempt + 1)
            print(f"[chat] {model} attempt {attempt+1} failed: {e} (retry in {wait}s)",
                  file=sys.stderr)
            time.sleep(wait)

def load_jsonl(fp):
    for line in pathlib.Path(fp).read_text(encoding="utf-8", errors="replace").splitlines():
        if line.strip():
            yield json.loads(line)

def append_jsonl(fp, obj):
    p = pathlib.Path(fp); p.parent.mkdir(parents=True, exist_ok=True)
    with p.open("a", encoding="utf-8") as f:
        f.write(json.dumps(obj, ensure_ascii=False) + "\n")
# --- 在尝试循环里加入反馈逻辑 ---
def first_failure_detail(lang, code, tests):
    """
    返回 (status, fail_input, stderr_excerpt)
    - 不泄露期望输出，只提供失败输入（在 feedback=full 时）
    """
    import tempfile

    cfg = LANG[lang]
    with tempfile.TemporaryDirectory(dir=TMP_VERIFY) as tdir:
        tdir = pathlib.Path(tdir)
        # 根据语言写源码文件名（Ada 特例）
        ext = cfg['ext'] if lang != 'Ada' else '.adb'
        src = tdir / f"Main{ext}"
        exe = tdir / "prog"
        src.write_text(code, encoding="utf-8")

        # 编译
        if cfg["compile"]:
            rc, out, err = _run(cfg["compile"].format(src=str(src), exe=str(exe)), timeout=20, cwd=tdir)
            if rc != 0:
                return ("COMPILE_ERROR", None, (err or "")[:300])

        # 逐条用例定位首个失败
        for tt in tests:
            rc, out, err = _run(cfg["run"].format(src=str(src), exe=str(exe)),
                                inp=tt["input"], timeout=6, cwd=tdir)
            if rc != 0 or out.strip() != tt["output"].strip():
                return ("RUNTIME_OR_WRONG", tt["input"], (err or "")[:300])
    return ("OK", None, "")

def build_prompt(instruction, fb_mode, fail_status, fail_input, stderr_excerpt):
    hint = ""
    if fb_mode == "minimal":
        if fail_status == "COMPILE_ERROR":
            hint = f"\nNote: Your previous attempt had a compiler error. Fix compilation issues."
        elif fail_status == "RUNTIME_OR_WRONG":
            hint = "\nNote: Your previous attempt produced a wrong answer or crashed. Re-check I/O parsing and corner cases."
    elif fb_mode == "full":
        if fail_status == "COMPILE_ERROR":
            hint = f"\nNote: Your previous attempt had a compiler error:\n{stderr_excerpt}\nFix errors."
        elif fail_status == "RUNTIME_OR_WRONG":
            # 给出失败输入但不透露期望输出
            hint = "\nNote: Your previous attempt failed on this input:\n" + fail_input
            hint += "\nDo not print explanations; only correct output."
    return instruction + hint

# 生成函数增加可选“带反馈prompt”的入参
def gen_solution(model, lang, prompt_text, temperature=0.7, seed=0):
    sys_prompt = f"You are a {lang} expert. Return ONLY executable {lang} code that reads from stdin and writes to stdout. No comments, no markdown."
    messages = [{"role":"system","content":sys_prompt},{"role":"user","content":prompt_text}]
    return chat(model, messages)

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--lang", required=True)
    ap.add_argument("--k", type=int, default=10)
    ap.add_argument("--model", required=True)
    ap.add_argument("--src", default=str(ROOT / "data" / "incoming"))
    ap.add_argument("--verified_out", default=str(ROOT / "benchmark"))
    ap.add_argument("--unverified_out", default=str(ROOT / "data" / "unverified"))
    ap.add_argument("--use_existing_first", action="store_true",
                    help="先尝试 canonical_solution 再抽样生成")
    ap.add_argument("--start_seed", type=int, default=0)
    ap.add_argument("--feedback", choices=["none","minimal","full"], default="none",
                help="给下一次尝试的反馈力度（建库可用 minimal/full，评测请用 none）")
    args = ap.parse_args()
    assert KEY, "Please set OPENROUTER_API_KEY"

    src_file = pathlib.Path(args.src) / f"{args.lang}.jsonl"
    ver_file = pathlib.Path(args.verified_out) / f"{args.lang}.jsonl"
    unv_file = pathlib.Path(args.unverified_out) / f"{args.lang}.jsonl"

    already_verified = set()
    if ver_file.exists():
        for obj in load_jsonl(ver_file):
            v = obj.get("verification", {})
            if v.get("verified"):
                already_verified.add(obj["task_id"])
    print(f"[verify_passk] Found {len(already_verified)} already-verified tasks in {ver_file}")

    report = {"lang": args.lang, "k": args.k, "model": args.model, "started_at": time.time(), "items":[]}
    total = okcnt = 0
    for task in load_jsonl(src_file):
        total += 1
        if task["task_id"] in already_verified:
            # 已经通过验证的题直接跳过，不再重复写入
            report["items"].append({"task_id": task["task_id"], "status":"SKIP(already)"})
            continue
        tests = task["tests"]; instr = task["instruction"]
        attempts = []
        # 0) 可选：先拿现有 canonical_solution 试一次
        if args.use_existing_first and task.get("canonical_solution"):
            r0 = eval_code_on_tests(args.lang, task["canonical_solution"], tests)
            attempts.append({"kind":"existing", "ok": r0["ok"], "status": r0["status"]})
            if r0["ok"]:
                task["canonical_solution"] = sanitize_code(task["canonical_solution"])
                task["verification"] = {"verified": True, "pass_k": f"pass@{args.k}", "verifier_model": "existing",
                                        "k": args.k, "seeds": [], "attempts": 1, "ts": time.time()}
                append_jsonl(ver_file, task); okcnt += 1
                report["items"].append({"task_id": task["task_id"], "status":"OK(existing)"})
                continue

        verified = False; passed_code = None
        fail_status = None; fail_input = None; fail_stderr = ""
        for i in range(args.start_seed, args.start_seed + args.k):
            prompt_now = build_prompt(instr, args.feedback, fail_status, fail_input, fail_stderr)
            try:
                code = gen_solution(args.model, args.lang, prompt_now, seed=i)
            except Exception as e:
                # 模型调用失败（网络问题、IncompleteRead 等），这次尝试就算作失败
                print(f"[{task['task_id']}] model call failed on attempt {i}: {e}",
                    file=sys.stderr)
                last_err = str(e)
                continue
            r = eval_code_on_tests(args.lang, code, tests)
            attempts.append({"kind":"gen", "i": i, "ok": r["ok"], "status": r["status"]})
            if r["ok"]:
                verified = True; passed_code = sanitize_code(code); break
            if args.feedback != "none":
                fail_status, fail_input, fail_stderr = first_failure_detail(args.lang, code, tests)

        if verified:
            task["canonical_solution"] = passed_code
            task["verification"] = {"verified": True, "pass_k": f"pass@{args.k}",
                                    "verifier_model": args.model, "k": args.k,
                                    "seeds": list(range(len(attempts))), "attempts": len(attempts), "ts": time.time()}
            append_jsonl(ver_file, task); okcnt += 1
            report["items"].append({"task_id": task["task_id"], "status":"OK"})
        else:
            task["verification"] = {"verified": False, "pass_k": f"pass@{args.k}",
                                    "verifier_model": args.model, "k": args.k,
                                    "seeds": list(range(len(attempts))), "attempts": len(attempts), "ts": time.time()}
            append_jsonl(unv_file, task)
            report["items"].append({"task_id": task["task_id"], "status":"FAIL"})

    report.update({"finished_at": time.time(), "total": total, "verified": okcnt})
    meta = ROOT / "meta" / "runs"; meta.mkdir(parents=True, exist_ok=True)
    (meta / f"verify_{args.lang}_{int(time.time())}.json").write_text(json.dumps(report, ensure_ascii=False, indent=2), "utf-8")

if __name__ == "__main__":
    main()
