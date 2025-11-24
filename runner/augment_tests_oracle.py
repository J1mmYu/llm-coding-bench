# runner/augment_tests_oracle.py  (REPLACE)
import argparse, json, pathlib, random, sys

# ---- 让脚本能找到 scripts/utils.py ----
HERE = pathlib.Path(__file__).resolve()
PROJECT_ROOT = HERE.parents[1]
SCRIPTS_DIR  = PROJECT_ROOT / "scripts"
if str(SCRIPTS_DIR) not in sys.path:
    sys.path.insert(0, str(SCRIPTS_DIR))

from utils import ROOT, LANG, _run, sanitize_code

WORK = ROOT / ".work_fuzz"
WORK.mkdir(exist_ok=True)

def gen_inp(mode: str) -> str:
    """根据简单 mode 生成输入字符串，最后都带一个换行"""
    if mode == "line":
        L = random.randint(0, 60)
        letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ  ,;:_-0123456789"
        s = "".join(random.choice(letters) for _ in range(L))
        return s + "\n"
    if mode == "int":
        return f"{random.randint(0, 10**6)}\n"
    if mode.startswith("ints:"):
        k = int(mode.split(":", 1)[1])
        arr = [str(random.randint(-10**6, 10**6)) for _ in range(k)]
        return " ".join(arr) + "\n"
    if mode == "csv_ints":
        k = random.randint(1, 30)
        arr = [str(random.randint(-1000, 1000)) for _ in range(k)]
        sep = random.choice([",", ", ", " , "])
        return sep.join(arr) + "\n"
    # 默认退回到“整行随机文本”
    return gen_inp("line")

def infer_mode_for_task(task):
    s = task["tests"][0]["input"]
    lines = s.strip().splitlines()
    # 多行：当前版本先跳过，返回 None
    if len(lines) != 1:
        return None
    line = lines[0]

    # 含字母：当作整行文本
    if any(c.isalpha() for c in line):
        return "line"

    parts = line.split()
    # 全是整数
    def is_int(tok):
        try: int(tok); return True
        except: return False
    if parts and all(is_int(p) for p in parts):
        if len(parts) == 1:
            return "int"
        if len(parts) <= 10:
            return f"ints:{len(parts)}"
        return "ints:10"

    # 其他情况兜底当作 line
    return "line"

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--lang", required=True, choices=list(LANG.keys()))
    ap.add_argument("--mode", required=True,
                    help="line | int | ints:<k> | csv_ints | auto")
    ap.add_argument(
        "--n",
        type=int,
        default=20,
        help="每个任务最多尝试新增多少条测试用例",
    )
    ap.add_argument(
        "--benchmark",
        default=str(ROOT / "benchmark"),
        help="存放 <lang>.jsonl 的目录（就地覆盖写回）",
    )
    args = ap.parse_args()

    bench_path = pathlib.Path(args.benchmark) / f"{args.lang}.jsonl"
    lines = bench_path.read_text(encoding="utf-8").splitlines()

    out_lines: list[str] = []

    for line in lines:
        if not line.strip():
            continue
            mode = args.mode
        if mode == "auto":
            mode = infer_mode_for_task(task)
            if mode is None:
                # 多行题：暂时直接写回，不 augment
                out_lines.append(json.dumps(task, ensure_ascii=False))
                continue
        task = json.loads(line)
        lang = task["language"]
        cfg = LANG[lang]

        # 为当前 task 准备独立工作目录
        tid = task["task_id"].replace("/", "_")
        tdir = WORK / tid
        if tdir.exists():
            for p in tdir.iterdir():
                p.unlink()
        else:
            tdir.mkdir(parents=True, exist_ok=True)

        # 写 canonical solution 源码
        ext = cfg["ext"] if lang != "Ada" else ".adb"
        src = tdir / f"Main{ext}"
        src.write_text(sanitize_code(task["canonical_solution"]), encoding="utf-8")
        exe = tdir / "prog"

        # 编译（如需要）
        if cfg["compile"]:
            rc, out, err = _run(
                cfg["compile"].format(src=str(src), exe=str(exe)),
                timeout=30,
                cwd=tdir,
            )
            if rc != 0:
                # 编译失败就不要乱增 tests，原样写回
                print(f"[SKIP compile error] {task['task_id']}", file=sys.stderr)
                out_lines.append(json.dumps(task, ensure_ascii=False))
                continue

        # 现有样例集合（避免完全重复）
        seen = {(t["input"], t["output"].strip()) for t in task["tests"]}
        added = 0
        max_attempts = args.n * 5  # 给一点冗余，避免全撞重复

        while added < args.n and max_attempts > 0:
            max_attempts -= 1
            inp = gen_inp(args.mode)
            rc, out, err = _run(
                cfg["run"].format(src=str(src), exe=str(exe)),
                inp=inp,
                timeout=10,
                cwd=tdir,
            )
            if rc != 0:
                # 运行失败的就丢弃，不加入 tests
                continue
            pair = (inp, out.strip())
            if pair in seen:
                continue
            seen.add(pair)
            task["tests"].append({"input": inp, "output": out.strip()})
            added += 1

        out_lines.append(json.dumps(task, ensure_ascii=False))

    bench_path.write_text("\n".join(out_lines) + "\n", encoding="utf-8")
    print(f"Augmented tests in {bench_path}")

if __name__ == "__main__":
    main()
