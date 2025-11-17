# scripts/audit_tests.py
import json, argparse, pathlib, re
from utils import eval_code_on_tests, sanitize_code, ROOT

def load_jsonl(p):
    for line in pathlib.Path(p).read_text(encoding="utf-8", errors="replace").splitlines():
        if line.strip(): yield json.loads(line)

def save_jsonl(p, arr):
    p = pathlib.Path(p); p.parent.mkdir(parents=True, exist_ok=True)
    with p.open("w", encoding="utf-8") as f:
        for x in arr: f.write(json.dumps(x, ensure_ascii=False) + "\n")

def wants_empty(instr:str)->bool:
    return bool(re.search(r"\b(empty|blank|可能为空|可为空)\b", instr, flags=re.I))

def audit_task(lang, t):
    code = sanitize_code(t["canonical_solution"])
    tests = t["tests"]
    # 精细化逐条跑，拿到第一个失败的原因
    for i, tt in enumerate(tests):
        r = eval_code_on_tests(lang, code, [tt], timeout=6)
        if not r["ok"]:
            status = "EOF" if r.get("status")=="FAIL" and "EOF" in r.get("last",{}).get("err","") else r.get("status","FAIL")
            return {"task_id": t["task_id"], "status": status, "idx": i,
                    "input": tt.get("input",""), "expected": tt.get("output","")}
    return {"task_id": t["task_id"], "status": "OK"}

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--in", dest="inp", required=True)
    ap.add_argument("--out", dest="outp")  # 可选：输出清洗后的版本
    ap.add_argument("--lang", required=True)
    ap.add_argument("--drop_empty", action="store_true", help="删除无声明的空输入测试")
    ap.add_argument("--report", required=True)
    args = ap.parse_args()

    tasks = list(load_jsonl(args.inp))
    report = {"total": len(tasks), "OK":0, "EOF":0, "WRONG_OUTPUT":0, "others":0, "items":[]}
    cleaned = []

    for t in tasks:
        res = audit_task(args.lang, t)
        report["items"].append(res)
        if res["status"]=="OK": report["OK"] += 1
        elif res["status"] in ("EOF","COMPILE_ERROR"): report["EOF"] += 1
        elif res["status"]=="FAIL": report["WRONG_OUTPUT"] += 1
        else: report["others"] += 1

        if args.drop_empty and not wants_empty(t["instruction"]):
            t["tests"] = [x for x in t["tests"] if x.get("input","").strip()!=""]
        cleaned.append(t)

    pathlib.Path(args.report).write_text(json.dumps(report, ensure_ascii=False, indent=2), "utf-8")
    if args.outp:
        save_jsonl(args.outp, cleaned)
        print(f"Saved cleaned -> {args.outp}")
    print(f"Report -> {args.report}")

if __name__ == "__main__":
    main()
