# scripts/generate_from_list.py  (REPLACE)
import os, sys, subprocess, json, pathlib, time, concurrent.futures
from scripts.utils import task_fingerprint, ROOT

def gen_one(lang, diff, modelA, modelB, instr):
    p = subprocess.run([sys.executable, "scripts/openrouter_ab.py", lang, diff, modelA, modelB, instr],
                       capture_output=True, text=True, timeout=300)
    if p.returncode != 0: return ("ERR", instr, p.stderr[:200])
    item = json.loads(p.stdout.strip())
    # 去重：在 append 之前先看指纹文件
    fp = task_fingerprint(lang, item["instruction"])
    fp_idx = ROOT / "data" / "index"; fp_idx.mkdir(parents=True, exist_ok=True)
    idx_file = fp_idx / f"fp_{lang}.txt"
    seen = set(idx_file.read_text(encoding="utf-8").splitlines()) if idx_file.exists() else set()
    if fp in seen: return ("DUP", item["instruction"], "")
    # 追加
    q = subprocess.run([sys.executable, "scripts/add_task_manual.py",
                        "--lang", lang, "--difficulty", diff,
                        "--instruction", item["instruction"],
                        "--solution", item["canonical_solution"],
                        "--tests-json", json.dumps(item["tests"])],
                       capture_output=True, text=True)
    if q.returncode==0:
        with open(idx_file, "a", encoding="utf-8") as f: f.write(fp+"\n")
        return ("OK", item["instruction"], q.stdout.strip())
    else:
        return ("ERR", item["instruction"], q.stderr[:200])

def main():
    if len(sys.argv)<6:
        print("Usage: generate_from_list.py <Lang> <Difficulty> <ModelA> <ModelB> <file>", file=sys.stderr); sys.exit(2)
    lang, diff, modelA, modelB, fpath = sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4], sys.argv[5]
    lines = [l.strip() for l in open(fpath, encoding="utf-8") if l.strip()]
    with concurrent.futures.ThreadPoolExecutor(max_workers=6) as ex:
        futs = [ex.submit(gen_one, lang, diff, modelA, modelB, instr) for instr in lines]
        for fut in concurrent.futures.as_completed(futs):
            status, instr, msg = fut.result()
            print(status, "|", instr[:60], "|", msg)
            time.sleep(0.2)  # 轻微节流
if __name__ == "__main__":
    main()
