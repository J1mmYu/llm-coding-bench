# scripts/bulk_ab_generate_balanced.py
import os, sys, json, time, pathlib, itertools, subprocess, random
from utils import task_fingerprint, ROOT, load_fp_index
import argparse

ADD = ROOT/"scripts"/"add_task_manual.py"
AB  = ROOT/"scripts"/"openrouter_ab.py"

parser = argparse.ArgumentParser()
parser.add_argument("lang")
parser.add_argument("difficulty")
parser.add_argument("modelA")
parser.add_argument("modelB")
parser.add_argument("families_dir")
parser.add_argument("count", type=int)
parser.add_argument("--min_cases", type=int, default=10)
parser.add_argument("--target_cases", type=int, default=10)
args = parser.parse_args()

lang, diff = args.lang, args.difficulty
modelA, modelB = args.modelA, args.modelB
famdir, count = args.families_dir, args.count
min_cases, target_cases = args.min_cases, args.target_cases


def load_families(dir_path):
    dirp = pathlib.Path(dir_path)
    fam2prompts = {}
    for p in sorted(dirp.glob("*.txt")):
        lines = [l.strip() for l in p.read_text(encoding="utf-8").splitlines() if l.strip()]
        if lines: fam2prompts[p.stem] = lines
    return fam2prompts

def call_ab(lang, diff, modelA, modelB, instr):
    cmd = [sys.executable, str(AB), lang, diff, modelA, modelB, instr,
        "--min_cases", str(min_cases), "--target_cases", str(target_cases)]
    try:
        p = subprocess.run(cmd,
                        capture_output=True, text=True, timeout=300)
    except subprocess.TimeoutExpired as e:
        return None, f"TimeoutExpired: {e}"
    if p.returncode!=0: return None, p.stderr[:200]
    try:
        return json.loads(p.stdout.strip()), ""
    except Exception as e:
        return None, str(e)

def append_with_add(item, topic, lang, diff, modelA, modelB):
    tags = f"sol_model={modelA},tests_model={modelB}"
    q = subprocess.run(
        [sys.executable, str(ADD),
         "--lang", lang, "--difficulty", diff,
         "--instruction", item["instruction"],
         "--solution", item["canonical_solution"],
         "--tests-json", json.dumps(item["tests"], ensure_ascii=False),
         "--topic", topic,
         "--tags", tags],
        capture_output=True, text=True
    )
    ok = (q.returncode == 0)
    return ok, (q.stdout.strip() if ok else q.stderr[:200])

def main():
    if len(sys.argv)<7:
        print("Usage: bulk_ab_generate_balanced.py <Lang> <Difficulty> <ModelA> <ModelB> <families_dir> <count>", file=sys.stderr); sys.exit(2)
    #lang, diff, modelA, modelB, famdir, count = sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4], sys.argv[5], int(sys.argv[6])
    fam2prompts = load_families(famdir)
    seen_fp = load_fp_index(lang)
    if not fam2prompts: print("No families found.", file=sys.stderr); sys.exit(2)

    # 为了均衡：每个家族一个循环迭代器，round-robin 抽取
    iters = {fam: itertools.cycle(lst) for fam, lst in fam2prompts.items()}
    fam_list = list(fam2prompts.keys())
    produced = 0
    while produced < count:
        for fam in fam_list:
            if produced >= count: break
            instr = next(iters[fam])

            fp = task_fingerprint(lang, instr)
            if fp in seen_fp:
                print("SKIP_PRE", fam, "|", instr[:60], "| already in index")
                continue

            item, err = call_ab(lang, diff, modelA, modelB, instr)
            if not item:
                print("AB error:", fam, "|", err, file=sys.stderr); continue
            ok, msg = append_with_add(item, fam, lang, diff, modelA, modelB)
            print(("OK" if ok else "ERR"), fam, "|", instr[:60], "|", msg)
            if ok: 
                produced += 1
                seen_fp.add(fp)
            time.sleep(0.3)  # 轻节流
    print("Done. Generated:", produced)

if __name__ == "__main__":
    main()
