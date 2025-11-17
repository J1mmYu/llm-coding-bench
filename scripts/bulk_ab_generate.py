# scripts/bulk_ab_generate.py  (REPLACE)
import os, sys, json, random, time, subprocess, pathlib
from scripts.utils import task_fingerprint, ROOT

ROOT = pathlib.Path(__file__).resolve().parent.parent
ADD = ROOT / "scripts" / "add_task_manual.py"
AB  = ROOT / "scripts" / "openrouter_ab.py"

TEMPLATES = [
  ("strings", "Read a line and print the string with vowels removed (aeiou, case-insensitive)."),
  ("strings", "Read a line and print 'YES' if it is a palindrome ignoring non-alphanumerics and case, else 'NO'."),
  ("strings", "Read a line and convert camelCase to snake_case (ASCII letters, digits preserved)."),
  ("arrays",  "Read n then n integers. Print the number of distinct values."),
  ("arrays",  "Read n then n integers. Print the length of the longest strictly increasing contiguous subarray."),
  ("math",    "Given n, print the sum of the digits of n."),
  ("math",    "Given n, print the number of set bits in binary representation of n."),
  ("parsing", "Read a line containing integers separated by commas and/or spaces; print their sum."),
  ("maps",    "Read m then m lines of 'key value'; then read q and q queries; for each key print value or 'NA'."),
  ("files",   "Read all input and print the number of lines, words, and bytes (like wc)."),
  ("graphs",  "Read n,m then m edges (1-indexed). Print the degree of each node on one line separated by spaces."),
]

def main():
    if "OPENROUTER_API_KEY" not in os.environ:
        print("Set OPENROUTER_API_KEY.", file=sys.stderr); sys.exit(2)
    if len(sys.argv)<5:
        print("Usage: bulk_ab_generate.py <Lang> <Difficulty> <ModelA> <ModelB> [count]", file=sys.stderr); sys.exit(2)
    lang, diff, modelA, modelB = sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4]
    count = int(sys.argv[5]) if len(sys.argv)>5 else 5
    fp_idx = ROOT / "data" / "index"; fp_idx.mkdir(parents=True, exist_ok=True)
    idx_file = fp_idx / f"fp_{lang}.txt"
    seen = set(idx_file.read_text(encoding="utf-8").splitlines()) if idx_file.exists() else set()

    for _ in range(count):
        _, instr = random.choice(TEMPLATES)
        # 生成
        p = subprocess.run([sys.executable, str(AB), lang, diff, modelA, modelB, instr],
                           capture_output=True, text=True, timeout=300)
        if p.returncode!=0:
            print("AB gen error:", p.stderr[:200], file=sys.stderr); continue
        try:
            item = json.loads(p.stdout.strip())
            fp = task_fingerprint(lang, item["instruction"])
            if fp in seen:
                print("SKIP dup:", item["instruction"][:60]); continue
            # 落盘
            q = subprocess.run([sys.executable, str(ADD), "--lang", lang, "--difficulty", diff,
                                "--instruction", item["instruction"], "--solution", item["canonical_solution"],
                                "--tests-json", json.dumps(item["tests"])],
                               capture_output=True, text=True)
            if q.returncode==0:
                print(q.stdout.strip()); seen.add(fp); idx_file.write_text("\n".join(sorted(seen))+"\n", encoding="utf-8")
            else:
                print("append error:", q.stderr[:200], file=sys.stderr)
        except Exception as e:
            print("Parse/append error:", e, file=sys.stderr)
        time.sleep(0.5)  # 更温和的节流
if __name__ == "__main__":
    main()
