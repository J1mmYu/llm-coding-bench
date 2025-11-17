# scripts/keep_remaining_after_verify.py
import json, pathlib, argparse
def load_jsonl(p):
    for line in pathlib.Path(p).read_text(encoding="utf-8").splitlines():
        if line.strip(): yield json.loads(line)

ap=argparse.ArgumentParser()
ap.add_argument("--lang", required=True)
ap.add_argument("--incoming", required=True)
ap.add_argument("--benchmark", default="benchmark")
ap.add_argument("--unverified", default="data/unverified")
ap.add_argument("--out", required=True)
args=ap.parse_args()

seen=set()
for p in [pathlib.Path(args.benchmark)/f"{args.lang}.jsonl",
          pathlib.Path(args.unverified)/f"{args.lang}.jsonl"]:
    if p.exists():
        for x in load_jsonl(p): seen.add(x.get("task_id") or x.get("fp"))

out=[]
for x in load_jsonl(args.incoming):
    key = x.get("task_id") or x.get("fp")
    if key not in seen: out.append(x)

pathlib.Path(args.out).write_text("\n".join(json.dumps(x, ensure_ascii=False) for x in out)+"\n","utf-8")
print(f"remaining {len(out)} -> {args.out}")
