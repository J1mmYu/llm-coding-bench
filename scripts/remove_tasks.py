import argparse, json, pathlib, re
ROOT=pathlib.Path(__file__).resolve().parent.parent
def keep(obj, args):
    if args.lang and obj.get("language")!=args.lang: return True
    tid=obj.get("task_id","")
    if args.prefix and tid.startswith(args.prefix): return False
    if args.suffix and tid.endswith(args.suffix): return False
    if args.contains and args.contains in tid: return False
    if args.regex and re.search(args.regex, tid): return False
    return True
if __name__=="__main__":
    ap=argparse.ArgumentParser()
    ap.add_argument("--lang", choices=["Python","Fortran","COBOL","Ada","Octave","Prolog"])
    ap.add_argument("--prefix"); ap.add_argument("--suffix"); ap.add_argument("--contains"); ap.add_argument("--regex")
    args=ap.parse_args()
    if not args.lang: ap.error("--lang required to avoid accidents")
    fp=ROOT/"benchmark"/f"{args.lang}.jsonl"
    lines=[json.loads(l) for l in open(fp,encoding="utf-8") if l.strip()]
    kept=[l for l in lines if keep(l,args)]
    with open(fp,"w",encoding="utf-8") as f:
        for o in kept: f.write(json.dumps(o,ensure_ascii=False)+"\n")
    print(f"Removed {len(lines)-len(kept)} items from {fp}")
