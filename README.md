## Create Docker Image
```bash
cd path\to\llm-coding-bench

docker build -t llm-coding-bench:latest .
```

## Set Env variables
```bash
$env:OPENROUTER_API_KEY = "Your OpenRouter API key"
```

## Choose Your Model and Run bulk_ab
```bash
$MODEL_A = "anthropic/claude-sonnet-4.5"  
$MODEL_B = "openai/gpt-5"                  

docker run --rm `
  -e OPENROUTER_API_KEY=$env:OPENROUTER_API_KEY `
  -v "${PWD}:/work" -w /work llm-coding-bench:latest `
  python3 scripts/bulk_ab_generate_balanced.py `
    Python easy `
    "$MODEL_A" `
    "$MODEL_B" `
    prompts/families `
    10 `
    --min_cases 10 `
    --target_cases 10

```

## Verify Canonical Solution
```bash
$VERIFIER_MODEL = $MODEL_A

docker run --rm `
  -e OPENROUTER_API_KEY=$env:OPENROUTER_API_KEY `
  -v "${PWD}:/work" -w /work llm-coding-bench:latest `
  python3 runner/verify_passk.py `
    --lang Python `
    --k 10 `
    --start_seed 0 `
    --feedback full `
    --model "$VERIFIER_MODEL" `
    --use_existing_first
```

## Solve and bucket
```bash
docker run --rm `
  -e OPENROUTER_API_KEY=$env:OPENROUTER_API_KEY `
  -v "${PWD}:/work" -w /work llm-coding-bench:latest `
  python3 runner/solve_and_bucket.py `
	--lang Fortran
```

## View metadata
```bash
docker run --rm `
  -e OPENROUTER_API_KEY=$env:OPENROUTER_API_KEY `
  -v "${PWD}:/work" -w /work llm-coding-bench:latest `
  python3 runner/dataset_stats.py 
  --langs Python Fortran COBOL Octave Prolog 
  --split both
```