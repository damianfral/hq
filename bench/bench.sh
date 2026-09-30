#!/usr/bin/env bash
# shellcheck disable=SC2086
set -ueo pipefail

jsonFile="${1}"
out="${2:-bench-results}"
runs="${3:-5}"


# Each command runs once as warmup, then 'runs' timed runs; every
# sample is recorded. Median aggregation happens in the Vega-Lite
# specs (aggregate op median grouped by command).
# Format: one shell command per entry (hq or jq reading JSON from stdin).
benches=(
    "hq fold '@users.each.@id'"
    "jq '[.users[].id]'"
    "hq fold '@users.each.@email'"
    "jq '[.users[].email]'"
    "hq fold '@users.each.@address.@city'"
    "jq '[.users[].address.city]'"
    "hq fold '@users.each.@bio'"
    "jq '[.users[].bio]'"
    "hq fold '@users.each.filter @age == 29'"
    "jq '[.users[] | select(.age == 29)]'"
    "hq fold '@users.each.filter @premium == true'"
    "jq '[.users[] | select(.premium == true)]'"
    "hq fold '@users.each.filter @age == 29 . @email'"
    "jq '[.users[] | select(.age == 29) | .email]'"
    "hq over '@users.each.filter @premium == true . @age' '+1'"
    "jq '.users |= map(if .premium == true then .age += 1 else . end)'"
    "hq over '@users.each.@age' '+1'"
    "jq '.users |= map(.age += 1)'"
    "hq over '@users.each.@balance' '+1'"
    "jq '.users |= map(.balance += 1)'"
    "hq set '@users.each.@age' '0'"
    "jq '.users |= map(.age = 0)'"
    "hq set '@users.each.filter @age == 29' '0'"
    "jq '.users |= map(if .age == 29 then 0 else . end)'"
    "hq delete '@users.each.@age'"
    "jq '.users |= map(del(.age))'"
    "hq delete '@users.each.filter @age == 29'"
    "jq '.users |= map(select(.age != 29))'"
)

# Print the benchmark commands without running them, so other
# tooling (e.g. profiling) can reuse the exact same commands.
if [[ "${1:-}" == "--list" ]]; then
    printf '%s\n' "${benches[@]}"
    exit 0
fi

raw="$(mktemp)"
trap 'rm -f "$raw" time_results.txt' EXIT
printf '%s\n' "command,runtime,peak_rss_mb" >"$raw"


# Run a single timed configuration 'runs' times and record every sample.
bench_run() {
    local command="$1"
    local csv="$2"
    local elapsed peak_rss_kb i
    # `command` prevents Bash from interpreting `time` as a shell keyword.
    # GNU `time` cannot run shell builtins like `eval`, so run the
    # benchmark command via `bash -c`.
    bash -c "$command" <$jsonFile >/dev/null # warmup
    for ((i = 1; i <= runs; i++)); do
        command time -f '%e,%M' -o time_results.txt \
            bash -c "$command" <$jsonFile >/dev/null
        IFS=, read -r elapsed peak_rss_kb <time_results.txt

        printf '"%s",%s,%s\n' "$command" "$elapsed" "$((peak_rss_kb / 1024))" >>"$csv"
    done
}

for bench in "${benches[@]}"; do bench_run "$bench" "$raw"; done

cp "$raw" "$out"
