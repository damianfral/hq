#!/usr/bin/env bash
# shellcheck disable=SC2086
set -ueo pipefail

jsonFile="${1}"
out="${2:-bench-results}"


# Benchmarks.  The jq programs are the closest equivalent of the hq
# query (hq streams and rewrites in place; jq parses the whole doc), so
# the columns are measuring comparable traversal work, not byte-identical
# output.
#
# Format: one shell command per entry (hq or jq reading JSON from stdin).
benches=(
    "hq fold 'each.#id'"
    "jq '[.[].id]'"
    "hq over 'each.#score' '+1'"
    "jq 'map(.score += 1)'"
    "hq over 'each.#version' '+1'"
    "jq 'map(.version += 1)'"
    "hq set 'each.#score' '0'"
    "jq 'map(.score = 0)'"
    "hq delete 'each.#score'"
    "jq 'map(del(.score))'"
)

printf '%s\n' "command,runtime,peak_rss_mb" >"$out"


# Run a single timed configuration and record wall-clock time plus peak RSS.
bench_run() {
    local command="$1"
    local csv="$2"
    local elapsed peak_rss_kb
    # `command` prevents Bash from interpreting `time` as a shell keyword.
    # GNU `time` cannot run shell builtins like `eval`, so run the
    # benchmark command via `bash -c`.
    bash -c "$command" <$jsonFile >/dev/null # warmup
    command time -f '%e,%M' -o time_results.txt \
        bash -c "$command" <$jsonFile >/dev/null
    IFS=, read -r elapsed peak_rss_kb <time_results.txt
    rm -f time_results.txt

    printf '"%s",%s,%s\n' "$command" "$elapsed" "$((peak_rss_kb / 1024))" >>"$csv"
}

for bench in "${benches[@]}"; do bench_run "$bench" "$out"; done
