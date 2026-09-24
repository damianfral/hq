#!/usr/bin/env bash
# shellcheck disable=SC2086
set -ueo pipefail

jsonFile="${1}"
out="${2:-bench-results}"
runs="${3:-5}"


# Each command runs once as warmup, then 'runs' timed runs; the
# reported runtime and peak RSS are the medians of the timed runs.
# Aggregation uses mlr when available, with an awk fallback.
#
# Format: one shell command per entry (hq or jq reading JSON from stdin).
benches=(
    "hq fold '#users.each.#id'"
    "jq '[.users[].id]'"
    "hq fold '#users.each.#email'"
    "jq '[.users[].email]'"
    "hq fold '#users.each.#address.#city'"
    "jq '[.users[].address.city]'"
    "hq fold '#users.each.#bio'"
    "jq '[.users[].bio]'"
    "hq over '#users.each.#age' '+1'"
    "jq '.users |= map(.age += 1)'"
    "hq over '#users.each.#balance' '+1'"
    "jq '.users |= map(.balance += 1)'"
    "hq set '#users.each.#age' '0'"
    "jq '.users |= map(.age = 0)'"
    "hq delete '#users.each.#age'"
    "jq '.users |= map(del(.age))'"
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

# Median of each numeric column grouped by command, preserving the
# bench order so hq/jq pairs stay adjacent in the vega-lite specs.
aggregate_median() {
    local agg="$1"
    if command -v mlr >/dev/null 2>&1; then
        mlr --csv stats1 -a median -f runtime,peak_rss_mb -g command "$raw" >"$agg"
    else
        awk -F, '
            function median(vals, n,  i,j,t) {
                for (i = 2; i <= n; i++) {
                    t = vals[i]; j = i - 1
                    while (j >= 1 && vals[j] > t) { vals[j+1] = vals[j]; j-- }
                    vals[j+1] = t
                }
                # Upper median for even n, mirroring mlr stats1.
                if (n % 2 == 1) return vals[(n+1)/2]
                return vals[n/2+1]
            }
            NR == 1 { next }
            {
                cmd = $1; sub(/^"/, "", cmd); sub(/"$/, "", cmd)
                if (!(cmd in seen)) { seen[cmd] = 1; order[++norder] = cmd }
                rt[cmd] = rt[cmd] " " $2
                rss[cmd] = rss[cmd] " " $3
            }
            END {
                print "command,runtime_median,peak_rss_mb_median"
                for (k = 1; k <= norder; k++) {
                    cmd = order[k]
                    nr = split(rt[cmd], a, " "); ns = split(rss[cmd], b, " ")
                    printf "\"%s\",%s,%s\n", cmd, median(a, nr), median(b, ns)
                }
            }' "$raw" >"$agg"
    fi
}

for bench in "${benches[@]}"; do bench_run "$bench" "$raw"; done

agg="$(mktemp)"
trap 'rm -f "$raw" "$agg" time_results.txt' EXIT
aggregate_median "$agg"

declare -A med_rt med_rss
while IFS=, read -r cmd rt rss; do
    cmd="${cmd#\"}"
    cmd="${cmd%\"}"
    med_rt["$cmd"]="$rt"
    med_rss["$cmd"]="$rss"
done < <(tail -n +2 "$agg")

{
    printf '%s\n' "command,runtime,peak_rss_mb"
    for bench in "${benches[@]}"; do
        printf '"%s",%s,%s\n' "$bench" "${med_rt[$bench]}" "${med_rss[$bench]}"
    done
} >"$out"
