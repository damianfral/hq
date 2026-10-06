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
    "hq preview '@users.ix 0.@email'"
    "jq '.users[0].email'"
    "hq preview '@users.each.filter @age >= 21 . @email'"
    "jq 'first(.users[] | select(.age >= 21) | .email)'"
    "hq fold '@users.each.@email'"
    "jq '[.users[].email]'"
    "hq fold '@users.each.filter @age >= 21 . @email'"
    "jq '[.users[] | select(.age >= 21) | .email]'"
    "hq fold '@users.each.filter @email (isSuffixOf \".es\" or isSuffixOf \".com\") . @username'"
    "jq '[.users[] | select(.email | (endswith(\".es\") or endswith(\".com\"))) | .username]'"
    "hq fold '@users.each.@tags.each'"
    "jq '[.users[].tags[]]'"
    "hq fold '@users.each.@address.values'"
    "jq '[.users[].address[]]'"
    "hq over '@users.each.filter @premium == true . @age' '+1'"
    "jq '.users |= map(if .premium == true then .age += 1 else . end)'"
    "hq over '@users.each.@email' 'stripSuffix \".com\"'"
    "jq '.users |= map(.email |= rtrimstr(\".com\"))'"
    "hq over '@users.each.@username' '++ \"_x\"'"
    "jq '.users |= map(.username += \"_x\")'"
    "hq over '@users.each.@first_name' 'stripPrefix \"A\" . trim'"
    "jq '.users |= map(.first_name |= (gsub(\"^\\\\s+|\\\\s+$\"; \"\") | ltrimstr(\"A\")))'"
    "hq over '@users.each.@tags' 'concat [\"new-tag\"]'"
    "jq '.users |= map(.tags += [\"new-tag\"])'"
    "hq over '@users.each.@tags' 'unique'"
    "jq '.users |= map(.tags |= unique)'"
    "hq over '@users.each.@tags' 'sort'"
    "jq '.users |= map(.tags |= sort)'"
    "hq over '@users.each.@premium' 'not'"
    "jq '.users |= map(.premium |= not)'"
    "hq over '@users.each.@age' '* 2 . + 1'"
    "jq '.users |= map(.age = ((.age + 1) * 2))'"
    "hq over '@users.each.@tags' 'length'"
    "jq '.users |= map(.tags |= length)'"
    "hq over '@users.each.@address.keys' '++ \"_x\"'"
    "jq '.users |= map(.address |= with_entries(.key += \"_x\"))'"
    "hq over '@users.each.@address' 'merge {\"city\": \"X\", \"extra\": 1}'"
    "jq '.users |= map(.address += {\"city\": \"X\", \"extra\": 1})'"
    "hq over '@users.each.@address' 'deepMerge {\"coordinates\": {\"zip\": \"00000\"}}'"
    "jq '.users |= map(.address = (.address * {\"coordinates\": {\"zip\": \"00000\"}}))'"
    "hq set '@users.each.@age' '0'"
    "jq '.users |= map(.age = 0)'"
    "hq delete '@users.each.@age'"
    "jq '.users |= map(del(.age))'"
    "hq delete '@users.each.filter @age < 21'"
    "jq '.users |= map(select(.age >= 21))'"
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
        peak_rss_mb=$(awk -v kb="$peak_rss_kb" 'BEGIN { printf "%.1f", kb / 1024 }')
        # RFC 4180: double embedded quotes, so commands containing
        # "..." (e.g. stripSuffix ".com") don't corrupt the row.
        escaped_command=${command//\"/\"\"}

        printf '"%s",%s,%s\n' "$escaped_command" "$elapsed" "$peak_rss_mb" >>"$csv"
    done
}

for bench in "${benches[@]}"; do bench_run "$bench" "$raw"; done

cp "$raw" "$out"
