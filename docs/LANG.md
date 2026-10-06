# hq language reference

## Commands

Optics are used with different queries:

| Command | Meaning |
| --- | --- |
| `fold OPTIC` | Print every focused value, one per line. |
| `preview OPTIC` | Print at most the first focused value. |
| `set OPTIC VALUE` | Replace each focus with `VALUE`. |
| `over OPTIC TRANSFORMATION` | Rewrite each focused value in place. |
| `delete OPTIC` | Remove each focused value. |

`preview` needs an optic focusing on at most one value (a field, `ix`,
prism, `filter`, `id`, or compositions of those); `each`, `keys` and
`values` are rejected before running. `set` takes a JSON literal and
is `over` with a constant transformation. `over` fails the whole query
when a focused value rejects the transformation (adding to a string,
for example). Deleting a missing field, or an empty focus, leaves the
input unchanged.

Input may hold any number of whitespace-separated JSON values
(JSON-lines style); every command except `preview` processes each
top-level value in turn. Trailing bytes that do not parse as JSON are
an error.

## Optic language

Optics select and traverse JSON values. Optics are composed with `.`;
each component focuses on a new set of values. Spaces around `.` are
optional, so `@a.each.@b` means `@a . each . @b`. Parentheses group
explicitly: `(@a . each) . @b`.

An optic may focus on nothing at all (a missing field, a value of the
wrong shape, an out-of-bounds `ix`); such focuses simply select no
values. Rewriting through `keys` renames object members.

| Optic | Meaning |
| --- | --- |
| `@field` | Focus the `"field"` member of an object. |
| `each` | Focus every element of an array, or every value of an object. |
| `keys` | Focus every object key, as a string. |
| `values` | Focus every value of an object. For arrays use `each` or `ix`. |
| `id` | Focus the whole value unchanged (the unit of `.`). |
| `_String` | Focus the value only if it is a string, and nothing otherwise. |
| `_Number` | Focus the value only if it is a number, and nothing otherwise. |
| `_Bool` | Focus the value only if it is a boolean, and nothing otherwise. |
| `_Null` | Focus the value only if it is null, and nothing otherwise. |
| `_Array` | Focus the value only if it is an array, and nothing otherwise. |
| `_Object` | Focus the value only if it is an object, and nothing otherwise. |
| `_Just` | Focus any non-null value. |
| `ix N` | Focus array element `N` (0-based). |
| `A . B` | Composition: focus `A`, then `B` within each target. |

### `filter OPTIC TRANSFORMATION`

Keep the input when `TRANSFORMATION` maps some value focused by `OPTIC`
to `true`, and focus on nothing otherwise. The transformation must
produce a boolean.

Single atoms stay bare:

    filter @age == 30

Anything longer takes one paren group:

    filter (each . @age) (== 30)

A `.` after a bare optic starts an outer composition, so a dotted optic
inside `filter` needs the grouped form:

    each . filter (@tags . each) (== "x")

Examples:

- `@name` : Focus the `"name"` field.
- `@users . each . @name` : Focus the `"name"` field of every
  element in `"users"`.
- `@users . each . @age` : Focus the `"age"` field of every element
  in `"users"`.
- `each . _Number` : Focus every number inside an array.
- `@a . ix 1` : Focus the second element of the array in `"a"`.

## Transformations

Transformations describe how a focused value is rewritten. They apply
to one value at a time and compose right-to-left with `.`: in `A . B`,
`B` runs first and `A` runs over its result. So `+ 1 . * 2` doubles
first, then adds one.

Operators bind tightest at the atoms, then `.`, then `and`/`&&`, then
`xor`/`^^`, then `or`/`||` loosest. Parentheses override:
`(== "x") or (== "hi")`.

### Numbers (input and output are numbers)

| Transformation | Meaning |
| --- | --- |
| `+ N` | Add `N`. Example: `+ 1`. |
| `* N` | Multiply by `N`. Example: `* 2`. |
| `- N` | Subtract `N`. Example: `- 1`. |

**Note:** an argument starting with `-` looks like a CLI flag, so
separate it with `--`: `over '@a' -- '- 1'`.
| `/ N` | Divide by `N`. Example: `/ 2`. |

### Strings (input is a string)

| Transformation | Meaning |
| --- | --- |
| `++ "s"` | Append `s`. Example: `++ "!"`. |
| `trim` | Strip surrounding whitespace. |
| `replace "a" "b"` | Replace occurrences of `a` with `b`. |
| `stripPrefix "p"` | Remove the prefix when present. |
| `stripSuffix "s"` | Remove the suffix when present. |
| `isPrefixOf "p"` | Test whether the value starts with `p`. |
| `isSuffixOf "s"` | Test whether the value ends with `s`. |
| `isInfixOf "i"` | Test whether the value contains `i`. |

`stripPrefix` and `stripSuffix` leave the value unchanged when the
affix is absent. The `is*` tests yield a boolean.

### Arrays (input is an array)

| Transformation | Meaning |
| --- | --- |
| `concat [...]` | Append the given elements. Example: `concat [3, 4]`. |
| `reverse` | Reverse the elements. |
| `unique` | Drop duplicates, keeping first occurrences. |
| `length` | Element count, as a number. |
| `isEmpty` | Test whether the array is empty, yielding a boolean. |

### Any value

| Transformation | Meaning |
| --- | --- |
| `== VALUE` (or `= VALUE`) | Test equality with a JSON literal. |
| `const VALUE` | Replace with `VALUE`. |

Equality examples: `== 30`, `== "hi"`, `== true`. `set` is `over`
with `const`.

### Booleans (inputs are booleans, outputs are booleans)

| Transformation | Meaning |
| --- | --- |
| `not` | Negate the value. |
| `A or B` (or `A \|\| B`) | Disjunction; `B` runs only when `A` is `false`. |
| `A and B` (or `A && B`) | Conjunction; `B` runs only when `A` is `true`. |
| `A xor B` (or `A ^^ B`) | Exclusive disjunction. |
| `A . B` | Composition: apply `B` first, then `A` over its result. |

For example:

    hq over '@users . each . @age' '+ 1'

increments every user's age, and

    hq over '@users . each . @name' 'stripPrefix "dr. "'

strips the prefix from every user's name when present.

## Examples

    echo '{"name":"ada","score":96}' | hq fold '@name'
    # "ada"

    echo '{"name":"ada"}' | hq fold '@name' -r
    # ada (raw: strings print without quotes)

    echo '{"a":[1,2,3]}' | hq fold '@a . each'
    # 1, 2, 3 (one per line)

    echo '[{"a":90},{"a":96}]' | hq fold 'each . filter (@a == 90)' -c
    # [{"a":90}]

    echo '{"a":1}' | hq over '@a' '+ 1' -c
    # {"a":2}

    echo '{"a":1,"b":2}' | hq set '@b' '3' -c
    # {"a":1,"b":3}

    echo '{"a":1,"b":2}' | hq delete '@b' -c
    # {"a":1}

    # nix
    nix build . --json \
      | hq -r fold 'each . @outputs . values' \
      | cachix push opensource

    # Kubernetes
    kubectl get pods -o json \
      | hq -r fold '@items . each . @metadata . @name'

    # ffmpeg
    ffprobe -v quiet -print_format json -show_streams video.mp4 \
      | hq -r fold 'each . @codec_name'

    # Github
    curl -s https://api.github.com/repos/damianfral/hq \
      | hq -r fold '@topics.each'
