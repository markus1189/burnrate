# burnrate

> ### ⚠️ Slop warning
>
> **Every line of this — code, tests, and this README — was written by an LLM in a single
> afternoon (2026-08-14), from a conversation rather than from a design.** Read it with that
> in mind. Specifically:
>
> **What was actually verified.** The Requesty and OpenRouter paths were exercised against
> live accounts and real keys, both budget shapes included; the tests are real, pure, and run
> as part of `nix build`. Four bugs were found and fixed *during* development, each of which
> produced confident, plausible, wrong output: an empty history reporting a burn rate of zero,
> two idle samples implying "never runs out", sparse day maps inflating the average, and a
> provisional marker that fired so often it stopped meaning anything. Assume the same class of
> bug survives somewhere in here.
>
> **What was never verified.** No projection has been checked against an actual end-of-month
> outcome — the forecast is untested against reality, only against arithmetic. The month
> rollover path has never run with real data; it is unit-tested and nothing more. Nor has any
> provider changed its API underneath this yet, which they will.
>
> **It leans on an undocumented endpoint.** `/v1/manage/apikey/self` is not in Requesty's
> docs; it was found by probing. It can disappear without notice or apology.
>
> Cross-check anything that matters against the provider's own console.

A small CLI that reports month-to-date LLM spend and projects where the month will end up, in
one line short enough for a status bar. Requesty is the primary target; OpenRouter works too,
with less to work from.

```
$ burnrate --all --group team=200 'pass:api/requesty/*' 'api/openrouter/**'
team $117/$200 1.2x → Aug 26
main $84/$120 1.5x → Aug 27
ci $31/$50 ~1.3x → Aug 29
scratch $6.42 left → Sep 12
docs $2.10/$40 0.1x
```

Reading a line: spend against budget, then **pace** (`1.0x` is exactly on budget for how far
into the month you are), then the date the budget runs out. **The arrow is the warning** — a
row without one is not projected to run out before the month resets. A `~` marks a provisional
estimate. Prepaid balances show runway (`$6.42 left`) instead of pace, because there is no
monthly reset to measure against.

## Try it with curl first

Nothing to install, and it works with an ordinary key — no management or admin permission.
Substitute your own and run:

```bash
export REQUESTY_API_KEY='rqsty-sk-…'

# 1. who the key is, what it has spent this month, what it is allowed to spend
curl -s https://api-v2.requesty.ai/v1/manage/apikey/self \
  -H "Authorization: Bearer $REQUESTY_API_KEY"
```
```json
{"id":"…","name":"main","monthly_spend":"83.38","monthly_limit":"150",
 "permissions":{"manage":"none","completions":"write"},"group":{"id":"…"}}
```

```bash
# 2. this month's spend, broken down by model, cheapest setup possible
curl -s -X GET https://api-v2.requesty.ai/v1/manage/apikey/self/usage \
  -H "Authorization: Bearer $REQUESTY_API_KEY" \
  -H 'Content-Type: application/json' \
  -d "{\"start\":\"$(date -u +%Y-%m-01)T00:00:00Z\",
       \"resolution\":\"month\",\"group_by\":[\"model_used\"]}" \
| jq -r '.usage | to_entries[] | .value.grouped_data[]
         | [.group_by_values.model_used, .spend, .total_requests] | @tsv' \
| sort -k2 -gr
```
```
claude-opus-5@eu          57.03506747     405
deepseek-v4-flash-0731    16.40327654     3763
claude-sonnet-5@eu        7.54391121      129
```

Swap `"resolution":"month"` for `"day"` or `"hour"` for a time series, and `model_used` for
`provider_used`, `model_requested`, `origin_title` or `is_byok` to slice it differently. The
date is computed, so the commands keep working next month.

Four things that trip people up:

- The host is `api-v2.requesty.ai`. The `api.requesty.ai` in the docs does not resolve at all.
- `self` stands in for the key's own UUID. The documented `/apikey/{uuid}` form needs manage
  permission; `self` does not. It is also undocumented, so treat it as something that works
  today rather than something promised.
- The usage endpoint is a `GET` that carries a request body. Some HTTP clients refuse.
- Ranges are capped at 100 days, and **days with no usage are simply absent** from the
  response — do not average over the keys you get back and call it a daily rate.

## Usage

```
burnrate [--all|--total] [--percent] [--group [NAME=]LIMIT] [--refresh]
         [--max-age S] [--halflife D] [--style tmux|ansi|none] SPEC...
```

With no flag it prints the single row nearest exhaustion, which is what a status bar wants.
`--all` prints every row; `--total` prints one aggregated, label-less line.

| Flag | Meaning |
| --- | --- |
| `--all` | One line per key, soonest exhaustion first |
| `--total` | One aggregated line, no label |
| `--percent` | Print only the projected end-of-month percentage |
| `--group [NAME=]LIMIT` | Declare a shared provider-side budget (see below) |
| `--max-age S` | Cache TTL in seconds, default 300 |
| `--refresh` | Ignore the cache |
| `--halflife D` | Recency half-life for the rate estimate, default 5 days |
| `--style` | `tmux` emits `#[fg=…]`, `ansi` emits escapes, `none` is plain |

### `--percent`

Replaces the line with a single projected figure: **where the month ends up**, not where spend
stands now — current spend plus the estimated rate over the days remaining, as a percentage of
budget.

```
$ burnrate --percent --group team=200 'pass:api/requesty/*'
team 118%
```

This is deliberately not a `--style` value: colour and content are orthogonal, so
`--percent --style tmux` gives you a coloured percentage rather than making you pick one.
Colour then follows the projection rather than the pace. Labels behave as everywhere else —
present under `--all`, absent for `--total`.

Note it answers a different question from pace, and can disagree with it. Pace is backward
looking (spend so far against how far into the month you are); the projection is forward
looking and recency-weighted, so a burst early in the month that has since cooled shows a high
pace and a lower projection. An unmetered key has nothing to divide by and shows `—`.

### Key specs

```
SPEC = [requesty:|openrouter:][pass:|env:|file:|cmd:]ARG
```

The provider is sniffed from the key prefix unless given. The source defaults to `pass`, so a
bare `api/requesty/main` is a password-store entry. `pass` specs may glob — `*` and `?` stop at
`/`, `**` crosses it — and expand to concrete entry names, so sampling history stays keyed per
key. **Quote them**, or the shell expands them first.

```
burnrate api/requesty/main api/openrouter        # two pass entries
burnrate 'pass:api/requesty/*'                   # a whole subtree
burnrate openrouter:env:OPENROUTER_API_KEY       # explicit provider and source
burnrate 'cmd:vault read -field=key secret/llm'  # anything that prints a token
```

### Shell completions

The package ships generated completion files for bash, zsh and fish — build from the same
`Parser` declaration as the CLI, so they cannot drift from it. They enable tab-completion of
SPEC arguments: `pass:` and bare tokens complete against the password store, `env:` against
the current environment, and `file:` against the filesystem (directories get a trailing `/`).
Globs never auto-expand at the prompt, matching how a `pass` glob is only expanded when a run
actually happens.

When installed via `nix`, the shell reads them from the standard locations automatically:

| Shell | File | 
| --- | --- |
| bash | `share/bash-completion/completions/burnrate` |
| zsh | `share/zsh/site-functions/_burnrate` |
| fish | `share/fish/vendor_completions.d/burnrate.fish` |

If you are not using a shell that auto-loads them, source them directly — these are the same
generated scripts, so however you load them they match the real parser:

```bash
eval "$(burnrate --bash-completion-script burnrate)"   # .bashrc
# --zsh-completion-script / --fish-completion-script likewise
```

### Group budgets

Requesty can put a shared budget over a group of keys, and where one exists it is usually the
binding constraint long before any per-key limit is. The API will not disclose the number
without manage permission — `/v1/manage/group/self` returns 403, and no group-usage endpoint
exists at any permission level — so pass it in:

```
burnrate --group team=200 'pass:api/requesty/*'
```

Only the *number* is configured. Membership comes from the group id the API reports for each
key, so a key added to or moved out of the group counts correctly with nothing to edit here. If
the given keys span more than one group the flag is ignored with a message rather than summing
the wrong set. Bare `--group 200` labels the row `group`.

Note the total covers exactly the keys you pass. Any other key in the same group draws on the
same budget without appearing here, so the row reads low if the list is incomplete. The budget
shown is capped at what those keys can actually reach — the sum of their own `monthly_limit`s —
so a single `$15` key of a `$150` group is measured against `$15`, not against a pool the rest
of the group holds most of.

## How the rate is estimated

Three tiers, degrading gracefully:

1. **Provider daily history** — recency-weighted over the completed days of the month,
   zero-filled. Providers omit idle days from their responses, and averaging only the days they
   return overstates the rate by however many days you did not work.
2. **Local sampling log** — for providers exposing no history, every live fetch appends
   `(timestamp, cumulative spend)` to `$XDG_STATE_HOME/burnrate/samples.json`, and consecutive
   deltas make a time series for free. Ignored until it spans at least six hours, since two idle
   samples minutes apart otherwise imply a burn rate of zero and a cheerful "never exhausts".
3. **Flat month-to-date run-rate** — marked `~`.

Intervals are weighted by *elapsed time*, not sample count, so a burst of idle polls cannot
outvote one long busy interval. Today is excluded from the daily history, being only partly
elapsed.

## State on disk

| Path | Contents | Refetchable |
| --- | --- | --- |
| `$XDG_CACHE_HOME/burnrate/responses.json` | Raw API responses, TTL'd | yes |
| `$XDG_STATE_HOME/burnrate/samples.json` | Sampling log | **no** |

Hence the split. The cache can be deleted at will; the sampling log is history that cannot be
reconstructed, and keys absent from a run keep theirs.

## Build

```
nix build            # or: nix run . -- --all 'pass:api/requesty/*'
nix develop          # cabal build / cabal test / HLS
cabal test           # pure tests, no network
```

A cached run is ~15 ms, which is what makes this viable at status-bar polling rates. Without
the cache every poll would mean a fresh round of HTTPS handshakes, so do not set `--max-age 0`
in a bar.

```tmux
set -g status-right '#(burnrate --style tmux --group team=200 "pass:api/requesty/*")'
```

## Supported providers

| Provider | Month-to-date | Daily history | Budget |
| --- | --- | --- | --- |
| Requesty | `monthly_spend` | yes, `/apikey/self/usage` | per-key `monthly_limit`, plus group budgets via `--group` |
| OpenRouter | `usage_monthly` | none — sampling log | per-key `limit`, else account balance from `/credits` |

Requesty is the better-supported of the two, simply because it returns a daily history and
OpenRouter does not: a fresh OpenRouter key has nothing to estimate from until the sampling log
has filled, and says so with a `~`.

Anthropic and OpenAI expose no per-key usage endpoint — both require org-level admin
credentials to read usage — so they cannot join this scheme. Adding a provider means one
`fetchRaw` case and one `parseAcct` case in `Burnrate.Provider`; everything downstream works in
terms of spend, budget and an optional history.

### Provider quirks worth knowing

- Requesty's documented host `api.requesty.ai` does not resolve; it is `api-v2.requesty.ai`.
- Its usage endpoint is a `GET` that takes a request body, and caps ranges at 100 days.
- Money arrives as a JSON string in some fields and a number in others. Both are parsed.
- OpenRouter labels unnamed keys with a masked form of the key itself, which is useless in a
  status bar, so the spec's basename is used instead.
- An OpenRouter key with a null `limit` is bounded by the account balance, not by nothing.
