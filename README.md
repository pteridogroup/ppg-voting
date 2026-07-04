# ppg-voting

This repo houses code to help conduct voting for the [Pteridophyte Phylogeny Group (PPG)](https://pteridogroup.github.io) on taxonomic proposals for ferns and lycophytes.

## Monthly ballot pipeline

Creating and tallying ballots is automated with [{targets}](https://books.ropensci.org/targets/). Each month, run:

```r
targets::tar_make()
```

This will, in order:

1. Read `ballot_state.yaml` to figure out the next ballot to create and the next one to tally (always "last completed + 1 month", never today's date, so a busy or skipped month can never silently disappear from voting).
2. Fetch open GitHub issues labeled "taxonomic proposal" for the new ballot's submission month, create a Google Form for them via the Forms API, and draft (but not send) the announcement email.
3. Fetch responses for the ballot being tallied directly from the Forms API, check them against the PPG mailing list, tally votes, write `results/ballot-<n>_results*.csv`, and draft (but not send) the results email.
4. Update `ballot_state.yaml` to record what was just done.

If a submission month has no eligible proposals, no form is created and no ballot number is spent on it — the month is just marked checked, and the next run picks up the following month with the same ballot number it would have used anyway.

**Manual steps that remain:**
- Review each drafted email in the `pteridogroup.no.reply@gmail.com` Gmail drafts folder and send it yourself.
- Manually set a closing timer on each new form (Settings > Responses in the Forms UI) — the Forms API has no way to schedule this. `tar_make()` prints the deadline to use.

**One-time setup:**
1. Enable the Forms API in the Google Cloud project behind `.secrets/client_secret*.json` (in addition to Gmail/Sheets, already enabled).
2. Before running `tar_make()` for the first time, authenticate in your live R console (not inside `tar_make()` itself, since it runs targets in an isolated subprocess where a browser consent flow can't happen):
   ```r
   source("R/google_forms.R")
   forms_token()
   ```
   Complete the browser consent for `pteridogroup.no.reply@gmail.com`. The token is cached in `.secrets`, so subsequent `tar_make()` runs can reuse it silently.

## License

All code is available under the [MIT license](LICENSE)