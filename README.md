# WIP dashboard for r/TheMotte

https://themotte.srid.ca

Run `bin/fetch` once (it pulls the reddit data), followed by `bin/run` to run the dashboard site using that data. Run `bin/fetch` at any point, and the site will hot reload.

## Tasks

- [x] Nixify the script to fetch reddit JSON data (using jq) and put it in ./content
- [x] Generate HTML
- [x] the same for WW and FF
- [x] BLR
- [ ] Generate RSS feeds (and test in Mailbrew)

Initial announcement,

- [x] Avoid nix and reuse the built binary (or docker image) for scheduled updates

Perfect it,

- [ ] Include latest thread *and* the one before (to account for the latest thread having zero or minimal postings)
- [ ] Show regular non-sticky submissions in separate section (and market this as a bird's eye alternative view)
- [ ] "N mins ago" client-side JS replacement on UTC times
- [ ] Compile using windicss, instead of using twind (upstream this to Ema)