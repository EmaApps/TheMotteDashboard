# Dashboard for r/TheMotte

Delineate the various sticky threads in r/TheMotte to present an unified and easy to glance its content. Like Twitter, but without [its toxicity](https://www.srid.ca/niche). An atom feed is also provided for reading in the likes of [Mailbrew](https://www.srid.ca/tdm/mailbrew). 

This is a no-frills statically generated site, powered by [Ema](https://ema.srid.ca/).

Production instance running at https://themotte.srid.ca

## Local develpoment

Run `bin/fetch` once (it pulls the reddit data), followed by `bin/run` to run the dashboard site using that data. Run `bin/fetch` at any point, and the site will hot reload.

## Tasks

- [x] Nixify the script to fetch reddit JSON data (using jq) and put it in ./content
- [x] Generate HTML
- [x] the same for WW and FF
- [x] BLR
- [x] Generate RSS/Atom feeds (and test in Mailbrew)

Initial announcement,

- [x] Avoid nix and reuse the built binary (or docker image) for scheduled updates

Perfect it,

- [ ] Include latest thread *and* the one before (to account for the latest thread having zero or minimal postings)
- [ ] Show regular non-sticky submissions in separate section (and market this as a bird's eye alternative view)
- [ ] "N mins ago" client-side JS replacement on UTC times
- [ ] Compile using windicss, instead of using twind (upstream this to Ema)
- [ ] In ViewFull mode, render full Markdown comment?

[Reddit feedback](https://old.reddit.com/r/TheMotte/comments/p9tvxl/culture_war_roundup_for_the_week_of_august_23_2021/ha2mhmd/),

- [ ] "count number of replies to the top level comments"
    - "find the time since the last comment reply to the top level comments, and display those things"
- [x] "expand the length of the preview field for the expanded view when clicking on one of the categories"