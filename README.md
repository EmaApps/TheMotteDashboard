# Dashboard for r/TheMotte

Delineate the various sticky threads in r/TheMotte to present an unified and easy-to-glance view of its content. Like Twitter, but without [its toxicity](https://www.srid.ca/niche). An atom feed is also provided for reading in the likes of [Mailbrew](https://www.srid.ca/tdm/mailbrew). 

This is a no-frills statically generated site, powered by [Ema](https://ema.srid.ca/).

Production instance running at https://themotte.srid.ca

## Newsletter

To receive a summary of r/TheMotte postings daily, you may subscribe here.

<link rel="stylesheet" href="https://embed.mailbrew.com/html-embed-style.css" />
<form
  action="https://embed.mailbrew.com/api/form_subscribe"
  method="get"
  style="width: 100%; max-width: 340px; color: #262629; background: #ffffff; padding: 10px; border-radius: 7px"
  id="mailbrew-embed-form"
>
  <a
    href="https://app.mailbrew.com/sridca"
    target="_blank"
    rel="noopener noreferrer"
    style="display: flex; width: 100%; align-items: center; margin-bottom: 6px"
    ><img
      src="https://www.gravatar.com/avatar/c25ae0f8b2c34d0766717abab05b2efb?s=100&amp;d=404&amp;r=g"
      style="width: 22px; height: 22px; border-radius: 22px; margin-right: 6px"
    /><span style="font-weight: 500; flex: 1 1 auto; min-width: 0">srid</span></a
  >
  <h3 style="font-size: 20px; margin-bottom: 6px">TheMotte Daily</h3>
  <div style="display: flex; width: 100%; margin-top: 12px">
    <input
      type="email"
      style="
        height: 32px;
        font-size: 14.5px;
        border-radius: 6px;
        appearance: none;
        padding: 0 8px;
        border: 1px solid rgba(120, 120, 120, 0.3);
        margin-right: 6px;
        flex: 1;
      "
      id="email"
      name="email"
      placeholder="Your email"
      value=""
    /><input type="hidden" name="id" value="U7cipp5tPZdQ" /><button
      type="submit"
      style="
        height: 32px;
        font-size: 14.5px;
        border-radius: 6px;
        appearance: none;
        padding: 0 16px;
        border: none;
        flex: 0 0 auto;
        font-weight: 500;
        cursor: pointer;
        background: #f75858;
        color: white;
      "
    >
      Subscribe
    </button>
  </div>
  <p id="mailbrew-success-message" style="font-size: 13px; margin-top: 5px; color: green"></p>
  <p id="mailbrew-error-message" style="font-size: 13px; margin-top: 5px; color: red"></p>
  <a
    href="https://mailbrew.com/?ref=brew-embed"
    target="_blank"
    rel="noopener noreferrer"
    style="display: block; font-size: 13px; margin-top: 8px; color: rgba(38, 38, 41, 0.6)"
    >Made with
    <img
      src="https://mailbrew-assets.s3.us-east-1.amazonaws.com/embed/embed-logo.png"
      width="18px"
      height="18px"
      style="margin-bottom: -4px; margin-right: 2px"
    />Mailbrew</a
  >
</form>
<script type="text/javascript" src="https://embed.mailbrew.com/html-embed-script.js"></script>

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
