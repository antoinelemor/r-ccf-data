# ccfdata — R client for the Canadian Climate Framing API

Authenticated, tibble-friendly access to the Canadian Climate Framing
(CCF) data platform: a corpus of ~250,000 Canadian newspaper articles
(1978–2024), 9.2 M sentences, and 67 climate-coverage annotations
(framings, messengers, events, solutions, tone, named entities).

Live API: <https://data.ccf-project.ca>

## Installation

```r
# install.packages("remotes")
remotes::install_github("antoinelemor/r-ccf-data")
```

Requires R ≥ 4.0. Hard dependencies: `httr2`, `jsonlite`, `tibble`,
`rlang`.

## Authentication

All requests need a long-lived JWT API key.

1. Sign in at <https://data.ccf-project.ca>.
2. Open the **Profile** page → **Generate API key**.
3. Copy the key. Each key is bound to one user account and a tier
   (see below) with daily request / search / export quotas.

```r
library(ccfdata)
Sys.setenv(CCF_TOKEN = "eyJhbG...")    # or pass token = directly
ccf <- ccf_client()                    # uses CCF_TOKEN by default
ccf_me(ccf)                            # username, role, tier, quotas
```

You can also point at a self-hosted instance:

```r
ccf <- ccf_client(token = "...", base_url = "http://localhost:8005")
```

## API tiers

The platform enforces five progressive tiers per token. The tier is
assigned by an administrator when the token is created and dictates
both *which* endpoints the token may call and *how many* requests it
can issue per day.

| Tier         | Default req/day | Endpoints unlocked                                              |
|--------------|----------------:|-----------------------------------------------------------------|
| `metadata`   | 1 000           | summary, schema, geo, articles-by-year/media, frame-trends      |
| `analyst`    | 5 000           | + distributions, trends, cross-tab, subcategory/messenger/event/solution analyses |
| `researcher` | 20 000          | + search/article/articles batch, all cascades + events, semantic search |
| `expert`     | unlimited       | + CSV exports                                                   |
| `writer`     | unlimited       | + admin endpoints (used for internal tools)                     |

Two extra search-specific quotas (`searches/day`, `exports/day`) layer
on top. After every call the client stashes `tier`,
`requests_remaining`, `searches_remaining`, `exports_remaining` from
the `X-CCF-*` response headers:

```r
ccf_summary(ccf)
ccf_last_status(ccf)
# $tier "researcher"; $requests_remaining 19999; ...
```

When a quota is exhausted the client raises a condition with class
`ccf_quota_error`; tier-too-low raises `ccf_tier_error`. Trap them with
`tryCatch`:

```r
tryCatch(
  ccf_search(ccf, "carbon tax"),
  ccf_tier_error = function(e) {
    message("need tier ", e$body$required_tier,
            ", you have ", e$body$tier)
  },
  ccf_quota_error = function(e) {
    message("quota ", e$body$reason, " on tier ", e$body$tier)
  }
)
```

## Quick tour

### Corpus stats and schema

```r
ccf_summary(ccf)              # totals, frame counts, date range
ccf_geo_data(ccf)             # province-level aggregates
ccf_codebook()                # offline list — operational definitions
ccf_define("eco_neg_impact")  # one column's definition
ccf_codebook_df()             # codebook as a tidy tibble
```

### Time series and distributions

```r
df <- ccf_distribution(ccf,
                       columns = c("economic_frame", "health_frame"),
                       group_by = "year", lang = "en")

ccf_tone_trends(ccf, media = "Globe and Mail")
ccf_canada_coverage(ccf, date_from = "2015-01-01")
ccf_cross_tabulation(ccf, "economic_frame", "tone_negative")
```

### Search

```r
# Plain full-text search (Postgres FTS, language-aware).
df <- ccf_search(ccf, "carbon tax", level = "sentence",
                 filters = list(lang = "en", date_from = "2015-01-01"),
                 limit = 500)

# Article-level with filters by frame, tone, media, threshold:
ccf_search(ccf, "carbon tax", level = "article",
           filters = list(frames = c("economic"), tone = "negative",
                          media = c("Globe and Mail", "Toronto Star")),
           thresholds = list(list(column = "economic_frame", min_pct = 0.3)))

# FAISS dense semantic search:
ccf_semantic_search(ccf, "climate refugees in the Arctic", k = 500)

# Aggregate stats for any query (year + media distribution):
ccf_search_summary(ccf, "carbon tax", filters = list(lang = "en"))

# Server-side CSV export → tibble (requires expert tier):
ccf_search_export(ccf, "carbon tax", filters = list(lang = "en"),
                  columns = c("doc_id", "sentence_text", "pub_date",
                              "media", "dominant_frame"))
```

`ccf_search()` auto-paginates. Pass `limit = N` to cap, or
`page_size = N` to tune the server page size. Set `raw = TRUE` to get
a list with the raw rows + last response.

### Articles

```r
art <- ccf_article(ccf, 123456L)
art$title; art$media; art$date

ccf_articles_batch(ccf, c(123456, 123457, 123458))
```

### Cascades — cross-year media bursts

```r
ccf_cascades_summary(ccf)
ccf_cascades_cross_year_all(ccf)              # all cascades, slim
ccf_cascade_year(ccf, 2020L)
ccf_cascade_detail(ccf, 2020L, "Eco_x_3")
ccf_cascade_network(ccf, 2020L, "Eco_x_3")
ccf_cascades_semantic_search(ccf, "IPCC report")
```

### Events — cross-year event clusters

```r
ccf_events_summary(ccf)
ccf_events_clusters(ccf, year_min = 2018L, types = c("evt_weather"),
                    limit = 200)
ccf_events_cluster(ccf, 2020L, 42L)
ccf_events_cluster_articles(ccf, 2020L, 42L)
ccf_events_semantic_search(ccf, "wildfire smoke")
```

## Annotation schema (offline)

The 67-category annotation framework is bundled in
`inst/extdata/codebook.json` and exposed through helpers that don't
hit the network:

```r
ccf_frame_names()           # 8 frame names
ccf_frame_columns()         # 8 DB column names
ccf_subcategories_of("economic")
ccf_media_outlets()         # 20 outlets
ccf_define("sci_skepticism")
ccf_codebook_df()           # tidy tibble
```

Two of the 67 categories (`health_pos_impact`, `health_footprint`) are
documented in the codebook but excluded from analysis (insufficient
training data). They appear in `ccf_codebook()$definitions` but not in
the operational column listing.

## Errors

The package raises the following typed conditions (all extend
`ccf_error`):

| Class               | Raised on |
|---------------------|-----------|
| `ccf_auth_error`    | 401, 403 (auth/account)               |
| `ccf_tier_error`    | 403 with `error = "tier_insufficient"` |
| `ccf_quota_error`   | 429 — daily / total quota exceeded     |
| `ccf_not_found`     | 404 — unknown article/cascade/event    |
| `ccf_bad_request`   | 400 — malformed parameters             |
| `ccf_server_error`  | 5xx                                    |

Each condition carries `$status`, `$body` (parsed JSON), and
`$tier_status` (the X-CCF-* headers).

## Running tests

```r
# install.packages("devtools")
devtools::install_dev_deps()
devtools::test()
```

The included tests don't touch the network.

## License

MIT. Part of the Canadian Climate Framing research project — Antoine
Lemorphic (Université de Sherbrooke), Tristan Boursier (Sciences Po
Paris & Université du Québec en Outaouais).
