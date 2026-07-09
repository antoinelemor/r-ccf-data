<div align="center">

<a href="https://ccf-project.ca">
  <img src="https://ccf-project.ca/static/assets/logos/ccf_icone.png" alt="Canadian Climate Framing" width="130">
</a>

# ccfdata · R client

**Authenticated, tibble-friendly access to the [CCF data platform](https://data.ccf-project.ca): 275,000+ Canadian newspaper articles (1978 to present, updated daily), 9.2 M sentences and 67 climate-coverage annotations.**

*A lighthouse on Canada's climate coverage*

[![Website](https://img.shields.io/badge/Website-ccf--project.ca-0f8a76?style=flat-square)](https://ccf-project.ca)
[![Live observatory](https://img.shields.io/badge/Live-observatory-12b48c?style=flat-square)](https://ccf-project.ca/observatory)
[![Data platform](https://img.shields.io/badge/Data-data.ccf--project.ca-0e2a47?style=flat-square)](https://data.ccf-project.ca)

</div>

---

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

The platform enforces six progressive tiers per token. The tier is
assigned by an administrator and dictates both *which* endpoints the
token may call and *how many* requests it can issue per day.

| Tier         | Default req/day | Default search/day | Default export/day | Unlocks                                               |
|--------------|----------------:|-------------------:|-------------------:|-------------------------------------------------------|
| `metadata`   |           1 000 |                  — |                  — | summary, schema, geo, articles-by-*, frame-trends     |
| `analyst`    |           5 000 |                100 |                  — | + distributions, trends, cross-tab, *_analysis        |
| `researcher` |          20 000 |              1 000 |                 20 | + search/article/cascades/events/semantic             |
| `expert`     |       unlimited |          unlimited |          unlimited | + CSV exports                                         |
| `writer`     |       unlimited |          unlimited |          unlimited | + admin endpoints (internal tooling)                  |
| `observer`   |       unlimited |          unlimited |          unlimited | + the real-time continuous feed (`corpus=continuous`/`all`) |

## Corpus provenance (`corpus =`)

Every data function takes an optional `corpus` argument choosing which slice
of the observatory to read: `"legacy"` (the frozen, citable study corpus —
**the default**, reproducible), `"continuous"` (the real-time extraction feed),
or `"all"` (both). `legacy` is open to every tier; `continuous` and `all`
require an `observer` token (other tiers get `403 corpus_forbidden`). Omitting
`corpus` keeps the legacy default, so existing code is unchanged.

```r
ccf_summary(ccf)                            # legacy (default)
ccf_search(ccf, "carbon tax", corpus = "all")        # needs observer tier
ccf_distribution(ccf, "economic_frame", corpus = "continuous")
```

After every call the client stashes `tier`, `requests_remaining`,
`searches_remaining`, `exports_remaining` from the `X-CCF-*` response
headers:

```r
ccf_summary(ccf)
ccf_last_status(ccf)
# $tier "researcher"; $requests_remaining 19999; ...
```

When a quota is exhausted you get a `ccf_quota_error` condition;
tier-too-low raises `ccf_tier_error`. Trap them with `tryCatch`:

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

You can introspect tiers offline (no token needed):

```r
ccf_tier_required("ccf_search_export")        # "expert"
ccf_tier_required("ccf_search")               # "researcher"
ccf_tier_required("ccf_define")               # NA (offline helper)
ccf_methods_by_tier("analyst", exact = TRUE)  # only analyst-tier functions
ccf_tier_descriptions()                       # one-line description per tier
```

## All functions at a glance

Every exported function with its endpoint, minimum tier, and what it
does. Functions tagged *offline* don't hit the network.

### Identity & quota introspection

| Function                                  | HTTP                       | Tier      | Description |
|-------------------------------------------|----------------------------|-----------|-------------|
| `ccf_me(ccf)`                             | GET /auth/me               | metadata  | Username, role, tier, quota usage. |
| `ccf_tiers(ccf)`                          | GET /auth/tiers            | metadata  | Public listing of all tiers + default quotas. |
| `ccf_last_status(ccf)`                    | (no HTTP — last call)      | offline   | Tier + remaining quotas from the last response headers. |

### Aggregate / static data — tier `metadata`

| Function                                  | HTTP                          | Description |
|-------------------------------------------|-------------------------------|-------------|
| `ccf_summary(ccf)`                        | GET /api/summary              | Corpus totals (articles, sentences, frames, annotation totals). |
| `ccf_geo_data(ccf)`                       | GET /api/geo-data             | Per-province aggregates. |
| `ccf_articles_by_year(ccf)`               | GET /api/articles-by-year     | Tibble: one row per year. |
| `ccf_articles_by_media(ccf)`              | GET /api/articles-by-media    | Tibble: one row per media outlet. |
| `ccf_frame_trends(ccf)`                   | GET /api/frame-trends         | Pre-computed monthly frame coverage. |

### Distributions / analyses — tier `analyst`

| Function                                            | HTTP                              | Description |
|-----------------------------------------------------|-----------------------------------|-------------|
| `ccf_distribution(ccf, columns, group_by, ...)`     | GET /api/distribution             | Annotation counts grouped by year/month/media/language. |
| `ccf_subcategory_detail(ccf, frame, ...)`           | GET /api/subcategory-detail       | Totals + monthly trend for a frame's subcategories. |
| `ccf_messenger_analysis(ccf, ...)`                  | GET /api/messenger-analysis       | Messenger column totals + monthly trend. |
| `ccf_event_analysis(ccf, ...)`                      | GET /api/event-analysis           | Event column totals + monthly trend. |
| `ccf_solution_analysis(ccf, ...)`                   | GET /api/solution-analysis        | Solution column totals + monthly trend. |
| `ccf_tone_trends(ccf, ...)`                         | GET /api/tone-trends              | Monthly positive/negative/neutral counts. |
| `ccf_urgency_trends(ccf, ...)`                      | GET /api/urgency-trends           | Monthly urgency-flag counts. |
| `ccf_canada_coverage(ccf, ...)`                     | GET /api/canada-coverage          | Monthly Canada-mention counts. |
| `ccf_cross_tabulation(ccf, row_var, col_var, ...)`  | POST /api/cross-tabulation        | 2×2 contingency table of two binary columns. |

### Search & articles — tier `researcher`

| Function                                                    | HTTP                              | Description |
|-------------------------------------------------------------|-----------------------------------|-------------|
| `ccf_search(ccf, query, level, mode, filters, ...)`         | POST /api/search/advanced         | Unified search (text/keyword/semantic/hybrid/entity/browse) at sentence or article level. Auto-paginates. |
| `ccf_search_summary(ccf, query, filters)`                   | POST /api/search/summary          | Aggregate stats for a query (year + media distribution, frame breakdown). |
| `ccf_semantic_search(ccf, query, k)`                        | POST /api/semantic-search         | FAISS-only dense retrieval. |
| `ccf_article(ccf, doc_id)`                                  | GET /api/article/<doc_id>         | Full article (metadata + sentences + annotations). |
| `ccf_articles_batch(ccf, doc_ids)`                          | POST /api/articles/batch          | Metadata-only batch fetch (much faster). |

### Cascades — tier `researcher`

| Function                                                  | HTTP                                              | Description |
|-----------------------------------------------------------|---------------------------------------------------|-------------|
| `ccf_cascades_summary(ccf)`                               | GET /api/cascades/summary                         | Cross-year cascade summary. |
| `ccf_cascade_year(ccf, year)`                             | GET /api/cascades/<year>                          | All cascades for one year. |
| `ccf_cascade_detail(ccf, year, cascade_id)`               | GET /api/cascades/<year>/<cid>                    | Full cascade record. |
| `ccf_cascade_events(ccf, year)`                           | GET /api/cascades/<year>/events                   | Year's event clusters. |
| `ccf_cascade_network(ccf, year, cascade_id)`              | GET /api/cascades/<year>/network/<cid>            | Network edges for a single cascade. |
| `ccf_cascade_year_network(ccf, year, ...)`                | GET /api/cascades/<year>/network                  | Whole-year edge list, filterable. |
| `ccf_cascade_paradigm_shifts(ccf, year)`                  | GET /api/cascades/<year>/paradigm-shifts          | Paradigm-shift episodes. |
| `ccf_cascade_convergence(ccf, year)`                      | GET /api/cascades/<year>/convergence              | Year's convergence statistics. |
| `ccf_cascade_time_series(ccf, year)`                      | GET /api/cascades/<year>/time-series              | Daily articles / journalists tables. |
| `ccf_cascade_impact(ccf, year)`                           | GET /api/cascades/<year>/impact                   | Year's impact summary. |
| `ccf_cascades_cross_year(ccf, page, page_size)`           | GET /api/cascades/cross-year                      | One page of the cross-year cascade table. |
| `ccf_cascades_cross_year_all(ccf)`                        | GET /api/cascades/cross-year/all                  | All cascades, slim metadata. |
| `ccf_cascades_paradigm_timeline(ccf)`                     | GET /api/cascades/cross-year/paradigm-timeline    | Cross-year paradigm timeline. |
| `ccf_cascades_search(ccf, query, mode, cascade_id)`       | POST /api/cascades/search                         | Keyword search or sub-index similarity. |
| `ccf_cascades_semantic_search(ccf, query, k)`             | POST /api/cascades/semantic-search                | FAISS → matching cascades. |

### Events — tier `researcher`

| Function                                                  | HTTP                                                          | Description |
|-----------------------------------------------------------|---------------------------------------------------------------|-------------|
| `ccf_events_summary(ccf)`                                 | GET /api/events/summary                                       | Cross-year event-cluster summary. |
| `ccf_events_clusters(ccf, ...)`                           | GET /api/events/clusters                                      | Filtered + paginated cluster list. Auto-paginates. |
| `ccf_events_cluster(ccf, year, cluster_id)`               | GET /api/events/clusters/<y>/<id>                             | Full cluster detail incl. occurrences. |
| `ccf_events_cluster_articles(ccf, year, cluster_id)`      | GET /api/events/clusters/<y>/<id>/articles                    | Articles attached to a cluster. |
| `ccf_events_type_network(ccf, year)`                      | GET /api/events/type-network                                  | Co-occurrence matrix between event types. |
| `ccf_events_search(ccf, query, ...)`                      | GET /api/events/search                                        | Keyword search for clusters. |
| `ccf_events_semantic_search(ccf, query, k)`               | POST /api/events/semantic-search                              | FAISS → matching clusters. |

### CSV export — tier `expert`

| Function                                                  | HTTP                              | Description |
|-----------------------------------------------------------|-----------------------------------|-------------|
| `ccf_search_export(ccf, query, filters, columns, ...)`    | POST /api/search/export           | Server-side CSV export, parsed back into a tibble by default. |

### Live observatory — public, **no token required**

The CCF observatory (<https://ccf-project.ca/observatory>) continuously
extracts, annotates and summarises Canadian climate coverage. Its public
read-only API is wrapped by a dedicated `ccf_live()` client. Events,
cascades and articles carry their **bilingual LLM summaries**
(`summary_en` / `summary_fr`, stamped `generated_at`).

```r
live <- ccf_live()                          # no token needed
ccf_live_latest_events(live, limit = 20)    # events + summaries EN/FR
ccf_live_article(live, 275849)$summary_fr   # LLM summary of one article
ccf_live_articles_timeline(live, days = 15) # day×outlet timeline + frames
ccf_live_daily_brief(live)                  # $en / $fr
```

| Function | HTTP | Description |
|---|---|---|
| `ccf_live_latest_events(live, limit, min_media)` | GET /api/latest-events | Latest detected events + titles/summaries EN-FR, strength, key articles. |
| `ccf_live_ongoing_events(live)` | GET /api/ongoing-events | Events detected today/yesterday. |
| `ccf_live_event(live, event_key)` | GET /api/event/{key} | Full event profile (articles, entities, summaries). |
| `ccf_live_search_events(live, q)` | GET /api/search-events | Full-text search over event titles + summaries. |
| `ccf_live_recent_cascades(live, limit)` | GET /api/recent-cascades | Recent cascades + frame, z-score, summaries EN-FR. |
| `ccf_live_cascade(live, cascade_id)` | GET /api/cascade/{id} | Full cascade profile. |
| `ccf_live_cascade_summary(live)` | GET /api/cascade-summary | Aggregate cascade statistics. |
| `ccf_live_search_cascades(live, q)` | GET /api/search-cascades | Full-text search over cascade titles + summaries. |
| `ccf_live_latest_articles(live)` / `ccf_live_latest_classified(live)` | GET /api/latest-articles, … | Freshest extracted / fully-classified articles + summaries. |
| `ccf_live_article(live, doc_id)` | GET /api/article/{id} | Metadata, province, 8-frame profile, entities, related events/cascades, summaries. |
| `ccf_live_articles_timeline(live, days)` | GET /api/articles-timeline | Day-by-day, outlet-by-outlet timeline (≤60 days). |
| `ccf_live_search_titles(live, q)` | GET /api/search-titles | Full-text search over live-corpus titles. |
| `ccf_live_geo_data(live)` / `ccf_live_province_panels(live)` / `ccf_live_frames_by_province(live)` | GET /api/geo-data, … | Provinces: volumes, outlets, LLM briefs, frame shares. |
| `ccf_live_media_panels(live)` / `ccf_live_media_coverage(live)` / `ccf_live_frames_by_media(live)` / `ccf_live_articles_by_media(live)` / `ccf_live_articles_by_month(live)` | GET /api/media-panels, … | Outlets: panels, freshness, frame shares, volumes. |
| `ccf_live_frames_national(live)` / `ccf_live_frames_data(live)` / `ccf_live_tone_over_time(live)` / `ccf_live_category_distribution(live)` / `ccf_live_network_data(live)` / `ccf_live_annotation_metrics(live)` | GET /api/frames-national, … | National trends, tone, categories, entity network, model metrics. |
| `ccf_live_daily_brief(live)` / `ccf_live_overview_summary(live)` / `ccf_live_observatory_summary(live)` / `ccf_live_observatory_stats(live)` / `ccf_live_stats(live)` | GET /api/daily-brief, … | LLM editorial briefs + observatory/site statistics. |

> The live corpus (`continuous`) is refreshed several times a day and is
> **not frozen** — cite the `legacy` corpus (authenticated API) in papers.

### Codebook & introspection (offline — no token, no network)

| Function                                  | Description |
|-------------------------------------------|-------------|
| `ccf_codebook()`                          | Bundled codebook as a list. |
| `ccf_codebook_df()`                       | Codebook as a tidy tibble. |
| `ccf_define(column)`                      | Operational definition of one annotation column. |
| `ccf_subcategories_of(frame)`             | Subcategory column names for a given frame. |
| `ccf_frame_names()`                       | 8 frame short names. |
| `ccf_frame_columns()`                     | 8 frame DB column names. |
| `ccf_media_outlets()`                     | 20 media outlets. |
| `ccf_tier_names()`                        | 5 tier names. |
| `ccf_tier_descriptions()`                 | One-line description per tier. |
| `ccf_method_tiers()`                      | Function-to-tier mapping. |
| `ccf_tier_required(fn_name)`              | Minimum tier for a function (or `NA` if offline). |
| `ccf_methods_by_tier(tier, exact)`        | List functions callable at a tier. |

## Quick tour

### Time series and distributions

```r
library(ggplot2)

df <- ccf_distribution(
  ccf,
  columns  = c("economic_frame", "health_frame", "environmental_frame"),
  group_by = "year", lang = "en"
)

long <- tidyr::pivot_longer(df,
  cols = c(economic_frame, health_frame, environmental_frame),
  names_to = "frame", values_to = "n_sentences"
)

ggplot(long, aes(year, n_sentences, color = frame)) +
  geom_line() + labs(title = "Frame coverage over time")
```

### Searching the corpus

```r
hits <- ccf_search(ccf, "carbon tax",
                    level = "sentence",
                    filters = list(lang = "en", date_from = "2015-01-01"),
                    limit = 500)

ccf_search_summary(ccf, "carbon tax", filters = list(lang = "en"))
ccf_semantic_search(ccf, "climate refugees in the Arctic", k = 500)
```

### Cross-year cascades and events

```r
ccf_cascades_summary(ccf)
ccf_cascades_cross_year_all(ccf)
ccf_events_clusters(ccf, year_min = 2018L,
                    types = c("evt_weather"), limit = 200)
ccf_events_semantic_search(ccf, "wildfire smoke")
```

## Annotation schema (offline)

The 67-category annotation framework is bundled in
`inst/extdata/codebook.json`:

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
training data).

## Errors

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
Lemor (Université de Sherbrooke), Tristan Boursier (Sciences Po
Paris & Université du Québec en Outaouais).
