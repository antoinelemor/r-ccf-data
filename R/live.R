# Live observatory namespace — the PUBLIC real-time API of the CCF website.
#
# The observatory (https://ccf-project.ca/observatory) continuously extracts,
# annotates and summarises Canadian climate coverage. Its backing API is
# public and unauthenticated (read-only, cached server-side): detected media
# events and cascades WITH their bilingual LLM summaries, the last-15-days
# article timeline with per-article frame profiles and summaries, province /
# media panels, national trends, and the daily editorial brief.
#
# The live corpus ("continuous") is refreshed several times a day and is NOT
# frozen: cite the "legacy" corpus (authenticated API) in papers.

.ccf_live_default_base_url <- function() {
  Sys.getenv("CCF_LIVE_BASE_URL", "https://ccf-project.ca/api")
}

#' Create a client for the public CCF live observatory API.
#'
#' No token is required: the observatory API is public and read-only.
#' Events, cascades and articles carry their bilingual LLM summaries in
#' `summary_en` / `summary_fr` (articles also expose a language-matched
#' `summary`), stamped with `generated_at`.
#'
#' @param base_url Override only for self-hosted deployments or tests.
#'   Defaults to environment variable `CCF_LIVE_BASE_URL`, then
#'   `https://ccf-project.ca/api`.
#' @param timeout Per-request timeout in seconds.
#'
#' @return A `ccf_live` object.
#'
#' @examples
#' \dontrun{
#' live <- ccf_live()
#' ccf_live_latest_events(live, limit = 20)     # events + summaries EN/FR
#' ccf_live_article(live, 275849)$summary_fr    # LLM summary of one article
#' }
#' @export
ccf_live <- function(base_url = .ccf_live_default_base_url(), timeout = 30) {
  client <- new.env(parent = emptyenv())
  client$base_url <- sub("/+$", "", base_url)
  client$timeout <- timeout
  client$user_agent <- .ccf_default_user_agent()
  class(client) <- "ccf_live"
  client
}

#' @export
print.ccf_live <- function(x, ...) {
  cat("<ccf_live> ", x$base_url, " (public, no token)\n", sep = "")
  invisible(x)
}

.ccf_live_check <- function(client) {
  if (!inherits(client, "ccf_live")) {
    stop("`client` must be created with ccf_live().", call. = FALSE)
  }
}

# GET + JSON for the public API (no auth headers, no tier bookkeeping).
.ccf_live_get <- function(client, path, params = list()) {
  .ccf_live_check(client)
  url <- paste0(client$base_url, "/", sub("^/+", "", path))
  req <- httr2::request(url)
  req <- httr2::req_headers(req, `User-Agent` = client$user_agent)
  req <- httr2::req_timeout(req, client$timeout)
  params <- params[!vapply(params, is.null, logical(1))]
  if (length(params)) req <- httr2::req_url_query(req, !!!params)
  req <- httr2::req_error(req, is_error = function(...) FALSE)
  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  if (status == 404) stop("Not found: ", url, call. = FALSE)
  if (status >= 400) {
    stop("Live API error ", status, " on ", url, call. = FALSE)
  }
  httr2::resp_body_json(resp, simplifyVector = TRUE)
}

# --- events (detected multi-outlet convergences) ------------------------------

#' Latest detected events, with bilingual LLM summaries.
#'
#' @param client A `ccf_live` client.
#' @param limit Number of events (1..400).
#' @param min_media Optional minimum number of distinct outlets.
#' @return A data frame (one row per event): `title_en` / `title_fr`,
#'   `summary_en` / `summary_fr`, `generated_at`, typed classification,
#'   strength score and key articles.
#' @export
ccf_live_latest_events <- function(client, limit = 20, min_media = NULL) {
  .ccf_live_get(client, "latest-events",
                list(limit = limit, min_media = min_media))
}

#' Events detected today or yesterday — the freshest signals.
#' @param client A `ccf_live` client.
#' @export
ccf_live_ongoing_events <- function(client) {
  .ccf_live_get(client, "ongoing-events")
}

#' Full profile of one event (articles, entities, summaries EN/FR).
#' @param client A `ccf_live` client.
#' @param event_key Event key as returned by `ccf_live_latest_events()`.
#' @export
ccf_live_event <- function(client, event_key) {
  .ccf_live_get(client, paste0("event/", event_key))
}

#' Full-text search over event titles and summaries (EN + FR).
#' @param client A `ccf_live` client.
#' @param q Query string.
#' @export
ccf_live_search_events <- function(client, q) {
  .ccf_live_get(client, "search-events", list(q = q))
}

# --- cascades (bursts of correlated coverage) ---------------------------------

#' Most recent media cascades, with bilingual LLM summaries.
#' @param client A `ccf_live` client.
#' @param limit Number of cascades (1..100).
#' @export
ccf_live_recent_cascades <- function(client, limit = 20) {
  .ccf_live_get(client, "recent-cascades", list(limit = limit))
}

#' Full profile of one cascade (articles, outlets, summaries EN/FR).
#' @param client A `ccf_live` client.
#' @param cascade_id Cascade identifier.
#' @export
ccf_live_cascade <- function(client, cascade_id) {
  .ccf_live_get(client, paste0("cascade/", cascade_id))
}

#' Aggregate cascade statistics for the observatory.
#' @param client A `ccf_live` client.
#' @export
ccf_live_cascade_summary <- function(client) {
  .ccf_live_get(client, "cascade-summary")
}

#' Full-text search over cascade titles and summaries (EN + FR).
#' @param client A `ccf_live` client.
#' @param q Query string.
#' @export
ccf_live_search_cascades <- function(client, q) {
  .ccf_live_get(client, "search-cascades", list(q = q))
}

# --- articles (continuous extraction feed) ------------------------------------

#' Most recent extracted articles with their LLM summaries.
#' @param client A `ccf_live` client.
#' @export
ccf_live_latest_articles <- function(client) {
  .ccf_live_get(client, "latest-articles")
}

#' Most recent articles fully classified by the 128 CCF models.
#' @param client A `ccf_live` client.
#' @export
ccf_live_latest_classified <- function(client) {
  .ccf_live_get(client, "latest-classified")
}

#' One article: metadata, province, frame profile, entities and summaries.
#' @param client A `ccf_live` client.
#' @param doc_id Article document id.
#' @return A list with `frame_profile` (all 8 frames), `entities`, related
#'   `events` / `cascades`, and LLM `summary` / `summary_en` / `summary_fr`.
#' @export
ccf_live_article <- function(client, doc_id) {
  .ccf_live_get(client, paste0("article/", doc_id))
}

#' Day-by-day, outlet-by-outlet article timeline (1..60 days).
#' @param client A `ccf_live` client.
#' @param days Window length in days.
#' @export
ccf_live_articles_timeline <- function(client, days = 15) {
  .ccf_live_get(client, "articles-timeline", list(days = days))
}

#' Full-text search over article titles of the live corpus.
#' @param client A `ccf_live` client.
#' @param q Query string.
#' @export
ccf_live_search_titles <- function(client, q) {
  .ccf_live_get(client, "search-titles", list(q = q))
}

# --- geography ------------------------------------------------------------------

#' Unified map payload (per-province volumes, outlets, city pins).
#' @param client A `ccf_live` client.
#' @export
ccf_live_geo_data <- function(client) {
  .ccf_live_get(client, "geo-data")
}

#' Per-province panels: volumes, frame profiles, LLM briefs, articles.
#' @param client A `ccf_live` client.
#' @export
ccf_live_province_panels <- function(client) {
  .ccf_live_get(client, "province-panels")
}

#' Monthly frame shares aggregated by province.
#' @param client A `ccf_live` client.
#' @export
ccf_live_frames_by_province <- function(client) {
  .ccf_live_get(client, "frames-by-province")
}

# --- media -----------------------------------------------------------------------

#' Per-outlet panels (volumes, frame profile, LLM brief).
#' @param client A `ccf_live` client.
#' @export
ccf_live_media_panels <- function(client) {
  .ccf_live_get(client, "media-panels")
}

#' Coverage freshness per outlet and per source database.
#' @param client A `ccf_live` client.
#' @export
ccf_live_media_coverage <- function(client) {
  .ccf_live_get(client, "media-coverage")
}

#' Monthly frame shares aggregated by outlet.
#' @param client A `ccf_live` client.
#' @export
ccf_live_frames_by_media <- function(client) {
  .ccf_live_get(client, "frames-by-media")
}

#' Article counts per outlet (live corpus).
#' @param client A `ccf_live` client.
#' @export
ccf_live_articles_by_media <- function(client) {
  .ccf_live_get(client, "articles-by-media")
}

#' Article counts per month (live corpus).
#' @param client A `ccf_live` client.
#' @export
ccf_live_articles_by_month <- function(client) {
  .ccf_live_get(client, "articles-by-month")
}

# --- national trends & analytics ---------------------------------------------------

#' National monthly frame shares (smoothed), live corpus.
#' @param client A `ccf_live` client.
#' @export
ccf_live_frames_national <- function(client) {
  .ccf_live_get(client, "frames-national")
}

#' Frame trend data used by the observatory charts.
#' @param client A `ccf_live` client.
#' @export
ccf_live_frames_data <- function(client) {
  .ccf_live_get(client, "frames-data")
}

#' Monthly tone (alarmist / reassuring) trends.
#' @param client A `ccf_live` client.
#' @export
ccf_live_tone_over_time <- function(client) {
  .ccf_live_get(client, "tone-over-time")
}

#' Distribution of detected categories across the corpus.
#' @param client A `ccf_live` client.
#' @export
ccf_live_category_distribution <- function(client) {
  .ccf_live_get(client, "category-distribution")
}

#' Entity co-occurrence network of the live corpus.
#' @param client A `ccf_live` client.
#' @export
ccf_live_network_data <- function(client) {
  .ccf_live_get(client, "network-data")
}

#' Per-category annotation performance metrics of the CCF models.
#' @param client A `ccf_live` client.
#' @export
ccf_live_annotation_metrics <- function(client) {
  .ccf_live_get(client, "annotation-metrics")
}

# --- editorial briefs & site stats ---------------------------------------------------

#' LLM-written editorial brief of the day (`$en`, `$fr`).
#' @param client A `ccf_live` client.
#' @export
ccf_live_daily_brief <- function(client) {
  .ccf_live_get(client, "daily-brief")
}

#' LLM overview of the 20 biggest events of the last 15 days (EN/FR).
#' @param client A `ccf_live` client.
#' @export
ccf_live_overview_summary <- function(client) {
  .ccf_live_get(client, "overview-summary")
}

#' Observatory headline summary.
#' @param client A `ccf_live` client.
#' @export
ccf_live_observatory_summary <- function(client) {
  .ccf_live_get(client, "observatory-summary")
}

#' Observatory counters (events, cascades, articles, freshness).
#' @param client A `ccf_live` client.
#' @export
ccf_live_observatory_stats <- function(client) {
  .ccf_live_get(client, "observatory-stats")
}

#' Site-wide corpus statistics (articles, sentences, time span).
#' @param client A `ccf_live` client.
#' @export
ccf_live_stats <- function(client) {
  .ccf_live_get(client, "stats")
}
