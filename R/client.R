#' Create an authenticated CCF client.
#'
#' @param token API JWT generated in your CCF Explorer profile
#'   (<https://data.ccf-project.ca>). Defaults to environment variable
#'   `CCF_TOKEN` if unset.
#' @param base_url Override only for self-hosted deployments or tests.
#' @param timeout Per-request timeout in seconds.
#'
#' @return A `ccf_client` object (an environment carrying the token,
#'   base URL, and the most recent tier/quota status).
#'
#' @examples
#' \dontrun{
#' ccf <- ccf_client(token = Sys.getenv("CCF_TOKEN"))
#' ccf_summary(ccf)
#' }
#' @export
ccf_client <- function(token = Sys.getenv("CCF_TOKEN"),
                       base_url = .ccf_default_base_url(),
                       timeout = 60) {
  if (!nzchar(token)) {
    stop("An API token is required. Set CCF_TOKEN or pass `token = ...`. ",
         "Generate one at https://data.ccf-project.ca (Profile page).",
         call. = FALSE)
  }
  client <- new.env(parent = emptyenv())
  client$token <- token
  client$base_url <- sub("/+$", "", base_url)
  client$timeout <- timeout
  client$user_agent <- .ccf_default_user_agent()
  client$last_status <- list(tier = NA_character_,
                              requests_remaining = NA_integer_,
                              searches_remaining = NA_integer_,
                              exports_remaining = NA_integer_)
  class(client) <- "ccf_client"
  client
}

#' @export
print.ccf_client <- function(x, ...) {
  cat("<ccf_client>\n")
  cat("  base_url: ", x$base_url, "\n", sep = "")
  st <- x$last_status
  if (!is.na(st$tier)) {
    cat("  tier:    ", st$tier, "\n", sep = "")
    cat("  remaining (today): requests=", st$requests_remaining,
        ", searches=", st$searches_remaining,
        ", exports=", st$exports_remaining, "\n", sep = "")
  }
  invisible(x)
}

#' @export
format.ccf_client <- function(x, ...) {
  paste0("<ccf_client ", x$base_url, ">")
}

#' Tier and remaining quotas from the most recent response.
#'
#' Surfaces values from the `X-CCF-*` response headers. `NA_integer_`
#' means unlimited (the server sent `unlimited`).
#'
#' @param client A `ccf_client`.
#' @return A list with components `tier`, `requests_remaining`,
#'   `searches_remaining`, `exports_remaining`.
#' @export
ccf_last_status <- function(client) {
  .ccf_check_client(client)
  client$last_status
}

# ============================================================================
# Auth / identity
# ============================================================================

#' Account info, tier, and quota usage for the calling token.
#'
#' Hits `GET /auth/me`. Returns a list including `tier`, a
#' `tier_description`, and a `quota` block with `requests`, `searches`,
#' and `exports` (each with `used_today`, `max_day`, etc.).
#'
#' @param client A `ccf_client`.
#' @export
ccf_me <- function(client) .ccf_get(client, "/auth/me")

#' Public listing of all tiers + their default quotas.
#'
#' @param client A `ccf_client`.
#' @export
ccf_tiers <- function(client) .ccf_get(client, "/auth/tiers")

# ============================================================================
# Static / aggregate data
# ============================================================================

#' Corpus-level stats: total articles, sentences, frames, annotation totals.
#' @param client A `ccf_client`.
#' @export
ccf_summary <- function(client) .ccf_get(client, "/api/summary")

#' Server-side annotation schema (frames, subcategories, columns).
#' @param client A `ccf_client`.
#' @export
ccf_geo_data <- function(client) .ccf_get(client, "/api/geo-data")

#' Articles by year.
#' @param client A `ccf_client`.
#' @param raw If `TRUE` return the raw JSON list instead of a tibble.
#' @export
ccf_articles_by_year <- function(client, raw = FALSE) {
  rows <- .ccf_get(client, "/api/articles-by-year")
  if (raw) rows else .ccf_to_tibble(rows)
}

#' Articles by media outlet.
#' @inheritParams ccf_articles_by_year
#' @export
ccf_articles_by_media <- function(client, raw = FALSE) {
  rows <- .ccf_get(client, "/api/articles-by-media")
  if (raw) rows else .ccf_to_tibble(rows)
}

#' Monthly frame coverage (precomputed view).
#' @inheritParams ccf_articles_by_year
#' @export
ccf_frame_trends <- function(client, raw = FALSE) {
  rows <- .ccf_get(client, "/api/frame-trends")
  if (raw) rows else .ccf_to_tibble(rows)
}

# ============================================================================
# Distributions / analyses (analyst tier)
# ============================================================================

#' Aggregate annotation counts grouped by year/month/media/language.
#'
#' @param client A `ccf_client`.
#' @param columns Character vector of annotation column names.
#' @param group_by One of `"year"`, `"month"`, `"media"`, `"language"`.
#' @param lang,media,date_from,date_to Optional filters.
#' @param raw If `TRUE` return the raw response list.
#' @export
ccf_distribution <- function(client, columns, group_by = "year",
                              lang = NULL, media = NULL,
                              date_from = NULL, date_to = NULL,
                              raw = FALSE) {
  if (!group_by %in% c("year", "month", "media", "language")) {
    stop("group_by must be one of: year, month, media, language", call. = FALSE)
  }
  q <- list(columns = paste(columns, collapse = ","), group_by = group_by,
            lang = lang, media = media,
            date_from = date_from, date_to = date_to)
  resp <- .ccf_get(client, "/api/distribution", query = q)
  if (raw) resp else .ccf_to_tibble(resp$data)
}

#' Subcategory totals + monthly trend for a frame.
#' @param client A `ccf_client`.
#' @param frame Frame name (e.g. `"economic"`).
#' @param date_from,date_to,media,language Optional filters.
#' @export
ccf_subcategory_detail <- function(client, frame,
                                    date_from = NULL, date_to = NULL,
                                    media = NULL, language = NULL) {
  q <- list(frame = frame, date_from = date_from, date_to = date_to,
            media = media, language = language)
  .ccf_get(client, "/api/subcategory-detail", query = q)
}

#' Messenger / event / solution analysis: totals + monthly trends.
#' @param client A `ccf_client`.
#' @param ... Optional filters (`date_from`, `date_to`, `media`, `language`).
#' @export
ccf_messenger_analysis <- function(client, ...) .ccf_get(client, "/api/messenger-analysis", query = list(...))

#' @rdname ccf_messenger_analysis
#' @export
ccf_event_analysis <- function(client, ...) .ccf_get(client, "/api/event-analysis", query = list(...))

#' @rdname ccf_messenger_analysis
#' @export
ccf_solution_analysis <- function(client, ...) .ccf_get(client, "/api/solution-analysis", query = list(...))

#' Monthly tone / urgency / Canada-mention trends.
#' @inheritParams ccf_messenger_analysis
#' @param raw If `TRUE` return the raw list.
#' @export
ccf_tone_trends <- function(client, raw = FALSE, ...) {
  rows <- .ccf_get(client, "/api/tone-trends", query = list(...))
  if (raw) rows else .ccf_to_tibble(rows)
}

#' @rdname ccf_tone_trends
#' @export
ccf_urgency_trends <- function(client, raw = FALSE, ...) {
  rows <- .ccf_get(client, "/api/urgency-trends", query = list(...))
  if (raw) rows else .ccf_to_tibble(rows)
}

#' @rdname ccf_tone_trends
#' @export
ccf_canada_coverage <- function(client, raw = FALSE, ...) {
  rows <- .ccf_get(client, "/api/canada-coverage", query = list(...))
  if (raw) rows else .ccf_to_tibble(rows)
}

#' 2x2 contingency table of two binary annotation columns.
#' @param client A `ccf_client`.
#' @param row_var,col_var Column names.
#' @param filters Optional named list of filters.
#' @export
ccf_cross_tabulation <- function(client, row_var, col_var, filters = list()) {
  .ccf_post(client, "/api/cross-tabulation", body = list(
    row_var = row_var, col_var = col_var, filters = filters
  ))
}

# ============================================================================
# Search (researcher tier)
# ============================================================================

#' Unified search over the CCF corpus.
#'
#' Wraps `POST /api/search/advanced`. Auto-paginates results.
#'
#' @param client A `ccf_client`.
#' @param query Search text. `"*"` or empty = browse mode.
#' @param level `"sentence"` or `"article"`.
#' @param mode `"text"` (default Postgres FTS), `"keyword"` (exact ILIKE),
#'   `"semantic"` (FAISS), `"hybrid"`, `"entity"`, `"browse"`,
#'   `"cascade_xref"`, or `"event_xref"`.
#' @param filters Named list of server-side filters
#'   (e.g. `list(lang = "en", media = c("Globe and Mail"),
#'   date_from = "2010-01-01", frames = c("economic"), tone = "negative")`).
#' @param thresholds Optional list of named lists
#'   (`list(list(column = "economic_frame", min_pct = 0.3))`).
#' @param filter_timing `"pre"` (server-side, default) or `"post"`
#'   (broader fetch + in-memory filter).
#' @param hybrid_weight Numeric in `[0,1]` blending semantic vs FTS.
#' @param page_size Server page size (auto-paginates).
#' @param limit Cap rows returned (`NULL` = all).
#' @param raw If `TRUE` return the raw rows + last response.
#' @export
ccf_search <- function(client, query, level = "sentence", mode = "text",
                       filters = list(), thresholds = NULL,
                       filter_timing = "pre", hybrid_weight = 0.5,
                       page_size = 100L, limit = NULL, raw = FALSE) {
  body <- list(query = query, mode = mode, level = level,
               filters = filters, filter_timing = filter_timing,
               hybrid_weight = hybrid_weight)
  if (!is.null(thresholds)) body$thresholds <- thresholds
  paged <- .ccf_paginate_post(client, "/api/search/advanced", body,
                               list_keys = c("sentences", "articles", "results"),
                               page_size = page_size, limit = limit)
  if (raw) paged else .ccf_to_tibble(paged$rows)
}

#' Aggregate stats for a search query (year/media distribution, frame breakdown).
#' @param client A `ccf_client`.
#' @param query Search text.
#' @param filters Named list.
#' @export
ccf_search_summary <- function(client, query, filters = list()) {
  .ccf_post(client, "/api/search/summary",
            body = list(query = query, filters = filters))
}

#' Server-side CSV export. Returns a tibble by default.
#' @param client A `ccf_client`.
#' @param query Search text.
#' @param filters Named list of filters.
#' @param columns Character vector of columns to include (NULL = all whitelisted).
#' @param max_rows Maximum rows to return.
#' @param mode Search mode (`"text"`, etc.).
#' @param include_search_params If `TRUE`, prepend a header block to the CSV.
#' @param to_tibble If `FALSE` return raw CSV text.
#' @export
ccf_search_export <- function(client, query, filters = list(),
                               columns = NULL, max_rows = 50000L,
                               mode = "text", include_search_params = FALSE,
                               to_tibble = TRUE) {
  body <- list(query = query, filters = filters, mode = mode,
               max_rows = as.integer(max_rows),
               include_search_params = include_search_params)
  if (!is.null(columns)) body$columns <- as.character(columns)
  resp <- .ccf_post_raw(client, "/api/search/export", body = body)
  csv <- httr2::resp_body_string(resp)
  if (!to_tibble) return(csv)
  tibble::as_tibble(read.csv(text = csv, comment.char = "#",
                              stringsAsFactors = FALSE))
}

#' FAISS dense semantic search (`POST /api/semantic-search`).
#' @param client A `ccf_client`.
#' @param query Search text.
#' @param k Number of nearest neighbours (default 100,000 = effectively unlimited).
#' @param raw If `TRUE` return the raw response list.
#' @export
ccf_semantic_search <- function(client, query, k = 100000L, raw = FALSE) {
  resp <- .ccf_post(client, "/api/semantic-search",
                    body = list(query = query, k = as.integer(k)))
  if (raw) resp else .ccf_to_tibble(resp$results)
}

# ============================================================================
# Articles (researcher tier)
# ============================================================================

#' Full article: metadata + all sentences + per-sentence annotations.
#' @param client A `ccf_client`.
#' @param doc_id Integer document ID.
#' @export
ccf_article <- function(client, doc_id) .ccf_get(client, sprintf("/api/article/%d", as.integer(doc_id)))

#' Batch-fetch article metadata (no sentences).
#' @param client A `ccf_client`.
#' @param doc_ids Integer vector of document IDs.
#' @param raw If `TRUE` return the raw response list.
#' @export
ccf_articles_batch <- function(client, doc_ids, raw = FALSE) {
  resp <- .ccf_post(client, "/api/articles/batch",
                    body = list(doc_ids = as.integer(doc_ids)))
  if (raw) resp else .ccf_to_tibble(resp$articles)
}

# ============================================================================
# Cascades (researcher tier)
# ============================================================================

#' Cross-year cascade summary.
#' @param client A `ccf_client`.
#' @export
ccf_cascades_summary <- function(client) .ccf_get(client, "/api/cascades/summary")

#' All cascades for a given year.
#' @param client A `ccf_client`.
#' @param year Integer year.
#' @export
ccf_cascade_year <- function(client, year) .ccf_get(client, sprintf("/api/cascades/%d", as.integer(year)))

#' Detail for a single cascade.
#' @param client A `ccf_client`.
#' @param year Integer.
#' @param cascade_id Cascade identifier.
#' @export
ccf_cascade_detail <- function(client, year, cascade_id) {
  .ccf_get(client, sprintf("/api/cascades/%d/%s", as.integer(year), cascade_id))
}

#' Event clusters listed under a year's cascade data.
#' @param client A `ccf_client`.
#' @param year Integer.
#' @param raw If `TRUE` return raw list.
#' @export
ccf_cascade_events <- function(client, year, raw = FALSE) {
  rows <- .ccf_get(client, sprintf("/api/cascades/%d/events", as.integer(year)))
  if (raw) rows else .ccf_to_tibble(rows)
}

#' Network edges for a single cascade.
#' @param client A `ccf_client`.
#' @param year Integer.
#' @param cascade_id Cascade ID.
#' @export
ccf_cascade_network <- function(client, year, cascade_id) {
  .ccf_get(client, sprintf("/api/cascades/%d/network/%s",
                            as.integer(year), cascade_id))
}

#' Whole-year network with optional filters.
#' @param client A `ccf_client`.
#' @param year Integer.
#' @param cascade_ids,frames,media,classifications Character vectors of filters (optional).
#' @param score_min,score_max Numeric (optional).
#' @export
ccf_cascade_year_network <- function(client, year,
                                      cascade_ids = NULL, frames = NULL,
                                      media = NULL, classifications = NULL,
                                      score_min = NULL, score_max = NULL) {
  q <- list(
    cascade_ids = if (!is.null(cascade_ids)) paste(cascade_ids, collapse = ","),
    frames      = if (!is.null(frames)) paste(frames, collapse = ","),
    media       = if (!is.null(media)) paste(media, collapse = ","),
    classifications = if (!is.null(classifications)) paste(classifications, collapse = ","),
    score_min = score_min, score_max = score_max
  )
  .ccf_get(client, sprintf("/api/cascades/%d/network", as.integer(year)), query = q)
}

#' Paradigm-shift episodes for a year.
#' @param client A `ccf_client`.
#' @param year Integer.
#' @export
ccf_cascade_paradigm_shifts <- function(client, year) {
  .ccf_get(client, sprintf("/api/cascades/%d/paradigm-shifts", as.integer(year)))
}

#' Cascade convergence statistics for a year.
#' @inheritParams ccf_cascade_paradigm_shifts
#' @export
ccf_cascade_convergence <- function(client, year) {
  .ccf_get(client, sprintf("/api/cascades/%d/convergence", as.integer(year)))
}

#' Daily time-series tables for a year.
#' @inheritParams ccf_cascade_paradigm_shifts
#' @export
ccf_cascade_time_series <- function(client, year) {
  .ccf_get(client, sprintf("/api/cascades/%d/time-series", as.integer(year)))
}

#' Cascade impact summary for a year.
#' @inheritParams ccf_cascade_paradigm_shifts
#' @export
ccf_cascade_impact <- function(client, year) {
  .ccf_get(client, sprintf("/api/cascades/%d/impact", as.integer(year)))
}

#' Cross-year cascade table (paginated).
#' @param client A `ccf_client`.
#' @param page,page_size Pagination.
#' @param raw If `TRUE` return raw response.
#' @export
ccf_cascades_cross_year <- function(client, page = 1L, page_size = 100L, raw = FALSE) {
  resp <- .ccf_get(client, "/api/cascades/cross-year",
                    query = list(page = page, page_size = page_size))
  if (raw) resp else .ccf_to_tibble(resp$data)
}

#' All cross-year cascades (slim metadata, single response).
#' @param client A `ccf_client`.
#' @param raw If `TRUE` return raw response.
#' @export
ccf_cascades_cross_year_all <- function(client, raw = FALSE) {
  resp <- .ccf_get(client, "/api/cascades/cross-year/all")
  if (raw) resp else .ccf_to_tibble(resp$cascades)
}

#' Cross-year paradigm-shift timeline.
#' @inheritParams ccf_cascades_cross_year_all
#' @export
ccf_cascades_paradigm_timeline <- function(client, raw = FALSE) {
  rows <- .ccf_get(client, "/api/cascades/cross-year/paradigm-timeline")
  if (raw) rows else .ccf_to_tibble(rows)
}

#' Keyword/metadata search over cascades.
#' @param client A `ccf_client`.
#' @param query Search text.
#' @param mode `"text"` or `"similar"` (with `cascade_id`).
#' @param cascade_id Used when `mode = "similar"`.
#' @param raw If `TRUE` return raw response.
#' @export
ccf_cascades_search <- function(client, query, mode = "text",
                                 cascade_id = NULL, raw = FALSE) {
  body <- list(query = query, mode = mode)
  if (!is.null(cascade_id)) body$cascade_id <- cascade_id
  resp <- .ccf_post(client, "/api/cascades/search", body = body)
  if (raw) resp else .ccf_to_tibble(resp$results)
}

#' FAISS-backed semantic search for cascades.
#' @param client A `ccf_client`.
#' @param query Search text.
#' @param k Top-k neighbours.
#' @param raw If `TRUE` return raw response.
#' @export
ccf_cascades_semantic_search <- function(client, query, k = 100000L, raw = FALSE) {
  resp <- .ccf_post(client, "/api/cascades/semantic-search",
                    body = list(query = query, k = as.integer(k)))
  if (raw) resp else .ccf_to_tibble(resp$results)
}

# ============================================================================
# Events (researcher tier)
# ============================================================================

#' Cross-year event-cluster summary.
#' @param client A `ccf_client`.
#' @export
ccf_events_summary <- function(client) .ccf_get(client, "/api/events/summary")

#' Filtered + paginated list of event clusters.
#' @param client A `ccf_client`.
#' @param year_min,year_max Year range.
#' @param types Character vector of event types (e.g. `c("evt_weather", "evt_protest")`).
#' @param strength_min,strength_max Numeric.
#' @param multi_type Logical (TRUE/FALSE) or NULL.
#' @param sort,order Sort field and direction.
#' @param search Free-text entity search.
#' @param page_size,limit Pagination + cap.
#' @param raw If `TRUE` return raw rows.
#' @export
ccf_events_clusters <- function(client,
                                 year_min = NULL, year_max = NULL,
                                 types = NULL, strength_min = NULL,
                                 strength_max = NULL, multi_type = NULL,
                                 sort = "strength", order = "desc",
                                 search = "", page_size = 200L,
                                 limit = NULL, raw = FALSE) {
  base_q <- list(
    year_min = year_min, year_max = year_max,
    types = if (!is.null(types)) paste(types, collapse = ","),
    strength_min = strength_min, strength_max = strength_max,
    multi_type = if (!is.null(multi_type)) tolower(as.character(multi_type)),
    sort = sort, order = order, search = if (nzchar(search)) search
  )
  rows <- .ccf_paginate_get(client, "/api/events/clusters", base_q,
                             key = "clusters", page_param = "page",
                             size_param = "per_page", page_size = page_size,
                             total_key = "total", limit = limit)
  if (raw) rows else .ccf_to_tibble(rows)
}

#' Single event-cluster detail.
#' @param client A `ccf_client`.
#' @param year,cluster_id Integer.
#' @export
ccf_events_cluster <- function(client, year, cluster_id) {
  .ccf_get(client, sprintf("/api/events/clusters/%d/%d",
                            as.integer(year), as.integer(cluster_id)))
}

#' Articles attached to an event cluster (grouped by occurrence).
#' @inheritParams ccf_events_cluster
#' @export
ccf_events_cluster_articles <- function(client, year, cluster_id) {
  .ccf_get(client, sprintf("/api/events/clusters/%d/%d/articles",
                            as.integer(year), as.integer(cluster_id)))
}

#' Co-occurrence matrix between event types.
#' @param client A `ccf_client`.
#' @param year Optional integer to filter.
#' @export
ccf_events_type_network <- function(client, year = NULL) {
  q <- if (!is.null(year)) list(year = as.integer(year)) else list()
  .ccf_get(client, "/api/events/type-network", query = q)
}

#' Keyword search for event clusters.
#' @param client A `ccf_client`.
#' @param query Search text.
#' @param types Character vector of event types.
#' @param year_min,year_max,strength_min Optional filters.
#' @param raw If `TRUE` return raw response.
#' @export
ccf_events_search <- function(client, query, types = NULL,
                               year_min = NULL, year_max = NULL,
                               strength_min = NULL, raw = FALSE) {
  q <- list(
    q = query,
    types = if (!is.null(types)) paste(types, collapse = ","),
    year_min = year_min, year_max = year_max, strength_min = strength_min
  )
  resp <- .ccf_get(client, "/api/events/search", query = q)
  if (raw) resp else .ccf_to_tibble(resp$results)
}

#' Semantic search for event clusters.
#' @param client A `ccf_client`.
#' @param query Search text.
#' @param k Top-k neighbours.
#' @param raw If `TRUE` return raw response.
#' @export
ccf_events_semantic_search <- function(client, query, k = 100000L, raw = FALSE) {
  resp <- .ccf_post(client, "/api/events/semantic-search",
                    body = list(query = query, k = as.integer(k)))
  if (raw) resp else .ccf_to_tibble(resp$results)
}
