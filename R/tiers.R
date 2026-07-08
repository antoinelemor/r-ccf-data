# Client-side mirror of the server's tier enforcement table.
# Lets users introspect which tier a function requires before calling.

#' Names of all CCF API tiers, in order of increasing privilege.
#'
#' @format Character vector of length 5.
#' @export
ccf_tier_names <- function() {
  c("metadata", "analyst", "researcher", "expert", "writer")
}

#' Short description of each tier.
#'
#' `public` is not a token tier: it flags the live observatory functions
#' (`ccf_live_*()`), which hit the public real-time API and require no
#' token at all.
#'
#' @return Named character vector indexed by tier name.
#' @export
ccf_tier_descriptions <- function() {
  c(
    metadata   = "Read summary, schema, geographic and pre-aggregated data.",
    analyst    = "Run distributions, time series, cross-tabulations, frame analyses.",
    researcher = "Search the corpus, fetch full articles, browse cascades and event clusters.",
    expert     = "Unlimited search and CSV exports for offline analysis.",
    writer     = "Full access (admin/maintainer-equivalent).",
    public     = "Live observatory (ccf_live_*) - public real-time API, no token required."
  )
}

#' Function-to-tier mapping for every exported `ccf_*` function.
#'
#' Functions whose tier is `NA_character_` are offline helpers (they do
#' not hit the network and do not require an authenticated client).
#'
#' @return Named character vector: function name -> minimum tier (or NA).
#' @export
ccf_method_tiers <- function() {
  c(
    # Identity & introspection
    ccf_me                          = "metadata",
    ccf_tiers                       = "metadata",
    ccf_last_status                 = NA_character_,

    # Aggregate / static data
    ccf_summary                     = "metadata",
    ccf_geo_data                    = "metadata",
    ccf_articles_by_year            = "metadata",
    ccf_articles_by_media           = "metadata",
    ccf_frame_trends                = "metadata",

    # Distributions / analyses
    ccf_distribution                = "analyst",
    ccf_subcategory_detail          = "analyst",
    ccf_messenger_analysis          = "analyst",
    ccf_event_analysis              = "analyst",
    ccf_solution_analysis           = "analyst",
    ccf_tone_trends                 = "analyst",
    ccf_urgency_trends              = "analyst",
    ccf_canada_coverage             = "analyst",
    ccf_cross_tabulation            = "analyst",

    # Search & articles
    ccf_search                      = "researcher",
    ccf_search_summary              = "researcher",
    ccf_semantic_search             = "researcher",
    ccf_article                     = "researcher",
    ccf_articles_batch              = "researcher",

    # Exports
    ccf_search_export               = "expert",

    # Cascades
    ccf_cascades_summary            = "researcher",
    ccf_cascade_year                = "researcher",
    ccf_cascade_detail              = "researcher",
    ccf_cascade_events              = "researcher",
    ccf_cascade_network             = "researcher",
    ccf_cascade_year_network        = "researcher",
    ccf_cascade_paradigm_shifts     = "researcher",
    ccf_cascade_convergence         = "researcher",
    ccf_cascade_time_series         = "researcher",
    ccf_cascade_impact              = "researcher",
    ccf_cascades_cross_year         = "researcher",
    ccf_cascades_cross_year_all     = "researcher",
    ccf_cascades_paradigm_timeline  = "researcher",
    ccf_cascades_search             = "researcher",
    ccf_cascades_semantic_search    = "researcher",

    # Events
    ccf_events_summary              = "researcher",
    ccf_events_clusters             = "researcher",
    ccf_events_cluster              = "researcher",
    ccf_events_cluster_articles     = "researcher",
    ccf_events_type_network         = "researcher",
    ccf_events_search               = "researcher",
    ccf_events_semantic_search      = "researcher",

    # Live observatory — PUBLIC real-time API (no token at all):
    # https://ccf-project.ca/api. Any tier (and no tier) can call these.
    ccf_live                        = "public",
    ccf_live_latest_events          = "public",
    ccf_live_ongoing_events         = "public",
    ccf_live_event                  = "public",
    ccf_live_search_events          = "public",
    ccf_live_recent_cascades       = "public",
    ccf_live_cascade                = "public",
    ccf_live_cascade_summary        = "public",
    ccf_live_search_cascades        = "public",
    ccf_live_latest_articles        = "public",
    ccf_live_latest_classified      = "public",
    ccf_live_article                = "public",
    ccf_live_articles_timeline      = "public",
    ccf_live_search_titles          = "public",
    ccf_live_geo_data               = "public",
    ccf_live_province_panels        = "public",
    ccf_live_frames_by_province     = "public",
    ccf_live_media_panels           = "public",
    ccf_live_media_coverage         = "public",
    ccf_live_frames_by_media        = "public",
    ccf_live_articles_by_media      = "public",
    ccf_live_articles_by_month      = "public",
    ccf_live_frames_national        = "public",
    ccf_live_frames_data            = "public",
    ccf_live_tone_over_time         = "public",
    ccf_live_category_distribution  = "public",
    ccf_live_network_data           = "public",
    ccf_live_annotation_metrics     = "public",
    ccf_live_daily_brief            = "public",
    ccf_live_overview_summary       = "public",
    ccf_live_observatory_summary    = "public",
    ccf_live_observatory_stats      = "public",
    ccf_live_stats                  = "public",

    # Offline helpers (no tier required)
    ccf_codebook                    = NA_character_,
    ccf_codebook_df                 = NA_character_,
    ccf_define                      = NA_character_,
    ccf_subcategories_of            = NA_character_,
    ccf_frame_names                 = NA_character_,
    ccf_frame_columns               = NA_character_,
    ccf_media_outlets               = NA_character_,
    ccf_tier_names                  = NA_character_,
    ccf_tier_descriptions           = NA_character_,
    ccf_method_tiers                = NA_character_,
    ccf_tier_required               = NA_character_,
    ccf_methods_by_tier             = NA_character_,
    ccf_client                      = NA_character_
  )
}

#' Minimum tier required to call a given client function.
#'
#' Returns `NA_character_` for offline helpers (codebook, schema,
#' introspection) that do not hit the network.
#'
#' @param fn_name Character scalar — the function name (e.g. `"ccf_search"`).
#' @return Character scalar, one of `ccf_tier_names()`, or `NA_character_`.
#' @examples
#' ccf_tier_required("ccf_search")          # "researcher"
#' ccf_tier_required("ccf_search_export")   # "expert"
#' is.na(ccf_tier_required("ccf_define"))   # TRUE
#' @export
ccf_tier_required <- function(fn_name) {
  m <- ccf_method_tiers()
  if (!fn_name %in% names(m)) {
    stop(sprintf("Unknown function %s. See ccf_method_tiers() for the full list.",
                 fn_name), call. = FALSE)
  }
  unname(m[fn_name])
}

#' List functions callable at a given tier.
#'
#' @param tier Tier name (one of `ccf_tier_names()`, or `"public"` for the
#'   token-free live observatory functions).
#' @param exact If `TRUE`, only functions whose minimum tier is exactly
#'   `tier`; if `FALSE` (default), all functions callable at this tier
#'   (i.e. requiring `tier` or a lower one; `public` functions are callable
#'   from any tier — and with no token at all).
#' @return Sorted character vector of function names.
#' @examples
#' ccf_methods_by_tier("expert", exact = TRUE)   # only "ccf_search_export"
#' "ccf_search" %in% ccf_methods_by_tier("researcher")  # TRUE
#' "ccf_live_latest_events" %in% ccf_methods_by_tier("metadata")  # TRUE (public)
#' @export
ccf_methods_by_tier <- function(tier, exact = FALSE) {
  ranks <- setNames(seq_along(ccf_tier_names()), ccf_tier_names())
  if (!identical(tier, "public") && !tier %in% names(ranks)) {
    stop(sprintf("Unknown tier %s. Valid: %s",
                 tier, paste(c(ccf_tier_names(), "public"), collapse = ", ")),
         call. = FALSE)
  }
  m <- ccf_method_tiers()
  m <- m[!is.na(m)]
  if (exact) {
    out <- names(m)[m == tier]
  } else if (identical(tier, "public")) {
    out <- names(m)[m == "public"]
  } else {
    rank_t <- ranks[[tier]]
    out <- names(m)[m == "public" | (m %in% names(ranks) & ranks[m] <= rank_t)]
  }
  sort(out)
}
