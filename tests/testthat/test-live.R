# Offline tests for the public live-observatory client.

test_that("ccf_live() needs no token and prints cleanly", {
  live <- ccf_live(base_url = "https://example.org/api")
  expect_s3_class(live, "ccf_live")
  expect_identical(live$base_url, "https://example.org/api")
  expect_output(print(live), "public, no token")
})

test_that("default base URL targets the public observatory API", {
  withr::local_envvar(CCF_LIVE_BASE_URL = NA)
  expect_identical(.ccf_live_default_base_url(), "https://ccf-project.ca/api")
})

test_that("CCF_LIVE_BASE_URL env var overrides the default", {
  withr::local_envvar(CCF_LIVE_BASE_URL = "http://localhost:8003/api")
  expect_identical(.ccf_live_default_base_url(), "http://localhost:8003/api")
})

test_that("live functions reject a non-live client", {
  expect_error(ccf_live_latest_events(list()), "ccf_live")
  expect_error(ccf_live_stats("nope"), "ccf_live")
})

test_that("all documented live endpoints are exported", {
  exported <- getNamespaceExports("ccfdata")
  expected <- c(
    "ccf_live", "ccf_live_latest_events", "ccf_live_ongoing_events",
    "ccf_live_event", "ccf_live_search_events",
    "ccf_live_recent_cascades", "ccf_live_cascade",
    "ccf_live_cascade_summary", "ccf_live_search_cascades",
    "ccf_live_latest_articles", "ccf_live_latest_classified",
    "ccf_live_article", "ccf_live_articles_timeline",
    "ccf_live_search_titles",
    "ccf_live_geo_data", "ccf_live_province_panels",
    "ccf_live_frames_by_province",
    "ccf_live_media_panels", "ccf_live_media_coverage",
    "ccf_live_frames_by_media", "ccf_live_articles_by_media",
    "ccf_live_articles_by_month",
    "ccf_live_frames_national", "ccf_live_frames_data",
    "ccf_live_tone_over_time", "ccf_live_category_distribution",
    "ccf_live_network_data", "ccf_live_annotation_metrics",
    "ccf_live_daily_brief", "ccf_live_overview_summary",
    "ccf_live_observatory_summary", "ccf_live_observatory_stats",
    "ccf_live_stats"
  )
  expect_true(all(expected %in% exported))
})
