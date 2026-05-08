test_that("ccf_tier_names returns the canonical 5 tiers", {
  tn <- ccf_tier_names()
  expect_equal(tn, c("metadata", "analyst", "researcher", "expert", "writer"))
})

test_that("ccf_tier_descriptions covers every tier", {
  d <- ccf_tier_descriptions()
  expect_true(all(ccf_tier_names() %in% names(d)))
  expect_true(all(nzchar(d)))
})

test_that("ccf_method_tiers values are valid tier names or NA", {
  m <- ccf_method_tiers()
  ok <- m %in% c(ccf_tier_names(), NA_character_)
  expect_true(all(ok | is.na(m)))
})

test_that("ccf_tier_required returns the right tier for core functions", {
  expect_equal(ccf_tier_required("ccf_summary"),       "metadata")
  expect_equal(ccf_tier_required("ccf_distribution"),  "analyst")
  expect_equal(ccf_tier_required("ccf_search"),        "researcher")
  expect_equal(ccf_tier_required("ccf_search_export"), "expert")
  expect_true(is.na(ccf_tier_required("ccf_define")))
})

test_that("ccf_tier_required errors on unknown function", {
  expect_error(ccf_tier_required("not_a_function"), "Unknown")
})

test_that("ccf_methods_by_tier nests by default and is exact when asked", {
  meta_set <- ccf_methods_by_tier("metadata")
  ana_set  <- ccf_methods_by_tier("analyst")
  res_set  <- ccf_methods_by_tier("researcher")
  expect_true(all(meta_set %in% ana_set))
  expect_true(all(ana_set %in% res_set))
  expect_true("ccf_search_export" %in% ccf_methods_by_tier("expert", exact = TRUE))
  expect_false("ccf_search_export" %in% ccf_methods_by_tier("researcher", exact = TRUE))
})
