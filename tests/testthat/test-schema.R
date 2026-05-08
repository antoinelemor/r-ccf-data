test_that("codebook loads and exposes 8 frames", {
  cb <- ccf_codebook()
  expect_type(cb, "list")
  expect_length(cb$frames, 8)
  expect_true("economic" %in% names(cb$frames))
})

test_that("ccf_define returns a definition for known columns", {
  txt <- ccf_define("eco_neg_impact")
  expect_type(txt, "character")
  expect_match(txt, "economy", ignore.case = TRUE)
})

test_that("ccf_define errors on unknown columns", {
  expect_error(ccf_define("not_a_real_column"), "Unknown")
})

test_that("ccf_subcategories_of returns expected length", {
  expect_length(ccf_subcategories_of("economic"), 5)
})

test_that("ccf_frame_columns and ccf_frame_names align", {
  expect_length(ccf_frame_names(), 8)
  expect_length(ccf_frame_columns(), 8)
  expect_true(all(grepl("_frame$", ccf_frame_columns())))
})

test_that("ccf_media_outlets has 20 outlets", {
  expect_length(ccf_media_outlets(), 20)
})

test_that("ccf_codebook_df returns a tibble with expected columns", {
  df <- ccf_codebook_df()
  expect_s3_class(df, "tbl_df")
  expect_true(all(c("column", "group", "subgroup", "definition") %in% names(df)))
  # 65 operational columns
  expect_equal(nrow(df), 65L)
})
