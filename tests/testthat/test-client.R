test_that("ccf_client requires a token", {
  withr::with_envvar(c(CCF_TOKEN = ""), {
    expect_error(ccf_client(), "API token is required")
  })
})

test_that("ccf_client builds an environment with expected fields", {
  cli <- ccf_client(token = "fake-token", base_url = "https://example.test")
  expect_s3_class(cli, "ccf_client")
  expect_equal(cli$base_url, "https://example.test")
  expect_equal(cli$token, "fake-token")
  expect_true(is.list(cli$last_status))
})

test_that("ccf_last_status returns NA fields before any call", {
  cli <- ccf_client(token = "fake-token")
  st <- ccf_last_status(cli)
  expect_true(is.na(st$tier))
  expect_true(is.na(st$requests_remaining))
})
