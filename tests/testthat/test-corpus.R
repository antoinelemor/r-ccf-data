test_that(".ccf_norm_corpus validates and passes through", {
  expect_null(.ccf_norm_corpus(NULL))
  expect_equal(.ccf_norm_corpus("legacy"), "legacy")
  expect_equal(.ccf_norm_corpus("continuous"), "continuous")
  expect_equal(.ccf_norm_corpus("all"), "all")
  expect_error(.ccf_norm_corpus("bogus"), "corpus must be one of")
})

test_that(".ccf_norm_filters normalizes a corpus key", {
  out <- .ccf_norm_filters(list(lang = "en", corpus = "all"))
  expect_equal(out$corpus, "all")
  expect_equal(out$lang, "EN")
  expect_error(.ccf_norm_filters(list(corpus = "nope")), "corpus must be one of")
})
