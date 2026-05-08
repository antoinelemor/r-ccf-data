# Pagination helpers shared across endpoints.

#' @keywords internal
.ccf_paginate_get <- function(client, path, base_query, key,
                              page_param = "page", size_param = "page_size",
                              page_size = 100L, total_key = "total",
                              limit = NULL) {
  out <- list()
  page <- 1L
  repeat {
    q <- base_query
    q[[page_param]] <- page
    q[[size_param]] <- if (!is.null(limit)) min(page_size, limit - length(out)) else page_size
    if (q[[size_param]] <= 0) break
    resp <- .ccf_get(client, path, query = q)
    items <- resp[[key]]
    if (is.null(items) || length(items) == 0) break
    out <- c(out, items)
    if (!is.null(total_key) && !is.null(resp[[total_key]])) {
      tot <- suppressWarnings(as.integer(resp[[total_key]]))
      if (!is.na(tot) && length(out) >= tot) break
    }
    if (length(items) < q[[size_param]]) break
    if (!is.null(limit) && length(out) >= limit) break
    page <- page + 1L
  }
  if (!is.null(limit)) head(out, limit) else out
}

#' @keywords internal
.ccf_paginate_post <- function(client, path, body_template,
                               list_keys = c("sentences", "articles", "results"),
                               page_size = 100L, limit = NULL) {
  out <- list()
  last <- list()
  page <- 1L
  repeat {
    body <- body_template
    body$page <- page
    body$page_size <- if (!is.null(limit)) min(page_size, limit - length(out)) else page_size
    if (body$page_size <= 0) break
    resp <- .ccf_post(client, path, body = body)
    last <- resp
    items <- NULL
    for (k in list_keys) {
      if (!is.null(resp[[k]]) && is.list(resp[[k]])) { items <- resp[[k]]; break }
    }
    if (is.null(items) || length(items) == 0) break
    out <- c(out, items)
    tot <- resp$total
    if (!is.null(tot)) {
      tot <- suppressWarnings(as.integer(tot))
      if (!is.na(tot) && length(out) >= tot) break
    }
    if (length(items) < body$page_size) break
    if (!is.null(limit) && length(out) >= limit) break
    page <- page + 1L
  }
  list(rows = if (!is.null(limit)) head(out, limit) else out, last = last)
}
