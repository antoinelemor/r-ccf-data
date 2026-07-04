# Internal HTTP layer for the CCF API client.
# Uses httr2 for requests, json round-trips, and structured error mapping.

#' @keywords internal
.ccf_default_base_url <- function() {
  Sys.getenv("CCF_BASE_URL", unset = "https://data.ccf-project.ca")
}

#' @keywords internal
.ccf_default_user_agent <- function() {
  paste0("ccfdata-R/0.2.0 (", R.version$version.string, ")")
}

#' @keywords internal
.ccf_check_client <- function(client) {
  if (!inherits(client, "ccf_client")) {
    stop("Pass a `ccf_client` (created with `ccf_client(token = ...)`)",
         call. = FALSE)
  }
  invisible(client)
}

#' @keywords internal
.ccf_build_request <- function(client, path) {
  url <- if (grepl("^https?://", path)) path else paste0(client$base_url, path)
  req <- httr2::request(url)
  req <- httr2::req_headers(req,
    Authorization = paste("Bearer", client$token),
    Accept = "application/json",
    `User-Agent` = client$user_agent
  )
  req <- httr2::req_timeout(req, client$timeout)
  req <- httr2::req_error(req, is_error = function(...) FALSE)
  req
}

#' @keywords internal
.ccf_extract_status <- function(resp) {
  parse_int <- function(name) {
    v <- httr2::resp_header(resp, name)
    if (is.null(v) || identical(v, "unlimited")) NA_integer_
    else suppressWarnings(as.integer(v))
  }
  list(
    tier = httr2::resp_header(resp, "X-CCF-Tier"),
    requests_remaining = parse_int("X-CCF-Requests-Remaining"),
    searches_remaining = parse_int("X-CCF-Searches-Remaining"),
    exports_remaining  = parse_int("X-CCF-Exports-Remaining")
  )
}

#' @keywords internal
.ccf_handle <- function(client, resp) {
  status <- .ccf_extract_status(resp)
  # Mutate client env-state — `ccf_client` is an environment so this persists
  client$last_status <- status
  http_status <- httr2::resp_status(resp)
  ctype <- tolower(httr2::resp_header(resp, "Content-Type") %||% "")

  if (httr2::resp_is_error(resp)) {
    body <- tryCatch(httr2::resp_body_json(resp), error = function(e) list(error = httr2::resp_body_string(resp)))
    msg <- body$error %||% httr2::resp_status_desc(resp)
    cls <- switch(as.character(http_status),
      "401" = "ccf_auth_error",
      "403" = if (identical(body$error, "tier_insufficient")) "ccf_tier_error" else "ccf_auth_error",
      "404" = "ccf_not_found",
      "429" = "ccf_quota_error",
      "400" = "ccf_bad_request",
      if (http_status >= 500) "ccf_server_error" else "ccf_error"
    )
    cond <- structure(
      class = c(cls, "ccf_error", "error", "condition"),
      list(
        message = sprintf("[%d] %s", http_status, msg),
        call = NULL,
        status = http_status,
        body = body,
        tier_status = status
      )
    )
    stop(cond)
  }

  if (grepl("json", ctype, fixed = TRUE)) {
    httr2::resp_body_json(resp, simplifyVector = FALSE)
  } else {
    resp  # caller deals with it (e.g. CSV)
  }
}

#' @keywords internal
.ccf_get <- function(client, path, query = NULL) {
  .ccf_check_client(client)
  query <- .ccf_drop_empty(query)
  req <- .ccf_build_request(client, path)
  if (length(query)) req <- httr2::req_url_query(req, !!!query)
  .ccf_handle(client, httr2::req_perform(req))
}

#' @keywords internal
.ccf_post <- function(client, path, body = NULL) {
  .ccf_check_client(client)
  req <- .ccf_build_request(client, path)
  req <- httr2::req_body_json(req, body %||% list(), auto_unbox = TRUE)
  .ccf_handle(client, httr2::req_perform(req))
}

#' @keywords internal
.ccf_post_raw <- function(client, path, body = NULL) {
  .ccf_check_client(client)
  req <- .ccf_build_request(client, path)
  req <- httr2::req_body_json(req, body %||% list(), auto_unbox = TRUE)
  resp <- httr2::req_perform(req)
  if (httr2::resp_is_error(resp)) return(.ccf_handle(client, resp))
  client$last_status <- .ccf_extract_status(resp)
  resp
}

#' @keywords internal
.ccf_norm_lang <- function(value) {
  # The CCF database stores language as uppercase 2-letter codes.
  # Accept either case from the user.
  if (is.null(value) || length(value) == 0) return(value)
  if (is.character(value)) {
    return(ifelse(nchar(value) == 2L, toupper(value), value))
  }
  value
}

#' @keywords internal
.ccf_valid_corpora <- c("legacy", "continuous", "all")

#' @keywords internal
.ccf_norm_corpus <- function(value) {
  # Corpus-provenance selector. `legacy` (frozen, citable) is the server
  # default and open to every tier; `continuous` (real-time feed) and `all`
  # require an `observer` token. NULL is passed through so the request omits it.
  if (is.null(value) || length(value) == 0) return(NULL)
  if (!value %in% .ccf_valid_corpora) {
    stop(sprintf("corpus must be one of %s, got '%s'",
                 paste(.ccf_valid_corpora, collapse = "|"), value), call. = FALSE)
  }
  value
}

#' @keywords internal
.ccf_norm_filters <- function(filters) {
  if (is.null(filters) || !length(filters)) return(filters)
  if ("lang" %in% names(filters))     filters$lang     <- .ccf_norm_lang(filters$lang)
  if ("language" %in% names(filters)) filters$language <- .ccf_norm_lang(filters$language)
  if ("corpus" %in% names(filters))   filters$corpus   <- .ccf_norm_corpus(filters$corpus)
  filters
}

#' @keywords internal
.ccf_drop_empty <- function(x) {
  if (is.null(x) || !length(x)) return(list())
  keep <- vapply(x, function(v) {
    if (is.null(v)) return(FALSE)
    if (length(v) == 0) return(FALSE)
    if (is.character(v) && length(v) == 1 && !nzchar(v)) return(FALSE)
    TRUE
  }, logical(1))
  x[keep]
}

#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x

#' @keywords internal
.ccf_to_tibble <- function(rows) {
  if (is.null(rows) || length(rows) == 0) return(tibble::tibble())
  if (is.data.frame(rows)) return(tibble::as_tibble(rows))
  # rows is a list of lists — convert to tibble via JSON round-trip for type safety
  json <- jsonlite::toJSON(rows, auto_unbox = TRUE, null = "null", na = "null")
  df <- jsonlite::fromJSON(json, simplifyDataFrame = TRUE, simplifyVector = TRUE,
                            simplifyMatrix = FALSE, flatten = FALSE)
  if (!is.data.frame(df)) {
    return(tibble::tibble(value = list(rows)))
  }
  tibble::as_tibble(df)
}
