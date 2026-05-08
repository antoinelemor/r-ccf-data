# Embedded codebook + helpers (no HTTP calls).

#' @keywords internal
.ccf_load_codebook <- function() {
  path <- system.file("extdata", "codebook.json", package = "ccfdata")
  if (!nzchar(path)) {
    # Dev fallback (devtools::load_all from the source tree)
    path <- file.path("inst", "extdata", "codebook.json")
  }
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

#' @keywords internal
.ccf_codebook_cache <- NULL

#' Return the embedded CCF codebook as an R list.
#'
#' Mirrors the JSON shipped with the package: frame metadata,
#' subcategory lists, messengers, events, solutions, tones,
#' operational definitions, media outlets, and tier descriptions.
#'
#' @return A nested list parsed from `inst/extdata/codebook.json`.
#' @examples
#' cb <- ccf_codebook()
#' names(cb$frames)
#' @export
ccf_codebook <- function() {
  if (is.null(.ccf_codebook_cache)) {
    .ccf_codebook_cache <<- .ccf_load_codebook()
  }
  .ccf_codebook_cache
}

#' Operational definition for a single annotation column.
#'
#' @param column Annotation column name (e.g. `"eco_neg_impact"`).
#' @return Character scalar.
#' @examples
#' ccf_define("sci_skepticism")
#' @export
ccf_define <- function(column) {
  cb <- ccf_codebook()
  if (is.null(cb$definitions[[column]])) {
    stop(sprintf("Unknown annotation column: %s", column), call. = FALSE)
  }
  cb$definitions[[column]]
}

#' Subcategory column names for a given frame.
#'
#' @param frame One of `"economic"`, `"health"`, `"security"`, `"justice"`,
#'   `"political"`, `"scientific"`, `"environmental"`, `"cultural"`.
#' @return Character vector of column names.
#' @examples
#' ccf_subcategories_of("economic")
#' @export
ccf_subcategories_of <- function(frame) {
  cb <- ccf_codebook()
  if (is.null(cb$frame_subcategories[[frame]])) {
    stop(sprintf("Unknown frame: %s. Valid frames: %s",
                 frame, paste(names(cb$frames), collapse = ", ")),
         call. = FALSE)
  }
  unlist(cb$frame_subcategories[[frame]])
}

#' Frame metadata.
#'
#' @return Character vector of frame names (length 8).
#' @export
ccf_frame_names <- function() names(ccf_codebook()$frames)

#' @rdname ccf_frame_names
#' @return Character vector of frame DB columns (length 8).
#' @export
ccf_frame_columns <- function() {
  vapply(ccf_codebook()$frames, function(x) x$col, character(1), USE.NAMES = FALSE)
}

#' List of media outlets covered by the corpus.
#'
#' @return Character vector of length 20.
#' @export
ccf_media_outlets <- function() unlist(ccf_codebook()$media_outlets)

#' Codebook as a tidy tibble.
#'
#' @return A tibble with columns `column`, `group`, `subgroup`, `definition`.
#' @export
ccf_codebook_df <- function() {
  cb <- ccf_codebook()
  rows <- list()
  add <- function(column, group, subgroup) {
    rows[[length(rows) + 1L]] <<- list(
      column = column, group = group, subgroup = subgroup,
      definition = cb$definitions[[column]] %||% ""
    )
  }
  for (fname in names(cb$frames)) add(cb$frames[[fname]]$col, "frame", fname)
  for (fname in names(cb$frame_subcategories)) {
    for (s in unlist(cb$frame_subcategories[[fname]])) {
      add(s, "frame_subcategory", fname)
    }
  }
  for (col in unlist(cb$messengers)) add(col, "messenger", "")
  for (col in unlist(cb$events))     add(col, "event", "")
  for (col in unlist(cb$solutions))  add(col, "solution", "")
  for (col in unlist(cb$tone))       add(col, "tone", "")
  for (col in unlist(cb$other))      add(col, "other", "")
  tibble::as_tibble(do.call(rbind.data.frame, lapply(rows, as.data.frame, stringsAsFactors = FALSE)))
}
