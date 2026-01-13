#' Mediation scan patterns (internal)
#'
#' A broad set of regex patterns used to detect mediation-analysis related code.
#' Kept as a function (rather than a global object) to avoid accidental mutation
#' and to keep package namespace clean.
#'
#' @keywords internal
.mediation_patterns <- function() {
  c(
    "mediation",
    "mediate\\s*\\(",
    "indirect effect",
    "indirect_effect",
    "bootstrap",

    # R packages / syntax
    "library\\s*\\(mediation\\)",
    "library\\s*\\(lavaan\\)",
    "lavaan\\s*\\(",
    "sem\\s*\\(",
    "processR",
    "lavaan::",

    # SPSS PROCESS macro style
    "PROCESS",
    "process macro",
    "PROCESS macro",
    "/model\\s*=\\s*4",
    "model\\s*4",
    "/model\\s*=",

    # variations of mediator-related vocab
    "mediat(e|or|ing)"
  )
}

#' Safely read a text file
#'
#' Reads a file as UTF-8 text and returns its lines. If reading fails (binary,
#' permissions, encoding issues), returns character(0).
#'
#' @param path Character scalar. File path.
#' @param max_lines Integer. Maximum number of lines to read.
#'
#' @return A character vector of lines (possibly length 0).
#'
#' @keywords internal
read_file_safe <- function(path, max_lines = 20000) {
  tryCatch(
    readLines(path, warn = FALSE, n = max_lines, encoding = "UTF-8"),
    error = function(e) character(0)
  )
}

#' Scan one file for mediation-related content
#'
#' Scans a single code/text file for mediation-analysis patterns and returns a
#' one-row tibble with the matched patterns and a short snippet around the first hit.
#'
#' @param path Character scalar. File path.
#' @param patterns Character vector of regex patterns (case-insensitive).
#'
#' @return A tibble with columns: file_path, matched_patterns, snippet.
#'   Returns NULL if no matches are found or the file can't be read.
#'
#' @export
#' @importFrom stringr str_detect regex
#' @importFrom purrr map_lgl
#' @importFrom tibble tibble
scan_one_file <- function(path, patterns = .mediation_patterns()) {
  lines <- read_file_safe(path)

  if (length(lines) == 0) {
    return(NULL)
  }

  text_full <- paste(lines, collapse = "\n")

  hits <- purrr::map_lgl(
    patterns,
    ~ stringr::str_detect(text_full, stringr::regex(.x, ignore_case = TRUE))
  )

  if (!any(hits)) {
    return(NULL)
  }

  matched_patterns <- patterns[hits]

  snippet <- NULL
  for (pat in matched_patterns) {
    idx <- which(stringr::str_detect(lines, stringr::regex(pat, ignore_case = TRUE)))[1]
    if (!is.na(idx)) {
      from <- max(1, idx - 2)
      to   <- min(length(lines), idx + 2)
      snippet <- paste(lines[from:to], collapse = "\n")
      break
    }
  }

  tibble::tibble(
    file_path        = path,
    matched_patterns = paste(matched_patterns, collapse = "; "),
    snippet          = snippet
  )
}

#' Scan many files for mediation-related content
#'
#' @param paths Character vector of file paths.
#' @param patterns Character vector of regex patterns (case-insensitive).
#'
#' @return A tibble of matches (0 rows if none).
#'
#' @export
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
scan_files <- function(paths, patterns = .mediation_patterns()) {
  res <- purrr::map(paths, ~ scan_one_file(.x, patterns = patterns))
  dplyr::bind_rows(res)
}

#' List code/text files under a directory
#'
#' @param dir Source directory.
#' @param recursive Logical. Recurse into subdirectories?
#' @param code_ext Character vector of file extensions (including dot).
#'
#' @return Character vector of file paths.
#'
#' @keywords internal
#' @importFrom fs dir_ls
list_code_files <- function(
    dir,
    recursive = TRUE,
    code_ext = c(".R", ".r", ".Rmd", ".qmd", ".py", ".do", ".sas", ".sps", ".txt", ".md")
) {
  all <- fs::dir_ls(dir, recurse = recursive, type = "file", fail = FALSE)
  all[tolower(fs::path_ext(all)) %in% tolower(gsub("^\\.", "", code_ext))]
}

#' List data files under a directory
#'
#' @param dir Source directory.
#' @param recursive Logical. Recurse into subdirectories?
#' @param data_ext Character vector of file extensions (including dot).
#'
#' @return Character vector of file paths.
#'
#' @keywords internal
#' @importFrom fs dir_ls
list_data_files <- function(
    dir,
    recursive = TRUE,
    data_ext = c(".csv", ".tsv", ".txt", ".xlsx", ".xls", ".sav", ".dta", ".rds", ".RData")
) {
  all <- fs::dir_ls(dir, recurse = recursive, type = "file", fail = FALSE)
  all[tolower(fs::path_ext(all)) %in% tolower(gsub("^\\.", "", data_ext))]
}

#' Find mediation-related code files in a directory
#'
#' @param dir Source directory.
#' @param recursive Logical. Recurse into subdirectories?
#' @param patterns Character vector of regex patterns (case-insensitive).
#'
#' @return A tibble with matches (may be 0 rows).
#'
#' @export
#' @importFrom dplyr distinct
find_mediation_code_files <- function(dir, recursive = TRUE, patterns = .mediation_patterns()) {
  code_files <- list_code_files(dir, recursive = recursive)
  matches <- scan_files(code_files, patterns = patterns)

  # De-duplicate in case the same path appears twice (edge cases)
  dplyr::distinct(matches, .data$file_path, .keep_all = TRUE)
}
