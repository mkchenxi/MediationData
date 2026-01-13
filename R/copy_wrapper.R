#' Copy mediation-relevant files from a source directory to a destination directory
#'
#' This wrapper:
#' 1) scans for mediation-related code/text files,
#' 2) optionally includes data files from the same directory tree,
#' 3) copies selected files into dest_dir WITHOUT preserving folder structure (flat copy).
#'
#' @param source_dir Character scalar. Root directory to scan.
#' @param dest_dir Character scalar. Destination directory (files copied directly here).
#' @param recursive Logical. Recurse into subdirectories?
#' @param include_data Logical. Also copy data files (by extension)?
#' @param data_strategy One of:
#'   - "all": copy all data files found under source_dir
#'   - "same_folders": copy data files only from folders that contain matched mediation code files
#' @param overwrite Logical. Overwrite existing files in dest_dir?
#' @param dry_run Logical. If TRUE, returns planned operations without copying.
#' @param patterns Character vector of regex patterns for code scanning.
#' @param make_unique Logical. If TRUE, automatically disambiguate duplicate file names by
#'   appending a short hash derived from the original relative path.
#'
#' @return A tibble describing files selected and destination paths.
#'
#' @export
#' @importFrom fs dir_create path_dir path_file path_rel path file_copy
#' @importFrom dplyr distinct mutate
#' @importFrom tibble tibble
copy_mediation_files <- function(
    source_dir,
    dest_dir,
    recursive = TRUE,
    include_data = TRUE,
    data_strategy = c("same_folders", "all"),
    overwrite = FALSE,
    dry_run = FALSE,
    patterns = .mediation_patterns(),
    make_unique = TRUE
) {
  data_strategy <- match.arg(data_strategy)

  # 1) find mediation-related code files
  code_hits <- find_mediation_code_files(source_dir, recursive = recursive, patterns = patterns)
  code_paths <- unique(code_hits$file_path)

  # 2) decide which data files to include
  data_paths <- character(0)
  if (isTRUE(include_data)) {
    all_data <- list_data_files(source_dir, recursive = recursive)

    if (data_strategy == "all") {
      data_paths <- all_data
    } else {
      # same_folders: include data only from folders where code hits were found
      hit_dirs <- unique(fs::path_dir(code_paths))
      data_paths <- all_data[fs::path_dir(all_data) %in% hit_dirs]
    }
  }

  selected <- unique(c(code_paths, data_paths))

  if (length(selected) == 0) {
    return(tibble::tibble(
      source_path = character(0),
      dest_path   = character(0),
      type        = character(0)
    ))
  }

  # 3) FLAT destination: put everything directly in dest_dir
  base_names <- fs::path_file(selected)
  dest_paths <- fs::path(dest_dir, base_names)

  out <- tibble::tibble(
    source_path = selected,
    dest_path   = dest_paths,
    type        = ifelse(selected %in% code_paths, "code", "data")
  )

  # Handle collisions: same filename from different source folders
  dup <- duplicated(out$dest_path) | duplicated(out$dest_path, fromLast = TRUE)
  if (any(dup) && isTRUE(make_unique)) {
    # create a short stable suffix from the original relative path
    rel <- fs::path_rel(out$source_path, start = source_dir)
    suffix <- substr(sprintf("%08x", as.integer(stats::runif(length(rel), 0, 2^31 - 1))), 1, 6)

    # More stable than random: use a simple deterministic hash-ish via utf8 ints
    # (keeps dependencies minimal)
    hash6 <- function(x) {
      z <- sum(utf8ToInt(x)) %% 0xFFFFFF
      sprintf("%06x", z)
    }
    suf <- vapply(rel, hash6, character(1))

    # insert suffix before extension
    ext <- fs::path_ext(out$dest_path)
    stem <- sub(paste0("\\.", ext, "$"), "", fs::path_file(out$dest_path))
    new_name <- ifelse(
      nzchar(ext),
      paste0(stem, "_", suf, ".", ext),
      paste0(stem, "_", suf)
    )

    out$dest_path[dup] <- fs::path(dest_dir, new_name[dup])
  } else if (any(dup) && !isTRUE(overwrite)) {
    stop(
      "Name collisions detected when flattening into dest_dir. ",
      "Set make_unique=TRUE (recommended) or overwrite=TRUE."
    )
  }

  out <- dplyr::distinct(out, .data$source_path, .keep_all = TRUE)

  if (isTRUE(dry_run)) {
    return(out)
  }

  # ensure destination directory exists and copy
  fs::dir_create(dest_dir, recurse = TRUE)

  fs::file_copy(out$source_path, out$dest_path, overwrite = overwrite)

  out
}
