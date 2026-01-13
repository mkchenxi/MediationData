#' Generate reproducible mediation dataset reconstruction code
#'
#' High-level wrapper that:
#' \itemize{
#'   \item detects data file names in a directory,
#'   \item bundles code files into a PDF,
#'   \item uploads the PDF and calls the Responses API,
#'   \item extracts tagged R code blocks and writes them to disk,
#'   \item validates whether generated code references missing local data files,
#'   \item optionally deletes the uploaded PDF.
#' }
#'
#' @param prompts Character scalar. Instruction prompt passed to the model.
#' @param api_key Character scalar. OpenAI API key.
#' @param folder_dir Character scalar. Directory containing code files and data files.
#' @param model Character scalar. Model name.
#' @param temperature Numeric scalar. Sampling temperature.
#' @param api_base Character scalar. API base URL.
#' @param recursive Logical. Whether to scan \code{folder_dir} recursively.
#' @param cleanup_uploaded_files Logical. Whether to delete the uploaded PDF after the request.
#' @param output_data_code Character scalar. Output filename for reconstruction code.
#' @param output_metadata_code Character scalar. Output filename for metadata code.
#'
#' @return Invisibly, a list with upload id, detected file info, output text, output paths,
#' and validation results about referenced vs. available data files.
#' @export
generate_mediation_repro_codes <- function(
    prompts,
    api_key,
    folder_dir,
    model = "gpt-4o-mini",
    temperature = 0,
    api_base = "https://api.openai.com/v1",
    recursive = TRUE,
    cleanup_uploaded_files = TRUE,
    output_data_code = "data_code.R",
    output_metadata_code = "metadata_code.R"
) {
  if (!is.character(prompts) || length(prompts) != 1L || !nzchar(prompts)) {
    stop("`prompts` must be a non-empty character scalar.")
  }
  if (!is.character(api_key) || length(api_key) != 1L || !nzchar(api_key)) {
    stop("`api_key` must be a non-empty character scalar.")
  }
  if (!is.character(folder_dir) || length(folder_dir) != 1L || !dir.exists(folder_dir)) {
    stop("`folder_dir` must be an existing directory.")
  }

  data_info <- detect_raw_data_files(dir = folder_dir, recursive = recursive)
  bundle <- write_code_files_pdf(folder = folder_dir, recursive = recursive)

  up <- upload_file_to_openai(
    path = bundle$pdf,
    purpose = "user_data",
    api_key = api_key,
    api_base = api_base
  )
  file_id <- up$id

  if (isTRUE(cleanup_uploaded_files)) {
    on.exit(
      try(delete_file_from_openai(file_id, api_key = api_key, api_base = api_base), silent = TRUE),
      add = TRUE
    )
  }

  raw_files <- data_info$raw_files
  formats <- data_info$formats

  data_listing <- if (length(raw_files) == 0) {
    "No raw data files detected in the directory."
  } else {
    paste0(
      "Detected data files in directory (name | format):\n",
      paste0("- ", raw_files, " | ", formats, collapse = "\n")
    )
  }

  user_text <- paste(
    "Use the attached code bundle to reproduce the mediation dataset construction.",
    "",
    data_listing,
    "",
    "Output exactly TWO fenced R code blocks:",
    "1) A block containing the reconstruction code tagged with a comment 'DATA_CODE'",
    "2) A block containing the metadata/description code tagged with a comment 'METADATA_CODE'",
    "",
    "Example tags:",
    "```r",
    "# DATA_CODE",
    "...",
    "```",
    "```r",
    "# METADATA_CODE",
    "...",
    "```",
    sep = "\n"
  )

  resp <- gpt_response_with_files(
    instructions = prompts,
    file_ids = list(file_id),
    user_text = user_text,
    model = model,
    temperature = temperature,
    api_key = api_key,
    api_base = api_base,
    store = FALSE
  )

  out_text <- extract_responses_output_text(resp)
  blocks <- extract_tagged_r_blocks(out_text)

  out_data_path <- file.path(folder_dir, output_data_code)
  out_meta_path <- file.path(folder_dir, output_metadata_code)

  if (!is.null(blocks$data_code)) {
    writeLines(blocks$data_code, out_data_path)
  } else {
    warning("Could not extract DATA_CODE block. Saving full model output instead.")
    writeLines(out_text, out_data_path)
  }

  if (!is.null(blocks$metadata_code)) {
    writeLines(blocks$metadata_code, out_meta_path)
  } else {
    warning("Could not extract METADATA_CODE block; not writing metadata file.")
  }

  # ---- NEW: Validate that generated code doesn't reference missing data files ----
  code_for_validation <- if (!is.null(blocks$data_code)) blocks$data_code else out_text
  referenced_data_files <- extract_referenced_data_files(code_for_validation)

  # Compare against detected local files (basename comparison)
  available_data_files <- unique(basename(raw_files))
  missing_referenced_data_files <- check_missing_inputs(
    referenced_files = referenced_data_files,
    available_files = available_data_files
  )

  validation_passed <- length(missing_referenced_data_files) == 0

  if (!validation_passed) {
    warning(
      "Generated code references data files not detected locally: ",
      paste(missing_referenced_data_files, collapse = ", ")
    )
  }

  invisible(list(
    file_id = file_id,
    uploaded_pdf = bundle$pdf,
    code_files = bundle$files,
    detected_data_files = data_info,
    output_text = out_text,
    data_code_path = out_data_path,
    metadata_code_path = out_meta_path,
    referenced_data_files = referenced_data_files,
    missing_referenced_data_files = missing_referenced_data_files,
    validation_passed = validation_passed
  ))
}

# -------------------------------------------------------------------
# Internal helpers (keep in same R file or a utilities R file)
# -------------------------------------------------------------------

#' Extract likely data file names referenced in generated R code
#'
#' Searches for common file-reading calls and extracts the first string argument.
#' Returns basenames to make comparison robust to relative/absolute paths.
#'
#' @param code_text Character scalar. R code (or model output containing R code).
#' @return Character vector of unique basenames of referenced files.
#' @keywords internal
extract_referenced_data_files <- function(code_text) {
  if (!is.character(code_text) || length(code_text) != 1L) return(character(0))

  patterns <- c(
    # base
    'read\\.csv\\s*\\(\\s*["\']([^"\']+)["\']',
    'read\\.csv2\\s*\\(\\s*["\']([^"\']+)["\']',
    'read\\.delim\\s*\\(\\s*["\']([^"\']+)["\']',
    'readRDS\\s*\\(\\s*["\']([^"\']+)["\']',
    'load\\s*\\(\\s*["\']([^"\']+)["\']',

    # readr
    'readr::read_csv\\s*\\(\\s*["\']([^"\']+)["\']',
    'readr::read_tsv\\s*\\(\\s*["\']([^"\']+)["\']',
    'readr::read_delim\\s*\\(\\s*["\']([^"\']+)["\']',

    # data.table
    'data\\.table::fread\\s*\\(\\s*["\']([^"\']+)["\']',

    # haven
    'haven::read_sav\\s*\\(\\s*["\']([^"\']+)["\']',
    'haven::read_dta\\s*\\(\\s*["\']([^"\']+)["\']',
    'haven::read_sas\\s*\\(\\s*["\']([^"\']+)["\']',
    'haven::read_xpt\\s*\\(\\s*["\']([^"\']+)["\']',

    # arrow
    'arrow::read_parquet\\s*\\(\\s*["\']([^"\']+)["\']',
    'arrow::read_feather\\s*\\(\\s*["\']([^"\']+)["\']',

    # openxlsx (common pattern is loadWorkbook("file.xlsx") or read.xlsx("file.xlsx", ...))
    'openxlsx::read\\.xlsx\\s*\\(\\s*["\']([^"\']+)["\']',
    'openxlsx::loadWorkbook\\s*\\(\\s*["\']([^"\']+)["\']'
  )

  files <- character(0)
  for (pat in patterns) {
    m <- gregexpr(pat, code_text, perl = TRUE)
    hits <- regmatches(code_text, m)[[1]]
    if (length(hits) > 0) {
      extracted <- sub(pat, "\\1", hits, perl = TRUE)
      files <- c(files, extracted)
    }
  }

  files <- files[nzchar(files)]
  unique(basename(files))
}

#' Identify which referenced files are missing from available files
#'
#' @param referenced_files Character vector of basenames referenced by generated code.
#' @param available_files Character vector of basenames available locally.
#' @return Character vector of missing basenames.
#' @keywords internal
check_missing_inputs <- function(referenced_files, available_files) {
  referenced_files <- unique(as.character(referenced_files))
  available_files  <- unique(as.character(available_files))

  referenced_files <- referenced_files[nzchar(referenced_files)]
  available_files  <- available_files[nzchar(available_files)]

  setdiff(referenced_files, available_files)
}
