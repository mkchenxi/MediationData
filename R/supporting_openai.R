#' OpenAI helper functions (support)
#'
#' These helpers wrap the OpenAI Files API and (optionally) Chat Completions.
#' They are designed to be used internally by higher-level wrappers such as
#' \code{\link{generate_mediation_repro_codes}}.
#'
#' @details
#' Functions in this file use the \pkg{httr2} package for HTTP requests.
#'
#' @name openai_support
NULL

#' Null coalescing helper
#' @keywords internal
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Call the Chat Completions API (legacy helper)
#'
#' @param messages A list of chat messages in the format required by the API.
#' @param model Character scalar. Model name.
#' @param temperature Numeric scalar. Sampling temperature.
#' @param api_key Character scalar. OpenAI API key.
#' @param api_base Character scalar. API base URL.
#'
#' @return A character scalar: the assistant reply text.
#' @keywords internal
#'
#' @importFrom httr2 request req_auth_bearer_token req_body_json req_error req_perform
#' @importFrom httr2 resp_status resp_body_json resp_body_string
gpt_chat <- function(messages,
                     model = "gpt-4o-mini",
                     temperature = 0,
                     api_key = Sys.getenv("OPENAI_API_KEY"),
                     api_base = "https://api.openai.com/v1") {
  if (!is.character(api_key) || length(api_key) != 1L || !nzchar(api_key)) {
    stop("API key is empty. Set OPENAI_API_KEY or pass api_key explicitly.")
  }
  if (!is.list(messages) || length(messages) == 0) {
    stop("`messages` must be a non-empty list.")
  }

  req <- httr2::request(paste0(api_base, "/chat/completions")) |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_body_json(list(
      model = model,
      messages = messages,
      temperature = temperature
    )) |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)

  if (status >= 400) {
    stop(
      "Chat Completions call failed (HTTP ", status, "):\n",
      httr2::resp_body_string(resp),
      call. = FALSE
    )
  }

  dat <- httr2::resp_body_json(resp)
  # Typical structure: choices[[1]]$message$content
  dat$choices[[1]]$message$content
}

#' Upload a file to the OpenAI Files API (multipart)
#'
#' @param path Character scalar. Path to file to upload.
#' @param purpose Character scalar. Purpose for the upload, e.g. "user_data".
#' @param api_key Character scalar. OpenAI API key.
#' @param api_base Character scalar. API base URL.
#'
#' @return A list parsed from JSON (includes $id).
#' @keywords internal
#' @importFrom httr2 request req_headers req_body_multipart req_perform resp_body_json
#' @importFrom curl form_file
upload_file_to_openai <- function(
    path,
    purpose = "user_data",
    api_key,
    api_base = "https://api.openai.com/v1"
) {
  if (!file.exists(path)) stop("File does not exist: ", path)
  if (!is.character(api_key) || length(api_key) != 1L || !nzchar(api_key)) {
    stop("`api_key` must be a non-empty character scalar.")
  }

  url <- paste0(sub("/+$", "", api_base), "/files")

  req <- httr2::request(url) |>
    httr2::req_headers(Authorization = paste("Bearer", api_key)) |>
    httr2::req_body_multipart(
      purpose = purpose,
      file = curl::form_file(path)
    )

  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp)
}


#' List files in the OpenAI Files API
#'
#' @param api_key Character scalar. OpenAI API key.
#' @param api_base Character scalar. API base URL.
#'
#' @return A data frame with at least \code{id}, \code{filename}, \code{purpose}.
#' @keywords internal
#'
#' @importFrom httr2 request req_auth_bearer_token req_error req_perform
#' @importFrom httr2 resp_status resp_body_json resp_body_string
list_openai_files <- function(api_key = Sys.getenv("OPENAI_API_KEY"),
                              api_base = "https://api.openai.com/v1") {
  if (!is.character(api_key) || length(api_key) != 1L || !nzchar(api_key)) {
    stop("API key is empty. Set OPENAI_API_KEY or pass api_key explicitly.")
  }

  req <- httr2::request(paste0(api_base, "/files")) |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)

  if (status >= 400) {
    stop(
      "List files failed (HTTP ", status, "):\n",
      httr2::resp_body_string(resp),
      call. = FALSE
    )
  }

  dat <- httr2::resp_body_json(resp)
  # Expected shape: list(data = [ {id, filename, purpose, ...}, ... ])
  if (is.null(dat$data) || length(dat$data) == 0) {
    return(data.frame(id = character(0), filename = character(0), purpose = character(0)))
  }

  # Coerce to data.frame safely
  rows <- lapply(dat$data, function(x) {
    data.frame(
      id = x$id %||% NA_character_,
      filename = x$filename %||% NA_character_,
      purpose = x$purpose %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Delete a file from the OpenAI Files API
#'
#' @param file_id Character scalar. File id to delete.
#' @param api_key Character scalar. OpenAI API key.
#' @param api_base Character scalar. API base URL.
#'
#' @return TRUE invisibly on success.
#' @keywords internal
#'
#' @importFrom httr2 request req_auth_bearer_token req_error req_perform
#' @importFrom httr2 resp_status resp_body_json resp_body_string
delete_file_from_openai <- function(file_id,
                                   api_key = Sys.getenv("OPENAI_API_KEY"),
                                   api_base = "https://api.openai.com/v1") {
  if (!is.character(api_key) || length(api_key) != 1L || !nzchar(api_key)) {
    stop("API key is empty. Set OPENAI_API_KEY or pass api_key explicitly.")
  }
  if (!is.character(file_id) || length(file_id) != 1L || !nzchar(file_id)) {
    stop("`file_id` must be a non-empty character scalar.")
  }

  req <- httr2::request(paste0(api_base, "/files/", file_id)) |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_method("DELETE") |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)

  if (status >= 400) {
    stop(
      "Delete failed (HTTP ", status, "):\n",
      httr2::resp_body_string(resp),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Concatenate code files in a folder into a single text file
#'
#' @param folder Folder to scan.
#' @param exts File extensions to include (character vector, without dot).
#' @param output_file Output .txt file path.
#' @param recursive Whether to scan recursively.
#'
#' @return Invisibly, \code{output_file}.
#' @keywords internal
append_code_files <- function(folder,
                              exts = c("R","r","Rmd","rmd","qmd","py","sas","sps","do","m","jl","stan"),
                              output_file = "all_codes.txt",
                              recursive = TRUE) {
  if (!dir.exists(folder)) stop("Folder does not exist: ", folder)
  pattern <- paste0("\\.(", paste(unique(exts), collapse = "|"), ")$")
  files <- list.files(folder, pattern = pattern, ignore.case = TRUE,
                      full.names = TRUE, recursive = recursive)

  if (length(files) == 0) {
    message("No code files found in the folder.")
    return(invisible(NULL))
  }

  con <- file(output_file, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)

  for (f in files) {
    cat("---- FILE:", basename(f), "----\n", file = con, append = TRUE)
    lines <- tryCatch(readLines(f, warn = FALSE),
                      error = function(e) paste("Could not read file:", f))
    writeLines(lines, con)
    cat("\n\n", file = con, append = TRUE)
  }

  message("Combined code written to: ", output_file)
  invisible(output_file)
}

#' Detect raw data files in a directory
#'
#' Scans a directory for files with common data extensions and returns the
#' detected filenames along with a best-effort format label.
#'
#' @param dir Character scalar. Directory to scan.
#' @param recursive Logical. Whether to scan subdirectories.
#' @param extensions Character vector of file extensions to consider (without dot).
#'
#' @return A list with:
#' \itemize{
#'   \item \code{raw_files}: detected filenames (basename only)
#'   \item \code{formats}: mapped format labels, aligned to \code{raw_files}
#' }
#'
#' @export
detect_raw_data_files <- function(
    dir = ".",
    recursive = FALSE,
    extensions = c(
      "csv", "tsv", "txt", "dat", "data", "fwf",
      "json", "ndjson", "yaml", "yml", "xml",
      "rds", "rdata",
      "dta", "stb", "stc",
      "sav", "zsav", "por", "spss",
      "sas7bdat", "xpt",
      "xlsx", "xls", "ods",
      "parquet", "feather", "arrow",
      "db", "sqlite", "sqlite3", "accdb", "mdb",
      "mat", "h5", "hdf5",
      "pkl", "pickle",
      "dct",
      "raw",
      "zip", "7z", "tar", "gz"
    )
) {
  if (!dir.exists(dir)) stop("Directory does not exist: ", dir)

  extensions <- unique(tolower(extensions))
  pattern <- paste0("\\.(", paste(extensions, collapse = "|"), ")$")
  data_paths <- list.files(dir, pattern = pattern, ignore.case = TRUE,
                           full.names = TRUE, recursive = recursive)

  # Drop temporary / hidden files
  data_paths <- data_paths[!grepl("/\\.|^\\.", data_paths)]
  data_files <- basename(data_paths)
  data_ext <- tolower(tools::file_ext(data_files))

  format_map <- list(
    csv = "CSV", tsv = "TSV", txt = "TEXT", dat = "TEXT", data = "TEXT", fwf = "FIXED_WIDTH",
    json = "JSON", ndjson = "NDJSON", yaml = "YAML", yml = "YAML", xml = "XML",
    rds = "RDS", rdata = "RDATA",
    dta = "STATA",
    sav = "SPSS", zsav = "SPSS", por = "SPSS",
    sas7bdat = "SAS", xpt = "SAS_XPORT",
    xlsx = "EXCEL", xls = "EXCEL", ods = "ODS",
    parquet = "PARQUET", feather = "FEATHER", arrow = "ARROW",
    db = "DATABASE", sqlite = "DATABASE", sqlite3 = "DATABASE", accdb = "ACCESS", mdb = "ACCESS",
    mat = "MATLAB", h5 = "HDF5", hdf5 = "HDF5",
    pkl = "PYTHON_PICKLE", pickle = "PYTHON_PICKLE",
    dct = "DATA_DICTIONARY",
    raw = "RAW_BINARY",
    zip = "ARCHIVE", `7z` = "ARCHIVE", tar = "ARCHIVE", gz = "ARCHIVE"
  )

  `%||%` <- function(a, b) if (!is.null(a)) a else b
  formats <- vapply(data_ext, function(e) format_map[[e]] %||% toupper(e), character(1))

  list(raw_files = data_files, formats = formats)
}

#' Call the Responses API with attached file IDs (PDFs)
#'
#' @param instructions Character scalar. System/developer instructions.
#' @param file_ids List of file id strings to attach.
#' @param user_text Character scalar. User message content.
#' @param model Character scalar. Model name.
#' @param temperature Numeric scalar.
#' @param api_key Character scalar. OpenAI API key.
#' @param api_base Character scalar. API base URL.
#' @param store Logical. Whether to store the response server-side.
#'
#' @return An \pkg{httr2} response object.
#' @keywords internal
#'
#' @importFrom httr2 request req_auth_bearer_token req_body_json req_error req_perform
#' @importFrom httr2 resp_status resp_body_string
gpt_response_with_files <- function(
    instructions,
    file_ids,
    user_text = "Use the attached files to perform the task described in the instructions.",
    model = "gpt-4o-mini",
    temperature = 0,
    api_key = Sys.getenv("OPENAI_API_KEY"),
    api_base = "https://api.openai.com/v1",
    store = FALSE
) {
  if (!is.character(api_key) || length(api_key) != 1L || !nzchar(api_key)) {
    stop("API key is empty. Set OPENAI_API_KEY or pass api_key explicitly.")
  }
  if (!is.character(instructions) || length(instructions) != 1L || !nzchar(instructions)) {
    stop("`instructions` must be a non-empty character scalar.")
  }
  if (!is.list(file_ids) || length(file_ids) == 0) {
    stop("`file_ids` must be a non-empty list of file id strings.")
  }

  attachments <- lapply(file_ids, function(fid) {
    list(type = "input_file", file_id = fid)
  })

  body <- list(
    model = model,
    temperature = temperature,
    store = store,
    input = list(
      list(
        role = "system",
        content = list(list(type = "input_text", text = instructions))
      ),
      list(
        role = "user",
        content = c(
          list(list(type = "input_text", text = user_text)),
          attachments
        )
      )
    )
  )

  req <- httr2::request(paste0(api_base, "/responses")) |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_body_json(body) |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)

  if (status >= 400) {
    stop(
      "Responses API call failed (HTTP ", status, "):\n",
      httr2::resp_body_string(resp),
      call. = FALSE
    )
  }

  resp
}
