#' PDF + parsing helpers for code-bundle workflows
#'
#' @name pdf_helpers
NULL

#' Bundle code files in a folder into a text-based PDF for upload
#'
#' Creates a monospaced, paginated PDF containing the contents of selected code files.
#' This avoids OCR (the PDF is text-based) and is suitable for APIs that accept PDFs
#' as input attachments.
#'
#' @param folder Character scalar. Directory containing code files.
#' @param output_pdf Character scalar. Path to output PDF file.
#' @param recursive Logical. Whether to scan \code{folder} recursively.
#' @param exts Character vector. File extensions to include (without dot).
#' @param files Optional character vector of explicit file paths to include.
#' @param header_prefix Character scalar. Prefix shown before each file header.
#'
#' @return Invisibly, a list with \code{pdf} (path) and \code{files} (included paths).
#' @export
#'
#' @importFrom grDevices pdf dev.off
#' @importFrom grid grid.newpage grid.text gpar
write_code_files_pdf <- function(
  folder,
  output_pdf = file.path(tempdir(), "all_codes.pdf"),
  recursive = TRUE,
  exts = c(
    "R","r","Rmd","rmd","qmd",
    "py","ipynb",
    "sas","lst","log",
    "sps","spss","spo","spv",
    "do",
    "m",
    "inp","mplus","out","gh5",
    "jl",
    "stan"
  ),
  files = NULL,
  header_prefix = "---- FILE: "
) {
  if (!dir.exists(folder)) stop("Folder does not exist: ", folder)

  if (is.null(files)) {
    pattern <- paste0("\\.(", paste(unique(exts), collapse = "|"), ")$")
    files <- list.files(folder, pattern = pattern, ignore.case = TRUE,
                        full.names = TRUE, recursive = recursive)
  } else {
    files <- normalizePath(files, mustWork = TRUE)
  }

  if (length(files) == 0) stop("No code files found in: ", folder)

  all_lines <- character(0)
  for (f in files) {
    all_lines <- c(all_lines, paste0(header_prefix, basename(f), " ----"))
    lines <- tryCatch(readLines(f, warn = FALSE),
                      error = function(e) c(paste("<<Could not read file:", f, ">>")))
    all_lines <- c(all_lines, lines, "")
  }

  lines_per_page <- 60
  grDevices::pdf(output_pdf, width = 8.5, height = 11, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)

  n <- length(all_lines)
  page_count <- ceiling(n / lines_per_page)

  for (p in seq_len(page_count)) {
    idx_start <- (p - 1) * lines_per_page + 1
    idx_end <- min(p * lines_per_page, n)
    page_lines <- all_lines[idx_start:idx_end]

    grid::grid.newpage()
    grid::grid.text(
      label = paste0("Code bundle (page ", p, " / ", page_count, ")"),
      x = 0.02, y = 0.98, just = c("left", "top"),
      gp = grid::gpar(fontsize = 9, fontfamily = "mono")
    )

    y_top <- 0.95
    line_step <- 0.0155

    for (i in seq_along(page_lines)) {
      grid::grid.text(
        label = page_lines[[i]],
        x = 0.02,
        y = y_top - (i - 1) * line_step,
        just = c("left", "top"),
        gp = grid::gpar(fontsize = 8, fontfamily = "mono")
      )
    }
  }

  invisible(list(pdf = output_pdf, files = files))
}

#' Extract text from an httr2 response returned by the Responses API
#'
#' @param resp An \pkg{httr2} response object (from \code{gpt_response_with_files()}).
#'
#' @return A single character scalar containing concatenated text output.
#' @keywords internal
#'
#' @importFrom httr2 resp_body_json
extract_responses_output_text <- function(resp) {
  dat <- httr2::resp_body_json(resp)

  out_txt <- character(0)

  if (!is.null(dat$output) && length(dat$output) > 0) {
    for (item in dat$output) {
      if (!is.null(item$content) && length(item$content) > 0) {
        for (blk in item$content) {
          if (!is.null(blk$type) && blk$type %in% c("output_text", "text")) {
            if (!is.null(blk$text)) out_txt <- c(out_txt, blk$text)
          }
        }
      }
      if (!is.null(item$text)) out_txt <- c(out_txt, item$text)
    }
  }

  if (length(out_txt) == 0 && !is.null(dat$output_text)) out_txt <- c(out_txt, dat$output_text)

  paste(out_txt, collapse = "\n")
}

#' Extract tagged R code blocks from model output text
#'
#' Looks for fenced R code blocks containing the tags
#' \code{DATA_CODE} and \code{METADATA_CODE}. If tags are missing but two
#' R blocks exist, uses the first as data code and second as metadata.
#'
#' @param text Character scalar. Model output text.
#'
#' @return A list with \code{data_code} and \code{metadata_code} (each character or NULL).
#' @keywords internal
extract_tagged_r_blocks <- function(text) {
  blocks <- regmatches(text, gregexpr("```r[\\s\\S]*?```", text, perl = TRUE))[[1]]

  clean_block <- function(b) {
    b <- sub("^```r\\s*", "", b)
    b <- sub("\\s*```$", "", b)
    b
  }

  data_code <- NULL
  metadata_code <- NULL

  if (length(blocks) > 0) {
    for (b in blocks) {
      bb <- clean_block(b)
      if (grepl("DATA_CODE", bb, fixed = TRUE) && is.null(data_code)) data_code <- bb
      if (grepl("METADATA_CODE", bb, fixed = TRUE) && is.null(metadata_code)) metadata_code <- bb
    }
  }

  if (is.null(data_code) && is.null(metadata_code) && length(blocks) >= 2) {
    data_code <- clean_block(blocks[[1]])
    metadata_code <- clean_block(blocks[[2]])
  } else if (is.null(data_code) && length(blocks) == 1) {
    data_code <- clean_block(blocks[[1]])
  }

  list(data_code = data_code, metadata_code = metadata_code)
}
