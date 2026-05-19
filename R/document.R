
#' Build output lines for a decision report document
#'
#' Converts a `rapsimng_decide_document` object into a character vector that can
#' be written to disk. A document prefix is written first when present, then
#' each non-`NULL` section body separated by blank lines.
#'
#' @param document A report object with class `rapsimng_decide_document`.
#'
#' @returns A character vector containing the ordered document lines.
#'
#' @export
document_lines <- function(document) {
    stopifnot(inherits(document, "rapsimng_decide_document"))
    res <- c()
    if (!is.null(document$prefix)) {
        res <- c(res, document$prefix$body, "")
    }
	c(
		res,
		unlist(lapply(document$sections, function(section) {
			if (is.null(section)) {
				return(NULL)
			}

			c(section$body, "")
		}), use.names = FALSE)
	)
}


#' Write a decision report document to disk
#'
#' Serialises a `rapsimng_decide_document` with `document_lines()` and writes the
#' resulting text file to `path`.
#'
#' @param document A report object with class `rapsimng_decide_document`.
#' @param path A single output file path.
#' @param overwrite Logical scalar indicating whether an existing file may be
#'   replaced.
#'
#' @returns Invisibly returns `path`.
#'
#' @export
write_document <- function(document, path, overwrite = FALSE) {
    stopifnot(inherits(document, "rapsimng_decide_document"))
    stopifnot(is.character(path), length(path) == 1L)
    stopifnot(is.logical(overwrite), length(overwrite) == 1L)

    if (file.exists(path) && !overwrite) {
        stop("File already exists. Set 'overwrite = TRUE' to overwrite.")
    }

    writeLines(document_lines(document), path)
    invisible(path)
}