test_that("document_lines includes prefix and non-null sections", {
    tmp_dir <- tempfile(pattern = "test-document-lines-")
    dir.create(tmp_dir)
    on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

	document <- structure(
		list(
			prefix = list(body = c("Title", "Summary")),
			sections = list(
				list(body = c("Section 1", "Line 2")),
				NULL,
				list(body = "Section 2")
			)
		),
		class = "rapsimng_decide_report"
	)

	expect_equal(
		document_lines(document),
		c(
			"Title", "Summary", "",
			"Section 1", "Line 2", "",
			"Section 2", ""
		)
	)
})

test_that("write_document refuses to overwrite unless requested", {
    tmp_dir <- tempfile(pattern = "test-write-document-overwrite-")
    dir.create(tmp_dir)
    on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

    document <- structure(
        list(
            prefix = NULL,
            sections = list(list(body = "Only section"))
        ),
        class = "rapsimng_decide_report"
    )
    path <- file.path(tmp_dir, "document.txt")
    writeLines("existing", path)

    expect_error(
        write_document(document, path),
        "File already exists. Set 'overwrite = TRUE' to overwrite."
    )
})

test_that("write_document writes lines and returns the output path invisibly", {
    tmp_dir <- tempfile(pattern = "test-write-document-output-")
    dir.create(tmp_dir)
    on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

    document <- structure(
        list(
            prefix = list(body = "Intro"),
            sections = list(list(body = c("Body", "Tail")))
        ),
        class = "rapsimng_decide_report"
    )
    path <- file.path(tmp_dir, "document.txt")

    returned_path <- write_document(document, path, overwrite = TRUE)

    expect_identical(returned_path, path)
    expect_equal(readLines(path), c("Intro", "", "Body", "Tail", ""))
})
