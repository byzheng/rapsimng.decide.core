test_that("read_output reads bundled APSIMX example", {
    apsimx_file <- system.file("example", "cultivar.apsimx", package = "rapsimng.decide")

    expect_true(nzchar(apsimx_file))
    expect_true(file.exists(apsimx_file))

    result <- read_output(apsimx_file)

    expect_s3_class(result, "data.frame")
    expect_gt(nrow(result), 0)
    expect_true(all(c(
        "Clock.Today",
        "SowingDate",
        "Cultivar",
        "Fertilisation",
        "SowingDOY",
        "Year"
    ) %in% names(result)))
    expect_s3_class(result$Clock.Today, "Date")
    expect_type(result$SowingDOY, "double")
    expect_type(result$Year, "double")
})
