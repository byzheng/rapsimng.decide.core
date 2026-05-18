test_that("evaluate_decision returns a report for valid minimal input", {
    mock_report <- list(
        meta = list(decision = "cultivar"),
        metrics = list(score = 1),
        tables = list(summary = data.frame(value = 1)),
        figures = list()
    )

    local_mocked_bindings(
        get_decision_evaluator = function(decision) {
            expect_identical(decision, "cultivar")

            function(data, context, criteria, options, ...) {
                expect_true(is.data.frame(data))
                expect_identical(context, list())
                expect_identical(criteria, list())
                expect_identical(options, list())

                mock_report
            }
        },
        .package = "rapsimng.decide"
    )

    result <- evaluate_decision(
        data = data.frame(yield = 4.2),
        decision = "cultivar"
    )

    expect_named(result, c("meta", "metrics", "tables", "figures"))
    expect_s3_class(result, "rapsimng_decide_report")
})

test_that("evaluate_decision errors when data is not a data frame", {
    expect_error(
        evaluate_decision(data = list(yield = 4.2), decision = "cultivar"),
        "`data` must be a data.frame or tibble"
    )
})

test_that("evaluate_decision errors when decision is unknown", {
    expect_error(
        evaluate_decision(data = data.frame(yield = 4.2), decision = "unknown"),
        "Unknown decision type: unknown"
    )
})

test_that("evaluate_decision enforces the standard report structure", {
    local_mocked_bindings(
        get_decision_evaluator = function(decision) {
            expect_identical(decision, "cultivar")

            function(data, context, criteria, options, ...) {
                list(
                    meta = list(),
                    metrics = list(),
                    tables = list()
                )
            }
        },
        .package = "rapsimng.decide"
    )

    expect_error(
        evaluate_decision(data = data.frame(yield = 4.2), decision = "cultivar"),
        "Decision result must contain: meta, metrics, tables, figures"
    )
})

test_that("evaluate_decision assigns the report class to returned output", {
    mock_report <- list(
        meta = list(),
        metrics = list(),
        tables = list(),
        figures = list(plot = NULL)
    )

    local_mocked_bindings(
        get_decision_evaluator = function(decision) {
            expect_identical(decision, "cultivar")

            function(data, context, criteria, options, ...) {
                mock_report
            }
        },
        .package = "rapsimng.decide"
    )

    result <- evaluate_decision(
        data = data.frame(yield = 4.2),
        decision = "cultivar"
    )

    expect_s3_class(result, "rapsimng_decide_report")
})