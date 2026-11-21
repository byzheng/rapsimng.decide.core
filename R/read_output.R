
#' Read APSIMX output 
#' 
#' @param apsimx_file The path to the APSIMX output file (.apsimx)
#' @param report The name of the report to read from the APSIMX output (
#' default: "HarvestReport")
#' @param years Optional numeric vector of years to filter the output by (e.g., 1996:2025)
#' 
#' @return A data frame containing the APSIMX output data
#' @examples
#' apsimx_file <- system.file(
#'   "example", "cultivar.apsimx",
#'   package = "rapsimng.decide"
#' )
#' output <- read_output(apsimx_file, years = 2010:2011)
#' head(output)
#' @export
read_output <- function(
    apsimx_file, 
    report = "HarvestReport",
    years = NULL) {
    stopifnot(is.character(apsimx_file), length(apsimx_file) == 1, file.exists(apsimx_file))
    stopifnot(is.character(report), length(report) == 1)
    if (!is.null(years)) {
        stopifnot(is.numeric(years), length(years) >= 1)
    }
    df <-  rapsimng::read_report(apsimx_file, report)
    if (!tibble::has_name(df, "Clock.Today")) {
        stop("Expected 'Clock.Today' column not found in APSIM output. Please check the model configuration.")
    }
    if (!tibble::has_name(df, "SowingDate")) {
        stop("Expected 'SowingDate' column not found in APSIM output. Please check the model configuration.")
    }
    if (!tibble::has_name(df, "Cultivar")) {
        stop("Expected 'Cultivar' column not found in APSIM output. Please check the model configuration.")
    }
    if (!tibble::has_name(df, "Fertilisation")) {
        stop("Expected 'Fertilisation' column not found in APSIM output. Please check the model configuration.")
    }
    df <- df |>
        dplyr::mutate(
            Clock.Today = as.Date(.data$Clock.Today),
            SowingDOY = as.numeric(as.Date(paste(.data$SowingDate, "-2011", sep = ""), format = "%d-%b-%Y")) - 
        as.numeric(as.Date("2010-12-31")),
            Year = as.numeric(format(.data$Clock.Today, "%Y")),
        )
    if (!is.null(years)) {
        df <- df |> dplyr::filter(.data$Year %in% years)
    }
    df
}
