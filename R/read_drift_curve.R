#' Loads a drift curve
#'
#' @param path File path from which the drift curve should be loaded.
#'
#' @return dummy
#' @export
#'
#' @importFrom utils read.csv
read_drift_curve <- function(path) {
  header <- read.csv(paste0(path, "/drift_curve_output.txt"), sep = "", header = F, nrows = 1)
  data <- read.csv(paste0(path, "/drift_curve_output.txt"), sep = "", header = F, skip = 2)
  names(data) <- header
  return(data)
}
