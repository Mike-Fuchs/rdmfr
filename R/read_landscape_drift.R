#' Reads a landscape drift pattern
#'
#' @param path dummy
#'
#' @return dummy
#' @export
#'
#' @importFrom raster raster
read_landscape_drift <- function(path) {
  map <- raster::raster(path)
  return(map)
}
