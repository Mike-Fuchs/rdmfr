#' Reads a landscape drift pattern
#'
#' @param path dummy
#'
#' @return dummy
#' @export
#'
#' @importFrom raster raster
#' @importFrom raster readAll
read_landscape_drift <- function(path) {
  map <- raster::raster(path)
  map <- raster::readAll(map)
  return(map)
}
