#' @title Drift Model Run Loader
#'
#' @description This function will load the results of \code{\link{run_drift_model}}
#'
#' @param project_folder The path where the results are saved.
#' @param save_file The name of the save file.
#'
#' @return dummy
#' @export
#'
#' @importFrom rlist list.load
load_drift_model_run <- function(project_folder, save_file) {
  data <- rlist::list.load(paste0(project_folder, "/", save_file, ".rdata"))
  return(data)
}
