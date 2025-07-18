#' This function will run the drift model
#'
#' @param project_folder dummy
#' @param input_data dummy
#' @param executable_source dummy
#' @param save_file dummy
#' @param return_results dummy
#' @param keep_folder (optional) Logical. If \code{keep_folder = TRUE} folder of the model run will be keept.
#' @param tries dummy
#'
#' @return dummy
#' @export
#'
#' @importFrom foreach "%dopar%" foreach
#' @importFrom doSNOW registerDoSNOW
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom lubridate now
#' @importFrom raster raster
#' @importFrom utils read.csv
#' @importFrom processx run
#' @importFrom withr with_dir
run_drift_model_direct <- function(project_folder, input_data, raster, executable_source, save_file = NULL, return_results = T, keep_folder = T, tries = 5) {
  # define header
	header <- list(
		  application_input = data.frame(tractor_speed = "[m/s]", boom_width = "[m]", boom_height = "[m]", nozzle_angle = "[\u00b0]", application_pres = "[kPa]", app_rate_mh = "[m\u00b3/h]", app_rate_mha = "[m\u00b3/ha]", app_rate_kgha = "[kg/ha]", sol_concentration = "[kg/m\u00b3]", AI_density = "[kg/m\u00b3]", AI_molar_mass = "[kg/mol]", AI_vapor_pressure = "[Pa]", swath_number = "[-]", field_length = "[m]"),
		  droplet_spectrum_input = data.frame(droplet_size = "[m]", cum_fraction = "[-]"),
		  environment_input = data.frame(temperature = "[\u00b0C]", humidity = "[-]", wind_speed = "[m/s]", wind_height = "[m]", wind_direction = "[\u00b0]", ambient_pressure = "[kPa]",sigma_horizontal = "[m]", sigma_vertical = "[m]", roughness_height = "[m]", Hc = "[m]", LAI = "m\u00b2/m\u00b2"))

	# create main directory
	project_path <- paste0(project_folder, "/run")
	dir.create(project_path, showWarnings = FALSE)

	# create input folder
	input_path <- paste0(project_path, "/input")
	dir.create(input_path, showWarnings = FALSE)

	# create output folder
	output_path <- paste0(project_path, "/output")
	dir.create(output_path, showWarnings = FALSE)

	# application_input
	con_file <- file(paste0(input_path, "/", names(input_data[1]), ".txt"),open="wt")
	writeLines(paste0(c(sprintf(c(rep("%20s", ncol(input_data[[1]]))), names(input_data[[1]]))), collapse = ""), con = con_file, useBytes = F)
	writeLines(paste0(c(sprintf(c(rep("%20s", ncol(header[[1]]))), header[[1]])), collapse = ""), con = con_file, useBytes = F)
	writeLines(paste0(c(sprintf(c(rep("%20.2f", 5),rep("%20.2e", 4),"%20.2f","%20.2e","%20.2e","%20.2f","%20.2f"), input_data[[1]][1, ])), collapse = ""), con = con_file, useBytes = F)
	close(con_file)

	# droplet_spectrum_input
	con_file <- file(paste0(input_path, "/", input_data$control_input$dsd_file_name),open="wt")
	writeLines(paste0(c(sprintf(c(rep("%20s", ncol(input_data[[2]]))), names(input_data[[2]]))), collapse = ""), con = con_file, useBytes = F)
	writeLines(paste0(c(sprintf(c(rep("%20s", ncol(header[[2]]))), header[[2]])), collapse = ""), con = con_file, useBytes = F)
	for (i in 1:nrow(input_data[[2]])) {
	  writeLines(paste0(c(sprintf(c("%20.6e", "%20.10e"), input_data[[2]][i, ])), collapse = ""), con = con_file, useBytes = F)
	}
	close(con_file)

	# environment_input
	con_file <- file(paste0(input_path, "/", names(input_data[3]), ".txt"),open="wt")
	writeLines(paste0(c(sprintf(c(rep("%20s", ncol(input_data[[3]]))), names(input_data[[3]]))), collapse = ""), con = con_file, useBytes = F)
	writeLines(paste0(c(sprintf(c(rep("%20s", ncol(header[[3]]))), header[[3]])), collapse = ""), con = con_file, useBytes = F)
	writeLines(paste0(c(sprintf(c(rep("%20.2f", 6),rep("%20.2e", 3),rep("%20.2f", 2)), input_data[[3]][1, ])), collapse = ""), con = con_file, useBytes = F)
	close(con_file)

	# control_input
	con_file <- file(paste0(input_path, "/", names(input_data[4]), ".txt"),open="wt")
	writeLines(paste0(c(sprintf(c(rep("%20s", 4)), c("mode", "dep_height", "max_dist", "field_count"))), collapse = ""), con = con_file, useBytes = F)
	writeLines(paste0(c(sprintf(c("%20.0f","%20.3f","%20.0f","%20.0f"), input_data[[4]][1:4])), collapse = ""), con = con_file, useBytes = F)
	writeLines(paste0(c(sprintf(c("%-25s", "%-25s"), c("dsd_file_name:", input_data[[4]][5]))), collapse = ""), con = con_file, useBytes = F)
	writeLines(paste0(c(sprintf(c("%-25s", rep("%-25s",input_data[[4]][4])), c("landscape_file_name:", input_data[[5]]))), collapse = ""), con = con_file, useBytes = F)
	close(con_file)

	# move raster file
	if (input_data[[4]][1] == 0){
	  writeRaster(raster,paste0(project_path,"/input/",input_data[[5]][1]))
	}

	# move executable
	exe_name <- basename(executable_source)
	exe_path_full <- file.path(project_path, exe_name)
	success <- file.copy(executable_source, exe_path_full, overwrite = TRUE)
	if (!success) stop("Failed to copy executable to run directory: ", exe_target)
	
	# run executable
	flag <- T
	n_try <- 1
	while(all(flag,n_try <= tries)){
	  # run model
	  Sys.sleep(runif(1,min = 0,max = 10))
	  withr::with_dir(project_path, {
		system2(exe_path_full, 
               stdout = file.path(project_path, "debug.txt"), 
               stderr = file.path(project_path, "debug.txt"),
               wait = TRUE)
	  })  
	  # check if results exist
	  if (input_data[[4]][1] == 1) {
		# check if drift_curve_output.txt exists
		flag <- !file.exists(paste0(output_path, "/drift_curve_output.txt"))
	  } else {
		# check if landscape_drift.asc exists
		flag <- !file.exists(paste0(output_path, "/landscape_drift.asc"))
	  }
	  # itterate n_try
	  n_try <- n_try + 1
	}

	# read results
	if (input_data[[4]][1] == 1) {
	  # read drift curve
	  header <- read.csv(paste0(output_path, "/drift_curve_output.txt"), sep = "", header = F, nrows = 1)
	  data <- read.csv(paste0(output_path, "/drift_curve_output.txt"), sep = "", header = F, skip = 2)
	  names(data) <- header
	  output <- data
	} else {
	  # read landscape drift
	  map <- raster::raster(paste0(output_path, "/landscape_drift.asc"))
	  map <- raster::readAll(map)
	  output <- map
	}

  # restructure results
  result <- list(output=output)

  # save results to file
  if (!is.null(save_file)) {
    rlist::list.save(result, paste0(project_folder, "/", save_file, ".rdata"))
  }

  # keep folder
  if (!keep_folder) {
    unlink(paste0(project_folder, "/run"), recursive = T, force = T)
  }

  # return results
  if (return_results) {
    return(result)
  }
}
