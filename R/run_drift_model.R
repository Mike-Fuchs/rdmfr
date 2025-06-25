#' This function will run the drift model
#'
#' @param project_folder dummy
#' @param input_data dummy
#' @param executable_source dummy
#' @param parameter dummy
#' @param n_thread dummy
#' @param save_file dummy
#' @param return_results dummy
#' @param keep_folder (optional) Logical. If \code{keep_folder = TRUE} folder of the model run will be keept.
#' @param quiet dummy
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
run_drift_model <- function(project_folder, input_data, executable_source, parameter = NULL, n_thread = NULL, save_file = NULL, return_results = T, keep_folder = T, quiet= F, tries = 5, dopar = F) {
  # checking inputs


  # parameters & run_index
  if (!is.null(parameter)) {
    n_run <- nrow(parameter)
    tmp1 <- unname(t(parameter))
    tmp2 <- matrix(ncol = 2, nrow = ncol(parameter))
    for (ii in 1:nrow(tmp2)) {
      tmp2[ii, ] <- unlist(strsplit(names(parameter)[ii], "\\|"))
    }
    parameter <- as.data.frame(cbind(tmp2, tmp1))
    run_index <- paste0("run_", c(1:n_run))
    names(parameter) <- c("parameter name", "change type", run_index)
  } else {
    n_run <- 1
    run_index <- paste0("run_", c(1:n_run))
  }

  # setup cluster
  if (!is.null(n_thread)) {
    n_thread <- min(max(n_run, 1), max(n_thread, 1), parallel::detectCores())
  } else {
    n_thread <- min(max(n_run, 1), parallel::detectCores())
  }
  if(dopar == T){
    cl <- parallel::makeCluster(n_thread)
    doSNOW::registerDoSNOW(cl)
  }
  # user massage
  if(!quiet) {
    cat("Performing", n_run, "simulation"%&%plural(n_run),"on", n_thread,
        "core"%&%plural(n_thread)%&%":", "\n")
    t0 <- lubridate::now()
    progress <- function(n){
      display_progress(n, n_run, t0, "Simulation")
    }
    opts <- list(progress = progress)
  } else {
    opts <- list()
  }

  # main loop
  if(dopar == T){
	  output <- foreach(i_run = 1:n_run, .packages = c("dplyr", "lubridate"), .options.snow = opts) %dopar% {
		# define header
		header <- list(
		  application_input = data.frame(tractor_speed = "[m/s]", boom_width = "[m]", boom_height = "[m]", nozzle_angle = "[\u00b0]", application_pres = "[kPa]", app_rate_mh = "[m\u00b3/h]", app_rate_mha = "[m\u00b3/ha]", app_rate_kgha = "[kg/ha]", sol_concentration = "[kg/m\u00b3]", AI_density = "[kg/m\u00b3]", AI_molar_mass = "[kg/mol]", AI_vapor_pressure = "[Pa]", swath_number = "[-]", field_length = "[m]"),
		  droplet_spectrum_input = data.frame(droplet_size = "[m]", cum_fraction = "[-]"),
		  environment_input = data.frame(temperature = "[\u00b0C]", humidity = "[-]", wind_speed = "[m/s]", wind_height = "[m]", wind_direction = "[\u00b0]", ambient_pressure = "[kPa]",sigma_horizontal = "[m]", sigma_vertical = "[m]", roughness_height = "[m]", Hc = "[m]", LAI = "m\u00b2/m\u00b2"))

		# input manipulation
		if (!is.null(parameter)) {
		  run_input <- input_data
		  for (i_par in 1:nrow(parameter)) {
			# find position
			for (list_i in 1:4) {
			  if (parameter[i_par, 1] %in% names(run_input[[list_i]])) {
				break()
			  }
			}
			table_i <- grep(names(run_input[[list_i]]), pattern = parameter[i_par, 1])
			# change value
			if (parameter[i_par, 2] == "pctchg") {
			  run_input[[list_i]][, table_i] <- run_input[[list_i]][, table_i] + (run_input[[list_i]][, table_i] * (as.numeric(parameter[i_par, i_run + 2]) / 100))
			}
			if (parameter[i_par, 2] == "absval") {
			  run_input[[list_i]][, table_i] <- as.numeric(parameter[i_par, i_run + 2])
			}
		  }
		} else {
		  run_input <- input_data
		}

		# create main directory
		project_path <- paste0(project_folder, "/", run_index[i_run])
		dir.create(project_path, showWarnings = FALSE)

		# create input folder
		input_path <- paste0(project_path, "/input")
		dir.create(input_path, showWarnings = FALSE)

		# create output folder
		output_path <- paste0(project_path, "/output")
		dir.create(output_path, showWarnings = FALSE)

		# application_input
		con_file <- file(paste0(input_path, "/", names(run_input[1]), ".txt"),open="wt")
		writeLines(paste0(c(sprintf(c(rep("%20s", ncol(run_input[[1]]))), names(run_input[[1]]))), collapse = ""), con = con_file, useBytes = F)
		writeLines(paste0(c(sprintf(c(rep("%20s", ncol(header[[1]]))), header[[1]])), collapse = ""), con = con_file, useBytes = F)
		for (i in 1:nrow(run_input[[1]])) {
		  writeLines(paste0(c(sprintf(c(rep("%20.2f", 5),rep("%20.2e", 4),"%20.2f","%20.2e","%20.2e","%20.2f","%20.2f"), run_input[[1]][i, ])), collapse = ""), con = con_file, useBytes = F)
		}
		close(con_file)

		# droplet_spectrum_input
		con_file <- file(paste0(input_path, "/", run_input$control_input$dsd_file_name),open="wt")
		writeLines(paste0(c(sprintf(c(rep("%20s", ncol(run_input[[2]]))), names(run_input[[2]]))), collapse = ""), con = con_file, useBytes = F)
		writeLines(paste0(c(sprintf(c(rep("%20s", ncol(header[[2]]))), header[[2]])), collapse = ""), con = con_file, useBytes = F)
		for (i in 1:nrow(run_input[[2]])) {
		  writeLines(paste0(c(sprintf(c("%20.6e", "%20.10e"), run_input[[2]][i, ])), collapse = ""), con = con_file, useBytes = F)
		}
		close(con_file)

		# environment_input
		con_file <- file(paste0(input_path, "/", names(run_input[3]), ".txt"),open="wt")
		writeLines(paste0(c(sprintf(c(rep("%20s", ncol(run_input[[3]]))), names(run_input[[3]]))), collapse = ""), con = con_file, useBytes = F)
		writeLines(paste0(c(sprintf(c(rep("%20s", ncol(header[[3]]))), header[[3]])), collapse = ""), con = con_file, useBytes = F)
		for (i in 1:nrow(run_input[[3]])) {
		  writeLines(paste0(c(sprintf(c(rep("%20.2f", 6),rep("%20.2e", 3),rep("%20.2f", 2)), run_input[[3]][i, ])), collapse = ""), con = con_file, useBytes = F)
		}
		close(con_file)

		# control_input
		con_file <- file(paste0(input_path, "/", names(run_input[4]), ".txt"),open="wt")
		writeLines(paste0(c(sprintf(c(rep("%20s", 4)), c("mode", "dep_height", "max_dist", "field_count"))), collapse = ""), con = con_file, useBytes = F)
		writeLines(paste0(c(sprintf(c("%20.0f","%20.3f","%20.0f","%20.0f"), run_input[[4]][1:4])), collapse = ""), con = con_file, useBytes = F)
		writeLines(paste0(c(sprintf(c("%-25s", "%-25s"), c("dsd_file_name:", run_input[[4]][5]))), collapse = ""), con = con_file, useBytes = F)
		writeLines(paste0(c(sprintf(c("%-25s", rep("%-25s",run_input[[4]][4])), c("landscape_file_name:", run_input[[5]]))), collapse = ""), con = con_file, useBytes = F)
		close(con_file)

		# move raster file
		if (run_input[[4]][1] == 0){
		  for(i in 1:as.numeric(run_input[[4]][4])){
			split <- strsplit(run_input[[6]][i], "/")[[1]]
			file_name <- split[length(split)]
			invisible(file.copy(run_input[[6]][i], paste0(project_path, "/input/", file_name), overwrite = F))
		  }
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
		if (run_input[[4]][1] == 1) {
		  # read drift curve
		  header <- read.csv(paste0(output_path, "/drift_curve_output.txt"), sep = "", header = F, nrows = 1)
		  data <- read.csv(paste0(output_path, "/drift_curve_output.txt"), sep = "", header = F, skip = 2)
		  names(data) <- header
		  return(data)
		} else {
		  # read landscape drift
		  map <- raster::raster(paste0(output_path, "/landscape_drift.asc"))
		  map <- raster::readAll(map)
		  return(map)
		}
	  }
	  # stopping the cluster
	  parallel::stopCluster(cl)
  }else{
      for(i_run in 1:n_run){
		# define header
		header <- list(
		  application_input = data.frame(tractor_speed = "[m/s]", boom_width = "[m]", boom_height = "[m]", nozzle_angle = "[\u00b0]", application_pres = "[kPa]", app_rate_mh = "[m\u00b3/h]", app_rate_mha = "[m\u00b3/ha]", app_rate_kgha = "[kg/ha]", sol_concentration = "[kg/m\u00b3]", AI_density = "[kg/m\u00b3]", AI_molar_mass = "[kg/mol]", AI_vapor_pressure = "[Pa]", swath_number = "[-]", field_length = "[m]"),
		  droplet_spectrum_input = data.frame(droplet_size = "[m]", cum_fraction = "[-]"),
		  environment_input = data.frame(temperature = "[\u00b0C]", humidity = "[-]", wind_speed = "[m/s]", wind_height = "[m]", wind_direction = "[\u00b0]", ambient_pressure = "[kPa]",sigma_horizontal = "[m]", sigma_vertical = "[m]", roughness_height = "[m]", Hc = "[m]", LAI = "m\u00b2/m\u00b2"))

		# input manipulation
		if (!is.null(parameter)) {
		  run_input <- input_data
		  for (i_par in 1:nrow(parameter)) {
			# find position
			for (list_i in 1:4) {
			  if (parameter[i_par, 1] %in% names(run_input[[list_i]])) {
				break()
			  }
			}
			table_i <- grep(names(run_input[[list_i]]), pattern = parameter[i_par, 1])
			# change value
			if (parameter[i_par, 2] == "pctchg") {
			  run_input[[list_i]][, table_i] <- run_input[[list_i]][, table_i] + (run_input[[list_i]][, table_i] * (as.numeric(parameter[i_par, i_run + 2]) / 100))
			}
			if (parameter[i_par, 2] == "absval") {
			  run_input[[list_i]][, table_i] <- as.numeric(parameter[i_par, i_run + 2])
			}
		  }
		} else {
		  run_input <- input_data
		}

		# create main directory
		project_path <- paste0(project_folder, "/", run_index[i_run])
		dir.create(project_path, showWarnings = FALSE)

		# create input folder
		input_path <- paste0(project_path, "/input")
		dir.create(input_path, showWarnings = FALSE)

		# create output folder
		output_path <- paste0(project_path, "/output")
		dir.create(output_path, showWarnings = FALSE)

		# application_input
		con_file <- file(paste0(input_path, "/", names(run_input[1]), ".txt"),open="wt")
		writeLines(paste0(c(sprintf(c(rep("%20s", ncol(run_input[[1]]))), names(run_input[[1]]))), collapse = ""), con = con_file, useBytes = F)
		writeLines(paste0(c(sprintf(c(rep("%20s", ncol(header[[1]]))), header[[1]])), collapse = ""), con = con_file, useBytes = F)
		for (i in 1:nrow(run_input[[1]])) {
		  writeLines(paste0(c(sprintf(c(rep("%20.2f", 5),rep("%20.2e", 4),"%20.2f","%20.2e","%20.2e","%20.2f","%20.2f"), run_input[[1]][i, ])), collapse = ""), con = con_file, useBytes = F)
		}
		close(con_file)

		# droplet_spectrum_input
		con_file <- file(paste0(input_path, "/", run_input$control_input$dsd_file_name),open="wt")
		writeLines(paste0(c(sprintf(c(rep("%20s", ncol(run_input[[2]]))), names(run_input[[2]]))), collapse = ""), con = con_file, useBytes = F)
		writeLines(paste0(c(sprintf(c(rep("%20s", ncol(header[[2]]))), header[[2]])), collapse = ""), con = con_file, useBytes = F)
		for (i in 1:nrow(run_input[[2]])) {
		  writeLines(paste0(c(sprintf(c("%20.6e", "%20.10e"), run_input[[2]][i, ])), collapse = ""), con = con_file, useBytes = F)
		}
		close(con_file)

		# environment_input
		con_file <- file(paste0(input_path, "/", names(run_input[3]), ".txt"),open="wt")
		writeLines(paste0(c(sprintf(c(rep("%20s", ncol(run_input[[3]]))), names(run_input[[3]]))), collapse = ""), con = con_file, useBytes = F)
		writeLines(paste0(c(sprintf(c(rep("%20s", ncol(header[[3]]))), header[[3]])), collapse = ""), con = con_file, useBytes = F)
		for (i in 1:nrow(run_input[[3]])) {
		  writeLines(paste0(c(sprintf(c(rep("%20.2f", 6),rep("%20.2e", 3),rep("%20.2f", 2)), run_input[[3]][i, ])), collapse = ""), con = con_file, useBytes = F)
		}
		close(con_file)

		# control_input
		con_file <- file(paste0(input_path, "/", names(run_input[4]), ".txt"),open="wt")
		writeLines(paste0(c(sprintf(c(rep("%20s", 4)), c("mode", "dep_height", "max_dist", "field_count"))), collapse = ""), con = con_file, useBytes = F)
		writeLines(paste0(c(sprintf(c("%20.0f","%20.3f","%20.0f","%20.0f"), run_input[[4]][1:4])), collapse = ""), con = con_file, useBytes = F)
		writeLines(paste0(c(sprintf(c("%-25s", "%-25s"), c("dsd_file_name:", run_input[[4]][5]))), collapse = ""), con = con_file, useBytes = F)
		writeLines(paste0(c(sprintf(c("%-25s", rep("%-25s",run_input[[4]][4])), c("landscape_file_name:", run_input[[5]]))), collapse = ""), con = con_file, useBytes = F)
		close(con_file)

		# move raster file
		if (run_input[[4]][1] == 0){
		  for(i in 1:as.numeric(run_input[[4]][4])){
			split <- strsplit(run_input[[6]][i], "/")[[1]]
			file_name <- split[length(split)]
			invisible(file.copy(run_input[[6]][i], paste0(project_path, "/input/", file_name), overwrite = F))
		  }
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
		if (run_input[[4]][1] == 1) {
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
	  }
  }
  ## Show total runs and elapsed time in console if not quiet
  if(!quiet) {
    finish_progress(n_run, t0, "simulation")
    ## Delete the time stamp t0 created for the progress estimation
    rm(t0)
  }

  # restructure results
  if (!is.null(parameter)) {
    result <- list(
      parameters = parameter,
      output = output
    )
    names(result$output) <- run_index
  } else {
    result <- list(output=output)
  }

  # save results to file
  if (!is.null(save_file)) {
    rlist::list.save(result, paste0(project_folder, "/", save_file, ".rdata"))
  }

  # keep folder
  if (!keep_folder) {
    for (i_run in 1:n_run) {
      unlink(paste0(project_folder, "/", run_index[i_run]), recursive = T, force = T)
    }
  }

  # return results
  if (return_results) {
    return(result)
  }
}
