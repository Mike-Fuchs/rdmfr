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
run_drift_model <- function(project_folder, input_data, executable_source, parameter = NULL, n_thread = NULL, save_file = NULL, return_results = T, keep_folder = T, quiet= F) {
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
  cl <- parallel::makeCluster(n_thread)
  doSNOW::registerDoSNOW(cl)

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
  output <- foreach(i_run = 1:n_run, .packages = c("dplyr", "lubridate"), .options.snow = opts) %dopar% {
    # define header
    header <- list(
      application_input = data.frame(tractor_speed = "[m/s]", boom_width = "[m]", boom_height = "[m]", nozzle_angle = "[\u00b0]", application_pres = "[kPa]", app_rate_mh = "[m\u00b3/h]", app_rate_mha = "[m\u00b3/ha]", app_rate_kgha = "[kg/ha]", sol_concentration = "[kg/m\u00b3]", AI_density = "[kg/m\u00b3]", AI_molar_mass = "[kg/mol]", AI_vapor_pressure = "[Pa]", swath_number = "[-]", field_length = "[m]"),
      droplet_spectrum_input = data.frame(droplet_size = "[m]", cum_fraction = "[-]"),
      environment_input = data.frame(temperature = "[\u00b0C]", humidity = "[-]", wind_speed = "[m/s]", wind_height = "[m]", wind_direction = "[\u00b0]", ambient_pressure = "[kPa]",sigma_horizontal = "[m]", sigma_vertical = "[m]", roughness_height = "[m]", A = "[m]", Hc = "[m]", LAI = "m\u00b2/m\u00b2"))

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
    sink(paste0(input_path, "/", names(run_input[1]), ".txt"))
    writeLines(paste0(c(sprintf(c(rep("%20s", ncol(run_input[[1]]))), names(run_input[[1]]))), collapse = ""), useBytes = F)
    writeLines(paste0(c(sprintf(c(rep("%20s", ncol(header[[1]]))), header[[1]])), collapse = ""), useBytes = F)
    for (i in 1:nrow(run_input[[1]])) {
      writeLines(paste0(c(sprintf(c(rep("%20.2f", 5),rep("%20.2e", 4),"%20.2f","%20.2e","%20.2e","%20.2f","%20.2f"), run_input[[1]][i, ])), collapse = ""), useBytes = F)
    }
    sink()

    # droplet_spectrum_input
    sink(paste0(input_path, "/", run_input$controle_input$dsd_file_name))
    writeLines(paste0(c(sprintf(c(rep("%20s", ncol(run_input[[2]]))), names(run_input[[2]]))), collapse = ""), useBytes = F)
    writeLines(paste0(c(sprintf(c(rep("%20s", ncol(header[[2]]))), header[[2]])), collapse = ""), useBytes = F)
    for (i in 1:nrow(run_input[[2]])) {
      writeLines(paste0(c(sprintf(c("%20.6e", "%20.10e"), run_input[[2]][i, ])), collapse = ""), useBytes = F)
    }
    sink()

    # environment_input
    sink(paste0(input_path, "/", names(run_input[3]), ".txt"))
    writeLines(paste0(c(sprintf(c(rep("%20s", ncol(run_input[[3]]))), names(run_input[[3]]))), collapse = ""), useBytes = F)
    writeLines(paste0(c(sprintf(c(rep("%20s", ncol(header[[3]]))), header[[3]])), collapse = ""), useBytes = F)
    for (i in 1:nrow(run_input[[3]])) {
      writeLines(paste0(c(sprintf(c(rep("%20.2f", 6),rep("%20.2e", 4),rep("%20.2f", 2)), run_input[[3]][i, ])), collapse = ""), useBytes = F)
    }
    sink()

    # controle_input
    sink(paste0(input_path, "/", names(run_input[4]), ".txt"))
    writeLines(paste0(c(sprintf(c(rep("%20s", 3)), c("mode", "dep_height", "max_dist"))), collapse = ""), useBytes = F)
    writeLines(paste0(c(sprintf(c("%20.0f","%20.3f","%20.0f"), run_input[[4]][1:3])), collapse = ""), useBytes = F)
    writeLines(paste0(c(sprintf(c("%-25s", "%-25s"), c("dsd_file_name:", run_input[[4]][4]))), collapse = ""), useBytes = F)
    writeLines(paste0(c(sprintf(c("%-25s", "%-25s"), c("landscape_file_name:", run_input[[4]][5]))), collapse = ""), useBytes = F)
    sink()

    # move executable
    split <- strsplit(executable_source, "/")[[1]]
    exe_name <- split[length(split)]
    invisible(file.copy(executable_source, paste0(project_path, "/", exe_name), overwrite = F))

    # run executable
    setwd(project_path)
    shell(shell(paste0(exe_name," > debug.txt")))

    #invisible(processx::run(exe_name, wd=project_path, error_on_status = FALSE))

    # read results
    if (run_input[[4]][1, 1] == 1) {
      # read drift curve
      header <- read.csv(paste0(output_path, "/drift_curve_output.txt"), sep = "", header = F, nrows = 1)
      data <- read.csv(paste0(output_path, "/drift_curve_output.txt"), sep = "", header = F, skip = 2)
      names(data) <- header
      return(data)
    } else {
      # read landscape drift
      map <- raster::raster(paste0(output_path, "/landscape_drift.asc"))
      return(map)
    }
  }
  # stopping the cluster
  parallel::stopCluster(cl)

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
    closeAllConnections()
    for (i_run in 1:n_run) {
      unlink(paste0(project_folder, "/", run_index[i_run]), recursive = T, force = T)
    }
  }

  # return results
  if (return_results) {
    return(result)
  }
}
