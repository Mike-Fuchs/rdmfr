#' An empty drift model input data set
#'
#' @return dummy
#' @export
drift_model_input <- function() {
  data <- list(
    application_input = data.frame(
      tractor_speed = NA,
      boom_width = NA,
      boom_height = NA,
      nozzle_angle = NA,
      application_pres = NA,
      app_rate_mh = NA,
      app_rate_mha = NA,
      app_rate_kgha = NA,
      sol_concentration = NA,
      AI_density = NA,
      AI_molar_mass = NA,
      AI_vapor_pressure = NA,
      swath_number = NA,
      field_length = NA
    ),
    droplet_spectrum_input = data.frame(
      droplet_size = NA,
      cum_fraction = NA
    ),
    environment_input = data.frame(
      temperature = NA,
      humidity = NA,
      wind_speed = NA,
      wind_height = NA,
      wind_direction = NA,
      ambient_pressure = NA,
      sigma_horizontal =NA,
      sigma_vertical = NA,
      roughness_height = NA,
      canopy_height = NA,
      LAI = NA
    ),
    control_input = data.frame(
      mode = NA,
      deposition_height = NA,
      max_dist = NA,
      field_count = NA,
      dsd_file_name = NA
    ),
    landscape_file_name = c(),
    landscape_file_paths = c()
  )
  return(data)
}
