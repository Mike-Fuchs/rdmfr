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
      AI_diffusion_coef = NA,
      AI_diffusion_temp = NA,
      swath_number = NA
    ),
    droplet_spectrum_input = data.frame(
      droplet_size = NA,
      cum_fraction = NA
    ),
    environment_input = data.frame(
      temperature = NA,
      humidity = NA,
      wind_speed = NA,
      wind_direction = NA,
      ambient_pressure = NA
    ),
    gaussian_input = data.frame(
      k_horizontal = NA,
      k_vertical = NA,
      deposition_height = NA,
      max_dist = NA
    ),
    controle_input = data.frame(
      code1 = NA,
      drop_out = NA,
      no_evap = NA,
      dsd_file_name = NA,
      landscape_file_name = NA
    )
  )
  return(data)
}
