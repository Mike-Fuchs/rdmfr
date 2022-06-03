#' Read input files
#'
#' @param input_folder Folder from which the input files should be laoded
#'
#' @return dummy
#' @export
read_input_files <- function(input_folder) {
  data <- rdmfr::drift_model_input()
  # read application_input
  data$application_input[] <- read.csv(paste0(input_folder, "/", names(data[1]), ".txt"), header = F, skip = 2, nrows = 1, sep = "")
  # read environment_input
  data$environment_input[] <- read.csv(paste0(input_folder, "/", names(data[3]), ".txt"), header = F, skip = 2, nrows = 1, sep = "")
  # read application_input
  data$gaussian_input[] <- read.csv(paste0(input_folder, "/", names(data[4]), ".txt"), header = F, skip = 2, nrows = 1, sep = "")
  # read controle_input
  tmp <- read.csv(paste0(input_folder, "/", names(data[5]), ".txt"), header = T, sep = "")
  data$controle_input[1:3] <- as.numeric(tmp[1, 1:3])
  data$controle_input[4] <- tmp[2, 2]
  data$controle_input[5] <- tmp[3, 2]
  # read droplet_spectrum_input
  tmp <- read.csv(paste0(input_folder, "/", data$controle_input[4]), header = F, skip = 2, sep = "")
  names(tmp) <- names(data$droplet_spectrum_input)
  data$droplet_spectrum_input <- tmp
  # return data
  return(data)
}
