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
  # read controle_input
  lines <- readLines(paste0(input_folder, "/", names(data[4]), ".txt"))
  data$controle_input[1:4] <- str_split(lines[2],pattern = " ")[[1]]%>%
    subset(., .!="")
  data$controle_input[5] <- str_split(lines[3],pattern = " ")[[1]]%>%
    subset(., .!="")%>%
    .[2]
  data$landscape_file_name <- str_split(lines[4],pattern = " ")[[1]]%>%
    subset(., .!="")%>%
    .[-1]
  # read droplet_spectrum_input
  tmp <- read.csv(paste0(input_folder, "/", data$controle_input[5]), header = F, skip = 2, sep = "")
  names(tmp) <- names(data$droplet_spectrum_input)
  data$droplet_spectrum_input <- tmp
  # return data
  return(data)
}
