#'@name synchr.read
#'@title Custom function to read in data files into a list
#'@description synchr requires data to be in list format. This function allows the user to 
#'read in all individual data files from one folder into a list. Data file must be in .csv format and must include a header.
#'@param data, enter the file path to your data folder.

synchr.read <- function(data) {
  #creates a list of names for each file from the folder based on entered filepath
  file.names <- list.files(data)
  #this for loop reads in each data file based on filenames and saves to list
  dyad.data <- list()
  for (p in 1:length(file.names)) dyad.data[[p]] <- read.csv(paste(data, file.names[p], sep = "/"), header = TRUE)
  #this ouputs the list, although user should save function to an object
  return(dyad.data)
}
