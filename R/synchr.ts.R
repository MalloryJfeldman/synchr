#'@name synchr.ts
#'@title Dyadic Time Series Plots.
#'@description Generates simple time series plot for each element in list.
#'@param raw_data_list, list containing dyadic data.
#'@export
synchr.ts <- function (raw_data_list) {
  par(mfrow = c(3,2))
  for (p in 1:length(raw_data_list)) {
    data <- raw_data_list[[p]]
    data <- data[,2:3]
    plot.ts(data, col = 3:4, plot.type = "single", main = paste("Dyad", p, sep = " "))
  }
}
