#'@name synchr.barplots
#'@title Plots correlation coefficients.
#'@description Plots correlation coefficients for each dyad across lag-1, lag0,lag1.
#'@param corr_data_list, list containing one coefficient per dyad at lag-1, lag0, and lag1.
#'@export
synchr.barplots <- function (corr_data_list) {

  corrplots <- list()

  for (p in 1:length(corr_data_list)) {
    data <- corr_data_list[[p]]
    meltdata <- melt(as.data.frame(data), id.vars = NULL)
    colnames(meltdata) <- c("Lag", "Pearson Correlation")
    plot <- ggplot(meltdata, aes(x= Lag, y = `Pearson Correlation`)) +
      scale_y_continuous(limits = c(-0.5, 0.5)) +
      geom_bar(stat = "identity", fill = "lightseagreen") +
      geom_hline(yintercept = 0) +
      theme_light() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
    corrplots[[p]] <- plot
  }

  Fig <- ggarrange(plotlist = corrplots)
  return(annotate_figure(Fig, bottom = "Lag", left = "Pearson Correlation Coefficient"))
}
