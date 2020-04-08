#'@name synchr.classify
#'@title Categorizes cross-corrrelations to reflect synchrony style.
#'@description synchr.classify uses correlation coefficients across lag-1, lag0, and lag1 to categorize dyads as either "mutual adaptors", "leader-follower", or "leader-leader."
#'@param corr_data_list, list containing one coefficient per dyad at lag-1, lag0, and lag1.
#'@param threshold, Threshold for what constitutes a "significant" correlation coefficient. Default is 0.10.
#'@export
synchr.classify <- function(corr_data_list, threshold = 0.10) { #will want a different name then criterion
  data <- data.frame(matrix(unlist(corr_data_list), nrow=length(corr_data_list), byrow=T))
  classified <- data %>%
    setNames(., c("lag-1", "lag0", "lag1")) %>%
     mutate(Style = ifelse(
      #This classifies a dyad as "mutual adaptors" when the lag-1 coef is greater than or equal to the threshold value
      # AND the lag0 coef is less than or equal to -1 * the threshold value
      # AND the lag1 coef is greater than or qual to the threshold value - if these conditions are not met, the code
      # moves on
      (`lag-1` >= threshold & `lag0` <= (-1 * threshold) & `lag1` >= threshold), "mutual adaptors",
      ifelse(
        #This classifies a dyad as "leader-follower" when the lag1 coef is greater than or equal to the threshold value.
        # AND the lag0 is less than  the threshold value.
        # AND the lag-1 is less than the threshold value - if these conditions are not met, the code moves on
        (`lag1` >= threshold & `lag0` < threshold & `lag-1` < threshold), "leader-follower",
        ifelse(
          #This classifies a dyad as "leader-follower" when the lag-1 coef is greater than or equal to the threshold value.
          # AND the lag0 is less than or equal to the threshold value.
          # AND the lag1 is less than or equal to the threshold value.
          (`lag-1` >= threshold & `lag0` < threshold & `lag1` < threshold), "leader-follower",
          ifelse(
            #This classifies a dyad as "leader-leader" when the |lag-1 coef| is less than or equal to the threshold value.
            # AND the |lag0 coef| is less than or equal to the threshold value.
            # AND the |lag1 coef| is less than or equal to the threshold value.
            # Else, the code applies an "NA"
            (abs(`lag0`) < threshold & abs(`lag0`) < threshold & abs(`lag1`) < threshold), "leader-leader", NA
          )))))
  return(classified)
}
