###analysis###
#Jenny and Lan#

synchrA <- function (data, lag.max=1, window = 0) {
  ##prem check
  # if(!lag.max == & !window)
  #   stop("lag or window should be non negative integers.")
  if(round(lag.max) != lag.max | lag.max < 0){
    stop(paste0("synchr ERROR: lag.max needs to be a non negative integer."))
  }
  if(round(window) != window | window < 0){
    stop(paste0("synchr ERROR: window needs to be a non negative integer."))
  }
    
  ##when no window is specified
  if(window == 0){
    ecvalues <- ccf(data[,2],data[,3], lag.max=lag.max, 
                  na.action=na.pass, plot=F)
    all_lag_cc <- t(as.data.frame(ecvalues$acf))
    colnames(all_lag_cc) <- c(-lag.max:lag.max)
    max_cc<-max(ecvalues$acf, na.rm=TRUE)
    max_cc_time <- colnames(all_lag_cc)[all_lag_cc[1, ] == max_cc]
    ##final object
    object <- list(
     all_lag_cc = ecvalues,
     max_cc     = max_cc,
     max_cc_time = max_cc_time
    )
    cat("The maximum cross-correlatin is ", max_cc, 
        ", and it is at lag", max_cc_time,". \n" )
  }
  ##when a window width is specified
  if(window > 0){
    prem_object <- rollccf(DATA = data.frame(Q = data[,2], 
                                             P = data[,3]),
                           width = window,
                           by = window,
                           lags = c(-lag.max: lag.max),
                           na.action = na.contiguous,
                           na.max.fraction = 1/3)
    # avg_window_cc <- matrix(NA, nrow = 1, ncol = length(c(-lag.max,lag.max)))
    # colnames(avg_window_cc) <- colnames(prem_object$rolls[[1]])
    # sapply(test$rolls[[1]], mean, na.rm = T)
    avg_window_cc <- t(as.data.frame(sapply(prem_object$rolls[[1]], mean, na.rm = T)))
    #rownames(avg_window_cc) <- "avg_cc"
    max_cc <- max(avg_window_cc[1,], na.rm=TRUE)
    max_cc_time <- colnames(avg_window_cc)[avg_window_cc[1, ] == max_cc]
    ##final object
    object <- list(
      windowed_cc    = prem_object$rolls[[1]],
      avg_window_cc  = avg_window_cc,
      max_cc         = max_cc,
      max_cc_time    = max_cc_time
    )
    cat("The maximum averaged windowed cross-correlatin is ", max_cc, 
        ", and it is at ", max_cc_time,". \n")
  }
      return(object)
}
