###analysis###
#Jenny and Lan#

synchrA <- function (data, lag.max=1, window = 0, by = 0) {
  ##prem check
  # if(!lag.max == & !window)
  #   stop("lag or window should be non negative integers.")
  if(round(lag.max) != lag.max | lag.max < 0){
    stop(paste0("synchr ERROR: lag.max needs to be a non negative integer."))
  }
  if(round(window) != window | window < 0){
    stop(paste0("synchr ERROR: window needs to be a non negative integer."))
  }
 if(by > window){
   stop(paste0("synchr ERROR: the overlap value cannot be bigger than the window size."))
 }
  ##setup
  cc <-function(data){ccf(data[,2],data[,3], lag.max=lag.max, 
                                  na.action=na.pass, plot=F)}
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
  ##when a window size is specified by no overlap
  if(window > 0 & by ==0){
    prem_object <- rollapply(data, width=window,by = window,
                             FUN = cc,
                             by.column = FALSE, align = "right")
    #windowed_cc_list <- lapply(prem_object, function(x) t(as.data.frame(x)))
    #the original output had way too many unneeded info so only getting ones aka we wat aka cross corr values 
    windowed_cc_list <- list()
    window_number <- floor(length(data[,2])/window)
    for (p in 1:window_number){
      windowed_cc_list[[p]] <-prem_object[[p]]
    }
    windowed_cc <- do.call(rbind, windowed_cc_list)
    colnames(windowed_cc) <- sapply(c(-lag.max:lag.max), function(x) paste0("lag", x))
    cc_rowtimes <- seq(from = 1, to = length(data[,2]), by = window)
    cc_rowtimes <- cc_rowtimes[1:window_number]
    rownames(windowed_cc) <- sapply(cc_rowtimes, function(x)paste0(paste0(x), "-",paste0(window-1+x)))
    max_cc <- max(windowed_cc)
    max_cc_time <- colnames(windowed_cc)[as.vector(which(windowed_cc == max_cc, arr.ind=TRUE))[2]]
    avg_windowed_cc <- matrix(colMeans(windowed_cc), ncol = dim(windowed_cc)[2])
    colnames(avg_windowed_cc) <- colnames(windowed_cc)
    max_avg_cc <- max(avg_windowed_cc)
    max_avg_cc_time <- colnames(avg_windowed_cc)[avg_windowed_cc[1, ] == max_avg_cc]
    
    #final object
    object <- list(
        windowed_cc    = windowed_cc,
        avg_windowed_cc  = avg_windowed_cc,
        max_cc         = max_cc,
        max_avg_cc     = max_avg_cc,
        max_cc_time    = max_cc_time,
        max_avg_cc_time = max_avg_cc_time
      )
      cat("The maximum averaged windowed cross-correlatin is ", max_avg_cc,
          ", and it is at ", mac_avg_cc_time,". \n")
  }
  ##when there is overlapped window
  if(window > 0 & !by==0){
   
    prem_object <- rollapply(data, width=window,by = by,
                    FUN = cc,
                    by.column = FALSE, align = "right")
    #windowed_cc_list <- lapply(prem_object, function(x) t(as.data.frame(x)))
    window_number <- floor((length(data[,2])-window)/by)+1
    windowed_cc_list <- list()
    for (p in 1:window_number){
      windowed_cc_list[[p]] <-prem_object[[p]]
    }
    
    windowed_cc <- do.call(rbind, windowed_cc_list)
    colnames(windowed_cc) <- sapply(c(-lag.max:lag.max), function(x) paste0("lag", x))
    cc_rowtimes <- seq(from = 1, to = length(data[,2]), by = by)
    cc_rowtimes <- cc_rowtimes[1:window_number]
    rownames(windowed_cc) <- sapply(cc_rowtimes, function(x)paste0(paste0(x), "-",paste0(window-1+x)))
    max_cc <- max(windowed_cc)
    max_cc_time <- colnames(windowed_cc)[as.vector(which(windowed_cc == max_cc, arr.ind=TRUE))[2]]
    avg_windowed_cc <- matrix(colMeans(windowed_cc), ncol = dim(windowed_cc)[2])
    colnames(avg_windowed_cc) <- colnames(windowed_cc)
    max_avg_cc <- max(avg_windowed_cc)
    max_avg_cc_time <- colnames(avg_windowed_cc)[avg_windowed_cc[1, ] == max_avg_cc]
    
    #final object
    object <- list(
      windowed_cc    = windowed_cc,
      avg_windowed_cc  = avg_windowed_cc,
      max_cc         = max_cc,
      max_avg_cc     = max_avg_cc,
      max_cc_time    = max_cc_time,
      max_avg_cc_time = max_avg_cc_time
    )
    cat("The maximum averaged windowed cross-correlatin is ", max_avg_cc,
        ", and it is at ", mac_avg_cc_time,". \n")
  }
      return(object)
}