

filter_e4data_datetime <- function(data, start, end){
  
  data$IBI$datetime <- lubridate::force_tz(data$IBI$DateTime, "UTC")
  data$EDA$datetime <- lubridate::force_tz(data$EDA$DateTime, "UTC")
  data$ACC$datetime <- lubridate::force_tz(data$ACC$DateTime, "UTC")
  data$TEMP$datetime <- lubridate::force_tz(data$TEMP$DateTime, "UTC")
  data$HR$datetime <- lubridate::force_tz(data$HR$DateTime, "UTC")
  
  data$IBI <- dplyr::filter(data$IBI, 
                     datetime >= start,
                     datetime <= end)
  data$EDA <- dplyr::filter(data$EDA, 
                     datetime >= start,
                     datetime <= end)
  data$ACC <- dplyr::filter(data$ACC, 
                     datetime >= start,
                     datetime <= end)
  data$TEMP <- dplyr::filter(data$TEMP, 
                      datetime >= start,
                      datetime <= end)
  data$HR <- dplyr::filter(data$HR, 
                    datetime >= start,
                    datetime <= end)
  
return(data)
}



#' calculate_heartrate_params
#' @description Function to calculate certain HR params
#' @export
calculate_heartrate_params <- function(ibi, eda){
  
    
  #Make a listcontainer for use in RHRV
  hrv_data <- CreateHRVData()
  
  #Sets verbose mode on or off, verbose is a boolean component of the data structure HRVData that allows to specify 
  #if all the functions return additional information
  hrv_data <- SetVerbose(hrv_data, TRUE)
  
  #Create vector with summary of the time between beats
  hr_beats_diff <- summary(diff(ibi$seconds, lag = 1))
  names(hr_beats_diff) <- c("beats_min_diff",
                            "beats_q1_diff",
                            "beats_mdn_diff",
                            "beats_mean_diff",
                            "beats_q3_diff",
                            "beats_max_diff")
  
  #Create a fraction index for the amount of beats detected relative to session duration 
  ses_dur_sec <- length(eda$EDA)/4
  hrv_art_index <- length(ibi$IBI)/(length(eda$EDA)/4)
  
  #Create a file for HRV analysis by mimicking an artefact free file 
  hrv_data <- LoadBeatVector(hrv_data, cumsum(ibi$IBI), scale = 1)
  
  #plot(hrv_data$Beat$Time)                                #Plot raw beats
  hrv_data <- BuildNIHR(hrv_data)                          #Build non-interpolated data
  #PlotNIHR(hrv_data)                                      #Plot non-interpolated data
  hrv_data <- FilterNIHR(hrv_data)                         #Filter non-interpolated data
  #PlotNIHR(hrv_data)                                      #Plot non-interpolated filtered data                                   
  hrv_data_bak <- hrv_data                                   #Make a new list to compare the filtered data
  hrv_data <- FilterNIHR(hrv_data, long=50, 
                        last=10, minbpm=25, maxbpm=180)
  #PlotNIHR(hrv_data)                                      #Plot the conditional filtered data
  hrv_data <- hrv_data_bak                                   #Use the filtered data in the original list
  hrv_data <- InterpolateNIHR (hrv_data, freqhr = 4)       #Interpolate filtered data
  #PlotHR(hrv_data)                                        #Plot filtered HR of interpolated data
  hrv_data <- CreateTimeAnalysis(hrv_data, size = 100, 
                                interval = 7.8125)        #Create Time analysis
  hrv_data <- CreateFreqAnalysis(hrv_data)                 #Create Frequency Analysis 
  hrv_data <- CalculatePowerBand( hrv_data , 
                                 indexFreqAnalysis= 1, 
                                 size = 100, shift = 10, 
                                 sizesp = 2048, 
                                 type = "fourier", 
                                 ULFmin = 0, ULFmax = 0.03,
                                 VLFmin = 0.03, VLFmax = 0.05,
                                 LFmin = 0.05, LFmax = 0.15,
                                 HFmin = 0.15, HFmax = 0.4 ) #Calculate Powerbands
  
  ulf_mean <- mean(hrv_data$FreqAnalysis[[1]]$ULF)        #Calculate all HRV parameters of the frequency analysis
  vlf_mean <- mean(hrv_data$FreqAnalysis[[1]]$VLF)
  lf_mean <- mean(hrv_data$FreqAnalysis[[1]]$LF)
  hf_mean <- mean(hrv_data$FreqAnalysis[[1]]$HF)
  lfhf_mean <- mean(hrv_data$FreqAnalysis[[1]]$LFHF)
  
  pwr_vlf <-(ulf_mean + vlf_mean) /(ulf_mean + vlf_mean + lf_mean + hf_mean)*100
  pwr_lf <- (lf_mean) / (ulf_mean + vlf_mean + lf_mean + hf_mean)*100
  pwr_hf <- (hf_mean) / (ulf_mean + vlf_mean + lf_mean + hf_mean)*100
  lf_nu <-  lf_mean / (lf_mean + hf_mean)*100
  hf_nu <- hf_mean / (lf_mean + hf_mean)*100
  
  lists_fft <- c(ulf_mean, vlf_mean, lf_mean,
                 hf_mean, lfhf_mean,
                 pwr_vlf,pwr_lf, pwr_hf,
                 lf_nu, hf_nu, ses_dur_sec,
                 hrv_art_index)                           #Make a list of the parameters
  
  names(lists_fft) <- c("ulf_mean", "vlf_mean", 
                        "lf_mean", "hf_mean",
                        "lfhf_mean", "pwr_vlf",
                        "pwr_lf", "pwr_hf",
                        "lf_nu", "hf_nu", 
                        "session_dur_sec",
                        "hrv_art_index")                 #Name the list
  
  hrv_all <- c(hrv_data$TimeAnalysis[[1]], lists_fft)    #Put in HRV container

return(hrv_all)
}

