


ibi_analysis <- function(IBI){
  
  # Select the heart beat positions in time. Use the amount of seconds since the start
  e4_hrv_data <- CreateHRVData()
  e4_hrv_data <- SetVerbose(e4_hrv_data, TRUE )
  e4_hrv_data$datetime <- as.POSIXlt(IBI$DateTime)[1]
  
  e4_hrv_data$Beat <- data.frame(Time = IBI$seconds)
  
  # Then build the non interpolated heart rate series
  e4_hrv_data <- BuildNIHR(e4_hrv_data)
  
  # Pay attention that we don't need the inter-beat-intervals as RHRV does not know how to handle these
  # as there are so much missing values in there.
  
  # Remove too short RR intervals or missed beats
  # This also provides the number of accepted beats
  n_beats_original <- nrow(e4_hrv_data$Beat)
  
  e4_hrv_data <- FilterNIHR(e4_hrv_data)
  
  n_beats_accepted <- nrow(e4_hrv_data$Beat)
  
  # Note that it is not necessary to specify freqhr since it matches with
  # the default value: 4 Hz
  suppressWarnings({
    e4_hrv_data <- InterpolateNIHR(e4_hrv_data, freqhr = 4)  
  
    e4_hrv_data <- CreateTimeAnalysis(e4_hrv_data, size = 100,
                                   interval = 7.8125)
  })
  
  # We have a lot of missing beats, so frequency analysis is difficult.
  e4_hrv_data <- CreateFreqAnalysis(e4_hrv_data)
  
  e4_hrv_data <- CalculatePowerBand(e4_hrv_data , indexFreqAnalysis = 1,
                       size = 300, shift = 30, type = "fourier",
                       ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05,
                       LFmin = 0.05, LFmax = 0.15, HFmin = 0.15, HFmax = 0.4 )
  
  list(
    time = list(
      SDNN = e4_hrv_data$TimeAnalysis$SDNN,
      pNN50 = e4_hrv_data$TimeAnalysis$pNN50,
      SDSD = e4_hrv_data$TimeAnalysis$SDSD,
      rMSSD = e4_hrv_data$TimeAnalysis$rMSSD
    ),
    frequency = list(
      HF = mean(e4_hrv_data$FreqAnalysis[[1]]$HF, na.rm = TRUE),
      LF = mean(e4_hrv_data$FreqAnalysis[[1]]$LF, na.rm = TRUE),
      HFLF = mean(e4_hrv_data$FreqAnalysis[[1]]$HFLF, na.rm = TRUE),
      VLF = mean(e4_hrv_data$FreqAnalysis[[1]]$VLF, na.rm = TRUE)
    ),
    beats = list(
      original = n_beats_original,
      accepted = n_beats_accepted
    )
  )
  
  
}
