
# In the original application, we used the column with the IBI to calculate
# the HRV parameters (i.e., 1.12, .69, .89 etc).

# During the tutorial it became clear that we might better use the seconds elapsed since
# the start as an index, as RHRV has a built-in function to deal with missing data
# over timeframes. Therefore use the column with seconds elapsed since
# starting time (i.e., 12.8, 13.9, 15.1 etc.)
source("../Chapter9/R/libraries.R")
e4_data <- read_e4("../Chapter9/Sensor data/Testdata/18-3-2020/1584533408_A00585.zip")
e4_data <- read_e4("../Chapter9/Sensor data/Testdata/15-4-2021_5min_clean/1618477723_A0047C.zip")

# Select the heart beat positions in time. Use the amount of seconds since the start
e4_hrv_data  = CreateHRVData()
e4_hrv_data = SetVerbose(e4_hrv_data, TRUE )
e4_hrv_data$datetime <- as.POSIXlt(e4_data$IBI$DateTime)[1]
e4_hrv_data$Beat <- data.frame(Time = e4_data$IBI[[3]])

# Then build the non interpolated heart rate series
e4_hrv_data = BuildNIHR(e4_hrv_data)

# Pay attention that we don't need the inter-beat-intervals as RHRV does not know how to handle these
# as there are so much missing values in there.

# Remove too short RR intervals or missed beats
# This also provides the number of accepted beats
e4_hrv_data = FilterNIHR(e4_hrv_data)

# Note that it is not necessary to specify freqhr since it matches with
# the default value: 4 Hz
e4_hrv_data = InterpolateNIHR(e4_hrv_data, freqhr = 4)

PlotNIHR(e4_hrv_data, main = "niHR")
plot(e4_hrv_data$Beat$niHR[1:200], type = 'l')

e4_hrv_data = CreateTimeAnalysis(e4_hrv_data, size = 100,
                                 interval = 7.8125)


#Check whether HR is the same from Empatica and the RHRC analysis, it is not.
mean(e4_data$HR$V1)
e4_hrv_data$HR[e4_hrv_data$HR == 0] <- NA
mean(e4_hrv_data$HR, na.rm =  TRUE)

plot(e4_hrv_data$HR)
summary(e4_hrv_data$HR)

# We have a lot of missing beats, so frequency analysis is difficult.
e4_hrv_data = CreateFreqAnalysis(e4_hrv_data)

e4_hrv_data = 
  CalculatePowerBand(e4_hrv_data , indexFreqAnalysis = 1,
                     size = 300, shift = 30, type = "fourier",
                     ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05,
                     LFmin = 0.05, LFmax = 0.15, HFmin = 0.15, HFmax = 0.4 )


