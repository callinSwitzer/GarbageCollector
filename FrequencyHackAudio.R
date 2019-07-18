# import packages

library("tuneR")
library("zoo")


dataDir= "D:\\Dropbox\\AcademiaDropbox\\Reviews\\PeerJ_BuzzPollinationMexico"


# Read in a portion of the audio recording
wave1 = readWave(file.path(dataDir, "supplemental\\peerj-38132-Supplemental_Audio_S1.wav"), from = 1, to = 2, units = "seconds")

# get info
print(wave1)

# convert to mono
wave2 = mono(wave1, which = c("both"))

# show that it is now mono
wave2


par(mfrow=  c(3,1))
# plot data
plot(wave2, main = "raw data")

plot(y = wave2@left[1:4410],x = (1:4410)/wave2@samp.rate,  main = "raw data, zoomed", type = "l", xlab = "time", ylab = "")

fft_calculator = function(timeSignal, log_rate){
  # calculate fft
  n = length(timeSignal)
  Y = fft(timeSignal)/n
  amplitudeAndPhase = Y[(1:(n/2))]*2 # these are complex numbers
  
  # calculate fft frequencies
  k = 0:n
  Ts = n/log_rate
  frq = k/Ts # two side frequency range
  frq = frq[1:(n/2)] # one side frequency range
  
  return(list('amplitudeAndPhase' = amplitudeAndPhase, "frq" =  frq))
}


fft_data = fft_calculator(wave2@left, wave2@samp.rate)


amplitudeAndPhase = fft_data[["amplitudeAndPhase"]]
frq = fft_data[["frq"]]

plot(abs(amplitudeAndPhase), x = frq, type = "l", xlab = "freq (Hz)", 
     ylab = "amplitude", main = "DFT spectrum")


##################################################################
# hack 1: make signal look more like a sine wave -- less spiky
##################################################################

par(mfrow=  c(3,1))

# you can tune the width 
rollingVar = rollapply(wave2@left, width = 20, FUN = var, fill = 0)

# remove mean
rollingVar = rollingVar - mean(rollingVar)

# plot data
plot(y = rollingVar, x = (1:length(wave2@left))/wave2@samp.rate, main = "rolling variance", type = "l", xlab = "time (s)")

plot(y = rollingVar[1:4410],x = (1:4410)/wave2@samp.rate,  main = "rolling var, zoomed", type = "l", xlab = "time", ylab = "")

fft_data = fft_calculator(rollingVar, wave2@samp.rate)


amplitudeAndPhase = fft_data[["amplitudeAndPhase"]]
frq = fft_data[["frq"]]

# note that harmonics are meaningless now
plot(abs(amplitudeAndPhase), x = frq, type = "l", xlab = "freq (Hz)", 
     ylab = "amplitude", main = "DFT spectrum of rolling variance", xlim = c(0, 500))


##################################################################
# hack 2: find distance between peaks
##################################################################



# find peaks
# function here: https://github.com/stas-g/findPeaks/blob/master/find_peaks.R
find_peaks <- function (x, m = 3){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}
pks = find_peaks(wave2@left, m = 150)


par(mfrow=  c(2,1))
plot(wave2, main = "raw data")
points(y = wave2@left[pks],x = ((1:length(wave2@left))/wave2@samp.rate)[pks])

# plot instantaneous freq
ifreq = 1/(diff(pks)/wave2@samp.rate)
plot(y = ifreq, x = ((1:length(wave2@left))/wave2@samp.rate)[pks][0:length(ifreq)], ylab = "freq (Hz)", xlab = "time (s)", type = "l")


