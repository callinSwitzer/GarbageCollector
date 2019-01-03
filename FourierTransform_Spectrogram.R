
# install packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tuneR", "seewave", "tidyverse")
ipak(packages)

setwd("~/Desktop/")


# set the audio player -- mac specific 
setWavPlayer("afplay")


wavFle = "/Users/cswitzer/Dropbox/AcademiaDropbox/SonicationBehavior/SonBehData/SonicationBehavior_HeavyLight/Bee1_27Sept_Hive4_W_S/2016_09_27__10_52_05/2016_09_27__10_52_42_974_220_450_test.txt"

# load sound
w1 <- as.numeric(scan(wavFle, what = "numeric", sep = "", skip = 1)) 
w1 <- w1[seq(0, length(w1), by = 40)]

samp.rate = 200000
#samp.rate = 5000

w1 <- Wave(as.data.frame(w1)$w1, samp.rate = samp.rate)


# view oscillogram chunks for each channel (left and right)
oscillo(w1, f = samp.rate, from = 0, to = 0.1)

length(w1)

str(w1)

dev.off()

# view oscillogram of mono wave
oscillo(w1)

op = par()
op$mai


# view spectrogram of a small portion of the recording
# make figure 1


sp1 <- spectro(w1, f = samp.rate, dB = "D", flim = c(0, 5),
               wl = 2**11, cexaxis = 1.2, cexlab = 1.2, scalecexlab = 1.2, oma = c(1,1,1,1),
               wn = "hanning", 
               ovlp = 10,
               osc = TRUE,
               palette = colorRampPalette(c("grey70", "black")))


dev.off()


library(signal)
library(oce)





specgram(w1, n = 2**9, Fs = 200000)

sampFreq = 44100

time_s = seq(0, 0.1, length.out = sampFreq / 10)
y_val = 4 * sin(2 * pi * time_s * 200) + 2*sin(2*pi * time_s * 2500) + 1*sin(2*pi * time_s * 5500)


plot(time_s, y_val, type = "l", main = "wave for analysis", bty = 'n')
specgram(y_val, n = 2**9, Fs = sampFreq)
spec = specgram(y_val, n = 2**9, Fs = sampFreq)

# discard phase information
P = abs(spec$S)

# normalize
P = P/max(P)

# convert to dB
#P = 10*log10(P)

# config time axis
t = spec$t

# plot spectrogram
imagep(x = t,
       y = spec$f,
       z = t(P)/100,
       col = oce.colorsViridis,
       ylab = 'Frequency [Hz]',
       xlab = 'Time [s]',
       drawPalette = T,
       decimate = F
)

?t


## _____________________________________

# redo fig 3

tiff("~/Dropbox/dataAnalysisForOthers/SoundAnalysisforSteve/Fig3_largeFonts.tiff",width = 6.5, height = 8, units = 'in', res = 1200)
{
  aa = cutw(w2, from = 1, to = 2, f = w2@samp.rate)
  layout(matrix(c(1,1,2), nrow = 3, ncol = 1, byrow = TRUE))
  par(mai = c(0,0,0,0))
  plot(y = aa / max(aa), x = seq(0, 1, length.out = length(aa)), type = 'l', bty = "n", ylim = c(-4.5, 1), 
       yaxt = "n", xlab = "", ylab = "", ann = FALSE, xaxt = "n"
  )
  # add scale
  arrows(x0 = 0.8, y0 = -1.1, x1 = 1, y1 = -1.1, lwd = 1.2, length = 0.05, code = 3)
  text(0.9, -1.15, "0.2 s", adj = c(0.5, 1.3), cex = 1.5)
  
  # add line segments
  x0 = (1.435 - 1) 
  x1 = (1.515 - 1) 
  y0 = -0.3
  y1 = -0.9
  segments(x0, y0, x1 = x0, y1, lwd = 1.2)
  segments(x0 = x1, y0, x1 = x1, y1, lwd = 1.2)
  
  # add arrows
  arrows(x0, y0 = y1, x1 = 0, y1 = y1 - 1, lwd = 1.2, length = 0.05)
  arrows(x0 = x1, y0 = y1, x1 = 1, y1 = y1 - 1, lwd = 1.2, length = 0.05)
  
  # add next layer
  bb = cutw(w2, from = x0 + 1, to = x1 + 1, f = w2@samp.rate)
  lines(bb/ max(bb) - 3,  x = seq(0, 1, length.out = length(bb)))
  
  # add scale
  arrows(x0 = 0.8, y0 = -4.5+0.3, x1 = 1, y1 = -4.5 + 0.3, lwd = 1.2, length = 0.05, code = 3)
  text(0.9, -4.55+0.3, "0.014 s", adj = c(0.5, 1.3), cex = 1.5)
  
  # add letters
  text('A', x= 0, y = 1, cex = 2)
  text('B', x= 0, y = -1.6, cex = 2)
  
  
  # plot spectrum
  spp <- spec((bb/max(bb)), f = w2@samp.rate, scaled = TRUE, fftw = TRUE, PSD = FALSE, wl = 1024, main = "FFT spectrum of zoomed section", plot = FALSE, norm = FALSE)
  par(mai = c(0.6,0.7,0,0))
  
  spectrum = data.frame(unlist(spp))
  xx <- c(spectrum$x * 1000, rev(spectrum$x * 1000))
  ref = max(spectrum$y)
  # calculate dB, with 0 being the max
  dB = 20* log10(spectrum$y / ref)
  yy <- c(rep(min(dB), length(dB)), rev(dB))
  
  plot(xx, yy, type = 'l', ylab = "Relative amplitude (dB)", xlab = "Frequency (Hz)", 
       bty = "n", log = "x", ylim = c(-50, 0), cex.axis = 1.5, cex.lab = 1.5, xlim = c(100, 22050))
  polygon(x = xx, y = yy, col='black', border=NA)
  
  # add text
  text(round(xx[yy == max(yy)]), y = 0, labels = paste(round(xx[yy == max(yy)]), "Hz", sep  = " "), adj = c(-0.2, 0.5), cex = 1.5)
  
  mtext('C', at = c(51, 15), cex = 2 * 2/3)
  
  
  
}
dev.off()



tiff("~/Dropbox/dataAnalysisForOthers/SoundAnalysisforSteve/Fig3_no_dB_largeFonts.tiff",width = 6.5, height = 8, units = 'in', res = 1200)
{
  aa = cutw(w2, from = 1, to = 2, f = w2@samp.rate)
  layout(matrix(c(1,1,2), nrow = 3, ncol = 1, byrow = TRUE))
  par(mai = c(0,0,0,0))
  plot(y = aa / max(aa), x = seq(0, 1, length.out = length(aa)), type = 'l', bty = "n", ylim = c(-4.5, 1), 
       yaxt = "n", xlab = "", ylab = "", ann = FALSE, xaxt = "n"
  )
  # add scale
  arrows(x0 = 0.8, y0 = -1.1, x1 = 1, y1 = -1.1, lwd = 1.2, length = 0.05, code = 3)
  text(0.9, -1.15, "0.2 s", adj = c(0.5, 1.3), cex = 1.5)
  
  # add line segments
  x0 = (1.435 - 1) 
  x1 = (1.515 - 1) 
  y0 = -0.3
  y1 = -0.9
  segments(x0, y0, x1 = x0, y1, lwd = 1.2)
  segments(x0 = x1, y0, x1 = x1, y1, lwd = 1.2)
  
  # add arrows
  arrows(x0, y0 = y1, x1 = 0, y1 = y1 - 1, lwd = 1.2, length = 0.05)
  arrows(x0 = x1, y0 = y1, x1 = 1, y1 = y1 - 1, lwd = 1.2, length = 0.05)
  
  # add next layer
  bb = cutw(w2, from = x0 + 1, to = x1 + 1, f = w2@samp.rate)
  lines(bb/ max(bb) - 3,  x = seq(0, 1, length.out = length(bb)))
  
  # add scale
  arrows(x0 = 0.8, y0 = -4.5+0.3, x1 = 1, y1 = -4.5 + 0.3, lwd = 1.2, length = 0.05, code = 3)
  text(0.9, -4.55+0.3, "0.014 s", adj = c(0.5, 1.3), cex = 1.5)
  
  # add letters
  text('A', x= 0, y = 1, cex = 2)
  text('B', x= 0, y = -1.6, cex = 2)
  
  
  # plot spectrum
  spp <- spec((bb/max(bb)), f = w2@samp.rate, scaled = TRUE, fftw = TRUE, PSD = FALSE, wl = 1024, main = "FFT spectrum of zoomed section", plot = FALSE, norm = FALSE)
  par(mai = c(0.6,0.7,0,0))
  
  spectrum = data.frame(unlist(spp))
  xx <- c(spectrum$x * 1000, rev(spectrum$x * 1000))
  ref = max(spectrum$y)
  # calculate dB, with 0 being the max
  dB = spectrum$y/max(spectrum$y)
  yy <- c(rep(min(dB), length(dB)), rev(dB))
  
  yy = yy[xx >= 99]
  xx = xx[xx>=99]
  
  plot(xx, yy, type = 'l', ylab = "Relative amplitude", xlab = "Frequency (Hz)", 
       bty = "n", log = "x", cex.lab=1.5, cex.axis = 1.5, xlim = c(100, 22050), yaxt="n")
  axis(side = 2, at = c(0,1), cex.axis = 1.5, cex.lab = 1.5)
  polygon(x = xx, y = yy, col='black', border=NA)
  
  # add text
  text(round(xx[yy == max(yy)]), y = 1, labels = paste(round(xx[yy == max(yy)]), "Hz", sep  = " "), adj = c(-0.2, 0.5), cex = 1.5)
  
  mtext('C', at = c(51, 15), cex = 2 * 2/3)
  
  
  
}
dev.off()



# find max 
xx[which.max(yy)]



xx2 <- xx[xx > 600 & xx < 1000]
yy2 <- yy[xx > 600 & xx < 1000]

xx2[which.max(yy2)]

points(xx2[which.max(yy2)], yy2[which.max(yy2)], col = 'red')
points(xx[which.max(yy)], yy[which.max(yy)], col = 'red')
xx[which.max(yy)]

#))))

sampFreq = 44100

time_s = seq(0, 0.1, length.out = sampFreq / 10)
y_val = 4 * sin(2 * pi * time_s * 200) + 2*sin(2*pi * time_s * 2500) + 1*sin(2*pi * time_s * 5500)

par(mfrow= c(4,1))

plot(time_s, y_val, type = "l", main = "wave for analysis", bty = 'n')



# plot spectrum
# note multiply y-val by 2 to get correct amplitude in spectrum
spp <- spec(y_val*2, f = sampFreq, scaled = TRUE, fftw = TRUE, PSD = FALSE, wl = 1024, main = "FFT spectrum", plot = FALSE, norm = FALSE)

spectrum = data.frame(unlist(spp))

linearSpectrum = spectrum

xx <- c(linearSpectrum$x * 1000, rev(linearSpectrum$x * 1000)) #  # convert from kHz to Hz
# calculate dB, with 0 being the max
dB = linearSpectrum$y
yy <- c(rep(min(dB), length(dB)), rev(dB))

plot(xx[xx > 99], yy[xx > 99], type = 'l', ylab = "Absolute amplitude", xlab = "Frequency (Hz)", bty = "n", log = "x", cex.lab=1.1, xlim = c(100, 22050))
polygon(x = xx, y = yy, col='black', border=NA)
abline(h = c(4,2,1), lty = 2)


plot(xx[xx > 99], yy[xx > 99] / max(yy[xx > 99]), type = 'l', ylab = "Relative amplitude (linear scale)", xlab = "Frequency (Hz)", bty = "n", log = "x", cex.lab=1.1, xlim = c(100, 22050))
polygon(x = xx, y = yy / max(yy[xx > 99]), col='black', border=NA)
abline(h = c(4,2,1)/4, lty = 2)


xx <- c(spectrum$x * 1000, rev(spectrum$x * 1000))
ref = max(spectrum$y)
# calculate dB, with 0 being the max
dB = 20* log10(spectrum$y / ref)
yy <- c(rep(min(dB), length(dB)), rev(dB))

plot(xx, yy, type = 'l', ylab = "Relative amplitude (dB)", xlab = "Frequency (Hz)", bty = "n", log = "x", ylim = c(-20, 0), cex.lab=1.1, xlim = c(100, 22050))
polygon(x = xx, y = yy, col='black', border=NA)
abline(h = c(0, -6, -12), lty = 2)

dev.off()
