#save 90% range for 1/2 octave (newborn and 6mth), PCA for pred(), and new model
freqs = c(250, 354, 500, 707, 1000, 1414, 2000, 2828, 4000, 5657, 8000)
freqs2 = rep(freqs, each = 2)
freqs1 = c(250, 500, 1000, 2000, 4000, 8000)
freqs1 = rep(freqs1, each = 2)
# 6mth 90% range 1/2 octave
abs.2 = wai.2[,c(3:13,36)]
abs.2 <- filter(abs.2, rs == "Pass")
abs.2 = abs.2[,-12]
abs.2 = na.omit(abs.2)
abs.Q.5 <- summarise_all(abs.2, funs(quantile(., probs = (0.05))))
abs.Q.95 <- summarise_all(abs.2, funs(quantile(., probs = (0.95))))
abs.90.range <- rbind.data.frame(abs.Q.5, abs.Q.95)
Stats.col <- c("five", "ninety5")
abs.90.range <- cbind.data.frame(abs.90.range, Stats.col)
abs.90.range <- gather(abs.90.range, Frequency, absorbance, abs250:abs8000)
abs.90.range <- cbind.data.frame(freqs2, abs.90.range)
abs.90.range <- spread(abs.90.range, Stats.col, absorbance)
sixmth.90.range.half.octave = abs.90.range[,c(1,3,4)]
names(sixmth.90.range.half.octave) = c("Frequency", "Five", "Ninetyfive")
# save 90% range at 1/2 octave - for manual entry
save(sixmth.90.range.half.octave, file = "six.mth.90.range.half.octave.rda")

# newborn 90% range 1 octave
abs.1 = wai.1[,c(4:9,23)]
abs.1 <- filter(abs.1, BP == "pass")
abs.1 = abs.1[,-7]
abs.Q.5 <- summarise_all(abs.1, funs(quantile(., probs = (0.05))))
abs.Q.95 <- summarise_all(abs.1, funs(quantile(., probs = (0.95))))
abs.90.range <- rbind.data.frame(abs.Q.5, abs.Q.95)
Stats.col <- c("five", "ninety5")
abs.90.range <- cbind.data.frame(abs.90.range, Stats.col)
abs.90.range <- gather(abs.90.range, Frequency, absorbance, abs250:abs8000)
abs.90.range = cbind.data.frame(freqs1, abs.90.range)
abs.90.range = spread(abs.90.range, Stats.col, absorbance)
neonate.90.range.octave = abs.90.range[,c(1,3,4)]
names(neonate.90.range.octave) = c("Frequency", "Five", "Ninetyfive")
# save 90% range at 1/2 octave - for manual entry
save(neonate.90.range.octave, file = "neonate.90.range.octave.rda")

# save penalized PCA model
save(f.pca.penal, file = "sixMonthModel.rda")

# save PCA for predict with new data
save(pca, file = "sixMonthPCA.rda")

# create and save bandwith ranges
oct.1.min = c(226, 353.56,707.12, 1414.22, 2828.44, 5656.86)
oct.1.max = c(353.55, 707.11, 1414.21, 2828.43, 5656.85, 8000)
oct.1.freqs = c(250, 500, 1000, 2000, 4000, 8000)
oct.1.freqs = as.integer(oct.1.freqs)
oct.1.band = cbind.data.frame(oct.1.freqs, oct.1.min, oct.1.max)
colnames(oct.1.band) = c("Frequency (Hz)", "From (Hz)", "To (Hz)")
save(oct.1.band, file = "oct.1.band.rda")

# 1/2 oct
oct.2.min = c(226, 297.31, 420.46, 594.61, 840.91, 1189.22, 1681.8, 2378.42, 3363.6, 4756.84, 6727.18)
oct.2.max = c(297.3, 420.45, 594.6, 840.9, 1189.21, 1681.79, 2378.41, 3363.59, 4756.83,  6727.17, 8000)
oct.2.freqs = c(250, 354, 500, 707, 1000, 1414, 2000, 2828, 4000, 5657, 8000)
oct.2.freqs = as.integer(oct.2.freqs)
oct.2.band = cbind.data.frame(oct.2.freqs, oct.2.min, oct.2.max)
colnames(oct.2.band) = c("Frequency (Hz)", "From (Hz)", "To (Hz)")
save(oct.2.band, file = "oct.2.band.rda")

