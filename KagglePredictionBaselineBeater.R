# train = file.choose()

#tt = read.csv("/Users/callinswitzer/Dropbox/MachineLearning/p3Data/train.csv")
ttab = tapply(INDEX = tt$user, X = tt$plays, median)
head(ttab)

ttdf <- data.frame(plays = ttab, user = names(ttab))
rownames(ttdf) <- NULL
head(ttdf)
summary(ttdf)

artistMeanPlays = read.csv("/Users/callinswitzer/Documents/GitRepos/CS181Regression/p3/ArtistMeanPlays.csv")
head(artistMeanPlays)

artistMeanPlays$scaledArtist = as.numeric(scale(artistMeanPlays$plays, center = TRUE, scale = TRUE))
colnames(artistMeanPlays)[2:3] = c("meanPlaysPerArtist", "artistMeanPlays_scaled")
head(artistMeanPlays)

hist(artistMeanPlays$artistMeanPlays_scaled)


test = read.csv("/Users/callinswitzer/Dropbox/MachineLearning/p3Data/test.csv")
summary(test)
colnames(test)

tsm <- test[1:100, ]

submit <- merge(x = test, y = ttdf, by= 'user')
head(submit)

submit2 <- merge(x= submit, y = artistMeanPlays, by = 'artist')
head(submit2)
nrow(submit2)

submit2$newPred = submit2$plays + submit2$artistMeanPlays_scaled

# here's another method
submit_sm = submit2[, c('Id', 'newPred')]
head(submit_sm)

colnames(submit_sm)[2] = "plays"



submit_sm <- submit_sm[order(submit_sm$Id), ]
head(submit_sm)
submit_sm$plays[submit_sm$plays < 0] = 0

sum(submit_sm$plays < 0)

# beat the baseline: refref -- move to Python!
write.csv(submit_sm, "/Users/callinswitzer/Dropbox/MachineLearning/p3Data/Sub4_IdMeansAdjArtist3.csv", row.names = FALSE)


# new ideas -- find the top people with high -play counts, and fit models for them specially 
# these are the biggest misses
