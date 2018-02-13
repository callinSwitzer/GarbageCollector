
ssf = function(o){
  guesses = (runif(n = 10, 1, 100))
  guesses = c(guesses, 35)
  guesses = guesses[!duplicated(guesses)]

  
  pick = (runif(n = 1, 1, 100))
  closest = which.min(abs(guesses - pick))
  
  cc = guesses[closest]
  
  cc
}

ssf()

reps = replicate(n = 100000, expr = ssf())

hist(reps, freq = FALSE)
lines(density(reps))


hist(reps[reps != 35], freq = FALSE)
lines(density(reps[reps != 35]))
abline(v = 80, col = "red")

which.max(density(reps[reps != 35]))
