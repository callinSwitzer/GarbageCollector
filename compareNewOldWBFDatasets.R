#f1 <- file.choose()
f1 <-   "C:\\Users\\calli\\Documents\\GitRepos\\FlappingKinematics\\data\\FinalBeeRespData_raw.csv"

# f_new <- file.choose()
f_new =  "C:\\Users\\calli\\Documents\\GitRepos\\FlappingKinematics\\data\\beeRespData_final.csv"


library(tidyverse)


oldF <- read_csv(f1)
newF <- read_csv(f_new)

bdta <- oldF %>%
  mutate(
    MT = (M2 + MF)/2,
    load = MT - Mstarved,
    perLoad = load / Mstarved * 100,
    arcL = (0.75 *wingLen) * (amp * (pi / 180)),
    U = arcL * freq * 2,
    freq2 = freq^2,
    arcL2 = arcL^2,
    frce = U^2 * S,
    Mass_Spec_MetR = MetR / Mstarved,
    
    # convert to factor variables
    order = as.factor(as.character(order)),
    Treatment = as.factor(as.character(Treatment)),
    BeeID = as.factor(as.character(BeeID))
  )

# calculate mean + sd for each treatment
tapply(bdta$amp, INDEX = bdta$Treatment, function(x) c(mean(x), sd(x)))


bdta2 <- newF %>%
  mutate(
    MT = (M2 + MF)/2,
    load = MT - Mstarved,
    perLoad = load / Mstarved * 100,
    arcL = (0.75 *wingLen) * (amp * (pi / 180)),
    U = arcL * freq * 2,
    freq2 = freq^2,
    arcL2 = arcL^2,
    frce = U^2 * S,
    Mass_Spec_MetR = MetR / Mstarved,
    
    # convert to factor variables
    order = as.factor(as.character(order)),
    Treatment = as.factor(as.character(Treatment)),
    BeeID = as.factor(as.character(BeeID))
  )

# calculate mean + sd for each treatment
tapply(bdta2$amp, INDEX = bdta$Treatment, function(x) c(mean(x), sd(x)))


all(colnames(bdta2) == colnames(bdta))
all(bdta2$BeeID == bdta$BeeID)

# visualize differences
CompareDF = bdta == bdta2

nds <- data.frame(allTrue = apply(CompareDF, MARGIN = 2, function(x) all(x == TRUE)))
rownames(nds)[nds[, 1] == FALSE]

