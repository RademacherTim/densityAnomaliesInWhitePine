#========================================================================================
# Script to get general stats about the occurence of density anomalies in white pine. 
# Results are mainly using binomial logistic regression. 
#----------------------------------------------------------------------------------------

# Load dependencies
#----------------------------------------------------------------------------------------
if (!existsFunction ('stan')) library ('rstan') # to fit more complex Bayesian models
if (!existsFunction ('ulam')) library ('rethinking') # to fit Bayesian model
if (!existsFunction ('tibble')) library ('tidyverse') # to wrangle data

# Download and read the data
#----------------------------------------------------------------------------------------
source ('readGrowthData.R')

# Load colour scheme for at reast height, near a branch whorl and in the 2010 section
#----------------------------------------------------------------------------------------
source ('plotingFunctions.R')

# Wrangle data into long format with all possible explanatory variable for presence data
#----------------------------------------------------------------------------------------
temp1 <- data %>% dplyr::filter (Year < 2017, DABH_1 %in% 1:2) %>% 
  mutate (WoodAge = WoodAgeBH, RingWidth = RingWidthBH_1, TOP = FALSE, 
          BRANCH = FALSE, densityAnomaly = TRUE, Pos1 = PositionDABH_1.1, 
          Pos2 = PositionDABH_1.1 + PositionDABH_1.2)
temp2 <- data %>% dplyr::filter (Year < 2017, DABH_2 %in% 1:2) %>% 
  mutate (WoodAge = WoodAgeBH, RingWidth = RingWidthBH_2, TOP = FALSE, 
          BRANCH = FALSE, densityAnomaly = TRUE, Pos1 = PositionDABH_2.1, 
          Pos2 = PositionDABH_2.1 + PositionDABH_2.2)
temp3 <- data %>% dplyr::filter (Year < 2017, DABranch_1 %in% 1:2) %>% 
  mutate (WoodAge = WoodAgeBH, RingWidth = RingWidthNearBranch_1, TOP = FALSE, 
          BRANCH = TRUE, densityAnomaly = TRUE, Pos1 = PositionDABranch_1.1, 
          Pos2 = PositionDABranch_1.1 + PositionDABranch_1.2)
temp4 <- data %>% dplyr::filter (Year < 2017, DABranch_2 %in% 1:2) %>% 
  mutate (WoodAge = WoodAgeBH, RingWidth = RingWidthNearBranch_2, TOP = FALSE, 
          BRANCH = TRUE, densityAnomaly = TRUE, Pos1 = PositionDABranch_2.1, 
          Pos2 = PositionDABranch_2.1 + PositionDABranch_2.2)
temp5 <- data %>% dplyr::filter (Year < 2017, DA2010_2 %in% 1:2) %>% 
  mutate (WoodAge = WoodAgeBH, RingWidth = RingWidth2010_1, TOP = TRUE, 
          BRANCH = FALSE, densityAnomaly = TRUE, Pos1 = PositionDA2010_1.1, 
          Pos2 = PositionDA2010_1.1 + PositionDA2010_1.2)
temp6 <- data %>% dplyr::filter (Year < 2017, DA2010_2 %in% 1:2) %>% 
  mutate (WoodAge = WoodAgeBH, RingWidth = RingWidth2010_2, TOP = TRUE, 
          BRANCH = FALSE, densityAnomaly = TRUE, Pos1 = PositionDA2010_2.1, 
          Pos2 = PositionDA2010_2.1 + PositionDA2010_2.2)
temp <- rbind (temp1, temp2, temp3, temp4, temp5, temp6)
rm (temp1, temp2, temp3, temp4, temp5, temp6)


# Wrangle data into long format with all possible explanatory variable for absence data
#----------------------------------------------------------------------------------------
tmp1 <- data %>% dplyr::filter (Year < 2017, DABH_1 == 0) %>% 
  mutate (WoodAge = WoodAgeBH, RingWidth = RingWidthBH_1, TOP = FALSE, 
          BRANCH = FALSE, densityAnomaly = FALSE, Pos1 = PositionDABH_1.1, 
          Pos2 = PositionDABH_1.1 + PositionDABH_1.2)
tmp2 <- data %>% dplyr::filter (Year < 2017, DABH_2 == 0) %>% 
  mutate (WoodAge = WoodAgeBH, RingWidth = RingWidthBH_2, TOP = FALSE, 
          BRANCH = FALSE, densityAnomaly = FALSE, Pos1 = PositionDABH_2.1, 
          Pos2 = PositionDABH_2.1 + PositionDABH_2.2)
tmp3 <- data %>% dplyr::filter (Year < 2017, DABranch_1 == 0) %>% 
  mutate (WoodAge = WoodAgeBH, RingWidth = RingWidthNearBranch_1, TOP = FALSE, 
          BRANCH = TRUE, densityAnomaly = FALSE, Pos1 = PositionDABranch_1.1, 
          Pos2 = PositionDABranch_1.1 + PositionDABranch_1.2)
tmp4 <- data %>% dplyr::filter (Year < 2017, DABranch_2 == 0) %>% 
  mutate (WoodAge = WoodAgeBH, RingWidth = RingWidthNearBranch_2, TOP = FALSE, 
          BRANCH = TRUE, densityAnomaly = FALSE, Pos1 = PositionDABranch_2.1, 
          Pos2 = PositionDABranch_2.1 + PositionDABranch_2.2)
tmp5 <- data %>% dplyr::filter (Year < 2017, DA2010_2 == 0) %>% 
  mutate (WoodAge = WoodAgeBH, RingWidth = RingWidth2010_1, TOP = TRUE, 
          BRANCH = FALSE, densityAnomaly = FALSE, Pos1 = PositionDA2010_1.1, 
          Pos2 = PositionDA2010_1.1 + PositionDA2010_1.2)
tmp6 <- data %>% dplyr::filter (Year < 2017, DA2010_2 == 0) %>% 
  mutate (WoodAge = WoodAgeBH, RingWidth = RingWidth2010_2, TOP = TRUE, 
          BRANCH = FALSE, densityAnomaly = FALSE, Pos1 = PositionDA2010_2.1, 
          Pos2 = PositionDA2010_2.1 + PositionDA2010_2.2)
tmp <- rbind (tmp1, tmp2, tmp3, tmp4, tmp5, tmp6)
rm (tmp1, tmp2, tmp3, tmp4, tmp5, tmp6)

# Combine presence and absence data
#----------------------------------------------------------------------------------------
longData <- rbind (temp, tmp) %>% 
  select (densityAnomaly, Year, TreeID, WoodAge, RingWidth, TOP, BRANCH, Pos1, Pos2) %>%
  mutate (PosPer = Pos1 / RingWidth * 100.0)

# H1A : Does the position in the ring vary between trees in the same stand during the same year?
# H1B : Does the position vary between years?
# Extract relevant data
#----------------------------------------------------------------------------------------
muPrior <- longData %>% dplyr::filter (densityAnomaly) %>% select (PosPer) %>% drop_na %>%
  unlist () %>% mean ()
sigmaPrior <- longData %>% dplyr::filter (densityAnomaly) %>% select (PosPer) %>% drop_na %>%
  unlist () %>% sd ()
H1Data <- longData %>% 
  dplyr::filter (densityAnomaly) %>% 
  select (Year, TreeID, PosPer) %>% 
  mutate (Year = as_factor (Year), TreeID = as_factor (TreeID)) %>% 
  drop_na () %>% 
  mutate (PosPerStd = (PosPer - muPrior) / sigmaPrior) %>%
  select (-PosPer)

# Fit model to test whether the position in the ring varies as a function of 
# tree ID and/or year of formation
#----------------------------------------------------------------------------------------
set.seed (1358) # the summer of 1353
H1m1 <- ulam (
  alist (
    PosPerStd ~ dnorm (muR, sigmaR),
    muR <- aT [TreeID] + aY [Year],
    aT [TreeID] ~ dnorm (0, 0.5), # N.B.: tried wider prior (sigma = 1), but results are robust
    aY [Year] ~ dnorm (0, 0.5), # N.B.: tried wider prior (sigma = 1), but results are robust
    sigmaR ~ dexp (1)
  ), data = H1Data, chains = 4, cores = 4, cmdstan = TRUE
)
#traceplot (H1m1)
#trankplot (H1m1)
post <- extract.samples (H1m1)
muPost <- mean ((apply (post$aT, 1, mean) + apply (post$aY, 1, mean)) * sigmaPrior + muPrior)
PIPost <- PI (link (H1m1, post = post), prob = 0.9) * sigmaPrior + muPrior

# Plot posterior mean and 89% compatibility interval for tree intercepts
#----------------------------------------------------------------------------------------
png (file = './fig/MeanPositionByTreeID.png', width = 600, height = 400) 
par (mfrow = c (1, 1), mar = c (4, 4, 1, 1))
plot (x = precis (H1m1, depth = 2, prob - 0.9)$mean [1:41] * sigmaPrior + muPrior,
      y = 1:41,
      ylab = '',
      xlab = '', las = 1, xlim = c (0, 100), 
      col = '#F38D48', pch = 19, axes = FALSE)
abline (v = muPost, lwd = 2, col = '#66666666')
rect (xleft = PIPost [1], xright = PIPost [2], ybottom = 0, ytop = 42, 
      col = '#aaaaaa22', lty = 0)
segments (x0 = precis (H1m1, depth = 2, prob = 0.9) [1:41,3] * sigmaPrior + muPrior,
          x1 = precis (H1m1, depth = 2, prob = 0.9) [1:41,4] * sigmaPrior + muPrior,
          y0 = 1:41, col = '#F38D48')
points (x = precis (H1m1, depth = 2, prob = 0.9)$mean [1:41] * sigmaPrior + muPrior,
        y = 1:41, pch = 19, col = '#F38D48')
axis (side = 1)
axis (side = 2, las = 1)
dev.off ()

# Plot posterior mean and 89% compatibility interval for year intercepts
#----------------------------------------------------------------------------------------
png (file = './fig/MeanPositionByYear.png', width = 600, height = 400)
par (mfrow = c (1, 1), mar = c (4, 4, 1, 1))
plot (x = precis (H1m1, depth = 2, prob = 0.9)$mean [42:58] * sigmaPrior + muPrior,
      y = levels (H1Data$Year),
      ylab = '',
      xlab = '', las = 1, xlim = c (0, 100), 
      col = '#F38D48', pch = 19, axes = FALSE)
abline (v = muPost, lwd = 2, col = '#66666666')
rect (xleft = PIPost [1], xright = PIPost [2], ybottom = 1998, ytop = 2017, 
      col = '#aaaaaa22', lty = 0)
segments (x0 = precis (H1m1, depth = 2, prob = 0.9) [42:58,3] * sigmaPrior + muPrior,
          x1 = precis (H1m1, depth = 2, prob = 0.9) [42:58,4] * sigmaPrior + muPrior,
          y0 = as.numeric (levels (H1Data$Year)), col = '#F38D48')
points (x = precis (H1m1, depth = 2, prob = 0.9)$mean [42:58] * sigmaPrior + muPrior,
        y = levels (H1Data$Year), pch = 19, col = '#F38D48')
axis (side = 1)
axis (side = 2, las = 1)
dev.off ()

# Of the 41 trees four had substantially earlier IADFs, while one had a tendency 
# to form IADFs later in the ring. 
precis (H1m1, depth = 2)
# IADFs are placed earlier by 74.03 [69.32, 78.74] in Tree 11 
# IADFs are placed earlier by 73.80 [68.73, 78.98] in Tree 19 
# IADFs are placed earlier by 74.62 [70.38, 78.74] in Tree 23
# IDAFs are placed later   by 85.10 [80.04, 90.16] in Tree 27
# IADFs are placed earlier by 74.74 [70.85, 78.63] in Tree 36

# Of the 17 years with IADFs 6 had either particularly early or late occurrences 
# of IADFs.
# IADFs comparatively late  in 1998 compared to the multi-year mean by 85.69 [80.04, 91.46]
# IADFs comparatively late  in 2001 compared to the multi-year mean by 85.69 [83.22, 88.28]
# IADFs comparatively late  in 2010 compared to the multi-year mean by 88.16 [84.28, 92.05]
# IADFs comparatively early in 2013 compared to the multi-year mean by 74.98 [72.19, 77.68]
# IADFs comparatively early in 2014 compared to the multi-year mean by 72.15 [64.73, 79.57]
# IADFs comparatively early in 2016 compared to the multi-year mean by 76.62 [74.27, 78.98]


# Wrangle data to test the second hypothesis
#----------------------------------------------------------------------------------------
muPrior <- longData %>% select (RingWidth) %>% 
  summarise (mean = mean (RingWidth, na.rm = TRUE)) %>% unlist ()
sigmaPrior <- longData %>% select (RingWidth) %>% 
  summarise (sd = sd (RingWidth, na.rm = TRUE)) %>% unlist ()
H2Data <- longData %>% select (-TreeID, -WoodAge,-Pos1, -Pos2, -PosPer) %>%
  mutate (Year = as.integer (as_factor (Year)),
          TOP = as.integer (as_factor (TOP)),
          BRANCH = as.integer (as_factor (BRANCH)),
          RingWidth = (RingWidth - muPrior [[1]]) / sigmaPrior [[1]]) %>% 
  drop_na ()

# Selection of years with high intercepts
#----------------------------------------------------------------------------------------
IADFproneYears <- c (1999, 2002, 2012, 2013, 2016)
IADFproneYearIndices <- c (4, 7, 17, 18, 21)

# Fit a logistic model to test whether ring widths, year of occurrence, being at the top 
# of the tree and/or near a branch affects the likelihood of formation of an IADFS
#----------------------------------------------------------------------------------------
set.seed (1353)
H2mBRA <- ulam (
  alist (
    densityAnomaly ~ dbinom (1, p),
    logit (p) <-  aB [BRANCH],
    aB [BRANCH] ~ dnorm (0, 1.5) # this is a pretty flat (uninformative prior)
  ), data = H2Data, chains = 4, cores = 4, cmdstan = TRUE, log_lik = TRUE
)
#trankplot (H2mBRA)
#traceplot (H2mBRA)
#precis (H2mBRA, depth = 2, prob = 0.9)
set.seed (1353)
H2mTOP <- ulam (
  alist (
    densityAnomaly ~ dbinom (1, p),
    logit (p) <-  aT [TOP],
    aT [TOP] ~ dnorm (0, 1.5) # this is a pretty flat (uninformative prior)
  ), data = H2Data, chains = 4, cores = 4, cmdstan = TRUE, log_lik = TRUE
)
#trankplot (H2mTOP)
#traceplot (H2mTOP)
#precis (H2mTOP, depth = 2, prob = 0.9)
set.seed (1353)
H2mYEAR <- ulam (
  alist (
    densityAnomaly ~ dbinom (1, p),
    logit (p) <-  aY [Year],
    aY [Year] ~ dnorm (0, 1.5) # this is a pretty flat (uninformative prior)
  ), data = H2Data, chains = 4, cores = 4, cmdstan = TRUE, log_lik = TRUE
)
#trankplot (H2mYEAR)
#traceplot (H2mYEAR)
#precis (H2mYEAR, depth = 2, prob = 0.9)
set.seed (1353)
H2mRW <- ulam (
  alist (
    densityAnomaly ~ dbinom (1, p),
    logit (p) <- bR * RingWidth,
    bR ~ dnorm (0, 0.5)
  ), data = H2Data, chains = 4, cores = 4, cmdstan = TRUE, log_lik = TRUE
)
#trankplot (H2mRW)
#traceplot (H2mRW)
#precis (H2mRW, depth = 2, prob = 0.9)
set.seed (1353)
postH2mRW <- extract.samples (H2mRW)
effect_3.5RW5.5 <- 
  inv_logit (postH2mRW$bR * 0.4775436) - 
  inv_logit (postH2mRW$bR * -0.4780113)
mean (effect_3.5RW5.5)
PI (effect_3.5RW5.5, prob = 0.9)

set.seed (1353)
H2m2 <- ulam (
  alist (
    densityAnomaly ~ dbinom (1, p),
    logit (p) <- aY [Year] + bR * RingWidth,
    aY [Year] ~ dnorm (0, 1.5),
    bR ~ dnorm (0, 0.5)
  ), data = H2Data, chains = 4, cores = 4, cmdstan = TRUE, log_lik = TRUE
)
#trankplot (H2m2)
#traceplot (H2m2)
#precis (H2m2, depth = 2, prob = 0.9)
set.seed (1353)
postH2mRWY <- extract.samples (H2m2)
effect_3.5RWY5.5 <- 
  inv_logit (apply (postH2mRWY$aY [, IADFproneYearIndices], 1, mean) + 
             postH2mRWY$bR * 0.4775436) - 
  inv_logit (apply (postH2mRWY$aY [, IADFproneYearIndices], 1, mean) + 
             postH2mRWY$bR * -0.4780113)
mean (effect_3.5RWY5.5)
PI (effect_3.5RWY5.5, prob = 0.9)
effect_3.5RWY5.5 <- 
  inv_logit (apply (postH2mRWY$aY [, -IADFproneYearIndices], 1, mean) + 
               postH2mRWY$bR * 0.4775436) - 
  inv_logit (apply (postH2mRWY$aY [, -IADFproneYearIndices], 1, mean) + 
               postH2mRWY$bR * -0.4780113)
mean (effect_3.5RWY5.5)
PI (effect_3.5RWY5.5, prob = 0.9)

set.seed (1353)
H2m3 <- ulam (
  alist (
    densityAnomaly ~ dbinom (1, p),
    logit (p) <- aY [Year] + bR * RingWidth + bRY * RingWidth * Year,
    aY [Year] ~ dnorm (0, 1.5),
    c (bR, bRY) ~ dnorm (0, 0.3)
  ), data = H2Data, chains = 4, cores = 4, cmdstan = TRUE, log_lik = TRUE
)
#trankplot (H2m3)
#traceplot (H2m3)
#precis (H2m3, depth = 2, prob = 0.9)
set.seed (1353)
postH2m3 <- extract.samples (H2m3)
effect_3.5RWY25.5 <- 
  inv_logit (apply (postH2mRWY$aY [, IADFproneYearIndices], 1, mean) + 
             postH2m3$bR * 0.4775436 +
             postH2m3$bRY * 0.4775436) - 
  inv_logit (apply (postH2mRWY$aY [, IADFproneYearIndices], 1, mean) + 
             postH2m3$bR * -0.4780113 +
             postH2m3$bRY * -0.4780113)
mean (effect_3.5RWY25.5)
PI (effect_3.5RWY25.5, prob = 0.9)

set.seed (1353)
H2m4 <- ulam (
  alist (
    densityAnomaly ~ dbinom (1, p),
    logit (p) <- aY [Year] + aT [TOP] + bR * RingWidth + bRY * RingWidth * Year,
    aY [Year] ~ dnorm (0, 1.0),
    aT [TOP] ~ dnorm (0, 1.0),
    c (bR, bRY) ~ dnorm (0, 0.3)
  ), data = H2Data, chains = 4, cores = 4, cmdstan = TRUE, log_lik = TRUE
)
#trankplot (H2m4)
#traceplot (H2m4)
#precis (H2m4, depth = 2, prob = 0.9)
set.seed (1353)
H2m5 <- ulam (
  alist (
    densityAnomaly ~ dbinom (1, p),
    logit (p) <- aY [Year] + bR * RingWidth + bRY * RingWidth * Year + aB [BRANCH],
    aY [Year] ~ dnorm (0, 1.0),
    aB [BRANCH] ~ dnorm (0, 1.0),
    c (bR, bRY) ~ dnorm (0, 0.3)
  ), data = H2Data, chains = 4, cores = 4, cmdstan = TRUE, log_lik = TRUE
)
#trankplot (H2m5)
#traceplot (H2m5)
#precis (H2m5, depth = 2, prob = 0.9)
set.seed (1353)
H2m6 <- ulam (
  alist (
    densityAnomaly ~ dbinom (1, p),
    logit (p) <- aY [Year] + bR * RingWidth + bRY * RingWidth * Year + aT [TOP] + aB [BRANCH],
    aY [Year] ~ dnorm (0, 0.9),
    aB [BRANCH] ~ dnorm (0, 0.9),
    aT [TOP] ~ dnorm (0, 0.9),
    c (bR, bRY) ~ dnorm (0, 0.3)
  ), data = H2Data, chains = 4, cores = 4, cmdstan = TRUE, log_lik = TRUE
)
#trankplot (H2m6)
#traceplot (H2m6)
precis (H2m6, depth = 2, prob = 0.9)
set.seed (1353)
H2m7 <- ulam (
  alist (
    densityAnomaly ~ dbinom (1, p),
    logit (p) <- aY [Year] + bR * RingWidth + aT [TOP] + aB [BRANCH],
    aY [Year] ~ dnorm (0, 0.9),
    aB [BRANCH] ~ dnorm (0, 0.9),
    aT [TOP] ~ dnorm (0, 0.9),
    bR ~ dnorm (0, 0.3)
  ), data = H2Data, chains = 4, cores = 4, cmdstan = TRUE, log_lik = TRUE
)
#trankplot (H2m7)
#traceplot (H2m7)
precis (H2m7, depth = 2, prob = 0.9)
compare (H2mBRA, H2mTOP, H2mYEAR, H2mRW, H2m2, H2m3, H2m4, H2m5, H2m6, H2m7, func = 'WAIC')

# Calculate the effect on posterior probability of being near a branch ot at the top of 
# the tree in years with many IADFs
#----------------------------------------------------------------------------------------
set.seed (1353)
postH2 <- extract.samples (H2m6)
effect_TOP <- 
  inv_logit (apply (postH2$aY [, IADFproneYearIndices], 1, mean) + 
               postH2$bR * 0 + # mean ring width is 0 as the distribution was centred and scaled
               postH2$bRY * 0 * 1.0 + # mean ring width is 0 as the distribution was centred and scaled
               postH2$aB [,1] + 
               postH2$aT [,2]) - 
  inv_logit (apply (postH2$aY [, IADFproneYearIndices], 1, mean) + 
               postH2$bR * 0 + 
               postH2$bRY * 0 * 1.0 + 
               postH2$aB [,1] + 
               postH2$aT [,1])
mean (effect_TOP)
PI (effect_TOP, prob = 0.9)
effect_TOP <- 
  inv_logit (apply (postH2$aY [, -IADFproneYearIndices], 1, mean) + 
               postH2$bR * 0 + # mean ring width is 0 as the distribution was centred and scaled
               postH2$bRY * 0 * 1.0 + # mean ring width is 0 as the distribution was centred and scaled
               postH2$aB [,1] + 
               postH2$aT [,2]) - 
  inv_logit (apply (postH2$aY [, -IADFproneYearIndices], 1, mean) + 
               postH2$bR * 0 + 
               postH2$bRY * 0 * 1.0 + 
               postH2$aB [,1] + 
               postH2$aT [,1])
mean (effect_TOP)
PI (effect_TOP, prob = 0.9)
effect_BRA <- 
  inv_logit (apply (postH2$aY [, IADFproneYearIndices], 1, mean) + 
             postH2$bR * 0 + # mean ring width is 0 as the distribution was centred and scaled
             postH2$bRY * 0 * 1.0 + # mean ring width is 0 as the distribution was centred and scaled
             postH2$aB [,2] + 
             postH2$aT [,1]) - 
  inv_logit (apply (postH2$aY [, IADFproneYearIndices], 1, mean) + 
             postH2$bR * 0 + 
             postH2$bRY * 0 * 1.0 + 
             postH2$aB [,1] + 
             postH2$aT [,1])
mean (effect_BRA)
PI (effect_BRA, prob = 0.9)
effect_BRA <- 
  inv_logit (apply (postH2$aY [, -IADFproneYearIndices], 1, mean) + 
               postH2$bR * 0 + # mean ring width is 0 as the distribution was centred and scaled
               postH2$bRY * 0 * 1.0 + # mean ring width is 0 as the distribution was centred and scaled
               postH2$aB [,2] + 
               postH2$aT [,1]) - 
  inv_logit (apply (postH2$aY [, -IADFproneYearIndices], 1, mean) + 
               postH2$bR * 0 + 
               postH2$bRY * 0 * 1.0 + 
               postH2$aB [,1] + 
               postH2$aT [,1])
mean (effect_BRA)
PI (effect_BRA, prob = 0.9)
effect_RW <- 
  inv_logit (apply (postH2$aY [, IADFproneYearIndices], 1, mean) + 
               postH2$bR * 0.4775436 + 
               postH2$bRY * 0.4775436 * 1.0 + 
               postH2$aB [,1] + 
               postH2$aT [,1]) - 
  inv_logit (apply (postH2$aY [, IADFproneYearIndices], 1, mean) + 
               postH2$bR * -0.4780113 + 
               postH2$bRY * -0.4780113 * 1.0 + 
               postH2$aB [,1] + 
               postH2$aT [,1])
mean (effect_RW)
PI (effect_RW, prob = 0.9)

# Wrangle data into tibble for plotting
#----------------------------------------------------------------------------------------
dataF2 <- cbind (inv_logit (apply (postH2$aY [, IADFproneYearIndices], 1, mean) + 
                            postH2$bR * -0.4780113 + 
                            postH2$bRY * -0.4780113 * 1.0 + 
                            postH2$aB [,1] + 
                            postH2$aT [,1]),
                 inv_logit (apply (postH2$aY [, IADFproneYearIndices], 1, mean) + 
                            postH2$bR * 0.4775436 + 
                            postH2$bRY * 0.4775436 * 1.0 + 
                            postH2$aB [,1] + 
                            postH2$aT [,1]),
                 inv_logit (apply (postH2$aY [, -IADFproneYearIndices], 1, mean) + 
                            postH2$bR * -0.4780113 + 
                            postH2$bRY * -0.4780113 * 1.0 + 
                            postH2$aB [,1] + 
                            postH2$aT [,1]),
                 inv_logit (apply (postH2$aY [, -IADFproneYearIndices], 1, mean) + 
                            postH2$bR * 0.4775436 + 
                            postH2$bRY * 0.4775436 * 1.0 + 
                            postH2$aB [,1] + 
                            postH2$aT [,1]),
                 inv_logit (apply (postH2$aY [, IADFproneYearIndices], 1, mean) + 
                            postH2$bR * 0 + 
                            postH2$bRY * 0 * 1.0 + 
                            postH2$aB [,1] + 
                            postH2$aT [,1]),
                 inv_logit (apply (postH2$aY [, IADFproneYearIndices], 1, mean) + 
                            postH2$bR * 0 + 
                            postH2$bRY * 0 * 1.0 + 
                            postH2$aB [,2] + 
                            postH2$aT [,1]),
                 inv_logit (apply (postH2$aY [, -IADFproneYearIndices], 1, mean) + 
                            postH2$bR * 0 + 
                            postH2$bRY * 0 * 1.0 + 
                            postH2$aB [,1] + 
                            postH2$aT [,1]),
                 inv_logit (apply (postH2$aY [, -IADFproneYearIndices], 1, mean) + 
                            postH2$bR * 0 + 
                            postH2$bRY * 0 * 1.0 + 
                            postH2$aB [,2] + 
                            postH2$aT [,1]),
                 inv_logit (apply (postH2$aY [, IADFproneYearIndices], 1, mean) + 
                            postH2$bR * 0 + 
                            postH2$bRY * 0 * 1.0 + 
                            postH2$aB [,1] + 
                            postH2$aT [,1]),
                 inv_logit (apply (postH2$aY [, IADFproneYearIndices], 1, mean) + 
                            postH2$bR * 0 + 
                            postH2$bRY * 0 * 1.0 + 
                            postH2$aB [,1] + 
                            postH2$aT [,2]), 
                 inv_logit (apply (postH2$aY [, -IADFproneYearIndices], 1, mean) + 
                            postH2$bR * 0 + 
                            postH2$bRY * 0 * 1.0 + 
                            postH2$aB [,1] + 
                            postH2$aT [,1]),
                 inv_logit (apply (postH2$aY [, -IADFproneYearIndices], 1, mean) + 
                            postH2$bR * 0 + 
                            postH2$bRY * 0 * 1.0 + 
                            postH2$aB [,1] + 
                            postH2$aT [,2]))

# Plot the effect of ring width, TOP and BRANCH on the probability of occurence of IADFs 
#----------------------------------------------------------------------------------------
png (file = './fig/probabilityOfOccurrence.png', width = 700, height = 400)
vioplot (x = dataF2 [, 1], at = rep (1, 2000), xlim = c (0, 9), ylim = c (0, 1),
         xlab = '', ylab = '', 
         col = addOpacity (colours [1], 0.5), xaxt = 'n', yaxt = 'n', 
         border = colours [1], axes = FALSE,
         lineCol = '#222222', rectCol = '#444444', colMed = '#666666', lwd = 2)
axis (side = 2, las = 1)
mtext (side = 2, line = 3, text = 'Probability of occurrence')
vioplot (x = dataF2 [, 2], at = rep (2, 2000), 
         xlab = '', ylab = '', add = TRUE,
         col = addOpacity (colours [1], 0.5), xaxt = 'n', yaxt = 'n', 
         border = colours [1], axes = FALSE,
         lineCol = '#222222', rectCol = '#444444', colMed = '#666666', lwd = 2)
vioplot (x = dataF2 [, 3], at = rep (1, 2000), add = TRUE,
         col = addOpacity (colours [1], 0.1), 
         border = colours [1], axes = FALSE,
         lineCol = '#888888', rectCol = '#999999', colMed = '#aaaaaa', lwd = 2)
vioplot (x = dataF2 [, 4], at = rep (2, 2000), add = TRUE,
         col = addOpacity (colours [1], 0.1), 
         border = colours [1], axes = FALSE,
         lineCol = '#888888', rectCol = '#999999', colMed = '#aaaaaa', lwd = 2)
axis (side = 1, at = 1:2, labels = c ('3.5 mm', '5.5 mm'))
vioplot (x = dataF2 [, 5], at = rep (4, 2000), xlim = c (0, 14), ylim = c (0, 1),
         xlab = '', ylab = '', add = TRUE,
         col = addOpacity (colours [2], 0.4), xaxt = 'n', yaxt = 'n', 
         border = colours [2], axes = FALSE,
         lineCol = '#222222', rectCol = '#444444', colMed = '#666666', lwd = 2)
vioplot (x = dataF2 [, 6], at = rep (5, 2000), xlim = c (0, 14), ylim = c (0, 1),
         xlab = '', ylab = '', add = TRUE,
         col = addOpacity (colours [2], 0.4), xaxt = 'n', yaxt = 'n', 
         border = colours [2], axes = FALSE,
         lineCol = '#222222', rectCol = '#444444', colMed = '#666666', lwd = 2)
vioplot (x = dataF2 [, 7], at = rep (4, 2000), add = TRUE,
         col = addOpacity (colours [2], 0.1), 
         border = colours [2], axes = FALSE,
         lineCol = '#888888', rectCol = '#999999', colMed = '#aaaaaa', lwd = 2)
vioplot (x = dataF2 [, 8], at = rep (5, 2000), add = TRUE,
         col = addOpacity (colours [2], 0.1), 
         border = colours [2], axes = FALSE,
         lineCol = '#888888', rectCol = '#999999', colMed = '#aaaaaa', lwd = 2)
axis (side = 1, at = 4:5, labels = c ('No branch', 'Branch'))
vioplot (x = dataF2 [, 9], at = rep (7, 2000), xlim = c (0, 14), ylim = c (0, 1),
         xlab = '', ylab = '', add = TRUE,
         col = addOpacity (colours [3], 0.4), xaxt = 'n', yaxt = 'n', 
         border = colours [3], axes = FALSE,
         lineCol = '#222222', rectCol = '#444444', colMed = '#666666', lwd = 2)
vioplot (x = dataF2 [, 10], at = rep (8, 2000), xlim = c (0, 14), ylim = c (0, 1),
         xlab = '', ylab = '', add = TRUE,
         col = addOpacity (colours [3], 0.4), xaxt = 'n', yaxt = 'n', 
         border = colours [3], axes = FALSE,
         lineCol = '#222222', rectCol = '#444444', colMed = '#666666', lwd = 2)
vioplot (x = dataF2 [, 11], at = rep (7, 2000), add = TRUE,
         col = addOpacity (colours [3], 0.1), 
         border = colours [3], axes = FALSE,
         lineCol = '#888888', rectCol = '#999999', colMed = '#aaaaaa', lwd = 2)
vioplot (x = dataF2 [, 12], at = rep (8, 2000), add = TRUE,
         col = addOpacity (colours [3], 0.1), 
         border = colours [3], axes = FALSE,
         lineCol = '#888888', rectCol = '#999999', colMed = '#aaaaaa', lwd = 2)
axis (side = 1, at = 7:8, labels = c ('BH', 'Top'))
dev.off ()

# Calculate the effect on posterior probability of being near a branch ot at the top of 
# the tree in years with many IADFs
#----------------------------------------------------------------------------------------
set.seed (1353)
postH2 <- extract.samples (H2m7)

# Wrangle data into tibble for plotting
#----------------------------------------------------------------------------------------
dataF2 <- cbind (inv_logit (apply (postH2$aY [, IADFproneYearIndices], 1, mean) + 
                              postH2$bR * -0.4780113 + 
                              postH2$aB [,1] + 
                              postH2$aT [,1]),
                 inv_logit (apply (postH2$aY [, IADFproneYearIndices], 1, mean) + 
                              postH2$bR * 0.4775436 + 
                              postH2$aB [,1] + 
                              postH2$aT [,1]),
                 inv_logit (apply (postH2$aY [, -IADFproneYearIndices], 1, mean) + 
                              postH2$bR * -0.4780113 + 
                              postH2$aB [,1] + 
                              postH2$aT [,1]),
                 inv_logit (apply (postH2$aY [, -IADFproneYearIndices], 1, mean) + 
                              postH2$bR * 0.4775436 + 
                              postH2$aB [,1] + 
                              postH2$aT [,1]),
                 inv_logit (apply (postH2$aY [, IADFproneYearIndices], 1, mean) + 
                              postH2$bR * 0 + 
                              postH2$aB [,1] + 
                              postH2$aT [,1]),
                 inv_logit (apply (postH2$aY [, IADFproneYearIndices], 1, mean) + 
                              postH2$bR * 0 + 
                              postH2$aB [,2] + 
                              postH2$aT [,1]),
                 inv_logit (apply (postH2$aY [, -IADFproneYearIndices], 1, mean) + 
                              postH2$bR * 0 + 
                              postH2$aB [,1] + 
                              postH2$aT [,1]),
                 inv_logit (apply (postH2$aY [, -IADFproneYearIndices], 1, mean) + 
                              postH2$bR * 0 + 
                              postH2$aB [,2] + 
                              postH2$aT [,1]),
                 inv_logit (apply (postH2$aY [, IADFproneYearIndices], 1, mean) + 
                              postH2$bR * 0 + 
                              postH2$aB [,1] + 
                              postH2$aT [,1]),
                 inv_logit (apply (postH2$aY [, IADFproneYearIndices], 1, mean) + 
                              postH2$bR * 0 + 
                              postH2$aB [,1] + 
                              postH2$aT [,2]), 
                 inv_logit (apply (postH2$aY [, -IADFproneYearIndices], 1, mean) + 
                              postH2$bR * 0 + 
                              postH2$aB [,1] + 
                              postH2$aT [,1]),
                 inv_logit (apply (postH2$aY [, -IADFproneYearIndices], 1, mean) + 
                              postH2$bR * 0 + 
                              postH2$aB [,1] + 
                              postH2$aT [,2]))

# Plot the effect of ring width, TOP and BRANCH on the probability of occurence of IADFs 
#----------------------------------------------------------------------------------------
png (file = './fig/probabilityOfOccurrenceWithoutRYInteraction.png', width = 700, height = 400)
vioplot (x = dataF2 [, 1], at = rep (1, 2000), xlim = c (0, 9), ylim = c (0, 1),
         xlab = '', ylab = '', 
         col = addOpacity (colours [1], 0.5), xaxt = 'n', yaxt = 'n', 
         border = colours [1], axes = FALSE,
         lineCol = '#222222', rectCol = '#444444', colMed = '#666666', lwd = 2)
axis (side = 2, las = 1)
mtext (side = 2, line = 3, text = 'Probability of occurrence')
vioplot (x = dataF2 [, 2], at = rep (2, 2000), 
         xlab = '', ylab = '', add = TRUE,
         col = addOpacity (colours [1], 0.5), xaxt = 'n', yaxt = 'n', 
         border = colours [1], axes = FALSE,
         lineCol = '#222222', rectCol = '#444444', colMed = '#666666', lwd = 2)
vioplot (x = dataF2 [, 3], at = rep (1, 2000), add = TRUE,
         col = addOpacity (colours [1], 0.1), 
         border = colours [1], axes = FALSE,
         lineCol = '#888888', rectCol = '#999999', colMed = '#aaaaaa', lwd = 2)
vioplot (x = dataF2 [, 4], at = rep (2, 2000), add = TRUE,
         col = addOpacity (colours [1], 0.1), 
         border = colours [1], axes = FALSE,
         lineCol = '#888888', rectCol = '#999999', colMed = '#aaaaaa', lwd = 2)
axis (side = 1, at = 1:2, labels = c ('3.5 mm', '5.5 mm'))
vioplot (x = dataF2 [, 5], at = rep (4, 2000), xlim = c (0, 14), ylim = c (0, 1),
         xlab = '', ylab = '', add = TRUE,
         col = addOpacity (colours [2], 0.4), xaxt = 'n', yaxt = 'n', 
         border = colours [2], axes = FALSE,
         lineCol = '#222222', rectCol = '#444444', colMed = '#666666', lwd = 2)
vioplot (x = dataF2 [, 6], at = rep (5, 2000), xlim = c (0, 14), ylim = c (0, 1),
         xlab = '', ylab = '', add = TRUE,
         col = addOpacity (colours [2], 0.4), xaxt = 'n', yaxt = 'n', 
         border = colours [2], axes = FALSE,
         lineCol = '#222222', rectCol = '#444444', colMed = '#666666', lwd = 2)
vioplot (x = dataF2 [, 7], at = rep (4, 2000), add = TRUE,
         col = addOpacity (colours [2], 0.1), 
         border = colours [2], axes = FALSE,
         lineCol = '#888888', rectCol = '#999999', colMed = '#aaaaaa', lwd = 2)
vioplot (x = dataF2 [, 8], at = rep (5, 2000), add = TRUE,
         col = addOpacity (colours [2], 0.1), 
         border = colours [2], axes = FALSE,
         lineCol = '#888888', rectCol = '#999999', colMed = '#aaaaaa', lwd = 2)
axis (side = 1, at = 4:5, labels = c ('No branch', 'Branch'))
vioplot (x = dataF2 [, 9], at = rep (7, 2000), xlim = c (0, 14), ylim = c (0, 1),
         xlab = '', ylab = '', add = TRUE,
         col = addOpacity (colours [3], 0.4), xaxt = 'n', yaxt = 'n', 
         border = colours [3], axes = FALSE,
         lineCol = '#222222', rectCol = '#444444', colMed = '#666666', lwd = 2)
vioplot (x = dataF2 [, 10], at = rep (8, 2000), xlim = c (0, 14), ylim = c (0, 1),
         xlab = '', ylab = '', add = TRUE,
         col = addOpacity (colours [3], 0.4), xaxt = 'n', yaxt = 'n', 
         border = colours [3], axes = FALSE,
         lineCol = '#222222', rectCol = '#444444', colMed = '#666666', lwd = 2)
vioplot (x = dataF2 [, 11], at = rep (7, 2000), add = TRUE,
         col = addOpacity (colours [3], 0.1), 
         border = colours [3], axes = FALSE,
         lineCol = '#888888', rectCol = '#999999', colMed = '#aaaaaa', lwd = 2)
vioplot (x = dataF2 [, 12], at = rep (8, 2000), add = TRUE,
         col = addOpacity (colours [3], 0.1), 
         border = colours [3], axes = FALSE,
         lineCol = '#888888', rectCol = '#999999', colMed = '#aaaaaa', lwd = 2)
axis (side = 1, at = 7:8, labels = c ('BH', 'Top'))
dev.off ()

# Evaluate ring width effect if only breast height samples are considered
#----------------------------------------------------------------------------------------
muPrior <- longData %>% filter (!TOP & !BRANCH) %>% select (RingWidth) %>% 
  summarise (mean = mean (RingWidth, na.rm = TRUE)) %>% unlist ()
sigmaPrior <- longData %>% filter (!TOP & !BRANCH) %>% select (RingWidth) %>% 
  summarise (sd = sd (RingWidth, na.rm = TRUE)) %>% unlist ()
H2Data <- longData %>% filter (!TOP & !BRANCH) %>% 
  select (-TreeID, -WoodAge,-Pos1, -Pos2, -PosPer) %>%
  mutate (Year = as.integer (as_factor (Year)),
          TOP = as.integer (as_factor (TOP)),
          BRANCH = as.integer (as_factor (BRANCH)),
          RingWidth = (RingWidth - muPrior [[1]]) / sigmaPrior [[1]]) %>% 
  drop_na ()

# Fit a logistic model to test whether ring widths, year of occurrence, being at the top 
# of the tree and/or near a branch affects the likelihood of formation of an IADFS
#----------------------------------------------------------------------------------------
set.seed (1353)
H2mYEAR <- ulam (
  alist (
    densityAnomaly ~ dbinom (1, p),
    logit (p) <-  aY [Year],
    aY [Year] ~ dnorm (0, 1.5) # this is a pretty flat (uninformative prior)
  ), data = H2Data, chains = 4, cores = 4, cmdstan = TRUE, log_lik = TRUE
)
#trankplot (H2mYEAR)
#traceplot (H2mYEAR)
#precis (H2mYEAR, depth = 2, prob = 0.9)
set.seed (1353)
H2mRW <- ulam (
  alist (
    densityAnomaly ~ dbinom (1, p),
    logit (p) <- bR * RingWidth,
    bR ~ dnorm (0, 0.5)
  ), data = H2Data, chains = 4, cores = 4, cmdstan = TRUE, log_lik = TRUE
)
#trankplot (H2mRW)
#traceplot (H2mRW)
#precis (H2mRW, depth = 2, prob = 0.9)
set.seed (1353)
postH2mRW <- extract.samples (H2mRW)
effect_3.5RW5.5 <- 
  inv_logit (postH2mRW$bR * 0.4775436) - 
  inv_logit (postH2mRW$bR * -0.4780113)
mean (effect_3.5RW5.5)
PI (effect_3.5RW5.5, prob = 0.9)

set.seed (1353)
H2m2 <- ulam (
  alist (
    densityAnomaly ~ dbinom (1, p),
    logit (p) <- aY [Year] + bR * RingWidth,
    aY [Year] ~ dnorm (0, 1.5),
    bR ~ dnorm (0, 0.5)
  ), data = H2Data, chains = 4, cores = 4, cmdstan = TRUE, log_lik = TRUE
)
#trankplot (H2m2)
#traceplot (H2m2)
#precis (H2m2, depth = 2, prob = 0.9)
set.seed (1353)
postH2mRWY <- extract.samples (H2m2)
effect_3.5RWY5.5 <- 
  inv_logit (apply (postH2mRWY$aY [, IADFproneYearIndices], 1, mean) + 
               postH2mRWY$bR * 0.4775436) - 
  inv_logit (apply (postH2mRWY$aY [, IADFproneYearIndices], 1, mean) + 
               postH2mRWY$bR * -0.4780113)
mean (effect_3.5RWY5.5)
PI (effect_3.5RWY5.5, prob = 0.9)
effect_3.5RWY5.5 <- 
  inv_logit (apply (postH2mRWY$aY [, -IADFproneYearIndices], 1, mean) + 
               postH2mRWY$bR * 0.4775436) - 
  inv_logit (apply (postH2mRWY$aY [, -IADFproneYearIndices], 1, mean) + 
               postH2mRWY$bR * -0.4780113)
mean (effect_3.5RWY5.5)
PI (effect_3.5RWY5.5, prob = 0.9)

set.seed (1353)
H2m3 <- ulam (
  alist (
    densityAnomaly ~ dbinom (1, p),
    logit (p) <- aY [Year] + bR * RingWidth + bRY * RingWidth * Year,
    aY [Year] ~ dnorm (0, 1.5),
    c (bR, bRY) ~ dnorm (0, 0.3)
  ), data = H2Data, chains = 4, cores = 4, cmdstan = TRUE, log_lik = TRUE
)
#trankplot (H2m3)
#traceplot (H2m3)
#precis (H2m3, depth = 2, prob = 0.9)
compare (H2m3, H2m2, H2mRW, H2mYEAR)

#----------------------------------------------------------------------------------------
# Wrangle data to test (H3) about difference in the circumferential arc and/or the length
# of the arc of IADFs at breast height versus at the top of the tree
#----------------------------------------------------------------------------------------
temp <- data [c (1:2, 6:9, 46:47)] %>% 
  pivot_longer (cols = 7:8, names_to = 'h', values_to = 'Arc') %>%
  drop_na (Arc) %>%
  mutate (Year = as.integer (as_factor (Year)),
          TreeID = as.integer (as_factor (TreeID)), # NB. 1 is Top and 2 is BH
          h = as.integer (as_factor (ifelse (h == 'ArcBH', 'BH', 'TOP')))) 
muPrior <- temp %>% select (Arc) %>% summarize (muPior = mean (Arc)) %>% unlist ()
sigmaPrior <- temp %>% select (Arc) %>% summarize (sigmaPior = sd (Arc)) %>% unlist ()
H3Data <- temp %>% rowwise () %>% 
  mutate (RingWidthBH = mean (c (RingWidthBH_1, RingWidthBH_2), na.rm = TRUE),
          RingWidth2010 = mean (c (RingWidth2010_1, RingWidth2010_2), na.rm = TRUE)) %>%
  mutate (RingWidth = ifelse (h == 'TOP', RingWidth2010, RingWidthBH)) %>%
  mutate (Len = Arc * (pi / 180) * RingWidth) %>% # length in 
  mutate (ArcStd = (Arc - muPrior) / sigmaPrior) %>%
  select (Year, TreeID, h, Arc, ArcStd, Len, RingWidth) 
muLen <- mean (H3Data$Len)
sigmaLen <- sd (H3Data$Len)
H3Data <- H3Data %>% mutate (LenStd = (Len - muLen) / sigmaLen) %>% ungroup ()

# Simulate some data to make sure the test works fine
#----------------------------------------------------------------------------------------
set.seed (1353)
N <- 1e2
kappaA <- rexp (n = N, rate = 1)
h <- sample.int (n = 2, size = N, replace = TRUE)
aH <- rnorm (n = N, mean = 1.74, sd = 0.34)
a0 <- rnorm (n = N, mean = 1.74, sd = 0.34)
muA <- a0 + aH *(h-1)
Arc <- rnorm (n = N, mean = muA, sd = sigmaA)

H3m1 <- stan (
  file = 'codeH3m1.stan', # Stan program
  data = list (h = h, Arc = Arc, N = N), # named list of data with arc in radians from -pi to pi
  chains = 1,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  cores = 1,              # number of cores (could use one per chain)
  refresh = 0             # no progress shown
)
precis (H3m1, depth = 2, prob = 0.9)
# Assuming that the model assumptions are correct, the model works fine with sample sizes 
# of 1e4 but starts to struggle around 1e3  and below that even with a difference of 100 
# degrees between heights.

# Test hypothesis (H3) about the conservation of the circumferential arc of IADFs
#----------------------------------------------------------------------------------------
set.seed (42)
H3m1 <- stan (
  file = 'codeH3m1.stan', # Stan program
  data = list (h = H3Data %>% select (h) %>%
    mutate (h = as.integer (ifelse (h == 'TOP', 2, 1))) %>% unlist (), 
               Arc = H3Data %>% select (Arc) %>%
      mutate (Arc = Arc / 180 * pi - pi) %>% unlist (),
            N = dim (H3Data) [1]), # named list of data with arc in radians from -pi to pi
  chains = 4,             # number of Markov chains
  warmup = 3000,          # number of warmup iterations per chain
  iter = 8000,            # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  refresh = 0             # no progress shown
)
#trankplot (H3m1)
#traceplot (H3m1)
precis (H3m1, depth = 2)
# Conclusion: We really have too few samples to reliably estimate the posterior 
# distribution for a von Mises distribution!

# Test hypothesis (H3) about the conservation of the length of arc of IADFs
#----------------------------------------------------------------------------------------
H3m0 <- ulam (
  alist (
    LenStd ~ dnorm (muA, sigmaA),
    muA <- aH [h],
    aH [h] ~ dnorm (0, 10),
    sigmaA ~ dexp (1)
  ), data = H3Data, chains = 4, cores = 4, cmdstan = TRUE, iter = 4000, warmup = 2000,
)
trankplot (H3m0)
traceplot (H3m0)
precis (H3m0, depth = 2)
H3m1 <- ulam (
  alist (
    LenStd ~ dnorm (muA, sigmaA),
    muA <- aY [Year] + aH [h],
    aY [Year] ~ dnorm (0, 10),
    aH [h] ~ dnorm (0, 10),
    sigmaA ~ dexp (1)
  ), data = H3Data, chains = 4, cores = 4, cmdstan = TRUE, iter = 4000, warmup = 2000,
)
trankplot (H3m1)
traceplot (H3m1)
precis (H3m1, depth = 2)
H3m2 <- ulam (
  alist (
    LenStd ~ dnorm (muA, sigmaA),
    muA <- aT [TreeID] + aH [h],
    aT [TreeID] ~ dnorm (0, 10),
    aH [h] ~ dnorm (0, 10),
    sigmaA ~ dexp (1)
  ), data = H3Data, chains = 4, cores = 4, cmdstan = TRUE, iter = 4000, warmup = 2000,
)
trankplot (H3m2)
traceplot (H3m2)
precis (H3m2, depth = 2)
set.seed (40)
H3m3 <- ulam (
  alist (
    LenStd ~ dnorm (muA, sigmaA),
    muA <- aT [TreeID] + aY [Year] + aH [h],
    aT [TreeID] ~ dnorm (0, 10),
    aY [Year] ~ dnorm (0, 10),
    aH [h] ~ dnorm (0, 10),
    sigmaA ~ dexp (1)
  ), data = H3Data, chains = 4, cores = 4, cmdstan = TRUE, iter = 4000, warmup = 2000,
)
trankplot (H3m3)
traceplot (H3m3)
precis (H3m3, depth = 2)

# Calculate data mean by height
#----------------------------------------------------------------------------------------
H3Data %>% group_by (h) %>% 
  summarise (meanArc = mean (Arc), 
             sdArc = sd (Arc),
             PIArcLow = PI (Arc, prob = 0.9) [1], 
             PIArcUpp = PI (Arc, prob = 0.9) [2], 
             meanLen = mean (Len), 
             sdLen = sd (Len),
             PILenLow = PI (Len, prob = 0.9) [1], 
             PILenUpp = PI (Len, prob = 0.9) [2])

# Calculate data mean by tree and year
#----------------------------------------------------------------------------------------
H3Data %>% group_by (TreeID) %>% summarise (meanArc = mean (Arc), sdArc = sd (Arc))
H3Data %>% group_by (Year) %>% summarise (meanArc = mean (Arc), sdArc = sd (Arc))
H3Data %>% group_by (TreeID, Year) %>% summarise (meanArc = mean (Arc), sdArc = sd (Arc))

# Median number of anomalies per year
#----------------------------------------------------------------------------------------
longData %>% group_by (Year) %>%
  summarise (sumDA = sum (densityAnomaly), n = sum (!is.na (densityAnomaly))) %>% 
  mutate (perc = sumDA * 100 / n) %>% select (perc) %>% summarise (median = median (perc), mean = mean (perc))
longData %>% group_by (Year) %>% filter (Year %in% c (1999, 2002, 2012, 2013, 2016)) %>%
  summarise (sumDA = sum (densityAnomaly), n = sum (!is.na (densityAnomaly))) %>% 
  mutate (perc = sumDA * 100 / n) %>% select (perc) %>% summarise (median = median (perc), mean = mean (perc))

#========================================================================================
