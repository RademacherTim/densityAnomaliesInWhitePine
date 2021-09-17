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
plot (x = precis (H1m1, depth = 2)$mean [1:41] * sigmaPrior + muPrior,
      y = 1:41,
      ylab = '',
      xlab = '', las = 1, xlim = c (0, 100), 
      col = '#F38D48', pch = 19, axes = FALSE)
abline (v = muPost, lwd = 2, col = '#66666666')
rect (xleft = PIPost [1], xright = PIPost [2], ybottom = 0, ytop = 42, 
      col = '#aaaaaa22', lty = 0)
segments (x0 = precis (H1m1, depth = 2) [1:41,3] * sigmaPrior + muPrior,
          x1 = precis (H1m1, depth = 2) [1:41,4] * sigmaPrior + muPrior,
          y0 = 1:41, col = '#F38D48')
points (x = precis (H1m1, depth = 2)$mean [1:41] * sigmaPrior + muPrior,
        y = 1:41, pch = 19, col = '#F38D48')
axis (side = 1)
axis (side = 2, las = 1)
dev.off ()

# Plot posterior mean and 89% compatibility interval for year intercepts
#----------------------------------------------------------------------------------------
png (file = './fig/MeanPositionByYear.png', width = 600, height = 400)
par (mfrow = c (1, 1), mar = c (4, 4, 1, 1))
plot (x = precis (H1m1, depth = 2)$mean [42:58] * sigmaPrior + muPrior,
      y = levels (H1Data$Year),
      ylab = '',
      xlab = '', las = 1, xlim = c (0, 100), 
      col = '#F38D48', pch = 19, axes = FALSE)
abline (v = muPost, lwd = 2, col = '#66666666')
rect (xleft = PIPost [1], xright = PIPost [2], ybottom = 1998, ytop = 2017, 
      col = '#aaaaaa22', lty = 0)
segments (x0 = precis (H1m1, depth = 2) [42:58,3] * sigmaPrior + muPrior,
          x1 = precis (H1m1, depth = 2) [42:58,4] * sigmaPrior + muPrior,
          y0 = as.numeric (levels (H1Data$Year)), col = '#F38D48')
points (x = precis (H1m1, depth = 2)$mean [42:58] * sigmaPrior + muPrior,
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
  summarise (mean = mean (RingWidth, na.rm = TRUE))
sigmaPrior <- longData %>% select (RingWidth) %>% 
  summarise (sd = sd (RingWidth, na.rm = TRUE))
H2Data <- longData %>% select (-TreeID, -WoodAge,-Pos1, -Pos2, -PosPer) %>%
  mutate (Year = as_factor (Year),
          TOP = as_factor (TOP),
          BRANCH = as_factor (BRANCH),
          RingWidth = (RingWidth - muPrior [[1]]) / sigmaPrior [[1]]) %>% 
  drop_na ()

# Fit a logistic model to test whether ring widths, year of occurrence, being at the top 
# of the tree and/or near a branch affects the likelihood of formation of an IADFS
#----------------------------------------------------------------------------------------
set.seed (1353)
H2m1 <- ulam (
  alist (
    densityAnomaly ~ dbinom (1, p),
    logit (p) <-  aY [Year],
    aY [Year] ~ dnorm (0, 10)
  ), data = H2Data, chains = 4, cores = 4, cmdstan = TRUE, log_lik = TRUE
)
#trankplot (H2m1)
#traceplot (H2m1)
#precis (H2m1, depth = 2)
set.seed (1353)
H2m2 <- ulam (
  alist (
    densityAnomaly ~ dbinom (1, p),
    logit (p) <- aY [Year] + bR * RingWidth,
    aY [Year] ~ dnorm (0, 10),
    c (bR) ~ dnorm (0, 10)
  ), data = H2Data, chains = 4, cores = 4, cmdstan = TRUE, log_lik = TRUE
)
#trankplot (H2m2)
#traceplot (H2m2)
#precis (H2m2, depth = 2)
set.seed (1353)
H2m3 <- ulam (
  alist (
    densityAnomaly ~ dbinom (1, p),
    logit (p) <- aY [Year] + bR * RingWidth + bRY * RingWidth * Year,
    aY [Year] ~ dnorm (0, 10),
    c (bR, bRY) ~ dnorm (0, 10)
  ), data = H2Data, chains = 4, cores = 4, cmdstan = TRUE, log_lik = TRUE
)
#trankplot (H2m3)
#traceplot (H2m3)
#precis (H2m3, depth = 2)
set.seed (1353)
H2m4 <- ulam (
  alist (
    densityAnomaly ~ dbinom (1, p),
    logit (p) <- aY [Year] + bR * RingWidth + bRY * RingWidth * Year + bT * TOP,
    aY [Year] ~ dnorm (0, 10),
    c (bR, bRY, bT) ~ dnorm (0, 10)
  ), data = H2Data, chains = 4, cores = 4, cmdstan = TRUE, log_lik = TRUE
)
#trankplot (H2m4)
#traceplot (H2m4)
#precis (H2m4, depth = 2)
H2m5 <- ulam (
  alist (
    densityAnomaly ~ dbinom (1, p),
    logit (p) <- aY [Year] + bR * RingWidth + bRY * RingWidth * Year + bB * BRANCH,
    aY [Year] ~ dnorm (0, 10),
    c (bR, bRY, bB) ~ dnorm (0, 10)
  ), data = H2Data, chains = 4, cores = 4, cmdstan = TRUE, log_lik = TRUE
)
#trankplot (H2m5)
#traceplot (H2m5)
#precis (H2m5, depth = 2)
H2m6 <- ulam (
  alist (
    densityAnomaly ~ dbinom (1, p),
    logit (p) <- aY [Year] + bR * RingWidth + bRY * RingWidth * Year + bT * TOP + bB * BRANCH,
    aY [Year] ~ dnorm (0, 10),
    c (bR, bRY, bT, bB) ~ dnorm (0, 10)
  ), data = H2Data, chains = 4, cores = 4, cmdstan = TRUE, log_lik = TRUE
)
#trankplot (H2m6)
#traceplot (H2m6)
precis (H2m6, depth = 2)
compare (H2m1, H2m2, H2m3, H2m4, H2m5, H2m6, func = 'WAIC')

# 
#----------------------------------------------------------------------------------------

# Wrangle data to test (H3) about the circumferential arc of IADFs
#----------------------------------------------------------------------------------------
temp <- data [c (1:2, 6:9, 46:47)] %>% 
  pivot_longer (cols = 7:8, names_to = 'h', values_to = 'Arc') %>%
  drop_na (Arc) %>%
  mutate (Year = as_factor (Year),
          TreeID = as_factor (TreeID),
          h = as_factor (ifelse (h == 'ArcBH', 'BH', 'TOP'))) 
muPrior <- temp %>% select (Arc) %>% summarize (muPior = mean (Arc)) %>% unlist ()
sigmaPrior <- temp %>% select (Arc) %>% summarize (sigmaPior = sd (Arc)) %>% unlist ()
H3Data <- temp %>% rowwise () %>% 
  mutate (RingWidthBH = mean (c (RingWidthBH_1, RingWidthBH_2), na.rm = TRUE),
          RingWidth2010 = mean (c (RingWidth2010_1, RingWidth2010_2), na.rm = TRUE)) %>%
  mutate (RingWidth = ifelse (h == 'TOP', RingWidth2010, RingWidthBH)) %>%
  mutate (Len = Arc * (pi / 180) * RingWidth) %>%
  mutate (ArcStd = (Arc - muPrior) / sigmaPrior) %>%
  select (Year, TreeID, h, Arc, ArcStd, Len, RingWidth) 
muLen <- mean (H3Data$Len)
sigmaLen <- sd (H3Data$Len)
H3Data <- H3Data %>% mutate (LenStd = (Len - muLen) / sigmaLen) %>% ungroup ()

# Test hypothesis (H3) about the conservation of the circumferential arc of IADFs
#----------------------------------------------------------------------------------------
# TR - Need to constrain the distribution because at the moment the posterior 
# distribution has angles larger than 360 degrees for some years!!!
set.seed (42)
H3m1 <- stan (
  file = 'codeH3m1.stan', # Stan program
  data = H3Data %>% select (-Year, -TreeID, -ArcStd, -Len, -LenStd, -RingWidth) %>%
    mutate (h = ifelse (h == 'TOP', 1, 2), 
            Arc = (Arc * pi / 180) - pi), # named list of data with arc in radians from -pi to pi
  chains = 4,             # number of Markov chains
  warmup = 2000,          # number of warmup iterations per chain
  iter = 4000,            # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  refresh = 0             # no progress shown
)
postArc <- extract.samples (H3m1)
print(H3m1, probs = c(.05,.95), digits=3)
trankplot (H3m1)
traceplot (H3m1)
precis (H3m1, depth = 2)

H3m1years <- ulam (
  alist (
    ArcStd ~ dnorm (muA, sigmaA),
    muA <- a0 [Year] + aH [h],
    a0 [Year] ~ dnorm (0, 10),
    bH ~ dnorm (0, 10),
    sigmaA ~ dexp (1)
  ), data = H3Data, chains = 4, cores = 4, cmdstan = TRUE, log_lik = TRUE
)
H3m1trees <- ulam (
  alist (
    ArcStd ~ dnorm (muA, sigmaA),
    muA <- a0 [TreeID] + aH * [h],
    a0 [TreeID] ~ dnorm (0, 10),
    bH ~ dnorm (0, 10),
    sigmaA ~ dexp (1)
  ), data = H3Data, chains = 4, cores = 4, cmdstan = TRUE, log_lik = TRUE
)
compare (H3m1, H3m1years, H3m1trees, func = 'WAIC')
# We tested whether this remains true if either TreeID or Year are included as intercepts 
# (commentted lines of code), but neither changes the results qualitatively.
trankplot (H3m1years)
traceplot (H3m1years)
precis (H3m1years, depth = 2)
precis (H3m1trees, depth = 2)

# Test hypothesis (H3) about the conservation of the length of arc of IADFs
#----------------------------------------------------------------------------------------
H3m2years <- ulam (
  alist (
    LenStd ~ dnorm (muA, sigmaA),
    muA <- a0 [Year] + bH * h,
    a0 [Year] ~ dnorm (0, 10),
    bH ~ dnorm (0, 10),
    sigmaA ~ dexp (1)
  ), data = H3Data, chains = 4, cores = 4, cmdstan = TRUE,
)
trankplot (H3m2years)
traceplot (H3m2years)
precis (H3m2years, depth = 2)

# Extract posterior distributions and to draw figures of arc and length of arc
#----------------------------------------------------------------------------------------
postArc <- extract.samples (H3m1years)
postLen <- extract.samples (H3m2years)


# Median number of anomalies per year
#----------------------------------------------------------------------------------------
longData %>% group_by (Year) %>%
  summarise (sumDA = sum (densityAnomaly), n = sum (!is.na (densityAnomaly))) %>% 
  mutate (perc = sumDA * 100 / n) %>% select (perc) %>% summarise (median = median (perc), mean = mean (perc))
longData %>% group_by (Year) %>% filter (Year %in% c (1999, 2002, 2012, 2013, 2016)) %>%
  summarise (sumDA = sum (densityAnomaly), n = sum (!is.na (densityAnomaly))) %>% 
  mutate (perc = sumDA * 100 / n) %>% select (perc) %>% summarise (median = median (perc), mean = mean (perc))

#========================================================================================
