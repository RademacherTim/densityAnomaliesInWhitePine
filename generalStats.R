#========================================================================================
# Script to get general stats about the occurence of density anomalies in white pine. 
# Results are mainly using binomial logistic regression. 
#----------------------------------------------------------------------------------------

# Load dependencies
#----------------------------------------------------------------------------------------
library ('caTools')
library ('glmnet')
library ('dominanceanalysis')
library ('boot')
library ('lme4')

# Download and read the data
#----------------------------------------------------------------------------------------
source ('readGrowthData.R')

# Load colour scheme for at reast height, near a branch whorl and in the 2010 section
#----------------------------------------------------------------------------------------
source ('plotingFunctions.R')

# Wrangle data into long format with all possible explanatory variable for presence data
#----------------------------------------------------------------------------------------
temp1 <- data %>% filter (Year < 2017) %>% filter (DABH_1 %in% 1:2) %>% 
  mutate (WoodAge = WoodAgeBH, RingWidth = RingWidthBH_1, TOP = FALSE, BRANCH = FALSE, 
          densityAnomaly = TRUE)
temp2 <- data %>% filter (Year < 2017) %>% filter (DABH_2 %in% 1:2) %>% 
  mutate (WoodAge = WoodAgeBH, RingWidth = RingWidthBH_2, TOP = FALSE, BRANCH = FALSE, 
          densityAnomaly = TRUE)
temp3 <- data %>% filter (Year < 2017) %>% filter (DABranch_1 %in% 1:2) %>% 
  mutate (WoodAge = NA, RingWidth = RingWidthNearBranch_1, TOP = FALSE, BRANCH = TRUE, 
          densityAnomaly = TRUE)
temp4 <- data %>% filter (Year < 2017) %>% filter (DABranch_2 %in% 1:2) %>% 
  mutate (WoodAge = NA, RingWidth = RingWidthNearBranch_2, TOP = FALSE, BRANCH = TRUE, 
          densityAnomaly = TRUE)
temp5 <- data %>% filter (Year < 2017) %>% filter (DA2010_2 %in% 1:2) %>% 
  mutate (WoodAge = WoodAge2010, RingWidth = RingWidth2010_1, TOP = TRUE, BRANCH = FALSE, 
          densityAnomaly = TRUE)
temp6 <- data %>% filter (Year < 2017) %>% filter (DA2010_2 %in% 1:2) %>% 
  mutate (WoodAge = WoodAge2010, RingWidth = RingWidth2010_2, TOP = TRUE, BRANCH = FALSE, 
          densityAnomaly = TRUE)
temp <- rbind (temp1, temp2, temp3, temp4, temp5, temp6)
rm (temp1, temp2, temp3, temp4, temp5, temp6)


# Wrangle data into long format with all possible explanatory variable for absence data
#----------------------------------------------------------------------------------------
tmp1 <- data %>% filter (Year < 2017) %>% filter (DABH_1 == 0) %>% 
  mutate (WoodAge = WoodAgeBH, RingWidth = RingWidthBH_1, TOP = FALSE, BRANCH = FALSE, 
          densityAnomaly = FALSE)
tmp2 <- data %>% filter (Year < 2017) %>% filter (DABH_2 == 0) %>% 
  mutate (WoodAge = WoodAgeBH, RingWidth = RingWidthBH_2, TOP = FALSE, BRANCH = FALSE, 
          densityAnomaly = TRUE)
tmp3 <- data %>% filter (Year < 2017) %>% filter (DABranch_1 == 0) %>% 
  mutate (WoodAge = NA, RingWidth = RingWidthNearBranch_1, TOP = FALSE, BRANCH = TRUE, 
          densityAnomaly = TRUE)
tmp4 <- data %>% filter (Year < 2017) %>% filter (DABranch_2 == 0) %>% 
  mutate (WoodAge = NA, RingWidth = RingWidthNearBranch_2, TOP = FALSE, BRANCH = TRUE, 
          densityAnomaly = TRUE)
tmp5 <- data %>% filter (Year < 2017) %>% filter (DA2010_2 == 0) %>% 
  mutate (WoodAge = WoodAge2010, RingWidth = RingWidth2010_1, TOP = TRUE, BRANCH = FALSE, 
          densityAnomaly = TRUE)
tmp6 <- data %>% filter (Year < 2017) %>% filter (DA2010_2 == 0) %>% 
  mutate (WoodAge = WoodAge2010, RingWidth = RingWidth2010_2, TOP = TRUE, BRANCH = FALSE, 
          densityAnomaly = TRUE)
tmp <- rbind (tmp1, tmp2, tmp3, tmp4, tmp5, tmp6)
rm (tmp1, tmp2, tmp3, tmp4, tmp5, tmp6)

# Combine presence and absence data
#----------------------------------------------------------------------------------------
longData <- rbind (temp, tmp) %>% select (densityAnomaly, Year, TreeID, WoodAge, 
                                          RingWidth, TOP, BRANCH)

# Create comprehensive logistic model to test for various effects
#----------------------------------------------------------------------------------------
modDensityAnomalyOccurence <- glm (densityAnomaly ~ factor (Year) + factor (TreeID) + 
                                   WoodAge + RingWidth + TOP + BRANCH, data = longData, 
                                   family = binomial (link = 'logit'))
summary (modDensityAnomalyOccurence)
anova (modDensityAnomalyOccurence, test = 'Chisq') # Look at ANOVA results to test for 
                                                   # importance of individual explanatory
                                                   # variables.

# Alternatively use elastic net parameter shrinkage to select best model 
#----------------------------------------------------------------------------------------
set.seed (42)
modMat <- model.matrix (densityAnomaly ~ factor (Year) + factor (TreeID) + WoodAge + 
                        RingWidth + TOP + BRANCH, data = longData)
cv.elasticNet <- cv.glmnet (x = modMat, 
                            y = as.matrix (filter (select (longData, densityAnomaly), 
                                                  !is.na (longData [['RingWidth']]),
                                                  !is.na (longData [['WoodAge']]))), 
                            alpha = 0, family = 'binomial')
coef (cv.elasticNet, cv.elasticNet [['lambda.1se']])

# Analyse dominance of explanatory variables
#----------------------------------------------------------------------------------------
daDensityAnomalyOccurence <- dominanceAnalysis (modDensityAnomalyOccurence)
getFits (daDensityAnomalyOccurence, "r2.m")
dominanceMatrix (daDensityAnomalyOccurence, type = 'complete', fit.functions = "r2.m", 
                 ordered = TRUE)
# Year dominantes TreeID, RingWidth, being at the top of the tree
# WoodAge dominantes being near a branch
plot (daDensityAnomalyOccurence, which.graph = 'conditional', fit.function = "r2.m")
averageContribution (daDensityAnomalyOccurence, fit.functions = "r2.m")
plot (daDensityAnomalyOccurence, which.graph = 'general', fit.function = "r2.m")
dominanceMatrix (daDensityAnomalyOccurence, type = 'general', fit.functions = "r2.m", 
                 ordered = TRUE)

bootModDensityAnomalies <- bootDominanceAnalysis (modDensityAnomalyOccurence, R = 1000)
summary (bootModDensityAnomalies, fit.functions = "r2.m")

#========================================================================================