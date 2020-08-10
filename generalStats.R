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
longData <- rbind (temp, tmp) %>% select (densityAnomaly, Year, TreeID, 
                                          RingWidth, TOP, BRANCH)

# Create comprehensive logistic model to test for various effects
#----------------------------------------------------------------------------------------
modDensityAnomalyOccurence <- glm (densityAnomaly ~ factor (Year) + factor (TreeID) + 
                                   RingWidth + TOP + BRANCH, data = longData, 
                                   family = binomial (link = 'logit'))
summary (modDensityAnomalyOccurence)
anova (modDensityAnomalyOccurence, test = 'Chisq') # Look at ANOVA results to test for 
                                                   # importance of individual explanatory
                                                   # variables.

# Alternatively use elastic net parameter shrinkage to select best model 
#----------------------------------------------------------------------------------------
set.seed (42)
modMat <- model.matrix (densityAnomaly ~ factor (Year) + factor (TreeID) + 
                        RingWidth + TOP + BRANCH, data = longData)
cv.elasticNet <- cv.glmnet (x = modMat, 
                            y = as.matrix (filter (select (longData, densityAnomaly), 
                                                  !is.na (longData [['RingWidth']]))), 
                            alpha = 1.0, family = 'binomial')
coef (cv.elasticNet, cv.elasticNet [['lambda.1se']])
# alpha = 0 basically means this is a ridge regression to choose a sparse model
# alpha = 1 basically lasso which helps to group

# Analyse dominance of explanatory variables
#----------------------------------------------------------------------------------------
daDensityAnomalyOccurence <- dominanceAnalysis (modDensityAnomalyOccurence)
getFits (daDensityAnomalyOccurence, "r2.m")
dominanceMatrix (daDensityAnomalyOccurence, type = 'complete', fit.functions = "r2.m", 
                 ordered = TRUE)
# Being near a branch dominates all other factors
# Year dominantes TreeID, RingWidth, being at the top of the tree
# Being at top of a tree dominantes ring width
# Ring width and tree id do not dominante over anything else, but ring width has a 
# slightly higher r2.m when all factors are included.
plot (daDensityAnomalyOccurence, which.graph = 'conditional', fit.function = "r2.m")
averageContribution (daDensityAnomalyOccurence, fit.functions = "r2.m")
plot (daDensityAnomalyOccurence, which.graph = 'general', fit.function = "r2.m")
dominanceMatrix (daDensityAnomalyOccurence, type = 'general', fit.functions = "r2.m", 
                 ordered = TRUE)

bootModDensityAnomalies <- bootDominanceAnalysis (modDensityAnomalyOccurence, R = 1000)
summary (bootModDensityAnomalies, fit.functions = "r2.m")
# Being near a branch completely dominates all other factors
# Year completely dominantes TreeID, RingWidth, being at the top of the tree

# Test whther ring width is greater in high-frequency years for ring with density anomaly
#----------------------------------------------------------------------------------------
t.test (RingWidth~densityAnomaly, alternative = 'less', 
        data = longData %>%  filter (Year %in% c (1999, 2002, 2012, 2013, 2016)))
longData %>% filter (Year < 2017) %>% 
  group_by (densityAnomaly) %>% summarise (meanRingWidth = mean (RingWidth, na.rm = TRUE))
longData %>% filter (Year %in% c (1999, 2002, 2012, 2013, 2016)) %>% 
  group_by (densityAnomaly) %>% summarise (meanRingWidth = mean (RingWidth, na.rm = TRUE))
#========================================================================================
