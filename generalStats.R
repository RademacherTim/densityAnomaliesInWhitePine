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


# Run test to see differences in density anomaly position within the ring between groups 
#----------------------------------------------------------------------------------------

# Wrangle data for ANOVA
#--------------------------------------------------------------------------------------
for (years in c ('all','select')) {
  if (years == 'all') Years <- 1993:2017
  if (years == 'select') Years <- c (1999, 2002, 2012, 2013, 2016) 
  
  # Wrangle data to get density distribution for when they occur
  #----------------------------------------------------------------------------------------
  temp1  <- data %>% filter (Year %in% Years) %>% filter (DABH_1 == 1) %>% 
    mutate (perc = PercentageDABH_1.1) %>% 
    select (perc) %>% filter (!is.na (perc)) %>% 
    mutate (years = years, height = 'BH')
  temp2  <- data %>% filter (Year %in% Years)%>% filter (DABH_2 == 1) %>%
    mutate (perc = PercentageDABH_2.1) %>% 
    select (perc) %>% filter (!is.na (perc)) %>% 
    mutate (years = years, height = 'BH')
  temp3  <- data %>% filter (Year %in% Years)%>% filter (DABranch_1 == 1) %>% 
    mutate (perc = PercentageDABranch_1.1) %>% 
    select (perc) %>% filter (!is.na (perc)) %>% 
    mutate (years = years, height = 'Branch')
  temp4  <- data %>% filter (Year %in% Years) %>% filter (DABranch_2 == 1) %>% 
    mutate (perc = PercentageDABranch_2.1) %>% 
    select (perc) %>% filter (!is.na (perc)) %>% 
    mutate (years = years, height = 'Branch')
  temp5  <- data %>% filter (Year %in% Years) %>% filter (DA2010_1 == 1) %>% 
    mutate (perc = PercentageDA2010_1.1) %>% 
    select (perc) %>% filter (!is.na (perc)) %>% 
    mutate (years = years, height = 'Top-of-tree')
  temp6  <- data %>% filter (Year %in% Years) %>% filter (DA2010_2 == 1) %>%
    mutate (perc = PercentageDA2010_2.1) %>% 
    select (perc) %>% filter (!is.na (perc)) %>% 
    mutate (years = years, height = 'Top-of-tree')
  temp7  <- data %>% filter (Year %in% Years) %>% filter (DABH_1 == 2) %>% 
    mutate (perc = PercentageDABH_1.2) %>% 
    select (perc) %>% filter (!is.na (perc))  %>% 
    mutate (years = years, height = 'BH')
  temp8  <- data %>% filter (Year %in% Years) %>% filter (DABH_2 == 2) %>%
    mutate (perc = PercentageDABH_2.2) %>% 
    select (perc) %>% filter (!is.na (perc))  %>% 
    mutate (years = years, height = 'BH')
  temp9  <- data %>% filter (Year %in% Years) %>% filter (DABranch_1 == 2) %>% 
    mutate (perc = PercentageDABranch_1.2) %>% 
    select (perc) %>% filter (!is.na (perc)) %>% 
    mutate (years = years, height = 'Branch')
  temp10 <- data %>% filter (Year %in% Years) %>% filter (DABranch_2 == 2) %>% 
    mutate (perc = PercentageDABranch_2.2) %>% 
    select (perc) %>% filter (!is.na (perc)) %>% 
    mutate (years = years, height = 'Branch')
  temp11 <- data %>% filter (Year %in% Years) %>% filter (DA2010_1 == 2) %>% 
    mutate (perc = PercentageDA2010_1.2) %>% 
    select (perc) %>% filter (!is.na (perc)) %>% 
    mutate (years = years, height = 'Top-of-tree')
  temp12 <- data %>% filter (Year %in% Years) %>% filter (DA2010_2 == 2) %>% 
    mutate (perc = PercentageDA2010_2.2) %>% 
    select (perc) %>% filter (!is.na (perc)) %>% 
    mutate (years = years, height = 'Top-of-tree')
  temp <- rbind (temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10, 
                 temp11, temp12)
  if (years == 'all') {
    percentages <- temp}
  else {
    percentages <- rbind (percentages, temp)
  }
}

# Compute differences between groups of within-ring position with years and heights 
#----------------------------------------------------------------------------------------
res.aov <- aov (perc ~ years + height, data = percentages)
# summary (res.aov)
# plot (res.aov, 1) # homogeneity of variances looks good
car::Anova (res.aov, type = 'III') # we use Anova, because of unbalanced group sizes
TukeyHSD (res.aov)

# Run test for differences between groups for ring width
#----------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------
for (years in c ('all', 'select')) {
  
  # Select years
  #--------------------------------------------------------------------------------------
  if (years == 'all')    Years <- 1993:2017
  if (years == 'select') Years <- c (1999, 2002, 2012, 2013, 2016) 
  #print (Years)
  
  # Wrangle data to get frequency over ring width for breast height and set of years
  #--------------------------------------------------------------------------------------
  tmp1 <- data %>% filter (Year %in% Years) %>% 
    filter (!is.na (RingWidthBH_1) & !is.na (DABH_1)) %>% 
    select (RingWidthBH_1, DABH_1) %>% rename (RingWidth = RingWidthBH_1, DA = DABH_1)
  tmp2 <- data %>% filter (Year %in% Years) %>% 
    filter (!is.na (RingWidthBH_2) & !is.na (DABH_2)) %>% 
    select (RingWidthBH_2, DABH_2) %>% rename (RingWidth = RingWidthBH_2, DA = DABH_2)
  tmpBH <- rbind (tmp1, tmp2); rm (tmp1, tmp2)
  tmpBH [['DA']] [tmpBH [['DA']] == 2] <- 1
  
  # Wrangle data to get frequency over ring width for near-branch and set of years
  #----------------------------------------------------------------------------------------
  tmp1 <- data %>% filter (Year %in% Years) %>%
    filter (!is.na (RingWidthNearBranch_1) & !is.na (DABranch_1)) %>% 
    select (RingWidthNearBranch_1, DABranch_1) %>% 
    rename (RingWidth = RingWidthNearBranch_1, DA = DABranch_1)
  tmp2 <- data %>% filter (Year %in% Years) %>% 
    filter (!is.na (RingWidthNearBranch_2) & !is.na (DABranch_2)) %>% 
    select (RingWidthNearBranch_2, DABranch_2) %>% 
    rename (RingWidth = RingWidthNearBranch_2, DA = DABranch_2)
  tmpBranch <- rbind (tmp1, tmp2); rm (tmp1, tmp2)
  tmpBranch [['DA']] [tmpBranch [['DA']] == 2] <- 1
  
  # Wrangle data to get frequency over ring width for top-of-tree and set of years
  #----------------------------------------------------------------------------------------
  tmp1 <- data %>% filter (Year %in% Years) %>% 
    filter (!is.na (RingWidth2010_1) & !is.na (DA2010_1)) %>% 
    select (RingWidth2010_1, DA2010_1) %>% 
    rename (RingWidth = RingWidth2010_1, DA = DA2010_1)
  tmp2 <- data %>% filter (Year%in% Years) %>%
    filter (!is.na (RingWidth2010_2) & !is.na (DA2010_2)) %>% 
    select (RingWidth2010_2, DA2010_2) %>% 
    rename (RingWidth = RingWidth2010_2, DA = DA2010_2)
  tmp2010 <- rbind (tmp1, tmp2); rm (tmp1, tmp2)
  tmp2010 [['DA']] [tmp2010 [['DA']] == 2] <- 1
  
  if (years == 'all') {
    temp <- rbind (mutate (tmpBH,     height = 'BH',          years = years),
                   mutate (tmpBranch, height = 'Branch',      years = years),
                   mutate (tmp2010,   height = 'Top-of-tree', years = years))
  } else {
    temp <- rbind (temp, mutate (tmpBH, height = 'BH',          years = years),
                   mutate (tmpBranch,   height = 'Branch',      years = years),
                   mutate (tmp2010,     height = 'Top-of-tree', years = years))
  }
}
temp [['DA']] <- factor (temp [['DA']])
res.aov <- aov (RingWidth ~ DA + height + years, data = temp)
summary (res.aov)
# plot (res.aov, 1) # homogeneity of variances looks good
car::Anova (res.aov, type = 'III') # we use Anova, because of unbalanced group sizes
TukeyHSD (res.aov)
leveneTest (log(perc) ~years + height, data = percentages)

#========================================================================================
