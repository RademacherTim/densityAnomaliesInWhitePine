#========================================================================================
# Script to plot distribution of density anomalies within the ring
#----------------------------------------------------------------------------------------

# Download and read the data
#----------------------------------------------------------------------------------------
source ('readGrowthData.R')

# Load colour scheme for at reast height, near a branch whorl and in the 2010 section
#----------------------------------------------------------------------------------------
source ('plotingFunctions.R')

# Wrangle data to get density distribution for when they occur
#----------------------------------------------------------------------------------------
temp1  <- data %>% filter (Year < 2017) %>% filter (DABH_1 == 1) %>% 
  mutate (perc = PercentageDABH_1.1) %>% 
  select (perc) %>% filter (!is.na (perc))
temp2  <- data %>% filter (Year < 2017)%>% filter (DABH_2 == 1) %>%
  mutate (perc = PercentageDABH_2.1) %>% 
  select (perc) %>% filter (!is.na (perc))
temp3  <- data %>% filter (Year < 2017)%>% filter (DABranch_1 == 1) %>% 
  mutate (perc = PercentageDABranch_1.1) %>% 
  select (perc) %>% filter (!is.na (perc))
temp4  <- data %>% filter (Year < 2017)%>% filter (DABranch_2 == 1) %>% 
  mutate (perc = PercentageDABranch_2.1) %>% 
  select (perc) %>% filter (!is.na (perc))
temp5  <- data %>% filter (Year < 2017)%>% filter (DA2010_1 == 1) %>% 
  mutate (perc = PercentageDA2010_1.1) %>% 
  select (perc) %>% filter (!is.na (perc))
temp6  <- data %>% filter (Year < 2017)%>% filter (DA2010_2 == 1) %>%
  mutate (perc = PercentageDA2010_2.1) %>% 
  select (perc) %>% filter (!is.na (perc))
temp7  <- data %>% filter (Year < 2017)%>% filter (DABH_1 == 2) %>% 
  mutate (perc = PercentageDABH_1.2) %>% 
  select (perc) %>% filter (!is.na (perc))
temp8  <- data %>% filter (Year < 2017)%>% filter (DABH_2 == 2) %>%
  mutate (perc = PercentageDABH_2.2) %>% 
  select (perc) %>% filter (!is.na (perc))
temp9  <- data %>% filter (Year < 2017)%>% filter (DABranch_1 == 2) %>% 
  mutate (perc = PercentageDABranch_1.2) %>% 
  select (perc) %>% filter (!is.na (perc))
temp10 <- data %>% filter (Year < 2017)%>% filter (DABranch_2 == 2) %>% 
  mutate (perc = PercentageDABranch_2.2) %>% 
  select (perc) %>% filter (!is.na (perc))
temp11 <- data %>% filter (Year < 2017)%>% filter (DA2010_1 == 2) %>% 
  mutate (perc = PercentageDA2010_1.2) %>% 
  select (perc) %>% filter (!is.na (perc))
temp12 <- data %>% filter (Year < 2017)%>% filter (DA2010_2 == 2) %>% 
  mutate (perc = PercentageDA2010_2.2) %>% 
  select (perc) %>% filter (!is.na (perc))
percentagesAll     <- rbind (temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10, temp11, temp12) [['perc']] 
percentagesBH      <- rbind (temp1, temp2, temp7,  temp8)  [['perc']]
percentagesBranch  <- rbind (temp3, temp4, temp9,  temp10) [['perc']]
percentages2010    <- rbind (temp5, temp6, temp11, temp12) [['perc']]
rhoAll    <- density (percentagesAll)
rhoBH     <- density (percentagesBH)
rhoBranch <- density (percentagesBranch)
rho2010   <- density (percentages2010)

# Open file for ploting
#----------------------------------------------------------------------------------------
png (file = 'fig/withinRingDADistribution.png')

# Plot density kernel 
#----------------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
plot (rhoAll, xlim = c (0, 100), ylim = c (0, 0.06), xlab = 'Percentage ring width (%)',
      ylab = 'Density distribution of position of density anomalies', main = '', lwd = 3, col = 'white', axes = FALSE)
axis (1)
axis (2, las = 1)
lines (rhoBH,     lwd = 3, col = colours [1])
lines (rhoBranch, lwd = 3, col = colours [2])
lines (rho2010,   lwd = 3, col = colours [3])
lines (rhoAll,    lwd = 3, col = '#666666')

# Add a legend 
#----------------------------------------------------------------------------------------
legend (x = 10, y = 0.06, legend = c ('breast height', 'near-branch','top-of-tree','all'), 
        col = c (colours, '#666666'), box.lty = 0, lwd = 3)

# Close ploting device
#----------------------------------------------------------------------------------------
dev.off ()

# Mean positions of density anomaly and standard error of the mean positions 
#----------------------------------------------------------------------------------------
mean (percentagesAll); se (percentagesAll)
mean (percentagesBH); se (percentagesBH)
mean (percentagesBranch); se (percentagesBranch)
mean (percentages2010); se (percentages2010)

# Wrangle data to get density distribution for when they occur
#----------------------------------------------------------------------------------------
temp1  <- data %>% filter (Year %in% c (1999, 2002, 2012, 2016)) %>% 
  filter (DABH_1 == 1) %>% 
  mutate (perc = PercentageDABH_1.1) %>% 
  select (perc) %>% filter (!is.na (perc))
temp2  <- data %>% filter (Year %in% c (1999, 2002, 2012, 2016))%>% 
  filter (DABH_2 == 1) %>%
  mutate (perc = PercentageDABH_2.1) %>% 
  select (perc) %>% filter (!is.na (perc))
temp3  <- data %>% filter (Year %in% c (1999, 2002, 2012, 2016))%>% 
  filter (DABranch_1 == 1) %>% 
  mutate (perc = PercentageDABranch_1.1) %>% 
  select (perc) %>% filter (!is.na (perc))
temp4  <- data %>% filter (Year %in% c (1999, 2002, 2012, 2016))%>% 
  filter (DABranch_2 == 1) %>% 
  mutate (perc = PercentageDABranch_2.1) %>% 
  select (perc) %>% filter (!is.na (perc))
temp5  <- data %>% filter (Year %in% c (1999, 2002, 2012, 2016))%>% 
  filter (DA2010_1 == 1) %>% 
  mutate (perc = PercentageDA2010_1.1) %>% 
  select (perc) %>% filter (!is.na (perc))
temp6  <- data %>% filter (Year %in% c (1999, 2002, 2012, 2016))%>% 
  filter (DA2010_2 == 1) %>%
  mutate (perc = PercentageDA2010_2.1) %>% 
  select (perc) %>% filter (!is.na (perc))
temp7  <- data %>% filter (Year %in% c (1999, 2002, 2012, 2016))%>% 
  filter (DABH_1 == 2) %>% 
  mutate (perc = PercentageDABH_1.2) %>% 
  select (perc) %>% filter (!is.na (perc))
temp8  <- data %>% filter (Year %in% c (1999, 2002, 2012, 2016))%>%
  filter (DABH_2 == 2) %>%
  mutate (perc = PercentageDABH_2.2) %>% 
  select (perc) %>% filter (!is.na (perc))
temp9  <- data %>% filter (Year %in% c (1999, 2002, 2012, 2016))%>% 
  filter (DABranch_1 == 2) %>% 
  mutate (perc = PercentageDABranch_1.2) %>% 
  select (perc) %>% filter (!is.na (perc))
temp10 <- data %>% filter (Year %in% c (1999, 2002, 2012, 2016))%>% 
  filter (DABranch_2 == 2) %>% 
  mutate (perc = PercentageDABranch_2.2) %>% 
  select (perc) %>% filter (!is.na (perc))
temp11 <- data %>% filter (Year %in% c (1999, 2002, 2012, 2016))%>% 
  filter (DA2010_1 == 2) %>% 
  mutate (perc = PercentageDA2010_1.2) %>% 
  select (perc) %>% filter (!is.na (perc))
temp12 <- data %>% filter (Year %in% c (1999, 2002, 2012, 2016))%>% 
  filter (DA2010_2 == 2) %>% 
  mutate (perc = PercentageDA2010_2.2) %>% 
  select (perc) %>% filter (!is.na (perc))
percentagesAll     <- rbind (temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10, temp11, temp12) [['perc']] 
percentagesBH      <- rbind (temp1, temp2, temp7,  temp8)  [['perc']]
percentagesBranch  <- rbind (temp3, temp4, temp9,  temp10) [['perc']]
percentages2010    <- rbind (temp5, temp6, temp11, temp12) [['perc']]
rhoAll    <- density (percentagesAll)
rhoBH     <- density (percentagesBH)
rhoBranch <- density (percentagesBranch)
rho2010   <- density (percentages2010)

# Open file for ploting
#----------------------------------------------------------------------------------------
png (file = 'fig/withinRingDADistributionInHighFreqYears.png')

# Plot density kernel 
#----------------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
plot (rhoAll, xlim = c (0, 100), ylim = c (0, 0.065), xlab = 'Percentage ring width (%)',
      ylab = 'Density distribution of position of density anomalies', main = '', lwd = 3, col = 'white', axes = FALSE)
axis (1)
axis (2, las = 1)
lines (rhoBH,     lwd = 3, col = colours [1])
lines (rhoBranch, lwd = 3, col = colours [2])
lines (rho2010,   lwd = 3, col = colours [3])
lines (rhoAll,    lwd = 3, col = '#666666')

# Add a legend 
#----------------------------------------------------------------------------------------
legend (x = 10, y = 0.065, legend = c ('breast height', 'near-branch','top-of-tree','all'), 
        col = c (colours, '#666666'), box.lty = 0, lwd = 3)

# Close ploting device
#----------------------------------------------------------------------------------------
dev.off ()

# Mean positions of density anomaly and standard error of the mean positions 
#----------------------------------------------------------------------------------------
mean (percentagesAll); se (percentagesAll)
mean (percentagesBH); se (percentagesBH)
mean (percentagesBranch); se (percentagesBranch)
mean (percentages2010); se (percentages2010)

#========================================================================================
