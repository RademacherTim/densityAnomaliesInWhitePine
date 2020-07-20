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
temp1 <- data %>% filter (MDABH1 == 1) %>% mutate (perc = PercMDABHPos1) %>% 
  select (perc) %>% filter (!is.na (perc))
temp2 <- data %>% filter (MDABH2 == 1) %>% mutate (perc = PercMDABHPos2) %>% 
  select (perc) %>% filter (!is.na (perc))
temp3 <- data %>% filter (MDABranch1 == 1) %>% mutate (perc = PercMDABranch1) %>% 
  select (perc) %>% filter (!is.na (perc))
temp4 <- data %>% filter (MDABranch2 == 1) %>% mutate (perc = PercMDABranch2) %>% 
  select (perc) %>% filter (!is.na (perc))
temp5 <- data %>% filter (MDA2010_1 == 1) %>% mutate (perc = PercMDA2010.1) %>% 
  select (perc) %>% filter (!is.na (perc))
temp6 <- data %>% filter (MDA2010_2 == 1) %>% mutate (perc = PercMDA2010.2) %>% 
  select (perc) %>% filter (!is.na (perc))
percentagesAll     <- rbind (temp1, temp2, temp3, temp4, temp5, temp6) [['perc']] 
percentagesBH      <- rbind (temp1, temp2) [['perc']]
percentagesBranch  <- rbind (temp3, temp4) [['perc']]
percentages2010    <- rbind (temp5, temp6) [['perc']]
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
plot (rhoAll, xlim = c (0, 100), ylim = c (0, 0.055), xlab = 'Percentage ring width (%)',
      ylab = 'Density distribution', main = '', lwd = 3, col = 'white', axes = FALSE)
axis (1)
axis (2, las = 1)
lines (rhoBH,     lwd = 3, col = colours [1])
lines (rhoBranch, lwd = 3, col = colours [2])
lines (rho2010,   lwd = 3, col = colours [3])
lines (rhoAll,    lwd = 3, col = '#666666')

# Add a legend 
#----------------------------------------------------------------------------------------
legend (x = 10, y = 0.05, legend = c ('breast height', 'near-branch','top-of-tree','all'), 
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
