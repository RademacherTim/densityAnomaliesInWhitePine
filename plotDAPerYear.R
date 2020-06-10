#========================================================================================
# Script to plot the density anomalies over time
#----------------------------------------------------------------------------------------

# Download and read the data
#----------------------------------------------------------------------------------------
source ('readGrowthData.R')

# Load colour scheme for at reast height, near a branch whorl and in the 2010 section
#----------------------------------------------------------------------------------------
source ('plotingFunctions.R')

# Wrangle data to get frequency per year
#----------------------------------------------------------------------------------------
summaryData <- data %>%
  mutate (MDABH     = ifelse (MDABH1     == 1 | MDABH2     == 1, 1, 0), 
          MDABranch = ifelse (MDABranch1 == 1 | MDABranch2 == 1, 1, 0),
          MDATop    = ifelse (MDA2010_1  == 1 | MDA2010_2  == 1, 1, 0)) %>% 
  group_by (Year) %>% summarise (sumBH     = sum (MDABH,     na.rm = TRUE),
                                 nBH       = sum (!is.na (MDABH)),
                                 sumBranch = sum (MDABranch, na.rm = TRUE),
                                 nBranch   = sum (!is.na (MDABranch)),
                                 sumTop    = sum (MDATop,    na.rm = TRUE),
                                 nTop      = sum (!is.na (MDATop))) %>%
  mutate (perBH  = sumBH  / nBH  * 100, perBranch = sumBranch / nBranch * 100,
          perTop = sumTop / nTop * 100) %>% 
  filter (Year < 2019)

# Open file for ploting
#----------------------------------------------------------------------------------------
png (file = 'fig/numberOfAnomaliesPerYear.png', width = 720)

# Plot the frequency of microdensity anomalies per year ring
#----------------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 5))
plot (x = summaryData [['Year']], y = summaryData [['perBH']], col = 'white', 
      typ = 'l', las = 1, xlab = 'Year', ylim = c (0, 100), xlim = c (1993, 2018),
      ylab = 'Percentage of annual rings with density anomaly (%)', lwd = 0.8)
lines (x = summaryData [['Year']], y = summaryData [['perBH']], col = colours [1],
       lwd = 3)
points (x = summaryData [['Year']], y = summaryData [['perBH']], col = colours [1], 
        pch = 21, bg = 'white', lwd = 2)

# Add frequency in cores near branches
#----------------------------------------------------------------------------------------
lines (x = summaryData [['Year']], y = summaryData [['perBranch']], col = colours [2],
       lwd = 3)
points (x = summaryData [['Year']], y = summaryData [['perBranch']], col = colours [2], 
        lwd = 2, pch = 21, bg = 'white')

# Add frequency in cookies from the top of trees
#----------------------------------------------------------------------------------------
lines (x = summaryData [['Year']], y = summaryData [['perTop']], col = colours [3],
       lwd = 3)
points (x = summaryData [['Year']], y = summaryData [['perTop']], col = colours [3], 
        pch = 21, bg = 'white', lwd = 2)

# Add a legend 
#----------------------------------------------------------------------------------------
legend (x = 1994, y = 100, box.lty = 0, pch = 21, col = colours, 
        legend = c ('breast height', 'near-branch', 'top-of-tree'), lwd = 3, lty = 1,
        bg = 'transparent', pt.bg = 'white', pt.lwd = 2)
legend (x = 1992.5, y = 100, box.lty = 0, col = colours, 
        legend = c ('', '', ''), lwd = 1, lty = 2,
        bg = 'transparent')
text (x = 1993.3, y = 103, pos = 1, labels = 'n')
text (x = 1994.5, y = 103, pos = 1, labels = '%')

# Add line for the sample size at BH
#----------------------------------------------------------------------------------------
par (new = TRUE)
plot (x = summaryData [['Year']], y = summaryData [['nBH']], col = colours [1], lty = 2, 
      axes = 'n', typ = 'l', xlab = '', ylab = '')

# Add line for the sample size near branch
#----------------------------------------------------------------------------------------
lines (x = summaryData [['Year']], y = summaryData [['nBranch']], col = colours [2], 
       lty = 2)

# Add line for the sample size at top-of-tree
#----------------------------------------------------------------------------------------
lines (x = summaryData [['Year']], y = summaryData [['nTop']], col = colours [3], 
       lty = 2)

# Add second y-axis
#----------------------------------------------------------------------------------------
axis (side = 4, las = 1)
mtext (side = 4, line = 3, text = 'Sample size (n)')

# Close ploting device
#----------------------------------------------------------------------------------------
dev.off ()

#========================================================================================