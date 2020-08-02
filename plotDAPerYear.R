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
  mutate (DABH     = ifelse (DABH_1     %in% 1:2 | DABH_2     %in% 1:2, 1, 
                             ifelse (is.na (DABH_1) | is.na (DABH_2), NA, 0)), 
          DABranch = ifelse (DABranch_1 %in% 1:2 | DABranch_2 %in% 1:2, 1, 
                             ifelse (is.na (DABranch_1) | is.na (DABranch_2), NA, 0)),
          DATop    = ifelse (DA2010_1   %in% 1:2 | DA2010_2   %in% 1:2, 1, 
                             ifelse (is.na (DA2010_1) | is.na (DA2010_2), NA, 0))) %>% 
  group_by (Year) %>% summarise (sumBH     = sum (DABH,     na.rm = TRUE),
                                 nBH       = sum (!is.na (DABH)),
                                 sumBranch = sum (DABranch, na.rm = TRUE),
                                 nBranch   = sum (!is.na (DABranch)),
                                 sumTop    = sum (DATop,    na.rm = TRUE),
                                 nTop      = sum (!is.na (DATop))) %>%
  mutate (perBH  = sumBH  / nBH  * 100, perBranch = sumBranch / nBranch * 100,
          perTop = sumTop / nTop * 100) %>% 
  filter (Year < 2017)

# Define width of the png files
#----------------------------------------------------------------------------------------
PNGwidth <- 1020

# Open file for ploting
#----------------------------------------------------------------------------------------
png (file = 'fig/numberOfAnomaliesPerYearBH.png', width = PNGwidth)

# Plot the frequency of microdensity anomalies per year ring
#----------------------------------------------------------------------------------------
par (mar = c (5, 9, 1, 6))
plot (x = summaryData [['Year']], y = summaryData [['nBH']], col = '#666666', lty = 2, 
      axes = FALSE, typ = 'l', xlab = '', ylab = '', lwd = 3, ylim = c (0, 41))

# Add second y-axis
#----------------------------------------------------------------------------------------
axis (side = 4, las = 1, cex.axis = 2)
mtext (side = 4, line = 4, text = 'Sample size (n)', cex = 2)

# Add line for frequency
#----------------------------------------------------------------------------------------
par (new = TRUE)
plot (x = summaryData [['Year']], y = summaryData [['perBH']], col = 'white', 
      typ = 'l', las = 1, xlab = '', ylim = c (0, 105), xlim = c (1993, 2018),
      ylab = '', axes = FALSE)
axis (side = 1, cex.axis = 2, at = seq (1990, 2020, by = 5))
axis (side = 2, cex.axis = 2, las = 1)
mtext (side = 1, line = 4, cex = 2, text = 'Year')
mtext (side = 2, line = 4.5, cex = 2, text = 'Percentage of annual rings \n with density anomaly (%)')
lines (x = summaryData [['Year']], y = summaryData [['perBH']], col = colours [1],
       lwd = 4)
points (x = summaryData [['Year']], y = summaryData [['perBH']], col = colours [1], 
        pch = 21, bg = 'white', lwd = 4, cex = 2)

# Add a legend 
#----------------------------------------------------------------------------------------
legend (x = 1992.5, y = 100, box.lty = 0, pch = c (21, 30), col = c (colours [1], '#666666'), 
        legend = c ('%','n'), lwd = c (4, 3), lty = c (1, 2), cex = 2,
        bg = 'transparent', pt.bg = 'white', pt.lwd = 4)

# Close ploting device
#----------------------------------------------------------------------------------------
dev.off ()

# Open file for ploting
#----------------------------------------------------------------------------------------
png (file = 'fig/numberOfAnomaliesPerYearNearBranch.png', width = PNGwidth)

# Plot the frequency of microdensity anomalies per year ring
#----------------------------------------------------------------------------------------
par (mar = c (5, 9, 1, 6))
plot (x = summaryData [['Year']], y = summaryData [['nBranch']], col = '#666666', lty = 2, 
      axes = FALSE, typ = 'l', xlab = '', ylab = '', lwd = 3, ylim = c (0, 41))

# Add second y-axis
#----------------------------------------------------------------------------------------
axis (side = 4, las = 1, cex.axis = 2)
mtext (side = 4, line = 4, text = 'Sample size (n)', cex = 2)

# Add line for frequency
#----------------------------------------------------------------------------------------
par (new = TRUE)
plot (x = summaryData [['Year']], y = summaryData [['perBranch']], col = 'white', 
      typ = 'l', las = 1, xlab = '', ylim = c (0, 105), xlim = c (1993, 2018),
      ylab = '', axes = FALSE)
axis (side = 1, cex.axis = 2, at = seq (1990, 2020, by = 5))
axis (side = 2, cex.axis = 2, las = 1)
mtext (side = 1, line = 4, cex = 2, text = 'Year')
mtext (side = 2, line = 4.5, cex = 2, text = 'Percentage of annual rings \n with density anomaly (%)')
lines (x = summaryData [['Year']], y = summaryData [['perBranch']], col = colours [2],
       lwd = 4)
points (x = summaryData [['Year']], y = summaryData [['perBranch']], col = colours [2], 
        pch = 21, bg = 'white', lwd = 4, cex = 2)

# Add a legend 
#----------------------------------------------------------------------------------------
legend (x = 1992.5, y = 100, box.lty = 0, pch = c (21, 30), col = c (colours [2], '#666666'), 
        legend = c ('%','n'), lwd = c (4, 3), lty = c (1, 2), cex = 2,
        bg = 'transparent', pt.bg = 'white', pt.lwd = 4)

# Close ploting device
#----------------------------------------------------------------------------------------
dev.off ()

# Open file for ploting
#----------------------------------------------------------------------------------------
png (file = 'fig/numberOfAnomaliesPerYearNearTop.png', width = PNGwidth)

# Plot the frequency of microdensity anomalies per year ring
#----------------------------------------------------------------------------------------
par (mar = c (5, 9, 1, 6))
plot (x = summaryData [['Year']], y = summaryData [['nTop']], col = '#666666', lty = 2, 
      axes = FALSE, typ = 'l', xlab = '', ylab = '', lwd = 3, ylim = c (0, 41))

# Add second y-axis
#----------------------------------------------------------------------------------------
axis (side = 4, las = 1, cex.axis = 2)
mtext (side = 4, line = 4, text = 'Sample size (n)', cex = 2)

# Add line for frequency
#----------------------------------------------------------------------------------------
par (new = TRUE)
plot (x = summaryData [['Year']], y = summaryData [['perTop']], col = 'white', 
      typ = 'l', las = 1, xlab = '', ylim = c (0, 105), xlim = c (1993, 2018),
      ylab = '', axes = FALSE)
axis (side = 1, cex.axis = 2, at = seq (1990, 2020, by = 5))
axis (side = 2, cex.axis = 2, las = 1)
mtext (side = 1, line = 4, cex = 2, text = 'Year')
mtext (side = 2, line = 4.5, cex = 2, text = 'Percentage of annual rings \n with density anomaly (%)')
lines (x = summaryData [['Year']], y = summaryData [['perTop']], col = colours [3],
       lwd = 4)
points (x = summaryData [['Year']], y = summaryData [['perTop']], col = colours [3], 
        pch = 21, bg = 'white', lwd = 4, cex = 2)

# Add a legend 
#----------------------------------------------------------------------------------------
legend (x = 1992.5, y = 100, box.lty = 0, pch = c (21, 30), col = c (colours [3], '#666666'), 
        legend = c ('%','n'), lwd = c (4, 3), lty = c (1, 2), cex = 2,
        bg = 'transparent', pt.bg = 'white', pt.lwd = 4)

# Close ploting device
#----------------------------------------------------------------------------------------
dev.off ()
#========================================================================================
