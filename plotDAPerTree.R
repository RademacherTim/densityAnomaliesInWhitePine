#========================================================================================
# Script to plot the density anomalies per tree
#----------------------------------------------------------------------------------------

# Download and read the data
#----------------------------------------------------------------------------------------
source ('readGrowthData.R')

# Load colour scheme for at reast height, near a branch whorl and in the 2010 section
#----------------------------------------------------------------------------------------
source ('plotingFunctions.R')

# Make bargraph of the number of density anomalies per tree in each radial profile at 
# breast height, near the branch and in the 2010 section
#----------------------------------------------------------------------------------------
summaryData <- data %>% group_by (TreeID) %>% 
  summarise (nMDAatBH1  = sum (MDABH1, na.rm = TRUE),     nMDAatBH2  = sum (MDABH2, na.rm = TRUE),
             nMDAatBra1 = sum (MDABranch1, na.rm = TRUE), nMDAatBra2 = sum (MDABranch2, na.rm = TRUE),
             nMDAatTop1 = sum (MDA2010_1, na.rm = TRUE),  nMDAatTop2 = sum (MDA2010_2, na.rm = TRUE),
             cambialAge = max (CambialAgeBH, na.rm = TRUE)) %>%
  mutate (nMDAatBH  = rowMeans (select (., nMDAatBH1, nMDAatBH2)),
          nMDAatBra = rowMeans (select (., nMDAatBra1, nMDAatBra2)),
          nMDAatTop = rowMeans (select (., nMDAatTop1, nMDAatTop2)))
  
# Set trees for which we had no 2010 sections to NA
#----------------------------------------------------------------------------------------
summaryData [c (1:4, 6:10, 12:22, 24:27, 30:38), c (6, 7, 10)] <- NA

# Set trees for which we had no core near a branch to NA
#----------------------------------------------------------------------------------------
summaryData [c (2, 5, 8, 10:17, 19:29, 32:35, 37:41), c (4, 5, 9)] <- NA

# Open file for ploting
#----------------------------------------------------------------------------------------
png (file = 'fig/numberOfAnomaliesPerTree.png', width = 720)

# Plot number of anomalies at breast height
#----------------------------------------------------------------------------------------
par (mar = c (3, 5, 1, 5))
plot (x = summaryData [['TreeID']] - 0.1,
      y = summaryData [['nMDAatBH1']], las = 1, 
      ylab = 'Number years with density anomalies in radial profile', ylim = c (-2, 30),
      lwd = 3, col = colours [1], pch = 21, bg = 'white', cex = 0.8, xaxt = 'n')
segments (x0 = summaryData [['TreeID']] - 0.1,
          y0 = summaryData [['nMDAatBH1']], y1 = summaryData [['nMDAatBH2']], 
          col = colours [1], lwd = 3)
points (x = summaryData [['TreeID']] - 0.1,
        y = summaryData [['nMDAatBH1']], pch = 21, col = colours [1], lwd = 3, 
        bg = 'white', cex = 0.8)
points (x = summaryData [['TreeID']] - 0.1,
        y = summaryData [['nMDAatBH2']], pch = 21, col = colours [1], lwd = 3, 
        bg = 'white', cex = 0.8)
con <- which (summaryData [['nMDAatBH1']] == summaryData [['nMDAatBH2']])
points (x = summaryData [['TreeID']] [con] - 0.1,
        y = summaryData [['nMDAatBH2']] [con], pch = 19, col = colours [1], lwd = 3, 
        cex = 0.8)
axis (side = 4, las = 1)
mtext (side = 4, line = 3, text = 'Cambial age at breat height')
mtext (side = 1, line = 1.4, text = 'Tree ID', col = '#666666')

# Plot number of anomalies near branch
#----------------------------------------------------------------------------------------
segments (x0 = summaryData [['TreeID']] + 0.1,
          y0 = summaryData [['nMDAatBra1']], y1 = summaryData [['nMDAatBra2']], 
          col = colours [2], lwd = 3)
points (x = summaryData [['TreeID']] + 0.1,
        y = summaryData [['nMDAatBra1']], pch = 21, col = colours [2], lwd = 3, 
        bg = 'white', cex = 0.8)
points (x = summaryData [['TreeID']] + 0.1,
        y = summaryData [['nMDAatBra2']], pch = 21, col = colours [2], lwd = 3, 
        bg = 'white', cex = 0.8)
con <- which (summaryData [['nMDAatBra1']] == summaryData [['nMDAatBra2']])
points (x = summaryData [['TreeID']] [con] + 0.1,
        y = summaryData [['nMDAatBra2']] [con], pch = 19, col = colours [2], lwd = 3, 
        cex = 0.8)

# Plot number of anomalies at the top of the tree
#----------------------------------------------------------------------------------------
segments (x0 = summaryData [['TreeID']] + 0.1,
          y0 = summaryData [['nMDAatTop1']], y1 = summaryData [['nMDAatTop2']], 
          col = colours [3], lwd = 3)
points (x = summaryData [['TreeID']] + 0.1,
        y = summaryData [['nMDAatTop1']], pch = 21, col = colours [3], lwd = 3, 
        bg = 'white', cex = 0.8)
points (x = summaryData [['TreeID']] + 0.1,
        y = summaryData [['nMDAatTop2']], pch = 21, col = colours [3], lwd = 3, 
        bg = 'white', cex = 0.8)
con <- which (summaryData [['nMDAatTop1']] == summaryData [['nMDAatTop2']])
points (x = summaryData [['TreeID']] [con] + 0.1,
        y = summaryData [['nMDAatTop2']] [con], pch = 19, col = colours [3], lwd = 3, 
        cex = 0.8)

# Add cambial age at breast height for all trees
#----------------------------------------------------------------------------------------
points (x = summaryData [['TreeID']], y = summaryData [['cambialAge']], pch = 23, 
        bg = 'white', lwd = 2, col = '#c90016')

# Add lines to separate the trees
#----------------------------------------------------------------------------------------
abline (v = 1.5:40.5, col = '#66666622')

# Add tree IDs
#----------------------------------------------------------------------------------------
text (x = 1:41, y = -2, labels = 1:41, col = '#666666', cex = 0.8)

# Add legend 
#----------------------------------------------------------------------------------------
legend (x = 0, y = 30, pch = c (19, 21, 19, 21, 19, 21, 23), lwd = c (rep (3, 6), 2), 
        lty = 0, cex = 0.8, col = c (colours [1], colours [1], colours [2], colours [2], 
                                     colours [3], colours [3], '#c90016'), 
        box.lty = 0, legend = c ('single breast height profile','both breast height profiles',
                                 'single near-branch profile','both near-branch profiles',
                                 'single top-of-tree profile','both top-of-tree profiles',
                                 'cambial age at breast height'), bg = 'transparent')
# Close ploting device
#----------------------------------------------------------------------------------------
dev.off ()
#========================================================================================