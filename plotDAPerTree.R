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
  summarise (nDABH_1  = sum (DABH_1, na.rm = TRUE),       
             nDABH_2  = sum (DABH_2, na.rm = TRUE),
             nDABra_1 = sum (DABranch_1, na.rm = TRUE),   
             nDABra_2 = sum (DABranch_2, na.rm = TRUE),
             nDATop_1 = sum (DA2010_1, na.rm = TRUE),    
             nDATop_2 = sum (DA2010_2, na.rm = TRUE),
             cambialAge = max (CambialAgeBH, na.rm = TRUE), 
             pithInImage = unique (PithInImageBH, na.rm = TRUE)) %>%
  mutate (nDABH  = rowMeans (select (., nDABH_1, nDABH_2)),
          nDABra = rowMeans (select (., nDABra_1, nDABra_2)),
          nDATop = rowMeans (select (., nDATop_1, nDATop_2)))
  
# Set trees for which we had no 2010 sections to NA
#----------------------------------------------------------------------------------------
summaryData [c (1:4, 6:10, 12:22, 24:27, 30:38), c (6, 7, 12)] <- NA

# Set trees for which we had no near-branch increment core to NA
#----------------------------------------------------------------------------------------
summaryData [c (2, 5, 8, 10:17, 19:29, 32:35, 37:41), c (4, 5, 10)] <- NA

# Open file for ploting
#----------------------------------------------------------------------------------------
png (file = 'fig/numberOfAnomaliesPerTree.png', width = 1020)

# Plot number of anomalies at breast height
#----------------------------------------------------------------------------------------
par (mar = c (5, 9, 1, 6))
plot (x = summaryData [['TreeID']] - 0.1,
      y = summaryData [['nDABH_1']], las = 1, 
      xlab = '', ylab = '', ylim = c (-2, 30),
      lwd = 3, col = colours [1], pch = 21, bg = 'white', cex = 0.8, axes = FALSE)
axis (side = 2, las = 1, cex.axis = 2)
mtext (side = 2, line = 4, cex = 2, text = 'Number of years with density anomalies \n in radial profile')
segments (x0 = summaryData [['TreeID']] - 0.1,
          y0 = summaryData [['nDABH_1']], y1 = summaryData [['nDABH_2']], 
          col = colours [1], lwd = 3)
points (x = summaryData [['TreeID']] - 0.1,
        y = summaryData [['nDABH_1']], pch = 21, col = colours [1], lwd = 3, 
        bg = 'white', cex = 0.8)
points (x = summaryData [['TreeID']] - 0.1,
        y = summaryData [['nDABH_2']], pch = 21, col = colours [1], lwd = 3, 
        bg = 'white', cex = 0.8)
con <- which (summaryData [['nDABH_1']] == summaryData [['nDABH_2']])
points (x = summaryData [['TreeID']] [con] - 0.1,
        y = summaryData [['nDABH_2']] [con], pch = 19, col = colours [1], lwd = 3, 
        cex = 0.8)
axis (side = 4, las = 1, cex.axis = 2)
mtext (side = 4, line = 4, cex = 2, text = 'Cambial age at breat height')
mtext (side = 1, line = 1.4, cex = 2, text = 'Tree ID', col = '#666666')

# Plot number of anomalies near branch
#----------------------------------------------------------------------------------------
segments (x0 = summaryData [['TreeID']] + 0.1,
          y0 = summaryData [['nDABra_1']], y1 = summaryData [['nDABra_2']], 
          col = colours [2], lwd = 3)
points (x = summaryData [['TreeID']] + 0.1,
        y = summaryData [['nDABra_1']], pch = 21, col = colours [2], lwd = 3, 
        bg = 'white', cex = 0.8)
points (x = summaryData [['TreeID']] + 0.1,
        y = summaryData [['nDABra_2']], pch = 21, col = colours [2], lwd = 3, 
        bg = 'white', cex = 0.8)
con <- which (summaryData [['nDABra_1']] == summaryData [['nDABra_2']])
points (x = summaryData [['TreeID']] [con] + 0.1,
        y = summaryData [['nDABra_2']] [con], pch = 19, col = colours [2], lwd = 3, 
        cex = 0.8)

# Plot number of anomalies at the top of the tree
#----------------------------------------------------------------------------------------
segments (x0 = summaryData [['TreeID']] + 0.1,
          y0 = summaryData [['nDATop_1']], y1 = summaryData [['nDATop_2']], 
          col = colours [3], lwd = 3)
points (x = summaryData [['TreeID']] + 0.1,
        y = summaryData [['nDATop_1']], pch = 21, col = colours [3], lwd = 3, 
        bg = 'white', cex = 0.8)
points (x = summaryData [['TreeID']] + 0.1,
        y = summaryData [['nDATop_2']], pch = 21, col = colours [3], lwd = 3, 
        bg = 'white', cex = 0.8)
con <- which (summaryData [['nDATop_1']] == summaryData [['nDATop_2']])
points (x = summaryData [['TreeID']] [con] + 0.1,
        y = summaryData [['nDATop_2']] [con], pch = 19, col = colours [3], lwd = 3, 
        cex = 0.8)

# Add cambial age at breast height for all trees
#----------------------------------------------------------------------------------------
points (x = summaryData [['TreeID']], y = summaryData [['cambialAge']], pch = 23, 
        bg = ifelse (summaryData [['pithInImage']], '#c90016','white'), lwd = 2, 
        col = '#c90016')

# Add lines to separate the trees
#----------------------------------------------------------------------------------------
abline (v = 1.5:40.5, col = '#66666622')

# Add tree IDs
#----------------------------------------------------------------------------------------
text (x = seq (1, 41, by = 2), y = -1, labels = seq (1, 41, by = 2), col = '#666666', 
      cex = 1.1)
text (x = seq (2, 40, by = 2), y = -2.6, labels = seq (2, 40, by = 2), col = '#666666', 
      cex = 1.1)

# Add legend 
#----------------------------------------------------------------------------------------
legend (x = 2, y = 31, pch = c (21, 19, 21, 19), lwd = rep (3, 4), 
        lty = 0, cex = 1.1, col = c (colours [1], colours [1], colours [2], colours [2]), 
        box.lty = 0, legend = c ('single breast height profile','both breast height profiles',
                                 'single near-branch profile','both near-branch profiles'), 
        bg = 'transparent')
legend (x = 14, y = 31, pch = c (21, 19, 23, 23), lwd = c (rep (3, 2), 2, 2), 
        lty = 0, cex = 1.1, col = c (colours [3], colours [3], '#c90016', '#c90016'), 
        box.lty = 0, legend = c ('single top-of-tree profile','both top-of-tree profiles',
                                 'cambial age at breast height (no pith)', 
                                 'cambial age at breast height (pith)'), 
        pt.bg = c (rep ('transparent', 3), '#c90016'), bg = 'transparent')

# Close ploting device
#----------------------------------------------------------------------------------------
dev.off ()
#========================================================================================
