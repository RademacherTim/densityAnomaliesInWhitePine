#========================================================================================
# Ring width versus wood age to check for co-linearity
#----------------------------------------------------------------------------------------

# Load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')

# Download and read the data
#----------------------------------------------------------------------------------------
source ('readGrowthData.R')

# Wrangle data
#----------------------------------------------------------------------------------------
summaryData <- data %>% 
  mutate (RingWidthBH         = rowMeans (select (data, RingWidthBH_1, RingWidthBH_2), na.rm = TRUE),
          RingWidthNearBranch = rowMeans (select (data, RingWidthNearBranch_1, RingWidthNearBranch_2), na.rm = TRUE),
          RingWidth2010       = rowMeans (select (data, RingWidth2010_1, RingWidth2010_2), na.rm = TRUE)) %>% 
  select (WoodAgeBH, WoodAgeBranch, WoodAge2010, RingWidthBH, RingWidthNearBranch, RingWidth2010)

# Plot wood age against ring width
#----------------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
plot (x = summaryData [['WoodAgeBH']], y = summaryData [['RingWidthBH']], las = 1, 
      ylab = 'Ring width (mm)', xlab = 'Wood cambial age (years)', pch = 19, 
      col = addOpacity (colours [1], 0.8))
abline (lm (RingWidthBH ~ WoodAgeBH, data = summaryData), col = colours [1], lwd = 2)
points (x = summaryData [['WoodAgeBranch']], y = summaryData [['RingWidthNearBranch']], 
        las = 1, pch = 19, col = addOpacity (colours [2], 0.8))
abline (lm (RingWidthNearBranch ~ WoodAgeBranch, data = summaryData), col = colours [2], lwd = 2)
points (x = summaryData [['WoodAge2010']], y = summaryData [['RingWidth2010']], 
        las = 1, pch = 19, col = addOpacity (colours [3], 0.8))
abline (lm (RingWidth2010 ~ WoodAge2010, data = summaryData), col = colours [3], lwd = 2)

# Add legend 
#----------------------------------------------------------------------------------------
legend (x = 15, y = 10, box.lty = 0, pch = 19, col = colours, 
        legend = c ('at breast height', 'near-branch','top-of-tree'))
#========================================================================================
