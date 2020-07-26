#========================================================================================
# Script to plot the relationship between ring width and density anomalies
#----------------------------------------------------------------------------------------

# Download and read the data
#----------------------------------------------------------------------------------------
source ('readGrowthData.R')
library ('tidyverse')

# Wrangle data to get frequency over ring width for breat height, near-branch and top-of-tree
#----------------------------------------------------------------------------------------
tmp1 <- data %>% filter (!is.na (RingWidthBH_1) & !is.na (DABH_1)) %>% 
  select (RingWidthBH_1, DABH_1) %>% rename (RingWidth = RingWidthBH_1, DABH = DABH_1)
tmp2 <- data %>% filter (!is.na (RingWidthBH_2) & !is.na (DABH_2)) %>% 
  select (RingWidthBH_2, DABH_2) %>% rename (RingWidth = RingWidthBH_2, DABH = DABH_2)
tmp <-  rbind (tmp1, tmp2)
rhoBH <- density (tmp [['RingWidth']])
rhoBHwith <- density (tmp [['RingWidth']] [tmp [['DABH']] >= 1])
rhoBHwithout <- density (tmp [['RingWidth']] [tmp [['DABH']] == 0])

# Plot graph of density distribution by ring width at breast height
#----------------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
plot (rhoBH, las = 1, ylim = c (0, 0.25), xlab = 'Ring width (mm)', lwd = 3,
      ylab = 'Density distribution of density anomalies', main = '', col = '#666666')
lines (rhoBHwith, col = colours [1], lwd = 2, lty = 1)
lines (rhoBHwithout, col = colours [1], lwd = 2, lty = 2)

# Plot graph of ring width by density anomaly at breast height
#----------------------------------------------------------------------------------------
plot (x = jitter (tmp  [['DABH']] [tmp [['DABH']] == 0], amount = 0.2), 
      y = tmp [['RingWidth']] [tmp [['DABH']] == 0], 
      xlab = 'Number of density anomalies in ring', xlim = c (-0.5, 2.5),
      ylab = 'Ring width (mm)', col = colours [1], pch = 1, axes = FALSE)
axis (2, las = 1)
axis (1, at = 0:2)
boxplot (tmp [['RingWidth']] [tmp [['DABH']] == 0], at = 0, add = TRUE, 
         col = 'transparent', axes = FALSE)
points (x = jitter (tmp  [['DABH']] [tmp [['DABH']] == 1], amount = 0.2), 
        y = tmp [['RingWidth']] [tmp [['DABH']] == 1], col = addOpacity (colours [1], 0.6), pch = 19)
boxplot (tmp [['RingWidth']] [tmp [['DABH']] == 1], at = 1, add = TRUE, 
         col = 'transparent', axes = FALSE)
points (x = jitter (tmp  [['DABH']] [tmp [['DABH']] == 2], amount = 0.2), 
        y = tmp [['RingWidth']] [tmp [['DABH']] == 2], col = colours [1], pch = 20)
boxplot (tmp [['RingWidth']] [tmp [['DABH']] == 2], at = 2, add = TRUE, 
         col = 'transparent', axes = FALSE)

# Wrangle data to get frequency over ring width for breat height, near-branch and top-of-tree
#----------------------------------------------------------------------------------------
tmp1 <- data %>% filter (!is.na (RingWidthNearBranch_1) & !is.na (DABranch_1)) %>% 
  select (RingWidthNearBranch_1, DABranch_1) %>% 
  rename (RingWidth = RingWidthNearBranch_1, DABH = DABranch_1)
tmp2 <- data %>% filter (!is.na (RingWidthNearBranch_2) & !is.na (DABranch_2)) %>% 
  select (RingWidthNearBranch_2, DABranch_2) %>% 
  rename (RingWidth = RingWidthNearBranch_2, DABH = DABranch_2)
tmp <-  rbind (tmp1, tmp2)
rhoBH <- density (tmp [['RingWidth']])
rhoBHwith <- density (tmp [['RingWidth']] [tmp [['DABH']] >= 1])
rhoBHwithout <- density (tmp [['RingWidth']] [tmp [['DABH']] == 0])

# Plot graph of density distribution by ring width near a branch
#----------------------------------------------------------------------------------------
plot (rhoBH, las = 1, ylim = c (0, 0.35), xlab = 'Ring width (mm)', lwd = 3,
      ylab = 'Density distribution of density anomalies', main = '', col = '#666666')
lines (rhoBHwith,    col = colours [2], lwd = 2, lty = 1)
lines (rhoBHwithout, col = colours [2], lwd = 2, lty = 2)

# Plot graph of ring width by density anomaly near a branch
#----------------------------------------------------------------------------------------
plot (x = jitter (tmp  [['DABH']] [tmp [['DABH']] == 0], amount = 0.2), 
      y = tmp [['RingWidth']] [tmp [['DABH']] == 0], 
      xlab = 'Number of density anomalies in ring', xlim = c (-0.5, 2.5), ylim = c (0, 13),
      ylab = 'Ring width (mm)', col = colours [2], pch = 1, axes = FALSE)
axis (2, las = 1, at = seq (0, 12, by = 2))
axis (1, at = 0:2)
boxplot (tmp [['RingWidth']] [tmp [['DABH']] == 0], at = 0, add = TRUE, 
         col = 'transparent', axes = FALSE)
points (x = jitter (tmp  [['DABH']] [tmp [['DABH']] == 1], amount = 0.2), 
        y = tmp [['RingWidth']] [tmp [['DABH']] == 1], col = addOpacity (colours [2], 0.6), pch = 19)
boxplot (tmp [['RingWidth']] [tmp [['DABH']] == 1], at = 1, add = TRUE, 
         col = 'transparent', axes = FALSE)
points (x = jitter (tmp  [['DABH']] [tmp [['DABH']] == 2], amount = 0.2), 
        y = tmp [['RingWidth']] [tmp [['DABH']] == 2], col = colours [2], pch = 20)
boxplot (tmp [['RingWidth']] [tmp [['DABH']] == 2], at = 2, add = TRUE, 
         col = 'transparent', axes = FALSE)

# Wrangle data to get frequency over ring width for breat height, near-2010 and top-of-tree
#----------------------------------------------------------------------------------------
tmp1 <- data %>% filter (!is.na (RingWidth2010_1) & !is.na (DA2010_1)) %>% 
  select (RingWidth2010_1, DA2010_1) %>% 
  rename (RingWidth = RingWidth2010_1, DABH = DA2010_1)
tmp2 <- data %>% filter (!is.na (RingWidth2010_2) & !is.na (DA2010_2)) %>% 
  select (RingWidth2010_2, DA2010_2) %>% 
  rename (RingWidth = RingWidth2010_2, DABH = DA2010_2)
tmp <-  rbind (tmp1, tmp2)
rhoBH <- density (tmp [['RingWidth']])
rhoBHwith <- density (tmp [['RingWidth']] [tmp [['DABH']] >= 1])
rhoBHwithout <- density (tmp [['RingWidth']] [tmp [['DABH']] == 0])

# Plot graph of density distribution by ring width near a 2010
#----------------------------------------------------------------------------------------
plot (rhoBH, las = 1, ylim = c (0, 0.35), xlab = 'Ring width (mm)', lwd = 3,
      ylab = 'Density distribution of density anomalies', main = '', col = '#666666')
lines (rhoBHwith,    col = colours [3], lwd = 2, lty = 1)
lines (rhoBHwithout, col = colours [3], lwd = 2, lty = 2)

# Plot graph of ring width by density anomaly near a 2010
#----------------------------------------------------------------------------------------
plot (x = jitter (tmp  [['DABH']] [tmp [['DABH']] == 0], amount = 0.2), 
      y = tmp [['RingWidth']] [tmp [['DABH']] == 0], 
      xlab = 'Number of density anomalies in ring', xlim = c (-0.5, 2.5), ylim = c (0, 8),
      ylab = 'Ring width (mm)', col = colours [3], pch = 1, axes = FALSE)
axis (2, las = 1, at = seq (0, 8, by = 2))
axis (1, at = 0:2)
boxplot (tmp [['RingWidth']] [tmp [['DABH']] == 0], at = 0, add = TRUE, 
         col = 'transparent', axes = FALSE)
points (x = jitter (tmp  [['DABH']] [tmp [['DABH']] == 1], amount = 0.2), 
        y = tmp [['RingWidth']] [tmp [['DABH']] == 1], col = addOpacity (colours [3], 0.6), pch = 19)
boxplot (tmp [['RingWidth']] [tmp [['DABH']] == 1], at = 1, add = TRUE, 
         col = 'transparent', axes = FALSE)
points (x = jitter (tmp  [['DABH']] [tmp [['DABH']] == 2], amount = 0.2), 
        y = tmp [['RingWidth']] [tmp [['DABH']] == 2], col = colours [3], pch = 20)
boxplot (tmp [['RingWidth']] [tmp [['DABH']] == 2], at = 2, add = TRUE, 
         col = 'transparent', axes = FALSE)
#========================================================================================
