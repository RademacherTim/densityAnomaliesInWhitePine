#========================================================================================
# Script to plot arc of density anomaly
#----------------------------------------------------------------------------------------

# Download and read the data
#----------------------------------------------------------------------------------------
source ('readGrowthData.R')

# Load colour scheme for at reast height, near a branch whorl and in the 2010 section
#----------------------------------------------------------------------------------------
source ('plotingFunctions.R')

# Wrangle data to get average and standard error of arc by year
#----------------------------------------------------------------------------------------
yearlyData <- data %>% filter (Year < 2017) %>% group_by (Year) %>% 
  summarise (meanArcBH = mean (ArcBH, na.rm = TRUE),
             seArcBH   = se   (ArcBH),
             meanArc2010 = mean (Arc2010, na.rm = TRUE),
             seArc2010   = se   (Arc2010))

# Wrangle data to get average and standard deviation of arc by tree
#----------------------------------------------------------------------------------------
treeData <- data %>% filter (Year < 2017) %>% group_by (TreeID) %>% 
  summarise (meanArcBH = mean (ArcBH, na.rm = TRUE),
             seArcBH   = se   (ArcBH),
             meanArc2010 = mean (Arc2010, na.rm = TRUE),
             seArc2010   = se   (Arc2010))

# Plot arc by year  
#----------------------------------------------------------------------------------------
png (file = './fig/ArcBy.png', width = 600, height = 400)
layout (matrix (1:2, nrow = 1, byrow = TRUE), widths = c (1.2, 1))

par (mar = c (5, 5, 1, 1))
plot (x = yearlyData [['Year']], y = yearlyData [['meanArcBH']], 
      xlab = 'Year', ylab = expression (paste ('Arc of density anomalies (', degree,')')), 
      xlim = c (1995, 2017), ylim = c (0, 380), col = colours [1], pch = 19, axes = FALSE, 
      cex = 1.5)
axis (1)
axis (2, at = seq (0, 360, by = 60), las = 1)
arrows (x0 = yearlyData [['Year']], 
        y0 = yearlyData [['meanArcBH']] - yearlyData [['seArcBH']],
        y1 = yearlyData [['meanArcBH']] + yearlyData [['seArcBH']], angle = 90, 
        length = 0.05, code = 3, col = colours [1], lwd = 3)
points (x = yearlyData [['Year']], y = yearlyData [['meanArc2010']], 
        col = colours [3], pch = 19, cex = 1.5)
arrows (x0 = yearlyData [['Year']], 
        y0 = yearlyData [['meanArc2010']] - yearlyData [['seArc2010']],
        y1 = yearlyData [['meanArc2010']] + yearlyData [['seArc2010']], angle = 90, 
        length = 0.05, code = 3, col = colours [3], lwd = 3)
legend (x = 2005, y = 50, box.lty = 0, col = colours [c (1,3)], pch = 19, lwd = 3,
        legend = c ('breast-height','top-of-tree'), bg = 'transparent')
# Plot arc by tree
#----------------------------------------------------------------------------------------
# par (mar = c (5, 5, 1, 1))
# plot (x = treeData [['TreeID']], y = treeData [['meanArcBH']], axes = FALSE,
#       xlab = 'Tree', ylab = expression (paste ('Arc of density anomalies (', degree,')')), 
#       xlim = c (-2, 42), ylim = c (0, 380), col = colours [1], pch = 19, cex = 1.5)
# axis (1)
# axis (2, at = seq (0, 360, by = 60), las = 1)
# arrows (x0 = treeData [['TreeID']], 
#         y0 = treeData [['meanArcBH']] - treeData [['seArcBH']],
#         y1 = treeData [['meanArcBH']] + treeData [['seArcBH']], angle = 90, 
#         length = 0.05, code = 3, col = colours [1], lwd = 3)
# points (x = treeData [['TreeID']], y = treeData [['meanArc2010']], 
#         col = colours [3], pch = 19)
# arrows (x0 = treeData [['TreeID']], 
#         y0 = treeData [['meanArc2010']] - treeData [['seArc2010']],
#         y1 = treeData [['meanArc2010']] + treeData [['seArc2010']], angle = 90, 
#         length = 0.05, code = 3, col = colours [3], lwd = 3)
# #abline (v = -0.5, col = '#666666', lwd = 2)
# legend (x = 30, y = 60, box.lty= 0, col = colours [c (1,3)], 
#         legend = c ('breast height','top-of-tree'), pch = 19, cex = 1.0)

# Wrangle data to get average and standard deviation of arc by ring width
#----------------------------------------------------------------------------------------
temp1 <- data %>% filter (Year < 2017) %>% filter (DABH_1 %in% 1:2) %>% 
  mutate (RingWidth = RingWidthBH_1, Arc = ArcBH) 
temp2 <- data %>% filter (Year < 2017) %>% filter (DABH_2 %in% 1:2) %>% 
  mutate (RingWidth = RingWidthBH_2, Arc = ArcBH) 
temp3 <- data %>% filter (Year < 2017) %>% filter (DA2010_1 %in% 1:2) %>% 
  mutate (RingWidth = RingWidth2010_1, Arc = Arc2010)
temp4 <- data %>% filter (Year < 2017) %>% filter (DA2010_2 %in% 1:2) %>% 
  mutate (RingWidth = RingWidth2010_2, Arc = Arc2010) 
temp <- rbind (temp1, temp2, temp3, temp4); rm (temp1, temp2, temp3, temp4)

# Plot arc by tree
#----------------------------------------------------------------------------------------
par (mar = c (5, 1, 1, 1))
plot (x = temp [['RingWidth']], y = temp [['Arc']], axes = FALSE,
      xlab = 'Ring width (mm)', 
      ylab = '', 
      xlim = c (-0.5, 8), ylim = c (0, 380),
      col = '#FFA500', pch = 19)
axis (1)

# Test for effect of wood age on arc of density anomaly 
#----------------------------------------------------------------------------------------
modRingWidth <- lm (Arc ~ RingWidth, data = temp); rm (temp)
summary (modRingWidth)
anova (modRingWidth)
abline (modRingWidth, col = '#FFA500', lwd = 2, lty = 2)

# Wrangle data to get average and standard deviation of arc by ring width for 
# high-frequency years only
#----------------------------------------------------------------------------------------
temp1 <- data %>% filter (Year %in% c (1999, 2002, 2012, 2013, 2016)) %>% 
  filter (DABH_1 %in% 1:2) %>% 
  mutate (RingWidth = RingWidthBH_1, Arc = ArcBH) 
temp2 <- data %>% filter (Year %in% c (1999, 2002, 2012, 2013, 2016)) %>% 
  filter (DABH_2 %in% 1:2) %>% 
  mutate (RingWidth = RingWidthBH_2, Arc = ArcBH) 
temp3 <- data %>% filter (Year %in% c (1999, 2002, 2012, 2013, 2016)) %>% 
  filter (DA2010_1 %in% 1:2) %>% 
  mutate (RingWidth = RingWidth2010_1, Arc = Arc2010)
temp4 <- data %>% filter (Year %in% c (1999, 2002, 2012, 2013, 2016)) %>% 
  filter (DA2010_2 %in% 1:2) %>% 
  mutate (RingWidth = RingWidth2010_2, Arc = Arc2010) 
temp <- rbind (temp1, temp2, temp3, temp4); rm (temp1, temp2, temp3, temp4)

# Plot arc by tree
#----------------------------------------------------------------------------------------
points (x = temp [['RingWidth']], y = temp [['Arc']], axes = FALSE,
        xlab = 'Ring width (mm)', ylab = expression (paste ('Arc of density anomalies (', degree,')')), 
        xlim = c (0, 8), ylim = c (0, 380),
        col = '#D46300', pch = 19)

# Test for effect of wood age on arc of density anomaly 
#----------------------------------------------------------------------------------------
modRingWidth <- lm (Arc ~ RingWidth, data = temp); rm (temp)
summary (modRingWidth)
anova (modRingWidth)
abline (modRingWidth, col = '#D46300', lwd = 2, lty = 2)

legend (x = 4.2, y = 50, box.lty = 0, pch = 19, col = c ('#FFA500','#D46300'), 
        legend = c ('all years', 'high-frequency years only'))
dev.off ()

# Test for effect of year on arc of density anomaly
#----------------------------------------------------------------------------------------
modYearBH <- lm (ArcBH ~ Year, data = data)
summary (modYearBH)
anova (modYearBH)
modYear2010 <- lm (Arc2010 ~ Year, data = data)
summary (modYear2010)
anova (modYear2010)

# Test for effect of year on arc of density anomaly
#----------------------------------------------------------------------------------------
modTreeBH <- lm (ArcBH ~ TreeID, data = data)
summary (modTreeBH)
anova (modTreeBH)
modTree2010 <- lm (Arc2010 ~ TreeID, data = data)
summary (modTree2010)
anova (modTree2010)

# Wrangle data to get average and standard deviation of arc by wood age
#----------------------------------------------------------------------------------------
temp1 <- data %>% filter (Year < 2017) %>% filter (DABH_1 == 1 | DABH_2 == 1) %>% 
  mutate (WoodAge = WoodAgeBH, Arc = ArcBH) 
temp2 <- data %>% filter (Year < 2017) %>% filter (DA2010_1 == 1 | DA2010_2 == 1) %>% 
  mutate (WoodAge = WoodAge2010, Arc = Arc2010)
temp <- rbind (temp1, temp2); rm (temp1, temp2)
woodAgeData <- temp %>% group_by (WoodAge) %>% 
  summarise (meanArc = mean (Arc, na.rm = TRUE),
             seArc = se (Arc))

# Plot arc by wood age
#----------------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
plot (x = woodAgeData [['WoodAge']], y = woodAgeData [['meanArc']], axes = FALSE,
      xlab = "Wood's cambial age (years)", ylab = expression (paste ('Arc of density anomalies (', degree,')')), 
      xlim = c (0, 25), ylim = c (0, 380),
      col = '#FFA500', pch = 19)
axis (1, at = seq (0, 25, by = 5))
axis (2, at = seq (0, 360, by = 60), las = 1)
arrows (x0 = woodAgeData [['WoodAge']], 
        y0 = woodAgeData [['meanArc']] - woodAgeData [['seArc']],
        y1 = woodAgeData [['meanArc']] + woodAgeData [['seArc']], angle = 90, 
        length = 0.05, code = 3, col = '#FFA500', lwd = 3)

# Test for effect of wood age on arc of density anomaly 
#----------------------------------------------------------------------------------------
modWoodAge <- lm (Arc ~ WoodAge, data = temp); rm (temp)
summary (modWoodAge)
anova (modWoodAge)
abline (modWoodAge, col = '#FFA500', lwd = 2, lty = 2)
#========================================================================================