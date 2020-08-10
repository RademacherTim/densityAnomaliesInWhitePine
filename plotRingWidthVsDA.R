#========================================================================================
# Script to plot the relationship between ring width and density anomalies
#----------------------------------------------------------------------------------------

# Download and read the data
#----------------------------------------------------------------------------------------
source ('readGrowthData.R')
library ('tidyverse')

# Plot graph of density distribution by ring width with one panel for each height and a 
# row for all data and height frequency years only
#----------------------------------------------------------------------------------------
png (file = 'fig/RingWidthVSDensityAnomalies.png', width =  800, height = 450) # 16:9 aspect ratio
layout (matrix (1:12, nrow = 4, byrow = TRUE), widths = c (1.2, 1, 1),
        heights = c (1.1, 0.7, 1, 0.8))

for (years in c ('all', 'select')) {

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
  tmpBH        <- rbind (tmp1, tmp2); rm (tmp1, tmp2)
  rhoBH        <- density (tmpBH [['RingWidth']])
  rhoBHwith    <- density (tmpBH [['RingWidth']] [tmpBH [['DA']] >= 1])
  rhoBHwithout <- density (tmpBH [['RingWidth']] [tmpBH [['DA']] == 0])
  
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
  tmpBranch        <- rbind (tmp1, tmp2); rm (tmp1, tmp2)
  rhoBranch        <- density (tmpBranch [['RingWidth']])
  rhoBranchwith    <- density (tmpBranch [['RingWidth']] [tmpBranch [['DA']] >= 1])
  rhoBranchwithout <- density (tmpBranch [['RingWidth']] [tmpBranch [['DA']] == 0])
  
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
  tmp2010        <- rbind (tmp1, tmp2); rm (tmp1, tmp2)
  rho2010        <- density (tmp2010 [['RingWidth']])
  rho2010with    <- density (tmp2010 [['RingWidth']] [tmp2010 [['DA']] >= 1])
  rho2010without <- density (tmp2010 [['RingWidth']] [tmp2010 [['DA']] == 0])

  # Plot the graphs at breast height
  #--------------------------------------------------------------------------------------
  if (years == 'all')    par (mar = c (0, 8, 2, 1))
  if (years == 'select') par (mar = c (0, 8, 1, 1))
  plot (rhoBH, las = 1, xlim = c (0, 14), ylim = c (0, 0.55), xlab = '', lwd = 3, 
        axes = FALSE, ylab = 'Density of \n density anomalies', main = '', col = 'white',
        cex.lab = 1.2)
  axis (side = 1, cex.axis = 1.5)
  axis (side = 2, las = 1, cex.axis = 1.5)
  lines (rhoBHwith,    col = colours [1], lwd = 3, lty = 1)
  lines (rhoBHwithout, col = colours [1], lwd = 3, lty = 3)
  #if (years == 'all')    mtext (side = 2, line = 6, text = 'All years',       cex = 1.3, 
  #                              at = 0.1)
  #if (years == 'all')    mtext (side = 3, line = 0.3, text = 'Breast height', cex = 1.5)
  #if (years == 'select') mtext (side = 2, line = 6, text = 'High-frequency years only',     
  #                              cex = 1.3, at = 0.1)
  
  # Plot graph of density distribution by ring width near a branch
  #----------------------------------------------------------------------------------------
  if (years == 'all')    par (mar = c (0, 0, 2, 1))
  if (years == 'select') par (mar = c (0, 0, 1, 1))
  plot (rhoBranch, las = 1, xlim = c (0, 14), ylim = c (0, 0.55), xlab = '', lwd = 3, axes = FALSE,
        ylab = '', main = '', col = 'white')
  axis (1, cex.axis = 1.5)
  lines (rhoBranchwith,    col = colours [2], lwd = 3, lty = 1)
  lines (rhoBranchwithout, col = colours [2], lwd = 3, lty = 3)
  #if (years == 'all') mtext (side = 3, line = 0.3, text = 'Near-branch', cex = 1.5)
  
  # Plot graph of density distribution by ring width near a 2010
  #----------------------------------------------------------------------------------------
  if (years == 'all')    par (mar = c (0, 0, 2, 1))
  if (years == 'select') par (mar = c (0, 0, 1, 1))
  plot (rho2010, las = 1, xlim = c (0, 14), ylim = c (0, 0.55), xlab = '', lwd = 3, axes = FALSE,
        ylab = '', main = '', col = 'white')
  axis (1, cex.axis = 1.5)
  lines (rho2010with,  col = colours [3], lwd = 3, lty = 1)
  lines (rhoBHwithout, col = colours [3], lwd = 3, lty = 3)
  if (years == 'all') legend (x = 4.5, y = 0.55, col = colours, lty = 1, 
                              legend = c ('breast height','near-branch','top-of-tree'),
                              lwd = 3, box.lty = 0, cex = 1.3)
  if (years == 'select') legend (x = 4.5, y = 0.55, col = '#666666', lty = c (1, 3), 
                                 legend = c ('with density anomalies','without density anomalies'),
                                 lwd = 3, box.lty = 0, cex = 1.3)
  #if (years == 'all') mtext (side = 3, line = 0.3, text = 'Top-of-tree', cex = 1.5)
  
  # Plot boxplot of ring width by density anomaly at breast height
  #----------------------------------------------------------------------------------------
  par (mar = c (2, 8, 2, 1))
  plot (x = tmpBH [['RingWidth']] [tmpBH [['DA']] == 0], 
        y = jitter (tmpBH  [['DA']] [tmpBH [['DA']] == 0], amount = 0.2),
        xlab = 'Ring width (mm)', xlim =  c (0, 14), ylim = c (-0.5, 1.5),
        ylab = '', col = "white",#addOpacity (colours [1], 0.3), 
        pch = 19, axes = FALSE, cex.lab = 1.2)
  axis (2, at = 0:1, labels = c ('without','with'), las = 1, cex.axis = 1.5)
  mtext (side = 1, line = 1, text = 'Ring width (mm)')
  boxplot (tmpBH [['RingWidth']] [tmpBH [['DA']] == 0], at = 0, add = TRUE, 
           col = 'transparent', axes = FALSE, horizontal = TRUE, border = colours [1])
  # points (x = tmpBH [['RingWidth']] [tmpBH [['DA']] >= 1], 
  #         y = jitter (tmpBH  [['DA']] [tmpBH [['DA']] >= 1], amount = 0.2), 
  #         col = addOpacity (colours [1], 0.3), pch = 19)
  boxplot (tmpBH [['RingWidth']] [tmpBH [['DA']] >= 1], at = 1, add = TRUE, 
           col = addOpacity (colours [1], 0.5), axes = FALSE, horizontal = TRUE,
           border = colours [1])
  
  # Plot graph of ring width by density anomaly near a branch
  #----------------------------------------------------------------------------------------
  par (mar = c (2, 0, 2, 1))
  plot (x = tmpBranch [['RingWidth']] [tmpBranch [['DA']] == 0], 
        y = jitter (tmpBranch  [['DA']] [tmpBranch [['DA']] == 0], amount = 0.2), 
        xlab = '', ylim = c (-0.5, 1.5), xlim = c (0, 14),
        ylab = '', col = "white", #addOpacity (colours [2], 0.3), 
        pch = 19, axes = FALSE)
  mtext (side = 1, line = 1, text = 'Ring width (mm)')
  boxplot (tmpBranch [['RingWidth']] [tmpBranch [['DA']] == 0], at = 0, add = TRUE, 
           col = 'transparent', axes = FALSE, horizontal = TRUE, border = colours [2])
  # points (x = tmpBranch [['RingWidth']] [tmpBranch [['DA']] >= 1], 
  #         y = jitter (tmpBranch  [['DA']] [tmpBranch [['DA']] >= 1], amount = 0.2), 
  #         col = addOpacity (colours [2], 0.3), pch = 19)
  boxplot (tmpBranch [['RingWidth']] [tmpBranch [['DA']] >= 1], at = 1, add = TRUE, 
           col = addOpacity (colours [2], 0.5), axes = FALSE, horizontal = TRUE, 
           border = colours [2])
  
  # Plot graph of ring width by density anomaly near a 2010
  #----------------------------------------------------------------------------------------
  par (mar = c (2, 0, 2, 1))
  plot (x = tmp2010 [['RingWidth']] [tmp2010 [['DA']] == 0],
        y = jitter (tmp2010  [['DA']] [tmp2010 [['DA']] == 0], amount = 0.2), 
        xlab = '', ylim = c (-0.5, 1.5), xlim = c (0, 14),
        ylab = '', col = "white", #addOpacity (colours [3], 0.3), 
        pch = 19, axes = FALSE)
  mtext (side = 1, line = 1, text = 'Ring width (mm)')
  boxplot (tmp2010 [['RingWidth']] [tmp2010 [['DA']] == 0], at = 0, add = TRUE, 
           col = 'transparent', axes = FALSE, horizontal = TRUE, border = colours [3])
  # points (x = tmp2010 [['RingWidth']] [tmp2010 [['DA']] >= 1], 
  #         y = jitter (tmp2010  [['DA']] [tmp2010 [['DA']] >= 1], amount = 0.2), 
  #         col = addOpacity (colours [3], 0.3), pch = 19)
  boxplot (tmp2010 [['RingWidth']] [tmp2010 [['DA']] >= 1], at = 1, add = TRUE, 
           col = addOpacity (colours [3], 0.5), axes = FALSE, horizontal = TRUE, border = colours [3])
}
dev.off ()
#========================================================================================
