#========================================================================================
# Script to plot the relationship between ring width and density anomalies
#----------------------------------------------------------------------------------------

# Download and read the data
#----------------------------------------------------------------------------------------
source ('readGrowthData.R')
library ('tidyverse')
library ('vioplot')

# Plot graph of density distribution by ring width with one panel for each height and a 
# row for all data and height frequency years only
#----------------------------------------------------------------------------------------
png (file = 'fig/RingWidthVSDensityAnomaliesViolinPlots.png', width =  800, height = 450) # 16:9 aspect ratio
layout (matrix (1:2, nrow = 1, byrow = TRUE), widths = c (1.2, 1))

# Loop over all different selections of years
#----------------------------------------------------------------------------------------
for (years in c ('all', 'select')) {
  
  # Select years
  #--------------------------------------------------------------------------------------
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
  tmpBH <- rbind (tmp1, tmp2); rm (tmp1, tmp2)
  tmpBH [['DA']] [tmpBH [['DA']] == 2] <- 1
  
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
  tmpBranch <- rbind (tmp1, tmp2); rm (tmp1, tmp2)
  tmpBranch [['DA']] [tmpBranch [['DA']] == 2] <- 1
  
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
  tmp2010 <- rbind (tmp1, tmp2); rm (tmp1, tmp2)
  tmp2010 [['DA']] [tmp2010 [['DA']] == 2] <- 1
  
  # Plot violin plot of ring width by density anomaly at breast height
  #----------------------------------------------------------------------------------------
  if (years == 'all') {
    par (mar = c (5, 8, 2, 1))
    labNames = c ('without', 'with') 
  } else if (years == 'select') {
    par (mar = c (5, 2, 2, 1))
    labNames = c ('','')
  }
  vioplot (RingWidth~DA, data = tmpBH, xlim = c (0.5, 6.5), ylim = c (0, 14), las = 1,
           ylab = '', names = labNames, 
           col = c ('transparent', addOpacity (colours [1], 0.5)), yaxt = 'n',
           cex.lab = 1.2, horizontal = TRUE, axes = FALSE, border = colours [1], 
           rectCol = '#444444', lineCol = colours [1], colMed = '#777777', lwd = 2)
  axis (side = 1)
  #mtext (side = 1, line = 1, text = 'Ring width (mm)')
  if (years == 'all') axis (side = 2, las = 1, at = 1:2, labels = c ('without','with'))
  if (years == 'all') axis (side = 2, las = 1, at = 3:4, labels = c ('without','with'))
  if (years == 'all') mtext (side = 2, line = 5, text = 'Breast height', at = 1.5)
  vioplot (RingWidth~DA, data = tmpBranch, add = TRUE,
           xlab = '', at = 3:4, las = 1,
           ylab = '', names = labNames, 
           col = c ('transparent', addOpacity (colours [2], 0.5)), 
           cex.lab = 1.2, horizontal = TRUE, axes = FALSE, border = colours [2], 
           rectCol = '#444444', lineCol = colours [2], colMed = '#777777', lwd = 2)
  if (years == 'all') axis (side = 2, las = 1, at = 3:4, labels = c ('without', 'with'))
  if (years == 'all') mtext (side = 2, line = 5, text = 'Near-branch', at = 3.5)
  vioplot (RingWidth~DA, data = tmp2010, add = TRUE,
           xlab = '', at = 5:6, las = 1,
           ylab = '', names = labNames, 
           col = c ('transparent', addOpacity (colours [3], 0.5)), 
           cex.lab = 1.2, horizontal = TRUE, axes = FALSE, border = colours [3], 
           rectCol = '#444444', lineCol = colours [3], colMed = '#777777', lwd = 2)
  if (years == 'all') axis (side = 2, las = 1, at = 5:6, labels = c ('without', 'with'))
  if (years == 'all') mtext (side = 2, line = 5, text = 'Top-of-tree', at = 5.5)
}
dev.off ()
#========================================================================================
