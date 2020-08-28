#========================================================================================
# Script to plot the relationship between ring width and density anomalies
#----------------------------------------------------------------------------------------

# Download and read the data
#----------------------------------------------------------------------------------------
source ('readGrowthData.R')
library ('tidyverse')
library ('vioplot')
library ('car')

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
           xlab = '', ylab = '', names = labNames, 
           col = c ('transparent', addOpacity (colours [1], 0.5)), yaxt = 'n',
           cex.lab = 1.2, horizontal = TRUE, axes = FALSE, border = colours [1], 
           rectCol = '#444444', lineCol = colours [1], colMed = '#777777', lwd = 2)
  axis (side = 1)
  mtext (side = 1, line = 3, text = 'Ring width (mm)')
  if (years == 'all') {
    axis (side = 2, las = 1, at = 1:2, labels = c ('without','with'))
    mtext (side = 2, line = 5, text = 'Breast height', at = 1.5)
  }
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
  
  print (tmpBH %>% filter (DA == 1) %>% nrow ()) # Number of rings with density anomaly
  print (tmpBH %>% filter (DA == 0) %>% nrow ()) # Number of rings without density anomaly
  print (tmpBranch %>% filter (DA == 1) %>% nrow ()) # Number of rings with density anomaly
  print (tmpBranch %>% filter (DA == 0) %>% nrow ()) # Number of rings without density anomaly
  print (tmp2010 %>% filter (DA == 1) %>% nrow ()) # Number of rings with density anomaly
  print (tmp2010 %>% filter (DA == 0) %>% nrow ()) # Number of rings without density anomaly
}
dev.off ()


#----------------------------------------------------------------------------------------
png (file = 'fig/withinRingDADistributionViolin.png', width =  800, height = 450) # 16:9 aspect ratio
layout (matrix (1:2, nrow = 1, byrow = TRUE), widths = c (1.2, 1))

# Loop over all different selections of years
#----------------------------------------------------------------------------------------
for (years in c ('all', 'select')) {

  # Select years
  #--------------------------------------------------------------------------------------
  if (years == 'all')    Years <- 1993:2017
  if (years == 'select') Years <- c (1999, 2002, 2012, 2013, 2016) 
  
  # Wrangle data to get density distribution for when they occur
  #----------------------------------------------------------------------------------------
  temp1  <- data %>% filter (Year %in% Years) %>% filter (DABH_1 == 1) %>% 
    mutate (perc = PercentageDABH_1.1) %>% 
    select (perc) %>% filter (!is.na (perc))
  temp2  <- data %>% filter (Year %in% Years)%>% filter (DABH_2 == 1) %>%
    mutate (perc = PercentageDABH_2.1) %>% 
    select (perc) %>% filter (!is.na (perc))
  temp3  <- data %>% filter (Year %in% Years)%>% filter (DABranch_1 == 1) %>% 
    mutate (perc = PercentageDABranch_1.1) %>% 
    select (perc) %>% filter (!is.na (perc))
  temp4  <- data %>% filter (Year %in% Years) %>% filter (DABranch_2 == 1) %>% 
    mutate (perc = PercentageDABranch_2.1) %>% 
    select (perc) %>% filter (!is.na (perc))
  temp5  <- data %>% filter (Year %in% Years) %>% filter (DA2010_1 == 1) %>% 
    mutate (perc = PercentageDA2010_1.1) %>% 
    select (perc) %>% filter (!is.na (perc))
  temp6  <- data %>% filter (Year %in% Years) %>% filter (DA2010_2 == 1) %>%
    mutate (perc = PercentageDA2010_2.1) %>% 
    select (perc) %>% filter (!is.na (perc))
  temp7  <- data %>% filter (Year %in% Years) %>% filter (DABH_1 == 2) %>% 
    mutate (perc = PercentageDABH_1.2) %>% 
    select (perc) %>% filter (!is.na (perc))
  temp8  <- data %>% filter (Year %in% Years) %>% filter (DABH_2 == 2) %>%
    mutate (perc = PercentageDABH_2.2) %>% 
    select (perc) %>% filter (!is.na (perc))
  temp9  <- data %>% filter (Year %in% Years) %>% filter (DABranch_1 == 2) %>% 
    mutate (perc = PercentageDABranch_1.2) %>% 
    select (perc) %>% filter (!is.na (perc))
  temp10 <- data %>% filter (Year %in% Years) %>% filter (DABranch_2 == 2) %>% 
    mutate (perc = PercentageDABranch_2.2) %>% 
    select (perc) %>% filter (!is.na (perc))
  temp11 <- data %>% filter (Year %in% Years) %>% filter (DA2010_1 == 2) %>% 
    mutate (perc = PercentageDA2010_1.2) %>% 
    select (perc) %>% filter (!is.na (perc))
  temp12 <- data %>% filter (Year %in% Years) %>% filter (DA2010_2 == 2) %>% 
    mutate (perc = PercentageDA2010_2.2) %>% 
    select (perc) %>% filter (!is.na (perc))
  percentagesBH      <- rbind (temp1, temp2, temp7,  temp8)  [['perc']]
  percentagesBranch  <- rbind (temp3, temp4, temp9,  temp10) [['perc']]
  percentages2010    <- rbind (temp5, temp6, temp11, temp12) [['perc']]
  
  # Plot violin plot of ring width by density anomaly at breast height
  #----------------------------------------------------------------------------------------
  if (years == 'all') {
    par (mar = c (5, 8, 2, 1)) 
  } else if (years == 'select') {
    par (mar = c (5, 2, 2, 1))
  }
  vioplot (percentagesBH, ylim = c (0, 100), xlim = c (0.5, 3.5), 
           xlab = 'Percentage of ring formed (%)', names = '', yaxt = 'n',
           col = addOpacity (colours [1], 0.5), 
           cex.lab = 1.2, horizontal = TRUE, axes = FALSE, border = colours [1], 
           rectCol = '#444444', lineCol = colours [1], 
           colMed = '#777777', lwd = 2)
  vioplot (percentagesBranch, ylim = c (0, 100), xlim = c (0.5, 3.5), 
           ylab = '', names = '', add = TRUE, at = 2,
           col = addOpacity (colours [2], 0.5), 
           cex.lab = 1.2, horizontal = TRUE, axes = FALSE, border = colours [2], 
           rectCol = '#444444', lineCol = colours [2], 
           colMed = '#777777', lwd = 2)
  vioplot (percentages2010, ylim = c (0, 100), xlim = c (0.5, 3.5), 
           ylab = '', names = '', add = TRUE, at = 3,
           col = addOpacity (colours [3], 0.5), 
           cex.lab = 1.2, horizontal = TRUE, axes = FALSE, border = colours [3], 
           rectCol = '#444444', lineCol = colours [3], 
           colMed = '#777777', lwd = 2)
  axis (side = 1, at =  seq (0, 100, by = 20))
  if (years == 'all') axis (side = 2, at = 1:3, labels = c ('breast height', 'near-branch', 'top-of-tree'), las =1)

  print (percentagesBH %>% length ()) # Number of rings with density anomaly at breast height
  print (percentagesBranch %>% length ()) # Number of rings with density anomaly near branches
  print (percentages2010 %>% length ()) # Number of rings with density anomaly at the top of the tree
}  
dev.off ()
#========================================================================================
