#========================================================================================
# Script to plot frequency of density anomalies over wood age
#----------------------------------------------------------------------------------------

# Download and read the data
#----------------------------------------------------------------------------------------
source ('readGrowthData.R')

# Wrangle data to get frequency over wood age for breat height, near-branch and top-of-tree
#----------------------------------------------------------------------------------------
temp <- data %>% filter (Year < 2019) %>% 
  mutate (DABH     = ifelse (DABH_1 %in% 1:2 | DABH_2 %in% 1:2, 1, 
                             ifelse (is.na (DABH_1) | is.na (DABH_2), NA, 0)),
          DABranch = ifelse (DABranch_1 %in% 1:2 | DABranch_2 %in% 1:2, 1, 
                             ifelse (is.na (DABranch_1) | is.na (DABranch_2), NA, 0)),
          DA2010   = ifelse (DA2010_1 %in% 1:2 | DA2010_2 %in% 1:2, 1, 
                             ifelse (is.na (DA2010_1) | is.na (DA2010_2), NA, 0)))
  
temp1 <- temp %>% group_by (WoodAgeBH) %>% 
  summarise (sumDABH = sum (DABH, na.rm = TRUE), nDABH = sum (!is.na (DABH))) %>% 
  mutate (perc = (sumDABH / nDABH) * 100.0, WoodAge = WoodAgeBH)

temp2 <- temp %>% group_by (WoodAgeBranch) %>% 
  summarise (sumDABranch = sum (DABranch, na.rm = TRUE), nDABranch = sum (!is.na (DABranch))) %>% 
  mutate (perc = (sumDABranch / nDABranch) * 100.0, WoodAge = WoodAgeBranch)

temp3 <- temp %>% group_by (WoodAge2010) %>% 
  summarise (sumDA2010 = sum (DA2010, na.rm = TRUE), nDA2010 = sum (!is.na (DA2010))) %>% 
  mutate (perc = (sumDA2010 / nDA2010) * 100.0, WoodAge = WoodAge2010)

tmp1 <- temp %>% filter (!is.na (DABH)) %>% mutate (DA = DABH, WoodAge = WoodAgeBH) %>% select (DA, WoodAge)
tmp2 <- temp %>% filter (!is.na (DABranch)) %>% mutate (DA = DABranch, WoodAge = WoodAgeBranch) %>% select (DA, WoodAge)
tmp3 <- temp %>% filter (!is.na (DA2010)) %>% mutate (DA = DA2010, WoodAge = WoodAge2010) %>% select (DA, WoodAge)
tmp <- rbind (tmp1, tmp2, tmp3) %>% group_by (WoodAge) %>% summarise (sumDA = sum (DA, na.rm = TRUE), nDA = sum (!is.na (DA)), perc = (sumDA / nDA) * 100)

# Plot frequency of occurence over age of the wood
#----------------------------------------------------------------------------------------
plot (x = tmp [['WoodAge']], y = tmp [['perc']], axes = FALSE, ylim = c (0, 100), 
      typ = 'l', col = 'white', xlab = 'Wood age (years)', 
      ylab = 'Frequency of density anomalies (%)', xlim = c (0, 25), lwd = 2)
axis (1, at = seq (0, 25, by = 5))
axis (2, las = 1)
lines (x = temp1 [['WoodAgeBH']],     y = temp1 [['perc']], col = colours [1], lwd = 2)
lines (x = temp2 [['WoodAgeBranch']], y = temp2 [['perc']], col = colours [2], lwd = 2)
lines (x = temp3 [['WoodAge2010']],   y = temp3 [['perc']], col = colours [3], lwd = 2)
lines (x = tmp   [['WoodAge']],       y = tmp   [['perc']], col = '#666666', lwd = 3)

#========================================================================================