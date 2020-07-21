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
  mutate (perc = (sumDABH / nDABH) * 100.0)

temp2 <- temp %>% group_by (WoodAgeBranch) %>% 
  summarise (sumDABranch = sum (DABranch, na.rm = TRUE), nDABranch = sum (!is.na (DABranch))) %>% 
  mutate (perc = (sumDABranch / nDABranch) * 100.0)

temp3 <- temp %>% group_by (WoodAge2010) %>% 
  summarise (sumDA2010 = sum (DA2010, na.rm = TRUE), nDA2010 = sum (!is.na (DA2010))) %>% 
  mutate (perc = (sumDA2010 / nDA2010) * 100.0)

plot (temp1 [['WoodAgeBH']], temp1 [['perc']])
plot (temp2 [['WoodAgeBranch']], temp2 [['perc']])
plot (temp3 [['WoodAge2010']], temp3 [['perc']])

#========================================================================================