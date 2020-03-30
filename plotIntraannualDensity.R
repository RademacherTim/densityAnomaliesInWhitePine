#========================================================================================
# produce figure of intraannual density for the average "normal" year versus the average 
# year with a density anomaly. 
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')

# set working directory
#----------------------------------------------------------------------------------------
setwd ('/home/tim/projects/densityAnomalies/densityAnomaliesInWhitePine/')

# read the x-ray density data
#----------------------------------------------------------------------------------------
densityData <- read_csv (file = '/media/tim/dataDisk/PlantGrowth/data/densiometry/Density.profiles.txt',
                         col_types = cols ())
# dist and dist1 seem to be the distance from the beginning of the ring
# dist2 ist the cumulative distance from the inner most ring

# convert the distance to percentage of ring width and bin the data in 1% wide bins
#----------------------------------------------------------------------------------------
densityData [['perDist']] <- (densityData [['dist']] / densityData [['TRW']]) * 100
densityData [['perDistBin']] <- floor (densityData [['perDist']])

# create %notin% operator 
#----------------------------------------------------------------------------------------
`%notin%` <- Negate (`%in%`)

# create summary data with first last year radii
#----------------------------------------------------------------------------------------
summaryData <- densityData %>% group_by (tree, radii) %>% summarise (minYear = min (year),
                                                                     maxYear = max (year)) 

# read the visually-dewtermined microdensity fluctuation data
#----------------------------------------------------------------------------------------
MDFO <- read_csv ('../data/densityFluctuationsinXRay.csv', col_types = cols ())
MDFO <- pivot_longer (MDFO, cols = 3:21, names_to = 'year', values_to = 'MDF')

# plot density data for all non MDA years
#----------------------------------------------------------------------------------------
con <- densityData [['tree']] == '01' & densityData [['radii']] == 'a' & 
       densityData [['year']] == 2015
layout (matrix (1:2, byrow = TRUE), height = c (1, 1.3))
par (mar = c (0, 5, 1, 1))
plot (x = densityData [['perDist']] [con],
      y = densityData [['density']] [con], 
      typ = 'l', lwd = 0, axes = FALSE,
      xlim = c (0, 100), ylim = c (0, 800),
      xlab = '', ylab = expression (paste ('density (kg ',m^-3,')')))

# add y-axis 
#----------------------------------------------------------------------------------------
axis (side = 2, las = 1, at = seq (200, 800, by = 200))

# loop over unique density profiles 
#----------------------------------------------------------------------------------------
for (i in 1:dim (summaryData) [1]) {

  # loop over years in each profile
  #--------------------------------------------------------------------------------------
  for (iYear in summaryData [['minYear']] [i]:summaryData [['maxYear']] [i]) {
    
    # does year contain a microdensity fluctuation
    #------------------------------------------------------------------------------------
    if (dim (filter (MDFO, year == iYear & 
                           tree == as.numeric (summaryData [['tree']] [i]) &
                           radii == summaryData [['radii']] [i])) [1] != 0) {
      if (filter (MDFO, year == iYear & 
                        tree == as.numeric (summaryData [['tree']] [i]) &
                        radii == summaryData [['radii']] [i]) [['MDF']] > 0) next
    }
    
    # create boolean
    #------------------------------------------------------------------------------------
    con <- densityData [['tree']]  == summaryData [['tree']]  [i] & 
           densityData [['radii']] == summaryData [['radii']] [i] & 
           densityData [['year']] == iYear
    
    # plot the year
    #------------------------------------------------------------------------------------
    lines (x = densityData [['perDist']] [con],
           y = densityData [['density']] [con],
           lwd = 0.1)
  }
}

# add panel descriptor  
#----------------------------------------------------------------------------------------
text (x = 20, y = 700, labels = 'without micro-density anomaly', col = '#333333')

# add separating line
#----------------------------------------------------------------------------------------
abline (h = 0, col = '#999999')

# estimate mean density for percentage ring width
#----------------------------------------------------------------------------------------
meanDensity <- densityData %>% filter (year %notin% c (2002, 2012, 2016)) %>% 
                               group_by (perDistBin) %>% select (perDistBin, density) %>%
                               summarise (meanDensity = mean (density))
lines (x = meanDensity [['perDistBin']] [meanDensity [['perDistBin']] <= 100],
       y = meanDensity [['meanDensity']] [meanDensity [['perDistBin']] <= 100], 
       lwd = 2, col = '#2081f9')

# plot second panel with all year with density anomalies
#----------------------------------------------------------------------------------------
con <- densityData [['tree']] == '01' & densityData [['radii']] == 'a' & 
       densityData [['year']] == 2016
par (mar = c (5, 5, 1, 1))
plot (x = densityData [['perDist']] [con],
      y = densityData [['density']] [con], 
      typ = 'l', lwd = 0, axes = FALSE,
      xlim = c (0, 100), ylim = c (100, 800),
      xlab = 'percentage ring width', ylab = expression (paste ('density (kg ',m^-3,')')))


# add y-axis 
#----------------------------------------------------------------------------------------
axis (side = 2, las = 1, at = seq (200, 800, by = 200))

# add x-axis 
#----------------------------------------------------------------------------------------
axis (side = 1, at = seq (0, 100, by = 20))

# loop over unique density profiles 
#----------------------------------------------------------------------------------------
for (i in 1:dim (summaryData) [1]) {
  # loop over years in each profile
  #--------------------------------------------------------------------------------------
  for (iYear in summaryData [['minYear']] [i]:summaryData [['maxYear']] [i]) {
    
    # does year contain a microdensity fluctuation
    #------------------------------------------------------------------------------------
    if (dim (filter (MDFO, year == iYear & 
                           tree == as.numeric (summaryData [['tree']] [i]) &
                           radii == summaryData [['radii']] [i])) [1] != 0) {
      if (filter (MDFO, year == iYear & 
                        tree == as.numeric (summaryData [['tree']] [i]) &
                        radii == summaryData [['radii']] [i]) [['MDF']]  == 0) next
    }
    
    # create boolean
    #------------------------------------------------------------------------------------
    con <- densityData [['tree']]  == summaryData [['tree']]  [i] & 
           densityData [['radii']] == summaryData [['radii']] [i] & 
           densityData [['year']] == iYear
    
    # plot the year
    #------------------------------------------------------------------------------------
    lines (x = densityData [['perDist']] [con],
           y = densityData [['density']] [con],
           lwd = 0.1)
  }
}

# add panel descriptor  
#----------------------------------------------------------------------------------------
text (x = 20, y = 700, labels = 'with micro-density anomaly', col = '#333333')

# estimate mean density for percentage ring width
#----------------------------------------------------------------------------------------
meanDensity <- densityData %>% filter (year %in% c (2002, 2012, 2016)) %>% 
               group_by (perDistBin) %>% select (perDistBin, density) %>%
               summarise (meanDensity = mean (density))
lines (x = meanDensity [['perDistBin']] [meanDensity [['perDistBin']] <= 100],
       y = meanDensity [['meanDensity']] [meanDensity [['perDistBin']] <= 100], 
       lwd = 2, col = '#fd6600')
#========================================================================================

