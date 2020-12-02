#========================================================================================
# plot climate for the intra-annual density fluctuation prone years
#----------------------------------------------------------------------------------------

# load depdendencies
#----------------------------------------------------------------------------------------
if (!existsFunction ('esat'))       library ('plantecophys')
if (!existsFunction ('floor_date')) library ('lubridate')

# create %notin% operator
#----------------------------------------------------------------------------------------
`%notin%` <- Negate (`%in%`)

# download and read the daily mean tempearture and relative humidity, and total precipitation 
# from Fisher meteorological station
#----------------------------------------------------------------------------------------
metData <- read_csv (url ('https://harvardforest.fas.harvard.edu/data/p00/hf001/hf001-06-daily-m.csv'),
                     col_types = 'Didcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdc') %>%
  select (date, airt, prec, rh, bar)


# calculate daily vapour pressure deficit
#----------------------------------------------------------------------------------------
metData <- metData %>% mutate (vpd = RHtoVPD (RH = rh, TdegC = airt, Pa = bar / 10))

# summarise monthly means and totals
#----------------------------------------------------------------------------------------
monthlyMetData <- metData %>% 
  group_by (date = cut (date, breaks = '1 mon', ordered_result = TRUE)) %>%
  summarise (airt = mean (airt, na.rm = TRUE),
             prec = sum  (prec, na.rm = TRUE),
             rh   = mean (rh,   na.rm = TRUE),
             bar  = mean (bar,  na.rm = TRUE),
             vpd  = mean (vpd, na.rm = TRUE)) %>% 
  mutate (date = as_date (date) + days (15))

# initialise IADF prone years
#----------------------------------------------------------------------------------------
IADFyears <- c (2002, 2012, 2013, 2016)

# summarise climate for "normal" years
#----------------------------------------------------------------------------------------
averageClimate <- monthlyMetData %>% filter (year (date) %notin% IADFyears) %>%
  group_by (month (date)) %>% summarise (meanAirt = mean (airt, na.rm = TRUE),
                                         sdAirt = sd (airt, na.rm = TRUE),
                                         meanPrec = mean (prec, na.rm = TRUE),
                                         sdPrec = sd (prec, na.rm = TRUE),
                                         meanRh   = mean (rh,  na.rm = TRUE),
                                         meanBar  = mean (bar, na.rm = TRUE),
                                         meanVpd  = mean (vpd, na.rm = TRUE),
                                         sdVpd    = sd   (vpd, na.rm = TRUE))

# plot average climate 
#----------------------------------------------------------------------------------------
par (mfrow = c (5, 1))
par (mar = c (5, 10, 1, 5))
plot (x = averageClimate [, 1] [[1]],
      y = averageClimate [['meanAirt']], typ = 'l',
      col = '#c90016', las = 1, lwd = 2, xlab = 'Month', 
      ylab = '', 
      axes = FALSE,
      xlim = c (1, 12), ylim = c (-10, 25))
polygon (x = c (1:12, 12:1), 
         y = c (averageClimate [['meanAirt']] + averageClimate [['sdAirt']],  
                rev (averageClimate [['meanAirt']] - averageClimate [['sdAirt']])),
         lty= 0, col = '#c9001666')
axis (side = 1, at = 1:12, labels = c ('J', 'F','M','A','M','J','J','A','S','O','N','D'))
axis (side = 2, las = 1, col = '#c90016', col.axis = '#c90016')
mtext (side = 2, line = 3, text = expression (paste ('Air temperature (',degree,'C)', sep = '')), 
       col = '#c90016')
par (new = TRUE)
plot (x = averageClimate [, 1] [[1]],
      y = averageClimate [['meanPrec']],
      col = '#80b1d3', lwd = 2, typ = 'l', las = 1, xlab = '', ylab = '',
      ylim = c (0, 240), axes = FALSE)
polygon (x = c (1:12, 12:1), 
         y = c (averageClimate [['meanPrec']] + averageClimate [['sdPrec']], 
                rev (averageClimate [['meanPrec']] - averageClimate [['sdPrec']])),
         lty= 0, col = '#80b1d366')
axis (side = 4, las = 1, col = '#80b1d3', col.axis = '#80b1d3')
mtext (side = 4, line = 3, text = 'Precipitation (mm)', 
       col = '#80b1d3')
par (new = TRUE)
plot (x = averageClimate [, 1] [[1]],
      y = averageClimate [['meanVpd']],
      col = '#fc8d62', lwd = 2, typ = 'l', las = 1, xlab = '', ylab = '',
      ylim = c (0, 0.8), axes = FALSE, lty = 2)
polygon (x = c (1:12, 12:1), 
         y = c (averageClimate [['meanVpd']] + averageClimate [['sdVpd']], 
                rev (averageClimate [['meanVpd']] - averageClimate [['sdVpd']])),
         lty= 0, col = '#fc8d6266')
axis (side = 2, line = 5, las = 1, col = '#fc8d62', col.axis = '#fc8d62')
mtext (side = 2, line = 8, text = 'VPD (kPa)', 
       col = '#fc8d62')

# add climate for the IADF prone years (e.g., 2002, 2012, 2013, 2016)
for (iYear in IADFyears) {
  par (mar = c (5, 10, 1, 5))
  plot (x = month (select (filter (monthlyMetData,  year (date) == iYear), date) [[1]]),
        y = select (filter (monthlyMetData,  year (date) == iYear), airt) [[1]], typ = 'l',
        col = '#c90016', las = 1, lwd = 2, xlab = 'Month', 
        ylab = '', 
        axes = FALSE,
        ylim = c (-10, 25))
  axis (side = 1, at = 1:12, labels = c ('J', 'F','M','A','M','J','J','A','S','O','N','D'))
  axis (side = 2, las = 1, col = '#c90016', col.axis = '#c90016')
  mtext (side = 2, line = 3, text = expression (paste ('Air temperature (',degree,'C)', sep = '')), 
         col = '#c90016')
  par (new = TRUE)
  plot (x = month (select (filter (monthlyMetData,  year (date) == iYear), date) [[1]]),
        y = select (filter (monthlyMetData,  year (date) == iYear), prec) [[1]], 
        col = '#80b1d3', lwd = 2, typ = 'l', las = 1, xlab = '', ylab = '',
        ylim = c (0, 240), axes = FALSE)
  axis (side = 4, las = 1, col = '#80b1d3', col.axis = '#80b1d3')
  mtext (side = 4, line = 3, text = 'Precipitation (mm)', 
         col = '#80b1d3')
  par (new = TRUE)
  plot (x = month (select (filter (monthlyMetData,  year (date) == iYear), date) [[1]]),
        y = select (filter (monthlyMetData,  year (date) == iYear), vpd) [[1]],
        col = '#fc8d62', lwd = 2, typ = 'l', las = 1, xlab = '', ylab = '',
        ylim = c (0, 0.8), axes = FALSE, lty = 2)
  axis (side = 2, line = 5, las = 1, col = '#fc8d62', col.axis = '#fc8d62')
  mtext (side = 2, line = 8, text = 'VPD (kPa)', 
         col = '#fc8d62')
}
#========================================================================================

