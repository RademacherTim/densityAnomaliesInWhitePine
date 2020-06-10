#========================================================================================
# Script with variables and functions relevant for ploting figuresin R
#----------------------------------------------------------------------------------------

# Load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')

# function for calculate the standard error
#----------------------------------------------------------------------------------------
se <-  function (x) {
  sd (x, na.rm = TRUE) / sqrt (sum (!is.na (x)))
}

# Create %notin% operator
#----------------------------------------------------------------------------------------
`%notin%` <- Negate (`%in%`)

# set colours for treatments: control, girdled, compressed, double compressed and chilled
#----------------------------------------------------------------------------------------
colours <- c ('#a6cee3','#1f78b4','#b2df8a')

# function to add opacity to a colour
#----------------------------------------------------------------------------------------
addOpacity <- function (colour, alpha = 1) {
  if (missing (colour)) stop ("Please provide a colour or vector of colours.")
  apply (sapply (colour, col2rgb) / 255, 2, 
         function (x) rgb (x [1], x [2], x [3], alpha = alpha))
}

#========================================================================================