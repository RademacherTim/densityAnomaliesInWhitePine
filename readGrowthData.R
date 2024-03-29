#========================================================================================
# Script to read the goole spreadsheet with the data for the microdensity, as well as 
# apical and radial growth for the white pines at Harvard Forest.
#----------------------------------------------------------------------------------------

# Load dependencies
#----------------------------------------------------------------------------------------
library ('googledrive') # to access the goolge spreadsheet with the data
library ('readxl') 

# Secify google user
#----------------------------------------------------------------------------------------
user <- 'TR'
if (user == 'TR') {
  drive_auth (email = 'rademacher.tim@gmail.com')
} else if (user == 'EM') {
  drive_auth (email = 'e04miller@gmail.com')
}
# TR - I will eventually submit the data to the Harvard Forest Archives and download it 
# directly from there to avoid using google accounts.

# Establish file id
#----------------------------------------------------------------------------------------
fileID <- '1QwPjl2vji-KAcD1F92B814KXgFd7ZtJW3Do_TEDh67c'

# Set operation to quiet to avoid messages when sourced
#----------------------------------------------------------------------------------------
options (googledrive_quiet = TRUE)

# Download spreadsheet
#----------------------------------------------------------------------------------------
drive_download (file = as_id (fileID), overwrite = TRUE); rm (fileID)

# Read and delete downloaded data sheet
#----------------------------------------------------------------------------------------
data <- read_excel (path = 'Density Anomaly Data Master Sheet.xlsx', na = 'NA')

# Clean up
#----------------------------------------------------------------------------------------
rm (user)

#========================================================================================