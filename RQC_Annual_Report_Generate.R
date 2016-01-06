# This file simply calls the long processes and outputs the raw files. 
# All the processing code is in the source file:

# Locate and run source file
fileLoc <- "I:\\04.Projects\\2015\\RQC Annual Report\\"
source(paste0(fileLoc, "RQC_Annual_Report_Functions.R"))

# Run the code to create separate objects for PT and FT
# initialise the output files and save
FT <- B11_Generate("Full Time")
if (file.exists(KHEA_Annual_Report_File)) file.remove(KHEA_Annual_Report_File)
write.csv(FT, KHEA_Annual_Report_File, row.names = FALSE)

PT <- B11_Generate("Part Time")
if (file.exists(KHEI_Annual_Report_File)) file.remove(KHEI_Annual_Report_File)
write.csv(PT, KHEI_Annual_Report_File, row.names = FALSE)

FT_WV <- B11_Waivers("Full Time")
if (file.exists(KHEA_Waiver_File)) file.remove(KHEA_Waiver_File)
write.csv(FT_WV, KHEA_Waiver_File, row.names = FALSE)

PT_WV <- B11_Waivers("Part Time")
if (file.exists(KHEI_Waiver_File)) file.remove(KHEI_Waiver_File)
write.csv(PT_WV, KHEI_Waiver_File, row.names = FALSE)
