# This file simply calls the long processes and outputs the raw files. 
# All the processing code is in the source file

# Location of source file with the report code
fileLoc <- "I:\\04.Projects\\2016\\RQC_Reports\\"

# utilities file
source(paste0(fileLoc, "Utilities.R"))

# PARAMETERS
# report parameters
thisReport <- "Annual_Report"
thisReport2 <- "Waiver_Report"
currentYear <- 2015
courseStruct <- "Full Time"

# Locate and run source file
source(pathTo("RQC_Report_Functions.R"))

# Run the code, initialise the output files and save
# Separately for FT and PT
  AR <- Run_report(courseStruct, thisReport, B11_Generate)
  Save_report(AR, courseStruct, thisReport)
  AW <- Run_report(courseStruct, thisReport2, B11_Waivers)
  Save_report(AW, courseStruct, thisReport2)

# Manual versions!
# FT <- B11_Generate("Full Time")
# KHEA_Annual_Report_File <- pathTo("KHEA_Annual_Report.csv")
# fileInit(KHEA_Annual_Report_File)
# write.csv(FT, KHEA_Annual_Report_File, row.names = FALSE)
# 
# PT <- B11_Generate("Part Time")
# KHEI_Annual_Report_File <- pathTo("KHEI_Annual_Report.csv")
# fileInit(KHEI_Annual_Report_File)
# write.csv(PT, KHEI_Annual_Report_File, row.names = FALSE)
# 
# FT_WV <- B11_Waivers("Full Time")
# KHEA_Waiver_File <- pathTo("KHEA_Waiver_Report.csv")
# fileInit(KHEA_Waiver_File)
# write.csv(FT_WV, KHEA_Waiver_File, row.names = FALSE)
# 
# PT_WV <- B11_Waivers("Part Time")
# KHEI_Waiver_File <- pathTo("KHEI_Waiver_Report.csv")
# fileInit(KHEI_Waiver_File)
# write.csv(PT_WV, KHEI_Waiver_File, row.names = FALSE)