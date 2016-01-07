# This file simply calls the long processes and outputs the raw files. 
# All the processing code is in the source file

# Location of source file with the report code
fileLoc <- "I:\\04.Projects\\2016\\RQC_Reports\\"

# utilities file
source(paste0(fileLoc, "Utilities.R"))

# PARAMETERS
# report parameters
thisReport <- "Edutrust"
currentYear <- 2015
prevYears <- c(2012, 2013, 2014, 2015)
courseStruct <- "Part Time"

# Locate and run source file
source(pathTo("RQC_Report_Functions.R"))

# Run the code. 
# The file save requirements are more complex than the annual report
  ET <- Run_report(courseStruct, thisReport, Edutrust_Generate)

  # initialise the output files
  Master_File <- pathTo(legalName(courseStruct, "Edutrust.xlsx"))
  fileInit(Master_File)
  
  CurrYearData_File <- pathTo(legalName(courseStruct, "CurrYearData.csv"))
  fileInit(CurrYearData_File)
  
  PrevYearData_File <- pathTo(legalName(courseStruct, "PrevYearData.csv"))
  fileInit(PrevYearData_File)
  
  PrevYearWaiverData_File <- pathTo(legalName(courseStruct, "PrevYearWaiverData.csv"))
  fileInit(PrevYearWaiverData_File)

  # save the extract to sheets
  write.xlsx(ET$PastStudents
             , Master_File, sheetName = "PastStudents"
             )
  write.xlsx(ET$PassTypeStats
             , Master_File, sheetName = "CitizensAndPassHolders"
             , append = TRUE
             )
  write.xlsx(ET$CourseStats
             , Master_File, sheetName = "CourseStats"
             , append = TRUE
             )
  write.xlsx(ET$NationalityStats
             , Master_File, sheetName = "NationalityStats"
             , append = TRUE)
  
  # Raw Data for validation
  write.csv(ET$currentYearData
             , CurrYearData_File
             , row.names = FALSE)
  write.csv(ET$PreviousYearData
             , PrevYearData_File
             , row.names = FALSE)
  write.csv(ET$PreviousYearsWaiverData
            , PrevYearWaiverData_File
            , row.names = FALSE)
