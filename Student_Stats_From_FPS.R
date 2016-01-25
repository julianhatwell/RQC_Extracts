# Ad hoc look ups to try to match students not found in FPS

# Location of source file with the report code
fileLoc <- "I:\\04.Projects\\2016\\RQC_Reports\\"
# utilities file
source(paste0(fileLoc, "Utilities.R"))
# Locate and run source file
source(pathTo("RQC_Report_Functions.R"))

# PARAMETERS
# report parameters
thisReport <- "Edutrust"
currentYear <- 2015
prevYears <- c(2012, 2013, 2014, 2015)
courseStruct <- "Part Time"

FPS_A_File <- pathTo("FPS_140116_Active.csv")
FPS_D_File <- pathTo("FPS_140116_Deletions.csv")
FPS_C_File <- pathTo("FPS_200116_Corrections_2.csv")

FPS_Submission <- Edutrust_FPS_Stats()

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
# write.xlsx(ET$PastStudents
#            , Master_File, sheetName = "PastStudents"
# )
write.xlsx(FPS_Submission$PassTypeStats
           , Master_File, sheetName = "CitizensAndPassHolders"
)
write.xlsx(FPS_Submission$CourseStats
           , Master_File, sheetName = "CourseStats"
           , append = TRUE
)
write.xlsx(FPS_Submission$NationalityStats
           , Master_File, sheetName = "NationalityStats"
           , append = TRUE)

# Raw Data for validation
write.csv(FPS_Submission$currentYearData
          , CurrYearData_File
          , row.names = FALSE)
