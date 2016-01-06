library(xlsx)
library(dplyr)
library(lubridate)
library(data.table)

# file names
# path should come from the calling file
Intake_Rep_File <- pathTo("IntakeReport.csv")
IC_File <- pathTo("NRIC_Fin.csv")
KHEA_PermittedCourses_File <- pathTo("KHEA Courses 271115.xlsx")
KHEI_PermittedCourses_File <- pathTo("KHEI Courses 271115.xlsx")
KHEA_CourseMapping_File <- pathTo("KHEA CourseMapping.xlsx")
KHEI_CourseMapping_File <- pathTo("KHEI CourseMapping.xlsx")
NationalityMapping_File <- pathTo("NationalityMapping.xlsx")
KHEA_CourseFees_File <- pathTo("KHEA Sales Line Amount.csv")
KHEI_CourseFees_File <- pathTo("KHEI Sales Line Amount.csv")
HighestQuals_File <- pathTo("HighestQuals.csv")

# Waivers Report files
Waivers_File <- pathTo("waivers.csv")

# AUTOMATION CODE
# Pull in nationality mapping tables
NationalityMapping <- read.xlsx(NationalityMapping_File, sheetIndex = 1, startRow = 1, stringsAsFactors = FALSE)
NationalityMapping$CPE[NationalityMapping$Nav == "NORTH KOREAN"] <- "DEMOCRATIC PEOPLE'S REPUBLIC OF KOREA"
NationalityMapping$CPE[NationalityMapping$Nav == "LAOTIAN"] <- "LAO PEOPLE'S DEMOCRATIC REPUBLIC"

# read in the data files and do any data cleaning
# the IC detail is currently mastered from 3 locations
IC <- read.csv(IC_File, stringsAsFactors = FALSE)
KSS <- read.csv(Intake_Rep_File, na.strings = "", stringsAsFactors = FALSE) %>%
  left_join(IC) %>%
  # because we don't want any pre-enrolment status anywhere in our counting
  filter(!studentStatus %in% c("Accepted"
                             , "Applicant"
                             , "Application Approved"
                             , "Application Deferred"
                             , "Application Withdrawn"
                             , "Application Rejected"
                             , "Admission Assessment Complete"
                             , "Submission to Admissions Office"
                             , "Admission Assessment in Progress"
                             , "Error Entry"))

B11_Subset <- function(courseStruct){
  # Create the various sets
  KSS_Active <- filter(KSS, studentStatus == "Active" &
                       year(as.Date(KSS$intakeStartDate)) <= currentYear)
  KSS_Grads <- filter(KSS, studentStatus %in% c("Graduated", "Course Ended") &
                  year(as.Date(KSS$statusDate)) == currentYear &
                  year(as.Date(KSS$intakeStartDate)) <= currentYear)
  KSS_Other <- filter(KSS, year(as.Date(KSS$intakeStartDate)) == currentYear &
                      !studentStatus %in% c("Active", "Graduated", "Course Ended"))
  # A local function to recombine the sets by FT/PT or Online in future
  ReCombine <- function(A, G, O, Org) {
    rbind(A, G, O) %>%
      filter(organisation == Org)
  }
  
  # put together the required records
  B11 <- ReCombine(KSS_Active, KSS_Grads, KSS_Other, courseStruct)
  # Find and keep only the latest admission 
  # for students with multiple admissions on the same program 
  LA <- setkey(data.table(B11), contactId, program)
  LatestAdmission <- LA[, .(applicationId = max(applicationId)), by=.(contactId, program)]
  B11 <- semi_join(B11, LatestAdmission)
  
  # pass the finished dataset back up to the call
  B11
}

# Function to translate KSS statuses to CPE values
MapStatusCPE <- function(cs) {
  if (is.na(cs)) return("UNKNOWN")
  if (cs == "Active") return("EXISTING")
  if (cs == "Withdrawn") return("LEFT BEFORE COMPLETION")
  if (!(cs %in% c("Deferred", "Graduated"))) return("OTHERS")
  toupper(cs)
}

# Routine to map Courses to CPE Permitted Course Titles
B11_Academic <- function(courseStruct) {

  CourseMapping_File <- get(paste0(KHEA_or_KHEI(courseStruct), "_CourseMapping_File"))
  CourseMapping <- read.xlsx(CourseMapping_File, sheetIndex = 1, startRow = 1, stringsAsFactors = FALSE)
  
  PermittedCourses_File <- get(paste0(KHEA_or_KHEI(courseStruct), "_PermittedCourses_File"))
  PermittedCourses <- read.xlsx(PermittedCourses_File, sheetIndex = 1, startRow = 1, stringsAsFactors = FALSE)
  
  # function to Map any that don't match the CPE registered name
  find_in_Course.Title <- find_in_vector(trimws(PermittedCourses$Course.Title))
  
  MapCourseTitles <<- function(ct,prtn) {
    # Some duplicate names are qualified by the university name - 
    # need to de-dupe first in order to avoid creating a list. must return an atomic vector.
    x <- length(which(trimws(CourseMapping$Course.Titles) == trimws(ct), arr.ind = TRUE))
    if (x > 1) ct <- toupper(paste0(ct, " (", trimws(prtn), ")"))
    
    # Now we've de deduped by partner, we should be able to test if the course is in the allowed list
    if (find_in_Course.Title(trimws(toupper(ct)))) return(trimws(toupper(ct)))
    
    # at this point we're not in the allowed list so we look again at the mapping table
    x <- length(which(trimws(CourseMapping$Course.Titles) == trimws(ct), arr.ind = TRUE))
    if (x > 1) ct <- return(paste("DUPLICATE:", ct))
    if (x == 0) return(paste("NO MATCH:", ct)) # no match in permitted and no mapping
    
    # here we have items we did match in mapping so will return the mapped value
    ct <- CourseMapping$CPE.registered.Course.Titles.[(which(trimws(CourseMapping$Course.Titles) == trimws(ct), arr.ind = TRUE))]
    return(ct)
  }
}

# Function to map Nationalities
mapNationalities <- function(Nat) {
  n <- NationalityMapping$CPE[NationalityMapping$Nav == Nat]
  ifelse(length(n) > 0, n, paste("Incorrect Label", Nat))
}

# Function to map identity and pass types to CPE values
MapIdentityPassType <- function(id) {
  if (is.na(id)) return("")
  if (id == "Singapore Citizen") return("SINGAPOREAN")
  if (id == "Dependent") return("DEPENDENT\'S PASS")
  if (id == "Student Pass") return("STUDENT\'S PASS")
  if (id %in% c("Employment", "S Pass", "Work Permit")) return("WORK PASS")
  return(toupper(id))
}

# Routine to create functions to find student financial data / sales order
B11_Financial <- function(courseStruct) {
  # Select the appropriate fees file
  CourseFees_File <- get(paste0(KHEA_or_KHEI(courseStruct), "_CourseFees_File"))
  
  # create an ordered table of Student, Doc and Total Fee
  CF <- data.table(read.csv(CourseFees_File, stringsAsFactors = FALSE))
  setkey(Fees <- data.table(CF[, .(Fee = sum(Amount)), by=.(sellTo, contactId, DocNo)]), sellTo)
  
  # create an ordered table of Student and Navision Intake Code
  CV <- CF[!(grepl("^ADMIN", LineType))
           , NavisionIntake := ifelse(Variant == ""
                                      , LineType
                                      , paste0(LineType, "_-", Variant))][, .(sellTo, contactId, DocNo, NavisionIntake)]
  
  setkey(Variants <- unique(data.table(
    sellTo = CV$sellTo
    , contactId = CV$contactId
    , DocNo = CV$DocNo
    , NavisionIntake = CV$NavisionIntake)
  ),sellTo)
  
  # write the closures to the top level functions for matching students, intake codes, docs and fees
  FindDoc <<- function(Std, Nav) {
    # fetch in the Variant Codes
    s <- Variants[Variants$sellTo == Std & !(is.na(Variants$NavisionIntake)),]
    if (nrow(s) == 0) s <- Variants[Variants$contactId == Std & !(is.na(Variants$NavisionIntake)),]
    if (nrow(s) == 0) return("No Sales Order")
    
    # utility
    doc <- character(0)
    return_doc <- function(d) {
      if (length(d) == 1) return(d)
      if (length(d) > 1) return(min(d))
    }
    
    # loop through once to get any exact match
    for (i in 1:nrow(s)) {
      if (s[i]$NavisionIntake == Nav) doc <- c(doc, s[i]$DocNo)
    }
    if (length(doc) != 0) { return_doc(doc)
    } else {
      # loop through a second time to get any match on same program
      for (i in 1:nrow(s)) {
        if (grepl(s[i]$NavisionIntake, Nav)) doc <- c(doc, s[i]$DocNo)
      }
    }
    if (length(doc) != 0) { return_doc(doc)
    } else return("No Matching Intake")
  }
  
  FindFee <<- function(Std, Doc) {
    f <- Fees[Fees$sellTo == Std & Fees$DocNo == Doc, Fee]
    Fee <- ifelse(length(f) > 0 , f, 0)
    Fee
  }
}

# compute new columns from above functions then return the required columns arranged in the right order
B11_Prep <- function(B11) {
 B11 %>% 
    # remove a few programs we know we're not interested in
    filter(!program %in% c("Study Tour", "English Experience Program", "Degree Experience Program") &
                  !grepl("Extended Induction Program", program)) %>%
    # calculate all the required columns and mappings
    mutate(Nationality = sapply(nationality, mapNationalities)
                , IdentityPassType = sapply(identificationType, MapIdentityPassType)
                , nric_or_fin = ifelse(IdentityPassType == "SINGAPORE PR", nric
                                       , ifelse(NRIC_FIN !="", NRIC_FIN, nric))
                , DateOfBirth = format(as.Date(dob), "%d/%m/%Y")
                , Org = sub(" ", "-", organisation)
                , Permitted_Course_Title = mapply(FUN = MapCourseTitles, ct = program, prtn = partner)     
                , TitleOfModule = ""
                , ModeOfDelivery = "Classroom Learning"
                , SalesOrder = mapply(FUN = FindDoc, Std = contactId, Nav = navisionCode) # Needs Nav Intake Codes
                , Fee = mapply(FUN = FindFee, Std = contactId, Doc = SalesOrder)
                , CommencementDate = format(as.Date(intakeStartDate), "%m/%Y")
                , studentStatusCPE = sapply(studentStatus, MapStatusCPE)
                # The next set of columns will all serve to calculate the reportable end date
                # I have separated it into steps for readability/maintainability
                , latestTermEndDate = ifelse(is.na(latestTermEndDate) | latestTermEndDate == "0000-00-00"
                                             , "1970-01-01", latestTermEndDate)
                , ExpectedGradDate = ifelse(studentStatus %in% c("Graduated", "Course Ended")
                                                      , statusDate, intakeEndDate)
                , LatestStudiesEndDate = ifelse(as.Date(ExpectedGradDate) >= as.Date(latestTermEndDate)
                                           , ExpectedGradDate, latestTermEndDate)
                , Calculated_End_Date = ifelse(studentStatusCPE == "EXISTING" &
                                                 month(LatestStudiesEndDate) >= 10 & 
                                                 month(LatestStudiesEndDate) <= 12 & 
                                                 year(LatestStudiesEndDate) == currentYear
                                               , paste0(as.character(currentYear + 1), "-01-31"), LatestStudiesEndDate)
                , ExpectedDateOfQualification = format(as.Date(Calculated_End_Date), "%m/%Y")
                ) %>%
    # return only the required columns
    select(contactId
         , navisionCode
         , SalesOrder
         , name
         , nric_or_fin, Nationality
         , IdentityPassType, gender
         , DateOfBirth, highestQualification
         , Org, Permitted_Course_Title
         , TitleOfModule, ModeOfDelivery
         , Fee, CommencementDate
         , studentStatusCPE
         , ExpectedDateOfQualification)
}

# Execute all the above processing functions
B11_Generate <- function(courseStruct) {
  B11_Academic(courseStruct)
  B11_Financial(courseStruct)
  B11_Subset(courseStruct) %>% B11_Prep
}

# Waivers Report
Waivers <- read.csv(Waivers_File, stringsAsFactors = FALSE)

find_in_KSS.contactId <- find_in_vector(KSS$contactId)

find_idType <- function(CT) {
  if (find_in_KSS.contactId(CT)) {
    k <- KSS[KSS$contactId == CT, c("applicationId", "identificationType") ]
    if (nrow(k) > 1) {
      k <- unique(k[k$applicationId == max(k$applicationId),])
    }
    return(k$identificationType)
  }
  return("KSS Not Found")
}

B11_Waivers <- function(courseStruct) {
  
  WV <- Waivers %>% 
    filter(endDate >= as.Date(paste0(currentYear, "-01-01")) &
             startDate <= as.Date(paste0(currentYear, "-12-15"))) %>%
    mutate(identificationType = sapply(contactId, find_idType)) %>%
    filter(!(identificationType == "KSS Not Found") & 
             CourseStructure == courseStruct) %>%
    mutate(Nationality = sapply(nationality, mapNationalities)
           , IdentityPassType = sapply(identificationType, MapIdentityPassType))
  
  n_runs <- select(WV, description, startDate) %>% 
    distinct(description, startDate) %>%
    group_by(description) %>% 
    summarise(runs = n())  
  n_students <- summarise(group_by(WV, description), students = n())
  n_singaporean <- summarise(group_by(WV, description, nationality), singaporean = n()) %>% 
    filter(nationality == "SINGAPOREAN") %>%
    select(-2)
  n_pr <- summarise(group_by(WV, description, IdentityPassType), pr = n()) %>% 
    filter(IdentityPassType == "SINGAPORE PR") %>%
    select(-2)
  
  WaiverReport <- inner_join(n_students, n_runs) %>%
    inner_join(n_singaporean) %>%
    inner_join(n_pr) %>%
    mutate(WSQ = "", Fee = "", Duration = "") %>%
    select(description, WSQ, Fee, Duration, runs, students, singaporean, pr)
  
  WaiverReport
}

# Edutrust Report doubles up as generic snapshot stats of Active Students
Edutrust_Stats <- function(B11, prevYears) {
  
  studyYear <- function(yr, startDate, endDate) {
    ifelse(year(as.Date(endDate)) >= yr &
             year(as.Date(startDate)) <= yr, TRUE, FALSE)
  }
  
  # start again with the full set as we're calculating previous year stats
  PY <- KSS %>% filter(organisation == courseStruct) %>%
    mutate(latestKnownEndDate = apply(cbind(intakeEndDate
                                            , latestTermEndDate)
                                      , 1, max))
           
  calculateStudyYears <- function(df, startDateCol, endDateCol, prevYears) {
    newCols <- c(names(df), paste0("studyYear", prevYears))
    for (yr in prevYears) {
      studyYear(yr, df[[startDateCol]], df[[endDateCol]])
      df <- cbind(df, studyYear(yr, df[[startDateCol]], df[[endDateCol]])) 
    }
    names(df) <- newCols
    df
  }
  
  PY <- calculateStudyYears(PY, "intakeStartDate", "latestKnownEndDate", prevYears)
  PY_WV <- calculateStudyYears(Waivers, "startDate", "endDate", prevYears)

  PastStudents <- as.data.frame(rbind(
    sapply(select(PY, starts_with("studyYear")), sum)
    , sapply(select(PY_WV, starts_with("studyYear")), sum)))
  PastStudents <- rbind(PastStudents
                        , apply(PastStudents, 2, sum))
  rownames(PastStudents) <- c("permitted", "short", "total")

  # here we only want current, active student stats 
  ET <- filter(B11, studentStatusCPE == "EXISTING")
  
  PassTypeStatsRaw <- sort(table(ET$IdentityPassType)
                           , decreasing = TRUE)
  PassTypeStats <- data.frame("NumberOfStudents" = PassTypeStatsRaw)
  
  CourseStatsRaw <- sort(table(ET$Permitted_Course_Title)
                         , decreasing = TRUE)
  CourseStats <- data.frame("CourseTitle" = names(CourseStatsRaw)
                            , "NumberOfStudents" = CourseStatsRaw) %>%
    mutate("PercentOfStudents" = round(
      NumberOfStudents/nrow(ET)*100
      , 2)
    )
  
  NationalityStatsRaw <- sort(table(ET$Nationality)
                              , decreasing = TRUE)
  NationalityStats <- data.frame("Nationality" = names(NationalityStatsRaw)
                                 , "NumberOfStudents" = NationalityStatsRaw) %>%
    mutate("PercentOfStudents" = round(
      NumberOfStudents/nrow(ET)*100
      , 2)
    )
  
  list(PastStudents = PastStudents
      , PassTypeStats = PassTypeStats
      , CourseStats = CourseStats
      , NationalityStats = NationalityStats
      , CurrentYearData = ET
      , PreviousYearData = PY
      , PreviousYearsWaiverData = PY_WV)
}

Edutrust_Prep <- function(prevYears) {
  function(courseStruct) {
    B11_Generate(courseStruct) %>% Edutrust_Stats(prevYears = prevYears)
  }
}

Edutrust_Generate <- Edutrust_Prep(prevYears)