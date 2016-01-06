# paste two strings together - for creating file names from params
me_File <- function(f) { paste0(f, "_File") }

# paste two strings together - for combining file paths
pathTo <- function(f) { paste0(fileLoc, f) }

# deletes a file
fileInit <- function(f) { if (file.exists(f)) file.remove(f) }

# give it a vector and returns a function to search that vector
find_in_vector <- function(vec) { function(find) { any(find == vec) } }

# return a string based on Full Time or Part Time - for prefixing file names
KHEA_or_KHEI <- function(courseStruct) {
  if (!(courseStruct %in% c("Full Time", "Part Time"))) {
    stop("Course Structure must be set as Full Time or Part Time.")
  }
  if (courseStruct == "Full Time") {
    "KHEA"
  } else {
    "KHEI"
  }
}

# paste two strings together - for making Org specific file names
legalName <- function(courseStruct, f) { 
  paste0(KHEA_or_KHEI(courseStruct), "_", f) 
}

Run_report <- function(courseStruct, report, reportFunc) {
  # dynamically assign the variable name before generating the data
  assign(legalName(courseStruct, report), reportFunc(courseStruct))
}

Run_and_save <- function(courseStruct, report, reportFunc) {
  # dynamically generate the file names
  outputFile <- pathTo(legalName(courseStruct, paste0(report, ".csv")))
  fileInit(legalName(courseStruct, outputFile))

  # dynamically assign the variable name before generating the data
  report <- Run_report(courseStruct, report, reportFunc)
  write.csv(report
          , outputFile
          , row.names = FALSE)
}