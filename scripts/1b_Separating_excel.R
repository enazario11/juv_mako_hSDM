library(readxl)
library(readr)
library(stringr)
setwd("data/Mako Data/test_dive")
getwd()
rm(list = ls())

########################################
#All folder names changed, moved, or deleted, so if need to rerun split command will need to update entries
########################################

# Read sheets and use for filenames - TEST
sheets <- excel_sheets("test_dive/io040620_S.xlsx")
filenames <- paste0(sheets, ".csv")

  #identifies the first five characters as the variable prefix
for (sheet in sheets) {
  id <- substr(sheet, 1, 5)
  print(id)
  if (str_detect(id, "^[:digit:]+$")) {
    prefix <- id
  }
}

  #pastes the prefix variable from the previous loop in front of the location sheet name
for (i in 1:length(sheets)) {
  id <- substr(sheets[i], 1, 5)
  if (id != prefix) {
     sheets[i] = paste(prefix, "-", sheets[i], sep="")
  }
}

#function that takes an excel workbook input and saves separate csv by sheet outputs plus for loops from above
excel_to_csv <- function(path, out_dir = NULL) {
  if (is.null(out_dir)) out_dir <- dirname(path)
  sheets <- readxl::excel_sheets(path)
  dats <- lapply(sheets, readxl::read_excel, path = path)
  for (sheet in sheets) {
    id <- substr(sheet, 1, 5)
    if (str_detect(id, "^[:digit:]+$")) {
      prefix <- id
    }
  }
  for (i in 1:length(sheets)) {
    id <- substr(sheets[i], 1, 5)
    if (id != prefix) {
      sheets[i] = paste(prefix, "-", sheets[i], sep="")
    }
  }
  filenames <- file.path(out_dir, paste0(sheets, ".csv"))
  lapply(seq_along(dats), function(i) readr::write_csv(dats[[i]], filenames[i]))
  invisible()
}

#test looping through two files
files_test = dir("test_split_raw")

for (f in files_test) {
  file <- file.path("test_split_raw", f)
  excel_to_csv(path = file, out_dir = "test_split")
}

#loop through all raw files
files = dir("Raw")

for (f in files) {
  file <- file.path("Raw", f)
  excel_to_csv(path = file, out_dir = "split_raw")
}





