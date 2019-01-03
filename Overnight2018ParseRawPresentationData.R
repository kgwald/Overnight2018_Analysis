#Function for parsing the Presentation Output

#Note the Presentation Data is messy, some of the lines do not have the 'right' number of columns so read_delim complains
#but still reads the data, these warnings can be ignored.

Overnight2018ParseRawPresentationData <- function(fname){
  
  library(readr)
  library(dplyr)
  
  #read first 5 lines:
  header <- readLines(fname,n=5)
  
  #store the first line as the script
  script <- header[1]
  
  #store the second line as the write date
  logdate <- header[2]
  
  #read file starting 3 lines in with the header (automatically removes empty rows!)
  d <-read_delim(fname, delim = "\t", skip =3)
  
  #remove the extra data at the end (not useful because records only first button press per screen...)
  #identify by not being the subject identifier
  id <- as.character(d[1,1])
  d <- subset(d, Subject == id)
  
  #create a SID column
  d$SID = strsplit(id, "_")[[1]][[1]] #assumes SID is written first
  #create a Session column
  d$Session = strsplit(id, "_")[[1]][[2]] #assumes Session is written first
  #create a script column
  d$Script = strsplit(script, "-")[[1]][[2]]
  #create a date column
  d$LogDate = strsplit(logdate, "-")[[1]][[2]]
  
  #special adjustment for files that were misnamed
  if(fname == "8003_M2-ProblemSolving_2021.log"){
    d$SID <- "8002"
    d$Session <- "E1"
  }
  
  return(d)
  
}