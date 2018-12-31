#Overnight 2018 Study Evening Memory Files Cleanup

#Before this script is run:
#1) Overnight2018ReadPresentationData.R is run and outputs the file RawEveningMemoryData_compiled.csv
#2) A human opens that file and enters the accuracy data (1 or 0) for the Evening Memory task. This data
#     is then saved as a new file titled EveningMemoryData_edited.csv.
#3) This data can be checked by looking at the raw paper files where accuracy was recorded on-line by the
#     experimenter or by listening to the PSRecall sound recording files

#Note: MemoryOrder column was not filled in for participant 8001

Overnight2018CleanEveningMemory <- function(){
  
  #suppresses the column information from the readr read_csv function
  options(readr.num_columns = 0) 
  
  #save starting directory
  startdir <- getwd()
  
  library(dplyr)
  library(readr)
  
  #Data folder exists one step above the project folder
  #Reason: don't want to store the data on github
  setwd("../PresentationData")
  
  #Read in two data files (should be identical except for the PSRecallAcc column)
  rawdata <- read_csv("RawEveningMemoryData_compiled.csv")
  editeddata <- read_csv("EveningMemoryData_edited.csv")
  
  #Check if the two data files are the same meaning all raw data is accounted for in the edited data
  #Note have to leave out the PSRecallRT column from the match because determining if two numeric columns are
  #the same is unnecessarily annoying for this calculation
  
  #if the two dataframes differ
  if(!isTRUE(all.equal(rawdata[,1:13], editeddata[,1:13]))) {
    
    warn = "Raw data and Edited data are not the same! Suggests that some of the raw data may not have been accounted for in the edited data"
    warning(warn)
    
  }
  
  #Check whether all 1's and 0's have been filled in for PSRecallAcc
  
  #if there are NAs in the PSRecallAcc column
  if(any(is.na(editeddata$PSRecallAcc))){
    
    warn = "Edited Data PSRecallAcc column contains NAs suggesting not all accuracy data has been recorded"
    warning(warn)
    
  }
  
  #Re-creates the MemoryOrder column because missing for 8001
  editeddata <- editeddata %>%
    group_by(SID, Session) %>%
    mutate(MemoryOrder = row_number())
  
  #Creates a "Round" column to identify which round of the memory test an item occurred
  numRounds = 4 #number of Puzzle-Sound recall rounds
  editeddata <- editeddata %>%
    group_by(SID, Session) %>%
    add_tally() %>% #adds a column titled 'n' that tallies the number of observations per SID/Session
    mutate(Round = ifelse(MemoryOrder <= n/numRounds, 1, ifelse(MemoryOrder <= (n/numRounds)*2, 2, ifelse(MemoryOrder <= (n/numRounds)*3,3,ifelse(MemoryOrder <= (n/numRounds)*4,4,NA))))) %>%
    select(-n) #remove the tally column
  
  setwd(startdir)

  return(editeddata)
  
}
