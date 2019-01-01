#Function for cleaning Overnight 2018 Data Morning Memory Data output

#Before this script is run:
#1) Overnight2018ReadPresentationData.R is run and outputs the file RawMorningMemoryData_compiled.csv
#2) A human opens that file and enters the accuracy data (1 or 0) for the Morning Puzzle-Sound Memory task. This data
#     is then saved as a new file titled MorningMemoryData_edited.csv.
#3) This data can be checked by looking at the raw paper files where accuracy was recorded on-line by the
#     experimenter or by listening to the PSRecall sound recording files.
#4) Note that the experimenter also made a 1-5 rating for the details memory but we have decided a more accurate memory
#     measure is to create an objective scoring guide rather than an on-line intuitive rating.


#Additional data notes:
# 8005 GESG puzzle, the participant remembered the puzzle two puzzles later, marked as incorrect because didn't remember at the time



Overnight2018CleanMorningMemory <- function(){
  
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
  rawdata <- read_csv("RawMorningMemoryData_compiled.csv")
  editeddata <- read_csv("MorningMemoryData_edited.csv")
  
  #Check that the two files contain the same items
  #if the two dataframes differ
  if(!isTRUE(all.equal(rawdata[,1:6], editeddata[,1:6]))) {
    
    warn = "Raw data and Edited data are not the same! Suggests that some of the raw data may not have been accounted for in the edited data"
    warning(warn)
    
  }
  
  #Check whether all 1's and 0's have been filled in for PSRecallAcc
  #if there are NAs in the PSRecallAcc column
  if(any(is.na(editeddata$PSRecallAcc))){
    
    warn = "Edited Data PSRecallAcc column contains NAs suggesting not all accuracy data has been recorded"
    warning(warn)
    
  }
  
  #Creates the MemoryOrder column (doesn't exist in Memory Datat output)
  #Note the same order is used for the recall, recognition, and details memory/morning solving order
  editeddata <- editeddata %>%
    group_by(SID, Session) %>%
    mutate(MemoryOrder = row_number())
  
  
  #Scores the recognition memory task
  #Note for the first four participants only the first 6 recognition memory options were recorded so only
  #the first six (out of 8) responses can be scored
  #Note for the first participant (8001) M1, no recognition memory data was recorded
  
  #create 'options key' for each participant - note when add options 7 and 8 need to add here too
  options <- editeddata %>%
    select(SID, Session, grep("Option", colnames(editeddata), value = TRUE)) %>%
    unique() %>%
    group_by(SID, Session) %>%
    gather(key = OptionNum, value = RecognitionRespTitle, RecognitionOption1, RecognitionOption2, RecognitionOption3, RecognitionOption4,RecognitionOption5,RecognitionOption6)
  #replace "RecognitionOption" with the number for joining purposes
  options$OptionNum<- gsub("RecognitionOption","",options$OptionNum)
  
  #create column that represents the puzzle title of the option selected
  editeddata <- left_join(editeddata,options, by = c("SID","Session","RecognitionResp"="OptionNum"))
  
  #update the RecognitionAcc column accordingly (again note that first four subjects are missing some RecognitionOptions)
  editeddata <- editeddata %>%
    mutate(RecognitionAcc = ifelse(ProblemTitle == RecognitionRespTitle,1,0))
  
  
  
  setwd(startdir)
  return(editeddata)
  
}