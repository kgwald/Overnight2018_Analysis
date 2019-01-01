#Function for cleaning Overnight 2018 Evening Solving Data

#Before this script is run:
#1) Overnight2018ReadPresentationData.R is run and outputs the file RawEveningData_compiled.csv
#2) No human editing is necessary for the overall EveningData
#3) A human does open and edit RawAttemptData_compiled.csv and enters the experimenter comments for each puzzle. This
#     data is then saved as AttemptData_edited.csv and is used for both the Evening and Morning Solving analyses.
#4) All item-specific adjustments in this script are a result of experimenter comments and can be checked by:
#     a) looking at the raw paper files for the Solving portion of the experiment
#     b) looking at the raw paper files of the Session comments
#     c) listening to the Solving sound recording files

Overnight2018CleanEveningData <- function(){
  
  #suppresses the column information from the readr read_csv function
  options(readr.num_columns = 0) 
  
  #save starting directory
  startdir <- getwd()
  
  library(dplyr)
  library(readr)
  
  #Data folder exists one step above the project folder
  #Reason: don't want to store the data on github
  setwd("../PresentationData")
  
  
  #Read in the raw EveningData file
  rawdata <- read_csv("RawEveningData_compiled.csv")
  #Read in edited AttemptData file (for comparison)
  editeddata <- read_csv("AttemptData_edited.csv")
  
  
  
  
  
  
}