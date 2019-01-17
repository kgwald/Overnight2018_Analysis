#Function for cleaning Overnight 2018 Morning Solving Data

#Before this script is run:
#1) Overnight2018ReadPresentationData.R is run and outputs the file RawMorningData_compiled.csv
#2) No human editing is necessary for the overall MorningData
#3) A human does open and edit RawAttemptData_compiled.csv and enters the experimenter comments for each puzzle. This
#     data is then saved as AttemptData_edited.csv and is used for both the Evening and Morning Solving analyses.
#4) All item-specific adjustments in this script are a result of experimenter comments and can be checked by:
#     a) looking at the raw paper files for the Solving portion of the experiment
#     b) looking at the raw paper files of the Session comments
#     c) listening to the Solving sound recording files
#     d) checking the participants' scrap paper

Overnight2018CleanMorningData <- function(){
  
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
  rawdata <- read_csv("RawMorningData_compiled.csv")
  #Read in edited AttemptData file (for comparison)
  attemptdata <- read_csv("AttemptData_edited.csv")
  
  #For consisency with other scipts, set editeddata = rawdata and then edit it
  editeddata <- rawdata
  
  ##### Add/Edit Columns #####
  
  #create a 1/0 column for solutions
  editeddata <- editeddata %>%
    mutate(Solved = ifelse(TrialStatus == "solved",1,0))
  
  #Change 1's to "insight" and 2's to "analysis"
  editeddata$InsightorAnalysisResp <- gsub("1","insight", editeddata$InsightorAnalysisResp)
  editeddata$InsightorAnalysisResp <- gsub("2","analysis", editeddata$InsightorAnalysisResp)
  
  ###### Solving Comments Adjustments #####
  
  #8005 E1/M1 M08d participant solved the puzzle on the closeness rating screen - too late to mark as solved
  #for the evening, exclude from morning data
  editeddata$TrialStatus[editeddata$SID =="8005" & editeddata$Session == "m1" & editeddata$ProblemID == "M08d"] <- "exclude-solvedlastnight"
  
  #8006 M1 M02d - participant said solution came to her last night but then forgot it and only realized half of it last night
  #(moving one triangle) and didn't know where to move it until this morning
  
  #8006 M1 S03a - participant thought she solved last night but forgot that diagonals couldn't reach square corners
  #no edit necessary
  
  #8006 M2 V13a - hard to tell exactly when participant solved this puzzle, based on given information, decided it was
  #this morning before the session started because didn't know full answer until then rather than last night (decision made blind to cueing condition)
  
  #8006 M2 R02a - actually solved during PS recall so maybe should adjust RT?
  
  
  
  
  setwd(startdir)
  return(editeddata)
  
} 
  