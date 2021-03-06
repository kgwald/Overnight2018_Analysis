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
#     d) checking the participants' scrap paper

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
  attemptdata <- read_csv("AttemptData_edited.csv")
  
  #For consisency with other scipts, set editeddata = rawdata and then edit it
  editeddata <- rawdata
  
  
  ##### Add/Edit Columns #####
  
  #create a 1/0 column for solutions
  editeddata <- editeddata %>%
    mutate(Solved = ifelse(TrialStatus == "solved",1,0))
  
  #Change InsightorAnalysisResp 1's to "insight" and 2's to "analysis"
  editeddata$InsightorAnalysisResp <- gsub("1","insight", editeddata$InsightorAnalysisResp)
  editeddata$InsightorAnalysisResp <- gsub("2","analysis", editeddata$InsightorAnalysisResp)
  
  #Change FixationRatingResp 1's to "stillworking" and 2's to "stuck"
  editeddata$FixationRatingResp <- as.character(editeddata$FixationRatingResp)
  editeddata$FixationRatingResp <- gsub("1","stillworking", editeddata$FixationRatingResp)
  editeddata$FixationRatingResp <- gsub("2","stuck", editeddata$FixationRatingResp) 
  
  ###### Solving Comments Adjustments #####
  
  #8002 E2 M13 should be solved by analysis not insight (changed her mind immediately after 'insight' was recorded)
  editeddata$InsightorAnalysisResp[editeddata$SID =="8002" & editeddata$ProblemID == "M13d" & editeddata$Session == "e2"] <- "analysis"
  
  #8005 E1 M08d participant solved the puzzle on the closeness rating screen - not solved in the Evening so leave as
  #'unsolved' but exclude from morning data
  
  setwd(startdir)
  return(editeddata)
  
}