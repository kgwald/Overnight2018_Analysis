#Read Presentation Data

#This script reads in all the different data files and returns a list of the data files

Overnight2018ReadPresentationData <- function() {
  
  #libraries
  library(plyr) # for rbind.fill
  library(dplyr)
  library(readr) #for reading files
  
  #Data folder exists one step above the project folder
  #Reason: don't want to store the data on github
  setwd("../PresentationData")
  
  #create storage variables
  PresData <- data.frame()
  AttemptData <- data.frame()
  Cues <- data.frame()
  EveningData <- data.frame()
  MorningData <- data.frame()
  EveningMemoryData <- data.frame()
  MorningMemoryData <- data.frame()
  OvernightData <- data.frame()
  
  homelessdata <- list() #for problematic data
  
  #create mini function for reading and writing the files
  kgread <- function(f, df){
    
    #before reading in the file, checks for trailing commas and eliminates
    #this was an issue for the MorningMemoryData for the first 4 participants (8001, 8002, 8003, 8005)
    
    #retrieves the file text
    t <- file(f, "r") 
    #checks for a comma at the end of the 1st (header) 2nd (first row of data) rows
    #if there is no comma at the end of the header but there IS one at the end of the data
    if (!(grepl(",$",readLines(t,n=2)[1])) && grepl(",$",readLines(t,n=2)[2])) {
      
      #reread the file because grepl changes it... still confused about this
      t <- file(f, "r") 
      
      #read all the lines
      r <- readLines(t)
      #remove commas from the end of the data and read that file
      r <- gsub(",$","",r)
      
      d <- data.frame()
      for (i in 2:length(r)) {
        
        #read in each line and add to the data frame
        l <- read.csv(textConnection(r[[i]]), header=FALSE)
        d <- rbind.fill(d,l)

      }
      #add the colnames from first row
      colnames(d) <- colnames(read.csv(textConnection(r[[1]])))
      
    }else{
      
      #reads in file normally
      d <- read.csv(f, header = TRUE,quote="",comment.char="")
      
    }
    
    #updates df
    df <- rbind.fill(df,d)
    
    #returns updated df
    return(df)
    
  }
    
  
  #variable containing all folders
  dir_list <- list.dirs(full.names=FALSE, recursive = FALSE)
  
  #loop through all folders
  for (dir in dir_list){
    
    #cd into the subject folder
    setwd(paste(getwd(),"/",dir,sep=""))
    
    #variable containing list of all files in folder
    file_list = list.files(pattern = ".csv")
    
    #Skips the files that start with the subject number
    #These files are Presentation's output, it's not well-formatted and therefore I'm not sure how I will parse them yet
    #PresData are .log files
    
    for (file in file_list){
      
      #AttemptData (both evening and morning together)
      if (grepl("^AttemptData", file) ){
        AttemptData <- kgread(file, AttemptData)
      
      #Cue references for overnight data  
      }else if(grepl("^Cues_", file) ){
        Cues <- kgread(file,Cues)
      
      #EveningData only but from the individual Evening files
      }else if(grepl("^Data_\\d{4}_e", file) ){
        EveningData <- kgread(file, EveningData)
        
      #MorningData only but from the individual Morning files
      }else if(grepl("^Data_\\d{4}_m", file) ){
        MorningData <- kgread(file,MorningData)
      
      #EveningMemoryData from the individual files
      }else if(grepl("^EveningMemoryData_\\d{4}_e", file) ){
        EveningMemoryData <- kgread(file, EveningMemoryData)

      #MorningMemoryData from the individual files
      }else if(grepl("^MorningMemoryData_\\d{4}_m", file) ){
         MorningMemoryData <- kgread(file, MorningMemoryData)
      
      #OvernightData   
      }else if(grepl("^OvernightData_", file) ){
        OvernightData <- kgread(file, OvernightData)
      
      #Add file name to the homeless data so can go back and check if there are any files that should be
      #properly housed
      }else{
        homelessdata <- c(homelessdata, file)
      }
    }
    
    #return to upper level for next subject folder
    setwd("..")
    
  }
  #return a list with all of the created dataframes
  return(list(PresData, AttemptData, Cues, EveningData, MorningData, EveningMemoryData, MorningMemoryData, OvernightData, homelessdata))
}