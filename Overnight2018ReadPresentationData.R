#Read Presentation Data

#This script reads in all the different data files and returns a list of the data files

Overnight2018ReadPresentationData <- function() {
  
  #save starting directory
  startdir <- getwd()
  
  #libraries
  library(plyr) # for rbind.fill
  library(dplyr)
  library(readr) #for reading files
  
  #source the Presentation Data Parsing Function
  source("Overnight2018ParseRawPresentationData.R")
  
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
   
    #checks for a comma at the end of the 1st (header) 2nd (first row of data) rows
    #if there is no comma at the end of the header but there IS one at the end of the data
    if (!(grepl(",$",readLines(f,n=2)[1])) && grepl(",$",readLines(f,n=2)[2])) {
      
      #read all the lines
      r <- readLines(f)
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

      
    #if the file name starts with "Cues" then it is not formatted the same way as the rest of the data  
    }else if (substring(f,1,4) == "Cues"){
      
      #no header
      d <- read.csv(f, header = FALSE,quote="",comment.char="")
      #add header
      colnames(d) <- c("C_Sound", "CueType", "CueRef", "CueAttenuation")
      #add SID and Session columns
      #define start/end
      sid_s <- regexpr("\\d",f)[1] #first digit
      sid_e <- regexpr("\\d_",f)[1] #first digit followed by an underscore
      sess_s <- regexpr("over",f)[1] #beginning of 'overnight"
      sess_e <- regexpr("\\d_\\d",f)[1] #first digit followed by underscore and then another digit
      #add columns
      d <- d %>%
        mutate(SID = substring(f,sid_s,sid_e)) %>%
        mutate(Session = substring(f, sess_s,sess_e))
      
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
    
    
    ##### Read in the raw Presentation Data Output Files (messy) #####
    
    #8001 does not have these output files
    if(dir != "8001"){
      
      #list of all .log Presentation output files
      pres_list = list.files(pattern = ".log") #Presentation Files are the only ones that start with a digit for the SID number
      
      for(file in pres_list){
        
        pd <- Overnight2018ParseRawPresentationData(file)
        PresData <- rbind.fill(PresData,pd)
        
      }
    }
    
    
    ##### Read in all formatted .csv Output Files #####
    
    #variable containing list of all .csv files in folder
    file_list = list.files(pattern = ".csv")
    
    for (file in file_list){
      
      #AttemptData (both evening and morning together)
      if (grepl("^AttemptData", file) ){
        
        #special edit for 8001 whose columns were incorrectly shifted
        if (dir == "8001"){
          temp <- read.csv(file, header = TRUE,quote="",comment.char="")
          temp <- temp %>%
            mutate(AttemptRT = AttemptResp) %>%
            mutate(AttemptResp = AttemptNumItem) %>%
            mutate(AttemptNumItem = CuedStatus) %>%
            mutate(CuedStatus = NA)
          AttemptData <- rbind.fill(AttemptData,temp)
        }else{
          AttemptData <- kgread(file, AttemptData)
        }
        
        #special edit for removing the X column from first 4 participants' files
        if(dir == "8001" | dir == "8002" | dir == "8003" | dir == "8005") {
          AttemptData <- select(AttemptData, -X)
        }

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
  
  #write compiled datafiles
  write_csv(PresData,"RawPresentationOutputData_compiled.csv")
  write_csv(AttemptData,"RawAttemptData_compiled.csv")
  write_csv(Cues,"RawCuesData_compiled.csv")
  write_csv(EveningData,"RawEveningData_compiled.csv")
  write_csv(MorningData,"RawMorningData_compiled.csv")
  write_csv(EveningMemoryData,"RawEveningMemoryData_compiled.csv")
  write_csv(MorningMemoryData,"RawMorningMemoryData_compiled.csv")
  write_csv(OvernightData,"RawOvernightData_compiled.csv")
 
  #return to starting directory
  setwd(startdir)
  
  #return a list with all of the created dataframes
  return(list(PresData, AttemptData, Cues, EveningData, MorningData, EveningMemoryData, MorningMemoryData, OvernightData, homelessdata))
}