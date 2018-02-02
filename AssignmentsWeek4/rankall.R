## This function asks outcome and num ("best", "worst" or a ranking number) as input
## It outputs a dataframe with the hospital name on the ranking number for each state

rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  data <- read.csv("C:/Users/HildeTaverne/Documents/R/Coursera/ProgrammingAssignment3/outcome-of-care-measures.csv",stringsAsFactors = FALSE)
  
  ## Check that outcome is valid,if not, stop and throw error
  if (!(outcome == "heart attack" | outcome == "heart failure" | outcome == "pneumonia")){
    stop('invalid outcome')
  }
  
  ## Preallocate the output dataframe 
  df <- data.frame(hospital = character(), state = character())  
  
  #Get column number for outcome rate of interest: heart attack = 11, heart failure = 17, pneumonia = 23
  if (outcome == 'heart attack'){
    colNR <- 11} 
  else if (outcome == 'heart failure') {
    colNR <- 17} 
  else {colNR <- 23}
  
  ## For each state, find the hospital of the given rank
  for (state in sort(unique(data$State))){
    
    data_state <- data[data[,7] == state,] #Select data for current state
    data_state_cols <- suppressWarnings(as.numeric(data_state[,colNR])) #Collect data from column of interest
    data_state <- data_state[!(is.na(suppressWarnings(as.numeric(data_state[,colNR])))), ] #Reselect based on outcomes and remove NA
    
    #Initiate dataframe with three columns for hospital name, rate and ranknr and number of rows left after selection
    df_ranking <- data.frame(Hospital.Name = character(nrow(data_state)), Rate = double(nrow(data_state)), Rank = double(nrow(data_state)))  
    
    #Copy rate and hospital names to output dataframe
    df_ranking["Rate"] <- data_state[,colNR]
    df_ranking["Hospital.Name"] <- data_state[,2]
    
    #Sort dataframe first by rate and then by alphabet
    df_ranking <- transform(df_ranking, Rate = as.numeric(Rate)) #Transform column Rate to numeric values
    df_ranking_sorted <- df_ranking[order(df_ranking["Rate"], df_ranking["Hospital.Name"]),]
    
    #Add rankings
    df_ranking_sorted["Rank"] <- 1:nrow(df_ranking_sorted)
    
    #Collect the ranknumber that was asked for
    if (num == "best"){
      output_hospital <- df_ranking_sorted[df_ranking_sorted["Rank"] ==  1,"Hospital.Name"]
    } else if (num == "worst"){
      output_hospital <- df_ranking_sorted[df_ranking_sorted["Rank"] ==  nrow(df_ranking_sorted),"Hospital.Name"]
    } else if (num <= nrow(df_ranking_sorted)){
      output_hospital <- df_ranking_sorted[df_ranking_sorted["Rank"] ==  num,"Hospital.Name"]
    } else {
      output_hospital <- NA #Return NA if anything else (e.g. if input num is higher than # of hospitals in state or if invalid characters are input)
    }
    
    df_state <- data.frame(hospital = output_hospital, state = state)
    
    #Fill output df with current state info 
    df <- rbind(df, df_state)
  } 
  
  ## Return a data frame with the hospital names and the (abbreviated) state name
  return(df)
  
}
