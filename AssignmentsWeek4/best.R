##FUnction that finds the best hospital in a state based on 30-day mortality rate of heart attacks, heart failures or pneumonia

best <- function(state, outcome){
  ##Read outcome data
  data <- read.csv("C:/Users/HildeTaverne/Documents/R/Coursera/ProgrammingAssignment3/outcome-of-care-measures.csv",stringsAsFactors = FALSE)
  
  ##Check that state and outcome are valid,if not, stop and throw error
  if (!any(state == data$State)){
    stop('invalid state')
    
  }
  if (!(outcome == "heart attack" | outcome == "heart failure" | outcome == "pneumonia")){
    stop('invalid outcome')
  }
  
  ##Proceed with data from state of interest
  data_state <- data[data[,7] == state,]
  
  ##Define columnnumber: heart attack = 11, heart failure = 17, pneumonia = 23
  if (outcome == 'heart attack'){
    colNR <- 11} 
  else if (outcome == 'heart failure') {
    colNR <- 17} 
  else {colNR <- 23}
  
  ##Return hospital name in that state with lowest 30-day death rate
  data_state_cols <- suppressWarnings(as.numeric(data_state[,colNR])) #Collect data from column of interest
  data_state <- data_state[!(is.na(data_state_cols)), ] #Reselect based on outcomes and remove NA
  data_state_cols <- as.numeric(data_state[, colNR]) #Select again after clearing from NAs
  data_state_rows <- which(data_state_cols == min(data_state_cols)) #Find row where columnvalue is min value
  
  hospital <- data_state[data_state_rows, 2] #Find hospital based on selected row
  
  HospitalName <- sort(hospital) #Sort to ensure output based on alphabethic when tie occurs between hospitals

  return(as.character(HospitalName))  
}