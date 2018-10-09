best <- function(state,outcome)
{
  ## Reading outcome data
  data_raw <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  
  # Extracting needed columns and bind together as data frame
  data_table <- as.data.frame(cbind(data_raw[,2],   # hospital
                                    data_raw[,7],   # state
                                    data_raw[,11],  # heart attack
                                    data_raw[,17],  # heart failure
                                    data_raw[,23]), # pneumonia
                              stringsAsFactors = FALSE) # will not auto-convert to factors
  
  # initially column names are V1, V2,... we change them to "hospital", "state", "heart attack", "heart failure", "pneumonia"
  colnames(data_table) <- c("hospital", "state_name", "heart attack", "heart failure", "pneumonia")
  
    
  ## Checking that state and outcome are valid
  
  if(!state %in% data_table[,"state_name"]){stop("invalid state")}
  
  else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){stop("invalid outcome")}
  
  ## hospital names in that state with lowest 30-day death

  else
  {
    min_val <- (tapply(suppressWarnings(as.numeric(data_table[,outcome])), data_table[,"state_name"],min,na.rm=TRUE))
    
    result <- data_table[,"hospital"][which(suppressWarnings(as.numeric(data_table[,outcome]) == min_val[state]) & (data_table[,"state_name"]==state))]
    
  }
  
  ## If tie
  
  result[order(result)]
  
}