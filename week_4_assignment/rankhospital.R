rankhospital <- function(state,outcome,num="best")
{
  data_raw <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  data_table <- as.data.frame(cbind(data_raw[,2],
                                    data_raw[,7],
                                    data_raw[,11],
                                    data_raw[,17],
                                    data_raw[,23]),
                              stringsAsFactors = FALSE)
  
  colnames(data_table) <- c("hospital", "state_name", "heart attack", "heart failure", "pneumonia")
  
  if(!state %in% data_table[,"state_name"]) {stop("invalid state")}
  
  else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){stop("invalid outcome")}
  
  else 
  {
      hos_out <- as.data.frame(cbind(data_table[,"hospital"][which(data_table[,"state_name"]==state)],
                       suppressWarnings(as.numeric(data_table[,outcome])[which(data_table[,"state_name"]==state)])))
      
      colnames(hos_out) <- c("hospital","rate")

      inc_rate <- hos_out[order(as.numeric(as.character(hos_out[,2])),as.character(hos_out[,1]),na.last = NA),]
    
      
  }
  
  if(is.numeric(num)==TRUE)
  {
    if(num > nrow(inc_rate))
    {
      return(NA)
    } 
    else
    {
      return(as.character(inc_rate[num,1]))
    }
  }
  else if(num == "best")
  {
    return(as.character(inc_rate[1,1]))
  }
  else if(num == "worst")
  {
    return(as.character(inc_rate[nrow(inc_rate),1]))
  }
  else
  {
    stop("invalid rank")
  }
  
}