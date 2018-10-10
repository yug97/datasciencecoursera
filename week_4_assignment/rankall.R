rankall <- function(outcome,num="best")
{
  data_raw <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available",colClasses = "character")
  
  data_f <- as.data.frame(cbind(data_raw[,2],
                              data_raw[,7],
                              data_raw[,11],
                              data_raw[,17],
                              data_raw[,23]),
                        stringsAsFactors = FALSE)
  
  colnames(data_f) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia"))
  {
    stop("invalid outcome")
  }
  else
  {
    data_working <- data_f[c("hospital", "state",outcome)]
    data_working_cleaned <-  data_working[complete.cases(data_working),]
    
     
     state_wise_list <-  split(data_working_cleaned,data_working_cleaned[,"state"])
     
     result_list <- list()
     
     
     if(is.numeric(num)==TRUE)
     {
       for(i in seq_along(state_wise_list))
       {
         state_wise_list[[i]] <- state_wise_list[[i]][order(as.numeric(state_wise_list[[i]][,outcome]),
                                                            state_wise_list[[i]][,"hospital"]),]
         
         result_list[[i]] <- c(state_wise_list[[i]][num,"hospital"],state_wise_list[[i]][1,"state"])
         
         
       }
     }
     else if(num == "best")
     {
       for(i in seq_along(state_wise_list))
       {
         state_wise_list[[i]] <- state_wise_list[[i]][order(as.numeric(state_wise_list[[i]][,outcome]),
                                                            state_wise_list[[i]][,"hospital"]),]
         
         result_list[[i]] <- c(state_wise_list[[i]][1,"hospital"],state_wise_list[[i]][1,"state"])
         
         
       }
     }
     else if(num == "worst")
     {
       for(i in seq_along(state_wise_list))
       {
         state_wise_list[[i]] <- state_wise_list[[i]][order(as.numeric(state_wise_list[[i]][,outcome]),
                                                            state_wise_list[[i]][,"hospital"],decreasing = TRUE),]
         
         result_list[[i]] <- c(state_wise_list[[i]][1,"hospital"],state_wise_list[[i]][1,"state"])
         
         
       }
     }
     else
     {
       stop("invalid")
     }
     
     
     
    
    result <- do.call(rbind,result_list)
    
    colnames(result) <- c("hospital","state")
    rownames(result) <- result[,2]
    
    allranks <- as.data.frame(result)
    
    allranks
    
      
  }
  
  
   
  
}

    
    
  
