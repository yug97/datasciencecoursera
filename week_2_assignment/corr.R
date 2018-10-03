corr <- function(directory,threshold=0)
{
  dir_path <- paste0(getwd(),"/",directory,"/")
  dir_files <- list.files(dir_path)
  dir_files_path <- paste0(dir_path,dir_files)
  
  d_frame <- data.frame()
  d_frame2 <- data.frame()
  result_vect <- NULL
  
  for(i in 1:332)
  {
    data_buffer<- read.csv(dir_files_path[i])
    
    d_frame <- data_buffer[complete.cases(data_buffer),]
    
    if(nrow(d_frame)>threshold)
    {
      result_vect <- c(result_vect,cor(d_frame[,"sulfate"],d_frame[,"nitrate"]))
    }
    
  }
  
  return(result_vect)
}