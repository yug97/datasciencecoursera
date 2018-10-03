complete <- function(directory,id)
{
  dir_path <- paste0(getwd(),"/",directory,"/")
  dir_files <- list.files(dir_path)
  dir_files_path <- paste0(dir_path,dir_files)
  
  d_frame <- data.frame()
  final_d_frame <- data.frame()
  
  for(i in id)
  {
    data_buffer <- read.csv(dir_files_path[i])
    d_frame <- data.frame(i,nobs = sum(complete.cases(data_buffer)))
    final_d_frame <- rbind.data.frame(final_d_frame,d_frame)
  }
  
  return(final_d_frame)
    
}