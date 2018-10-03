pollutantmean <- function(directory,pollutant,id=1:332)
{
  dir_path <- paste0(getwd(),"/",directory,"/")
  dir_files <- list.files(dir_path)
  dir_files_path <- paste0(dir_path,dir_files)
  
  mean_data <- data.frame()
  data_table <- data.frame()
  
  for(i in id)
  {
    data_buffer <- read.csv(dir_files_path[i])
    data_table <- rbind.data.frame(data_table,data_buffer)
    filter <- complete.cases(data_table)
    clean_data <- data_table[filter,]
  }
  mean(clean_data[,pollutant])
}
