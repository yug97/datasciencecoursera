q1 <- function()
{
  if(!file.exists("./cloud.csv"))
  {
    URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
    download.file(URL, destfile = "./cloud.csv")
  }


data <- read.csv("./cloud.csv")
yo <- strsplit(names(data),split = "wgtp")
yo[123]
}

q2 <- function()
{
  
  if(!file.exists("./FGDP.csv"))
  {
    URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
    download.file(URL, destfile = "./FGDP.csv")
  }
  
  data <- data.table::fread("./FGDP.csv",
                            skip = 5,
                            nrows = 190,
                            select =c(1,2,4,5),
                            col.names = c("country","rank","economy","GDP"))
  
 data[,mean(as.integer(gsub(",","",GDP)))]
} 

q3 <- function()
{
  
  if(!file.exists("./FGDP.csv"))
  {
    URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
    download.file(URL, destfile = "./FGDP.csv")
  }
  
  data <- data.table::fread("./FGDP.csv",
                            skip = 5,
                            nrows = 190,
                            select =c(1,2,4,5),
                            col.names = c("countryNames","rank","economy","GDP"))
  
  grep("^a",data[,countryNames])
}