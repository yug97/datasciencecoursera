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
grep('^United',data$economy)
}

q4 <- function()
{
  if(!file.exists("./FGDP.csv"))
  {
    URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
    download.file(URL, destfile = "./FGDP.csv")
  }
  
  if(!file.exists("./F_Country.csv"))
  {
    URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
    download.file(URL, destfile = "./F_Country.csv")
  }
  
  FGDP <- data.table::fread('./FGDP.csv'
                            ,skip = 5
                            ,nrows = 190
                            ,select = c(1,2,4,5)
                            ,col.names = c("CountryCode","Rank", "Country", "GDP"))
  F_country <- data.table::fread('./F_Country.csv')
  
  merge_dt <- merge(FGDP,F_country,by = 'CountryCode')
  
  merge_dt[grepl(pattern = 'Fiscal year end: June 30;',merge_dt[,`Special Notes`]),.N]
  
}

q5 <- function()
{
  library('quantmod')
  amzn = getSymbols("AMZN",auto.assign=FALSE)
  sampleTimes = index(amzn)
  
  tab <- data.table::data.table(timeCol = sampleTimes)
  
  tab[(timeCol >= "2012-01-01") & (timeCol) < "2013-01-01", .N ]
 
  tab[((timeCol >= "2012-01-01") & (timeCol < "2013-01-01")) & (weekdays(timeCol) == "Monday"), .N ] 
}
