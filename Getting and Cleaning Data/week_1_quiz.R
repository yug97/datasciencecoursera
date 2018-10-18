q1 <- function()
{
library(data.table)  
filedata <- fread("survey.csv",na.strings =c("NA",""))
#filedata <- filedata[complete.cases(filedata[,.(VAL)])]
filedata[VAL == 24,.N]
}

q3 <- function()
{
  library(xlsx)
  if(!file.exists("NGAP.xlsx"))
  {
    fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
    download.file(fileurl,destfile = "./NGAP.xlsx", mode = 'wb') 
  }
  dat <- read.xlsx("NGAP.xlsx",sheetIndex = 1, rowIndex = 18:23, colIndex = 7:15)
  sum(dat$Zip*dat$Ext,na.rm=T)
}

q4 <- function()
{
  library(XML)
  fileurl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
  doc <- xmlTreeParse(fileurl,useInternalNodes = TRUE)
  
  rootnode <- xmlRoot(doc)
  zipcodes <- xpathSApply(rootnode,"//zipcode",xmlValue)
  data_table <- data.table(zipcode = zipcodes)
  data_table[zipcode == 21231, .N]
  }

q5 <- function()
{
  library(data.table)
  if(!file.exists("idaho.csv"))
  {
    fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
    download.file(fileurl,destfile = "./idaho.csv")
  }
  
  DT <- fread("idaho.csv")
  
  #system.time(mean(DT[DT$SEX==1,]$pwgtp15))
  #system.time(mean(DT[DT$SEX==2,]$pwgtp15))
   system.time(DT[,mean(pwgtp15),by=SEX])
  #system.time(sapply(split(DT$pwgtp15,DT$SEX),mean))
  #system.time(tapply(DT$pwgtp15,DT$SEX,mean))
  #system.time(rowMeans(DT)[DT$SEX==1])
  #system.time(rowMeans(DT)[DT$SEX==2])
  #system.time(mean(DT$pwgtp15,by=DT$SEX))
  
  }

