q1 <- function()
{ 
    
  
  if(!file.exists("./housing.csv"))
  {
    URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
    download.file(URL,"./housing.csv")  
  }
  
  data <- read.csv("housing.csv", na.strings = "NA") 
  agricultureLogical <- data$ACR == 3 & data$AGS == 6
  head(which(agricultureLogical),3) 
}

q2 <- function()
{
  library(jpeg)
  
  if(!file.exists('./jeff.jpeg'))
  {
    download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg'
                ,'jeff.jpeg',mode = 'wb')
    
  }
  
  img <- jpeg::readJPEG('jeff.jpeg',native = TRUE)
  
  quantile(img,probs = c(0.3,0.8))  
}

q3 <- function()
{
  library(dplyr)
  
  if(!file.exists("./data1.csv"))
  {
    URL1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
    download.file(URL1,destfile = "./data1.csv")
  }
  
  if(!file.exists("./data2.csv"))
  {
    URL2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
    download.file(URL2,destfile = "./data2.csv")
  }
  
  data1 <- read.csv("data1.csv",nrows = 190,skip = 4)[c(1,2,4,5)]
         
  colnames(data1) <- c("CountryCode", "Rank", "Economy", "Total")
                    
  data2 <- read.csv("data2.csv")
  
  
  mergeddata <- merge(data1,data2,by = "CountryCode")
  print(nrow(mergeddata))
  sorteddata <- arrange(mergeddata,desc(Rank))
  as.character(sorteddata[13,"Economy"])
}

q4 <- function()
{
  library(dplyr)
  
  if(!file.exists("./data1.csv"))
  {
    URL1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
    download.file(URL1,destfile = "./data1.csv")
  }
  
  if(!file.exists("./data2.csv"))
  {
    URL2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
    download.file(URL2,destfile = "./data2.csv")
  }
  
  data1 <- read.csv("data1.csv",nrows = 190,skip = 4)[c(1,2,4,5)]
  
  colnames(data1) <- c("CountryCode", "Rank", "Economy", "Total")
  
  data2 <- read.csv("data2.csv")
  
  
  mergeddata <- merge(data1,data2,by = "CountryCode")
  
  yo <- subset(mergeddata, Income.Group== 'High income: OECD') 
  
  avg_GDP_OECD <- mean(yo[,'Rank'])
  
  yo1 <- subset(mergeddata, Income.Group== 'High income: nonOECD')
  
  avg_GDP_nonOECD <- mean(yo1[,'Rank'])  
  
  cat(avg_GDP_OECD," ",avg_GDP_nonOECD)
  
}

q5 <- function()
{
  library(dplyr)
  library(Hmisc)
  
  if(!file.exists("./data1.csv"))
  {
    URL1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
    download.file(URL1,destfile = "./data1.csv")
  }
  
  if(!file.exists("./data2.csv"))
  {
    URL2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
    download.file(URL2,destfile = "./data2.csv")
  }
  
  data1 <- read.csv("data1.csv",nrows = 190,skip = 4)[c(1,2,4,5)]
  
  colnames(data1) <- c("CountryCode", "Rank", "Economy", "Total")
  
  data2 <- read.csv("data2.csv")
  
  
  mergeddata <- merge(data1,data2,by = "CountryCode")
  
  mergeddata$RankGroups <- cut2(mergeddata$Rank,g=5)
  table(mergeddata$RankGroups,mergeddata$Income.Group)
  
}
