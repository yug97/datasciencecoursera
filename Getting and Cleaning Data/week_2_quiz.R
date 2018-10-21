q4 <- function(){

library(httr)
library(XML)

link <- url("http://biostat.jhsph.edu/~jleek/contact.html
")

data <- readLines(link)
close(link)
print(nchar(data[10]))
print(nchar(data[20]))
print(nchar(data[30]))
print(nchar(data[100]))

}

q5 <- function()
{
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"
  widths <- c(1, 9, 5, 4, 1, 3, 5, 4, 1, 3, 5, 4, 1, 3, 5, 4, 1, 3)
  data <- read.fwf(url,widths,header = FALSE,skip=4)
  sum(data$V8)
    }
