library(data.table)
library(readr)
library(dplyr)
library(magrittr)

d <- fread("data/ratings20191231_downloaded20211001.xml", fill = TRUE, 
           strip.white = TRUE, 
           col.names = c("ID-Number", "Name", 
                         "Tit", "WTit", "OTit", "Fed", "Sex", 
                         "FOA", "JAN20", "GMS", "K", "B-day", "Flag"))
d <- readr::read_csv("data/standard_ratings_jan2020.txt")

require(XML)
xml_data <- xmlParse("data/ratings20191231_downloaded20211001.xml")
data <- xmlToDataFrame(xml_data)
fwrite(data, file = "data/ratings20191231_downloaded20211001.csv")

