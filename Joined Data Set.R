calls <- read.csv("311.csv")

shelters <- read.csv("shelters.csv")

crime <- read.csv("crime.csv")

tract <- read.csv("tract.csv")

homeless <- read.csv("TOT.csv")

library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)

df <- left_join(calls, shelters, by = c("LATITUDE" ,"LONGITUDE"))

df <- left_join(df, crime, by = c("LATITUDE" ,"LONGITUDE"))

df1 <- left_join(tract, homeless, by=c("tract"))

df <- inner_join(df, df1, by = c("ZIPCODE"))


write.csv(df, file="combinedDF.csv")


