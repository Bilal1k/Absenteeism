library(knitr)
library(tidyverse)
library(caret)
library(textreadr)
library(lubridate)

temp <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00445/Absenteeism_at_work_AAA.zip",temp)
data <- read.csv2(unz(temp, "Absenteeism_at_work.csv"))
unlink(temp)
str(data)

temp <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00445/Absenteeism_at_work_AAA.zip",temp)
info <- read_docx(unzip(temp, "Attribute Information.docx"), skip = 1)[1:44]
info

sum(apply(data, 2, anyNA))

summary(data)

data$Month.of.absence <- as.factor(month.abb[data$Month.of.absence])
range(data$Month.of.absence)
summary(data$Month.of.absence)  
hist(data$Month.of.absence)
which(data$Month.of.absence == 0)
data[738:740,]
summary(data)

