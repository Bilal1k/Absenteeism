library(knitr)
library(tidyverse)
library(caret)
library(textreadr)
library(lubridate)
library(data.table)


# The database was created with records of absenteeism at work from July 2007 to July 2010 at a courier company in Brazil.
temp <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00445/Absenteeism_at_work_AAA.zip",temp)
data <- read.csv2(unz(temp, "Absenteeism_at_work.csv"))
unlink(temp)

temp <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00445/Absenteeism_at_work_AAA.zip",temp)
info <- read_docx(unzip(temp, "Attribute Information.docx"), skip = 1)[1:44]
info

str(data)

data$Absenteeism.time.in.hours[which(data$Reason.for.absence == 0)]

str(data)

sum(apply(data, 2, anyNA))

length(unique(data$ID))

levels(data$Reason.for.absence)

data %>% ggplot(aes(Day.of.the.week)) + geom_histogram()
data %>% ggplot(aes(ID)) + geom_histogram()
data %>% ggplot(aes(Service.time)) + geom_histogram()
data %>% ggplot(aes(Absenteeism.time.in.hours)) + geom_histogram()

variable <- colnames(data)[1:20] # is a `vector` now
data1 <- list() # initialize as a `list`
for(i in variable){ 
  f <- data %>%
    group_by_at(i) %>% #changed to `group_by_at`
    summarise(mean = mean(Absenteeism.time.in.hours))
  data1[[i]] <- sd(f$mean)
}

kable(arrange(tibble(var = names(data1), absent.sd = unlist(data1)), desc(absent.sd)))


data %>% select(reason, Absenteeism.time.in.hours) %>%
  gather(reason, Absenteeism.time.in.hours) %>% 
  ggplot(aes(Absenteeism.time.in.hours, reason)) +
  geom_boxplot()

+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
         
    
data <- data %>% mutate_at(c(1,2,3,4,5,12,13,15,16,17), as.factor) %>% 
      mutate(Work.load.Average.day = as.numeric(Work.load.Average.day))

reason_n <- info[4:24]
c <- c("patient follow-up", "medical consultation", "blood donation", "laboratory examination", "unjustified absence", "physiotherapy", "dental consultation")
reason_n[22:28] <- c
reason <- data$Reason.for.absence
levels(reason) <- reason_n
data$reason <- reason
