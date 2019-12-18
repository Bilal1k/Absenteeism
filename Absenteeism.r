pack <- c("knitr", "tidyverse", "textreadr", "lubridate",
          "stringr", "scales","rpart","randomForest","Rborist",
          "caret", "arm", "mboost","MASS",
          "glmnet","Matrix","ipred","e1071","Rborist", "nnet", "nodeHarvest")
          


# In orderto run keras and Tensorflow, conda is needed, it can be downloaded from this link https://www.anaconda.com/distribution/#download-section

new.packages <- pack[!(pack %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(pack, library, character.only = TRUE)



# The database was created with records of absenteeism at work from July 2007 to July 2010 at a courier company in Brazil.
temp <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00445/Absenteeism_at_work_AAA.zip",temp)
data <- read.csv2(unz(temp, "Absenteeism_at_work.csv"))
unlink(temp)

temp <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00445/Absenteeism_at_work_AAA.zip",temp)
info <- read_docx(unzip(temp, "Attribute Information.docx"), skip = 1)[1:44]
unlink(temp)
info


str(data)

sum(apply(data, 2, anyNA))
# No NAs

length(unique(data$ID))

sort(unique(data$Reason.for.absence))
# No absenteeism due to reason 20.

# creast a list with reasons
reason_n <- c(0,info[4:22],info[24]) # get reasons names form info file. Skip the 23rd line as reason 20 is absent form the data
c <- c("patient follow-up", "medical consultation",
       "blood donation", "laboratory examination",
       "unjustified absence", "physiotherapy",
       "dental consultation") # list non ICD reasons
reason_n[22:28] <- c # add non ICD reasons


data %>% ggplot(aes(Day.of.the.week)) + geom_histogram()
data %>% ggplot(aes(ID)) + geom_histogram()
data %>% ggplot(aes(Service.time)) + geom_histogram()
data %>% ggplot(aes(Absenteeism.time.in.hours)) + geom_histogram()
data %>% ggplot(aes(as.factor(Reason.for.absence))) + 
  geom_histogram(stat = 'count') +
  scale_x_discrete(label = str_trunc(reason_n, 40)) +
  theme(axis.text.x = element_text(angle = 65, hjust = 1, size = 10))

variable <- colnames(data)[1:20] # is a `vector` now
data1 <- list() # initialize as a `list`
for(i in variable){ 
  f <- data %>%
    group_by_at(i) %>% #changed to `group_by_at`
    summarise(median = median(Absenteeism.time.in.hours))
  data1[[i]] <- sd(f$median)
}

kable(arrange(tibble(var = names(data1),
                     absent.sd = unlist(data1)),
              desc(absent.sd)))



data %>% group_by(Day.of.the.week) %>%
  summarise(n = n(), 
            avg = mean(Absenteeism.time.in.hours), 
            sd = sd(Absenteeism.time.in.hours),
            se = sd/sqrt(n)) %>%
  ggplot(aes(as.factor(Day.of.the.week), avg)) + geom_point() +
  geom_errorbar(aes(ymin=avg - 2*se, ymax=avg+2*se)) +
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

data %>% group_by(Work.load.Average.day) %>%
  summarise(n = n(), 
            avg = mean(Absenteeism.time.in.hours), 
            sd = sd(Absenteeism.time.in.hours),
            se = sd/sqrt(n)) %>%
  ggplot(aes(as.factor(Work.load.Average.day), avg)) + geom_point() +
  geom_errorbar(aes(ymin=avg - 2*se, ymax=avg+2*se)) +
  theme(axis.text.x = element_text(angle = 65, hjust = 1)) 


data %>% group_by(Reason.for.absence) %>%
  summarise(n = n(), 
            avg = mean(Absenteeism.time.in.hours), 
            sd = sd(Absenteeism.time.in.hours),
            se = sd/sqrt(n)) %>%
  filter(se <= 3.5) %>%
  ggplot(aes(as.factor(Reason.for.absence), avg)) + geom_point() +
  scale_x_discrete(label = str_trunc(reason_n, 40)) +
  theme(axis.text.x = element_text(angle = 65, hjust = 1, size = 10)) +
  geom_errorbar(aes(ymin=avg - 2*se, ymax=avg+2*se))

# Most of the avareges' standard errors overlap. this shows that if each variable is used on its own, it will not be a good predicter in a liner model.  



# change catogorical data into class factor and change work load avarages to a numerical vector.
data_w <- data %>% 
  mutate_at(c(1,2,3,4,5), as.factor) %>% 
  mutate(Work.load.Average.day =
           as.numeric(levels(Work.load.Average.day))[Work.load.Average.day])


# Creat a subset of numeric predictors
nums <- unlist(lapply(data_w, is.numeric))  


# scale numerical variables to avoid model bias towards variable with higher ranges
data_w[,nums] <- apply(data_w[,nums], 2,rescale)

# calculate correlation matrix and visualize as heatmap
cor.dat <- cor(data_w[,nums])
heatmap(cor.dat, scale="column")


with(data_w,c(cor(Weight, Body.mass.index)))
# Since weight and body mass index are highly correlated, the body mass index variable will be removed to avoid collinearity. Intuitvly, it is calculated from weight and height and as both are avaiblein the data, it was removed.


# Multicollinearity can reduce prediction accuracy.

data_w$Body.mass.index <- NULL

# create a test set
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = data_w$Absenteeism.time.in.hours, times = 1, p = 0.2, list = FALSE)
train <- data_w[-test_index,]
test <- data_w[test_ind0ex,]


# models
models <- c("glm","knn","bayesglm", "glmboost",
            "treebag","Rborist","rf", "nnet", "nodeHarvest")

# train models on dataset
set.seed(2, sample.kind = "Rounding")
fits <- lapply(models, function(model){
  set.seed(2, sample.kind = "Rounding")
  print(model)
  caret::train(Absenteeism.time.in.hours ~ ., method = model, data = train)
}) 
names(fits) <- models

# predict using models
y_hat <- sapply(fits, function(fits){
  predict(fits, test)
})

# get model prediction accuracy
acc <- apply(y_hat, 2, function(y_hat){
  RMSE(y_hat,test$Absenteeism.time.in.hours)
})

acc_res <- acc*(max(y)-min(y))-min(y)

# Ensembles by majoraty vote
ensemble <- apply(y_hat, 1, function(x) {
  tabulatedOutcomes <- table(x)
  sortedOutcomes <- sort(tabulatedOutcomes, decreasing=TRUE)
  mostCommonLabel <- names(sortedOutcomes)[1]
  mostCommonLabel
})
RMSE(ensemble,test$Absenteeism)

# individual methods comparision to the ensemble
acc > mean(ensemble == mnist_27$test$y)

# mean of training set accuracy estimates
train_acc <- lapply(fits, function(x){
  tacc <- x$results$Accuracy
  mean(tacc)
})
mean(train_acc[[1]])

# create ensemble using methods with train accuracy of >= 0.8 
model_names <- names(train_acc[train_acc >= 0.8])
y_hat.8 <- y_hat[,model_names]
ensemble.8 <- apply(y_hat.8, 1, function(x) {
  tabulatedOutcomes <- table(x)
  sortedOutcomes <- sort(tabulatedOutcomes, decreasing=TRUE)
  mostCommonLabel <- names(sortedOutcomes)[1]
  mostCommonLabel
})
mean(ensemble.8 == mnist_27$test$y)

