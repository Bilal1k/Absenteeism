pack <- c("knitr", "tidyverse", "textreadr", "lubridate",
          "stringr", "scales","rpart","randomForest", "KRLS"
          "caret", "MASS", "foreach", "import", 'neuralnet', "foba",
          "Matrix","ipred","e1071", "penalized","rqPen","bst")



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

unique(data$Absenteeism.time.in.hours)

length(unique(data$Absenteeism.time.in.hours))

# with only 19 unique values, it is possible to  treat it as an ordinal catagorical variable rather than a continuous one.
summary(as.factor(data$Absenteeism.time.in.hours))
# with multiple bins having only one or few observation, it would be hard to treat this as a classifieng problem

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
# Create an index for numeric variables
nums <- unlist(lapply(data_w, is.numeric))

# visualize outliers in each variable
data_w %>% gather(key = "key", value ="value", -c(names(data_w[,!nums]))) %>% 
  ggplot(aes(key,value)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 65, hjust = 1, size = 10))

# Examining outliers
head(sort(data_w$Age, decreasing = TRUE))
head(sort(data_w$Height, decreasing = TRUE))
head(sort(data_w$Height))
summary(data$Height)
summary(data_w$Hit.target)
summary(data_w$Service.time)
summary(data_w$Absenteeism.time.in.hours)
summary(as.factor(data_w$Absenteeism.time.in.hours))
# the only variable with many outliers is the target variable, no observation will be removed from it.
# since there were no extreme or unrealistic values, no need to remove any outliers.

####    
# For outliers  that lie outside the 1.5 * interquartile range "IQR" limits, we could cap it by replacing those observations outside the lower limit with the value of 5th percentile and those that lie above the upper limit, with the value of 95th percentile. Below is a sample code that achieves this.
# outliers can decrease accuracy of models.
# create a fucntion that exchange the outliers with the 5th  and 95th percintiles.

caps_outliers <- function(x){
  qnt <- quantile(x, c(0.25, 0.75))
  cap <- quantile(x, c(.05, .95 ) )
  H <- 1.5 * IQR(x)
  x[x < (qnt[1] - H)] <- cap[1]
  x[x > (qnt[2] + H)] <- cap[2]
  x
}



#use the caps function to convert outliers from all numerical variables except Absenteeism
nums_t <- nums
nums_t[21] <- FALSE
data_w[,nums_t] <- apply(data_w[,nums_t],2,caps_outliers)
###


# scale numerical variables to avoid model bias towards variable with higher ranges
data_w[,nums] <- apply(data_w[,nums], 2, scales::rescale)

# calculate correlation matrix and visualize it as a heatmap
cor.dat <- cor(data_w[,nums])
heatmap(cor.dat, scale="column")


with(data_w,c(cor(Weight, Body.mass.index)))
# Since weight and body mass index are highly correlated, the body mass index variable will be removed to avoid collinearity. Intuitvly, it is calculated from weight and height and as both are avaiblein the data, it was removed.


# Multicollinearity can reduce prediction accuracy.
data_w$Body.mass.index <- NULL

# Dummy all factors
data_w <- dummy_cols(data_w, remove_selected_columns = TRUE)


# PCA
data_w$Absenteeism.time.in.hours -> y
data_w$Absenteeism.time.in.hours <- NULL
pca <-prcomp(data_w)
dim(pca$x)

#proportion of variance explained
variablity <- pca$sdev^2/sum(pca$sdev^2)

sum(variablity[1:45])

plot(cumsum(variablity))

data_pca <- pca$x[,1:45]

# create a test set
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y,times = 1, p = 0.2, list = FALSE)
train <- data_pca[-test_index,]
test <- data_pca[test_index,]

train_y <- y[-test_index]
test_y <- y[test_index]
# models
models <- c("lm", "mlp", "knn", "penalized","rqnc","krlsPoly","foba","BstLm")

# train models on dataset
set.seed(2, sample.kind = "Rounding")
fits <- lapply(models, function(model){
  set.seed(2, sample.kind = "Rounding")
  print(model)
  caret::train(train,train_y, method = model)
})


fits$krlsPoly <-  caret::train(train,train_y, method = "krlsPoly")
fits$foba <-  caret::train(train,train_y, method = "foba")
fits$BstLm <-  caret::train(train,train_y, method = "BstLm")
BstLm
names(fits) <- models

# predict using models
y_hat <- sapply(fits, function(fits){
  predict(fits, test)
})

y_hats_rescaled <- y_hat*(max(y)-min(y))-min(y)

# get model prediction accuracy
acc <- apply(y_hat, 2, function(x){
  RMSE(x, test_y)
})

acc

real_y <- data$Absenteeism.time.in.hours

acc_res <- acc*(max(real_y)-min(real_y))-min(real_y)
summary(acc_res)
acc_res
```

# Ensembles by majoraty vote
ensemble <- apply(y_hat[,c(1,4,6)], 1, mean)


acc_ens <- RMSE(ensemble, test_y)
acc_ens

acc_ens*(max(real_y)-min(real_y))-min(real_y)

y_hats_rescaled <- y_hat*(max(Y)-min(Y))-min(Y)


error <- as.data.frame(abs(y_hats_rescaled - data$Absenteeism.time.in.hours[]))
summary(error)
boxplot(error)

train_ns <- data[-test_index,]
temp <- data[test_index,]

test_ns <- temp %>% 
  semi_join(train_ns, by = "Absenteeism.time.in.hours")

test_ns <- cbind(test_ns$error)
head(test_ns)
test_ns %>% filter(knn > 11)


cat <- data_w[,!nums]
dum <- dummy_cols(cat, remove_selected_columns = TRUE)
