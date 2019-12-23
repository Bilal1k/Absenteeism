# packages needed to run the project's code
pack <- c("knitr", "tidyverse", "textreadr", "lubridate","KRLS", "rmarkdown",
          "stringr", "scales", "caret", "MASS", "foreach","fastDummies",
          "import", "foba", "Matrix", "penalized","bst")

# Check if all packages are already installed, if not, install them.
new.packages <- pack[!(pack %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# loop to load all packages.
lapply(pack, FUN = function(X){
  do.call("require", list(X)) 
})

# load dataset from UCI repo
temp <- tempfile()
download.file(
  "https://archive.ics.uci.edu/ml/machine-learning-databases/00445/Absenteeism_at_work_AAA.zip",
  temp)
data <- read.csv2(unz(temp, "Absenteeism_at_work.csv"))
unlink(temp)

# load info file
temp <- tempfile()
download.file(
  "https://archive.ics.uci.edu/ml/machine-learning-databases/00445/Absenteeism_at_work_AAA.zip",
  temp)
info <- read_docx(unzip(temp, "Attribute Information.docx"), skip = 1)[1:44]
unlink(temp)

# explore data
str(data)

# loop to check if any variable has NAs
sum(apply(data, 2, anyNA))

# count unique values in tahe target variable
length(unique(data$Absenteeism.time.in.hours))

# check how many times each value appear.
summary(as.factor(data$Absenteeism.time.in.hours))

# explore reasons
sort(unique(data$Reason.for.absence))
# check what does zero value stand for in the reasons variable by exploring target values for those observations.
data$Absenteeism.time.in.hours[data$Reason.for.absence == 0]

# get reasons names form info file. Skip the 23rd line as reason 20 is absent from the data
reason_n <- c(0,info[4:22],info[24]) 

# list non ICD reasons
c <- c("patient follow-up", "medical consultation",
       "blood donation", "laboratory examination",
       "unjustified absence", "physiotherapy",
       "dental consultation") # list non ICD reasons
c # add non ICD reasons
reason_n[22:28] <- c 
reason_n


# histograms of numerical variables
data %>% ggplot(aes(Hit.target)) + geom_histogram()
data %>% ggplot(aes(Transportation.expense)) + geom_histogram()
data %>% ggplot(aes(Age)) + geom_histogram()
data %>% ggplot(aes(Service.time)) + geom_histogram()
data %>% ggplot(aes(Absenteeism.time.in.hours)) + geom_histogram()
data %>% ggplot(aes(Work.load.Average.day)) + geom_histogram(stat="count")
data %>% ggplot(aes(Height)) + geom_histogram()
data %>% ggplot(aes(Weight)) + geom_histogram()

# histogram of reasons
data %>% ggplot(aes(as.factor(Reason.for.absence))) + 
  geom_histogram(stat = 'count') +
  scale_x_discrete(label = str_trunc(reason_n, 40)) +
  theme(axis.text.x = element_text(angle = 65, hjust = 1, size = 10))


# create a vectore wiht varaible names 
variable <- colnames(data)[1:20] 
# initialize as a list
data1 <- list()
# create a loop that calculate target sd for each variable grouped
for(i in variable){ 
  f <- data %>%
    group_by_at(i) %>%
    summarise(median = median(Absenteeism.time.in.hours))
  data1[[i]] <- sd(f$median)
}

kable(arrange(tibble(var = names(data1),
                     absent.sd = unlist(data1)),
              desc(absent.sd)))

# error bar showing effect of week day on absenteeism
data %>% group_by(Day.of.the.week) %>%
  summarise(n = n(), 
            avg = mean(Absenteeism.time.in.hours), 
            sd = sd(Absenteeism.time.in.hours),
            se = sd/sqrt(n)) %>%
  ggplot(aes(as.factor(Day.of.the.week), avg)) + geom_point() +
  geom_errorbar(aes(ymin=avg - 2*se, ymax=avg+2*se)) +
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

# error bar showing effect of Work load on absenteeism
data %>% group_by(Work.load.Average.day) %>%
  summarise(n = n(), 
            avg = mean(Absenteeism.time.in.hours), 
            sd = sd(Absenteeism.time.in.hours),
            se = sd/sqrt(n)) %>%
  ggplot(aes(as.factor(Work.load.Average.day), avg)) + geom_point() +
  geom_errorbar(aes(ymin=avg - 2*se, ymax=avg+2*se)) +
  theme(axis.text.x = element_text(angle = 65, hjust = 1)) 

# error bar showing effect of Reason for absence on absenteeism
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

# changing variable class according to type of data.
data_w <- data %>% 
  mutate_at(c(1,2,3,4,5), as.factor) %>% 
  mutate(Work.load.Average.day =
            as.numeric(levels(Work.load.Average.day))[Work.load.Average.day])

# create a list of numeric variables' indices
nums <- unlist(lapply(data_w, is.numeric))


# boxplot outliers
data_w %>% gather(key = "key", value ="value", -c(names(data_w[,!nums]))) %>% 
  ggplot(aes(key,value)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 65, hjust = 1, size = 10))

# examine outliers
summary(data_w$Height)
summary(data_w$Hit.target)
summary(data_w$Service.time)
summary(data_w$Absenteeism.time.in.hours)

# create a function that caps outliers.
caps_outliers <- function(x){
  qnt <- quantile(x, c(0.25, 0.75))
  cap <- quantile(x, c(0.05, 0.95))
  fence <- 1.5 * IQR(x)
  x[x < (qnt[1] - fence)] <- cap[1]
  x[x > (qnt[2] + fence)] <- cap[2]
  x
}

# create a new numerical variables index that doesn't include the target
nums_t <- nums
nums_t[21] <- FALSE

# use the new function to cap outliers from all numerical variables except target 
data_w[,nums_t] <- apply(data_w[,nums_t],2,caps_outliers)

# rescale numerical variable to have a value from 0 to 1.
data_w[,nums] <- apply(data_w[,nums], 2, scales::rescale)

# create correlation matrix
cor.dat <- cor(data_w[,nums])

# create heat map
heatmap(cor.dat)

# check correlation between BMI and weight
with(data_w,c(cor(Weight, Body.mass.index)))

# remove BMI due to high correlation with weight 
data_w$Body.mass.index <- NULL

# binarize catagorical varaibles 
data_w <- dummy_cols(data_w, remove_selected_columns = TRUE)
dim(data_w)

# remove target from data
data_w$Absenteeism.time.in.hours -> y
data_w$Absenteeism.time.in.hours <- NULL

# calculate the principal components of the rest of the data
pca <-  prcomp(data_w)
dim(pca$x)


# Proportion of variance calculated
variability <- pca$sdev^2/sum(pca$sdev^2)

# plot variability maintained vs number of components 
plot(cumsum(variability))

# check how much variability is kept if the first 45 components were used.
sum(variability[1:45])
 
# reduce dimesions
data_pc <- pca$x[,1:45]

# split data and target.
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y,times = 1, p = 0.2, list = FALSE)
train <- data_pc[-test_index,]
test <- data_pc[test_index,]
train_y <- y[-test_index]
test_y <- y[test_index]

# list models
models <- c("lm","mlp","knn", "penalized","krlsPoly","foba","BstLm")

# train models on the train set
fits <- lapply(models, function(model){
  set.seed(2, sample.kind = "Rounding")
  print(model)
  caret::train(train,train_y, method = model)
})
names(fits) <- models

# predict using models
y_hat <- sapply(fits, function(x){
  predict(x, test)
})

# calculate RMSEs 
rmses <- apply(y_hat, 2, function(x){
  RMSE(x,test_y)
})
rmses

# Ensembles by mean
ensemble <- apply(y_hat[,c(4,6,7)], 1, mean)

RMSE(ensemble, test_y)

# unscaled target
real_y <- data$Absenteeism.time.in.hours

# rescale models' RMSEs to real scale
rmses_res <- rmses*(max(real_y)-min(real_y))-min(real_y)
rmses_res

# rescale ensemble RMSE
paste("ensemble", round(RMSE(ensemble, test_y)*(max(real_y)-min(real_y))-min(real_y), digits = 4))

# predictions will be rescaled to original scale
y_hats_rescaled <- y_hat*(max(real_y)-min(real_y))-min(real_y)

# error vectors will be created to measure each models predction error
error <- as.data.frame(abs(y_hats_rescaled - test_y*(max(real_y)-min(real_y))-min(real_y)))
boxplot(error)

# unscaled test set target 
Absent <- data$Absenteeism.time.in.hours[test_index]

# bind errors and target
test_ns <- cbind(Absent,error)

# plot errors against test target
test_ns %>% gather(key = "model", value = "error", -Absent) %>%
  ggplot(aes(x = Absent, y = error, color = model)) + 
  geom_point() + scale_x_sqrt()

# plot errors against amount of data on Absenteeism
test_ns  %>%  group_by(Absent)  %>%  mutate(n = n()) %>% ungroup() %>%
  gather(key = "model", value = "error", -c(n,Absent)) %>%
  ggplot(aes(x = n, y = error, color = model)) + 
  geom_point()

