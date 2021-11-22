#Classification of complete shapes using the SRVF framework. 
#We are going to compare the cross validation error rates using the SRVF 
#to the error rates that we found using the elliptical fourier analysis framework.


#Load packages
library(randomForest)
library(tidyverse)


toothtype <- "LM1"
i <- 1

#Read in the data
results <- list()

X_train <- read.csv(paste0("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/data/",toothtype,"/",toothtype,"fold_train",i,".csv"), header = FALSE)
X_test <- read.csv(paste0("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/data/",toothtype,"/",toothtype,"fold_test",i,".csv"), header = FALSE)

y_train <- read.csv(paste0("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/data/",toothtype,"/",toothtype,"fold_train_cats",i,".csv"), header = FALSE)
y_test <- read.csv(paste0("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/data/",toothtype,"/",toothtype,"fold_test_cats",i,".csv"), header = FALSE)

y_train <- (as.factor(y_train$V1))
y_test <- (as.factor(y_test$V1))

a <- randomForest(X_train, y_train)
results[[i]] <- data.frame(pred_class = predict(a, X_test), real_class = y_test)

