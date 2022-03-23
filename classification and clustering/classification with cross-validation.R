
# split data into 80% training and validation, and 20% testing. I then used k-fold cross validation with a k-value of 4, with data being split up using sampling and set.seed for reproducible pseudo-random data. 
# Using k-nearest neighbors, I coded out the individual steps of cross validation and found a k value of 6 to produce the highest prediction accuracy of 84.92366%	
# 
# If we do not do a set.seed, we can see there is quite a bit of variation in model performance from run-to-run as the random sampling changes, which shows there is a relatively high amount of random patterns in the data.
# 
# When testing this model on our test set, we get an accuracy of 83.07692%. Again, the slightly lower prediction accuracy on the test set is not surprising due to the random patterns in the data. 

library(kernlab)
library(kknn)
library(dplyr)

data <- read.table("C:/Users/namee/Documents/Masters/ISYE6501 - Intro Analytics Modelling/HW2/credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE)

#split data into 80% train/validate & 20% test via random sampling
s <- sample(nrow(data),524)
train_val <- data[s,]
test <- data[-s,]

#Split data into k-sets------------------------------------------
k <- 4

#define number of rows for each kth set
set_size <- nrow(train_val)/k
print(set_size)

#initialize vector of length k to store created sets
sets <- list()

#initialize empty df for storing rows already utilized for a set
sampled <- train_val[0,] 

#loop through data to split into k sets
for(i in 1:k){
  #subtract already sampled data to get unused data for creating next set
  unsampled <- anti_join(train_val,sampled)
  
  set.seed(1)
  s <- sample(nrow(unsampled),set_size)
  set <- unsampled[s,]
  
  sets[[i]] <- set
  
  #update df of utilized data with recently created set
  sampled <- rbind.data.frame(sampled,set)
}

#train & validate--------------------------------------------------

#setup empty df to record accuracies
avg_accuracies <- data.frame(k=c(),accuracy=c())

for(k_val in 5:20){
  
  #initiate results vector to store accuracy for each k-fold
  fold_accuracies <- vector()
  
  for(i in 1:length(sets)){
    data_train <- do.call(rbind,sets[-i])
    data_valid <- sets[[i]]
    model <- kknn(V11~., data_train, data_valid, k=k_val, kernel="optimal", scale=TRUE)
    result <- round(model$fitted.values)
    
    #append prediction accuracy of this fold to vector
    fold_accuracies <- rbind(fold_accuracies,sum(result == data_valid[,11]) / nrow(data_valid))
  }
  
  #average accuracy over k-folds and store in df with corresponding k-value
  avg_accuracy <- data.frame(k_val,mean(fold_accuracies))
  
  #append to df with all k-values
  avg_accuracies <- rbind(avg_accuracies,avg_accuracy)
}

print(avg_accuracies)
plot(avg_accuracies)

#train on entire train_val dataset with chosen model-------------------------
model <- kknn(V11~., train_val, test, k=6, kernel="optimal", scale=TRUE)
result <- round(model$fitted.values)
accuracy <- sum(result == test[,11])/nrow(test)
print(accuracy)




# 
# Using a 60/20/20 split of train/validate/test to choose and test a kknn model, we find a k-value of 10, 11, or 12 to yield the best results of 87.02290%, again with a high variability in between runs. On the test set, this results in an accuracy of 84.61538%. 

library(kernlab)
library(kknn)
library(dplyr)

data <- read.table("C:/Users/namee/Documents/Masters/ISYE6501 - Intro Analytics Modelling/HW2/credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE)

#split data into 60% train, 20% validate, 20% test-------------------------------
set.seed(1)
s1 <- sample(nrow(data),393)
train <- data[s1,]

#remove selected train data from dataset
remaining <- anti_join(data,train)   #or do remaining <- data[-s1]

set.seed(2)
s2 <- sample(nrow(remaining),131)
val <- remaining[s2,]
test <- remaining[-s2,]

accuracies <- data.frame(k=c(),accuracy=c())

for(i in 5:20){
  model <- kknn(V11~.,train,val,k=i,kernel='optimal',scale=TRUE)
  result <- round(model$fitted.values)
  
  #append accuracies
  accuracy <- data.frame(i,sum(result == val[,11])/nrow(val))
  accuracies <- rbind(accuracies,accuracy)
}

print(accuracies)
plot(accuracies)

#test model with k=12
model <- kknn(V11~.,train,test,k=12,kernel='optimal',scale=TRUE)
result2 <- round(model$fitted.values)
print(sum(result2 == test[,11])/nrow(test))


# I looped through various k values and different combinations of predictor columns (where 1=column1 in the iris dataset, 2=column2 etc.)
# 
# To measure how each kmeans algorithm performed, I chose the tot.withinss distance metric as it matches the formula provided in class for minimizing distance between clusters and their data points.
# 
# Looking at our distance-clusters graphs, k=3 seems to consistently show where the kink starts, meaning it is the best k to choose before we see diminishing returns. 
# 
# Looking at our "k vs distance vs predictors" graph, we see relationship between columns 2,4 seems to fair the best with minimum distance given any number of clusters. 
# 
# Then, looking at the different predictor combinations that include both columns 2 and 4, we have [2,4] or [2,3,4]. 


library(datasets)
library(ggplot2)
library(dplyr)

data <- iris[,-5]

ks <- c(2,3,4,5,6,7)
predictors <- list(c(1,2),c(1,3),c(1,4),c(2,3),c(2,4),c(3,4))
#c(1,2,3,4), c(1,2,3),c(1,2,4),c(1,3,4),c(2,3,4))

df <- data.frame(predictors=c(),k=c(),distance=c())

#Identify number of clusters & predictors to use------------------------------
for(i in predictors){
  distances <- c()
  
  for(k in ks){
    cl <- kmeans(data[,as.vector(i)],k)
    
    #calculate total within-cluster sum of square distance
    distance <- cl$tot.withinss
    distances <- rbind(distances,distance)
    
    #output all 3 variables (predictors, k,distance,) to a dataframe
    result <- data.frame(Predictors=toString(i),k,distance)
    df <- rbind(df,result)
  } 
  
  plot(ks,distances,type="b",main=toString(i))
}

ggplot(data=df)+geom_point(mapping=aes(x=k,y=Predictors,size=distance))


#Test cluster performance- with columns 2,4 as predictors------------------------
cl2 <- kmeans(data[,c(2,4)],3)

#add cluster as columns to iris dataframe
iris$result <- cl2$cluster

#count occurence of each flower in each cluster
for(i in 1:3){
  count <- iris[,5:6] %>% filter(result==i)
  print(summary(count)) 
}

#Test cluster performance- with columns 2,3,4 as predicto------------------------
cl2 <- kmeans(data[,c(2,3,4)],3)

#add cluster as columns to iris dataframe
iris$result <- cl2$cluster

#count occurence of each flower in each cluster
for(i in 1:3){
  count <- iris[,5:6] %>% filter(result==i)
  print(summary(count)) 
}

