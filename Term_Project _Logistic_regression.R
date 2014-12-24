setwd("C:/Users/Avva_2/Desktop/ML/T-61_3050_additional_materials/Term Project")
library(nnet)
data = read.csv("T-61_3050_training_dataset.csv", header = TRUE)
data_test = read.csv("T-61_3050_challenge_dataset.csv", header = TRUE)
data_test = data_test[,-1]
source(file="C:\\Users\\Avva_2\\Desktop\\ML\\T-61_3050_additional_materials\\Term Project\\T-61_3050_challenge_evaluator.r")
wine_quality = data$quality
data$quality = as.factor(data$quality)
library(pracma)
alpha = 0.01 # learning rate
getTheta <- function(X,result)
{
	no_parameters = dim(X)[2]
	N = dim(X)[1]
	theta = matrix(0,1,no_parameters)
	h_theta = 1/(1+exp(-(X%*%t(theta))))
	likeliHood = (1/N)*sum(result*log(h_theta)+(1-result)*(log(1-h_theta)))
	prev_likeliHood = 0
	#while(prev_likeliHood !=likeliHood ) 
	for(i in 1:100000)
	{
		prev_likeliHood = likeliHood
		y_p =(sigmoid(X%*%t(theta)))
		for (j in 1:no_parameters)
		{
			theta[1,j] = theta[1,j] - (((alpha)*sum((y_p-result)*X[,j]))/N) - ((alpha/N)*theta[1,j]);
		}
		h_theta = sigmoid(X%*%t(theta))
		likeliHood = (1/N)*sum(result*log(h_theta)+(1-result)*(log(1-h_theta)))
	}
	return (theta)
}
training_set = 1:3500
X = as.matrix(data[training_set,1:11])
X = scale(X, center = TRUE, scale = TRUE) 
X = cbind(matrix(1, dim(X)[1],1),X)
X_test = as.matrix(data_test[,1:11])
X_test = scale(X_test, center = TRUE, scale = TRUE) 
X_test = cbind(matrix(1, dim(X_test)[1],1),X_test)
temp= class.ind(data[training_set,13])
target_class =  as.matrix(temp[,1])
no_parameters = dim(X)[2]
theta =matrix(0,1,no_parameters)
theta = getTheta(X,target_class)
output = 1/(1+exp(-(X_test%*%t(theta))))
prediction_class = ifelse(output>0.5,"Red","White")
temp= class.ind(data[training_set,12])
target_quality =  as.matrix(temp)
theta_7 = matrix(0,7,no_parameters) 
for(i in 1:7)
{
	theta_7[i,] = getTheta(X,target_quality[,i])
}
output = 1/(1+exp(-(X_test%*%t(theta_7))))
prediction = list(1:dim(X_test)[1])
for(i in 1:dim(X_test)[1])
	prediction[i] = which.max(output[i,])
prediction_quality<- as.numeric(unlist(prediction))
fileConn<-file("results.csv")
results = cbind(prediction_quality,prediction_class)
write.table(results, fileConn, row.names=FALSE,na="",col.names=FALSE, sep=",")
