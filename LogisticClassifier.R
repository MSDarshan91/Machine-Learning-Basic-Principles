#  Implement the gradient ascent algorithm to find parameters θ = (w, w0) for the logistic discrimination binary classifier
data = read.csv("T-61_3050_pima_indian_diabete_data.csv", header = FALSE)
library(pracma)
theta =matrix(0,1,9)
X = as.matrix(data[,1:8])
X = scale(X, center = TRUE, scale = TRUE) 
X_test = X[101:768,]
X_test = cbind(matrix(1, dim(X_test)[1],1),X_test)
X = X[1:100,]
N = dim(X)[1]
X = cbind(matrix(1, dim(X)[1],1),X)
result =  as.matrix(data[1:100,9])
result_test =  as.matrix(data[101:768,9])
h_theta = 1/(1+exp(-(X%*%t(theta))))
likeliHood = (1/N)*sum(result*log(h_theta)+(1-result)*(log(1-h_theta)))
prev_likeliHood = 0
while(prev_likeliHood !=likeliHood ) 
{
	prev_likeliHood = likeliHood
	y_p =(sigmoid(X%*%t(theta)))
	for (j in 1:9)
	{
		theta[1,j] = theta[1,j] - (((0.01)*sum((y_p-result)*X[,j]))/N)
	}
	h_theta = sigmoid(X%*%t(theta))
	likeliHood = (1/N)*sum(result*log(h_theta)+(1-result)*(log(1-h_theta)))
}
output = 1/(1+exp(-(X_test%*%t(theta))))
prediction = ifelse(output>0.5,1,0)
testing_accuracy = 100*sum(ifelse(prediction==result_test,1,0))/length(result_test)
print(testing_accuracy)
