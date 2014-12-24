# The task is to predict the class of a patient (the final diagnosis, normal or abnormal) in the test set based on the 22 image features
# Compute classification errors on both training and test sets. Take a look at the parameters of the classifier. Can you say which features are important in differentiating normal and abnormal patients?

data = read.csv("spect_training.txt", header = FALSE)
alpha=1
K=2
r = data[,1]
P=as.data.frame(table(r))
N = P[1,2] + P[2,2]
P_0 = P[1,2] / (P[1,2] + P[2,2])
P_1 = P[2,2] / (P[1,2] + P[2,2])
A = cbind(data.frame(data[,2:23]),data.frame(r))
M = aggregate(. ~ r, data=A, FUN=sum)
P_0_J = (alpha+as.numeric(M[1,2:23]))/(K*alpha+P[1,2])
P_1_J = (alpha+as.numeric(M[2,2:23]))/(K*alpha+P[2,2])
W = log(P_0_J)-log(1-P_0_J)-log(P_1_J)+log(1-P_1_J)
w_0=sum(log(1-P_0_J))+log(P_0)-sum(log(1-P_1_J))-log(P_1)
D=as.matrix(data[,2:23])
W=as.matrix(W)
prediction = 1/(1+exp(w_0+D%*%W))
prediction = ifelse(prediction>0.5,1,0)
val_result=r
training_accuracy = 100*sum(ifelse(prediction==val_result,1,0))/length(val_result)
data = read.csv("spect_test.txt", header = FALSE)
D=as.matrix(data[,2:23])
r = data[,1]
prediction = 1/(1+exp(w_0+D%*%W))
prediction = ifelse(prediction>0.5,1,0)
val_result=r
testing_accuracy = 100*sum(ifelse(prediction==val_result,1,0))/length(val_result)