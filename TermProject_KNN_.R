setwd("C:/Users/Avva_2/Desktop/ML/T-61_3050_additional_materials/Term Project")
library('class')
library(ggplot2)
data = read.csv("T-61_3050_training_dataset.csv", header = TRUE)
testData = read.csv("T-61_3050_test_dataset.csv", header = TRUE)
source(file="C:\\Users\\Avva_2\\Desktop\\ML\\T-61_3050_additional_materials\\Term Project\\T-61_3050_challenge_evaluator.r")
trainingData=data[1:3750,]
validationData=data[3751:5000,]
X = trainingData[,1:11]
Y = trainingData[,12]
Z = trainingData[,13]
k = 100
training_set_accuracy = 1:k
validation_set_accuracy = 1:k
test_set_accuracy = 1:k
for(i in 1:k)
{
	prediction_quality_val = knn(trainingData[,1:11],validationData[,1:11],Y,k=i,prob=TRUE)
	prediction_quality_test = knn(trainingData[,1:11],testData[,1:11],Y,k=i,prob=TRUE)
	validation_set_accuracy[i] =(sum(prediction_quality_val==validationData$quality)/length(prediction_quality_val))*100
	test_set_accuracy[i] = (sum(prediction_quality_test==testData$quality)/length(prediction_quality_test))*100
}
 plot_data = cbind(i=1:k,validation_set_accuracy,test_set_accuracy)
 plot_data= data.frame(plot_data)
 ggplot(plot_data, aes(x=i,y=Accuracy,colour = Set)) +   geom_line(aes(y = test_set_accuracy, colour = "Testing")) +   geom_line(aes(y = validation_set_accuracy, colour = "Validation")) +  xlab("K") +
  ylab("Accuracy (%)") +
  ggtitle("Accuracy Vs K")
 val_quality_k = which(max(validation_set_accuracy) ==validation_set_accuracy)
 test_quality_k = which(max(test_set_accuracy) ==test_set_accuracy)
 for(i in 1:k)
{
	prediction_types_val=knn(trainingData[,1:11],validationData[,1:11],Z,k=i,prob=TRUE)
	prediction_types_test=knn(trainingData[,1:11],testData[,1:11],Z,k=i,prob=TRUE)
	validation_set_accuracy[i] = (sum(prediction_types_val==validationData$type)/length(prediction_types_val))*100
	test_set_accuracy[i] = (sum(prediction_types_test==testData$type)/length(prediction_types_test))*100
}
 plot_data = cbind(i=1:k,validation_set_accuracy,test_set_accuracy)
 plot_data= data.frame(plot_data)
 dev.new()
 ggplot(plot_data, aes(x=i,y=Accuracy,colour = Set)) +   geom_line(aes(y = test_set_accuracy, colour = "Testing")) +   geom_line(aes(y = validation_set_accuracy, colour = "Validation")) +  xlab("K") +
  ylab("Accuracy (%)") +
  ggtitle("Accuracy Vs K")
  val_type_k = which(max(validation_set_accuracy) ==validation_set_accuracy)
 test_type_k = which(max(test_set_accuracy) ==test_set_accuracy)
 