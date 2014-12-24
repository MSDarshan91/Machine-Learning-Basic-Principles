setwd("C:/Users/Avva_2/Desktop/ML/T-61_3050_additional_materials/Term Project")
library('nnet')
data = read.csv("T-61_3050_training_dataset.csv", header = TRUE)
testData = read.csv("T-61_3050_test_dataset.csv", header = TRUE)
source(file="C:\\Users\\Avva_2\\Desktop\\ML\\T-61_3050_additional_materials\\Term Project\\T-61_3050_challenge_evaluator.r")
trainingData=data[1:3750,]
validationData=data[3751:5000,]
X = trainingData[,1:11]
Y = trainingData[,12]
Z = trainingData[,13]
hidden_layers = 40
training_set_accuracy = 1:hidden_layers
validation_set_accuracy = 1:hidden_layers
test_set_accuracy = 1:hidden_layers
for(i in 1:hidden_layers)
{
	ANN = nnet(trainingData[,1:11],class.ind(Y), size=i+5, softmax=TRUE)
	prediction_quality_train=predict(ANN, trainingData[,1:11], type="class")
	prediction_quality_val=predict(ANN, validationData[,1:11], type="class")
	prediction_quality_test=predict(ANN, testData[,1:11], type="class")
	training_set_accuracy[i] = sum(prediction_quality_train==trainingData$quality)*100/length(prediction_quality_train)
	validation_set_accuracy[i] =sum(prediction_quality_val==validationData$quality)*100/length(prediction_quality_val)
	test_set_accuracy[i] = sum(prediction_quality_test==testData$quality)*100/length(prediction_quality_test)
}
 val_quality_k = which(max(validation_set_accuracy) ==validation_set_accuracy)
 test_quality_k = which(max(test_set_accuracy) ==test_set_accuracy)
 plot_data = cbind(i=6:45,training_set_accuracy,validation_set_accuracy,test_set_accuracy)
 plot_data= data.frame(plot_data)
 ggplot(plot_data, aes(x=i,y=Accuracy,colour = Set)) +   geom_line(aes(y = training_set_accuracy, colour = "Training")) +   geom_line(aes(y = test_set_accuracy, colour = "Testing")) +   geom_line(aes(y = validation_set_accuracy, colour = "Validation")) +  xlab("No of Hidden neurons") +
  ylab("Accuracy (%)") +
  ggtitle("Accuracy Vs No of Hidden Neurons")
 for(i in 1:hidden_layers)
{
	ANN_1 = nnet(trainingData[,1:11],class.ind(Z), size=i+5, softmax=TRUE)
	prediction_types_train=predict(ANN_1, trainingData[,1:11], type="class")
	prediction_types_val=predict(ANN_1, validationData[,1:11], type="class")
	prediction_types_test=predict(ANN_1, testData[,1:11], type="class")
	training_set_accuracy[i] = sum(prediction_types_train==trainingData$type)*100/length(prediction_types_train)
	validation_set_accuracy[i] = sum(prediction_types_val==validationData$type)*100/length(prediction_types_val)
	test_set_accuracy[i] = sum(prediction_types_test==testData$type)*100/length(prediction_types_test)
}
dev.new()
 plot_data = cbind(i=6:45,training_set_accuracy,validation_set_accuracy,test_set_accuracy)
 plot_data= data.frame(plot_data)
 ggplot(plot_data, aes(x=i,y=Accuracy,colour = Set)) +   geom_line(aes(y = training_set_accuracy, colour = "Training")) +   geom_line(aes(y = test_set_accuracy, colour = "Testing")) +   geom_line(aes(y = validation_set_accuracy, colour = "Validation")) +  xlab("No of Hidden neurons") +
  ylab("Accuracy (%)") +
  ggtitle("Accuracy Vs No of Hidden Neurons")
   val_type_k = which(max(validation_set_accuracy) ==validation_set_accuracy)
 test_type_k = which(max(test_set_accuracy) ==test_set_accuracy)