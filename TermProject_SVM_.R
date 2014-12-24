setwd("C:/Users/Avva_2/Desktop/ML/T-61_3050_additional_materials/Term Project")
library('e1071')
data = read.csv("T-61_3050_training_dataset.csv", header = TRUE)
testData = read.csv("T-61_3050_test_dataset.csv", header = TRUE)
source(file="C:\\Users\\Avva_2\\Desktop\\ML\\T-61_3050_additional_materials\\Term Project\\T-61_3050_challenge_evaluator.r")
trainingData=data[1:3750,]
validationData=data[3751:5000,]
X = trainingData[,1:11]
Y = trainingData[,12]
Z = trainingData[,13]


	model_1 = svm(trainingData[,1:11],Y, kernel = 'polynomial',type= 'C-classification')
	prediction_quality_train=predict(model_1, trainingData[,1:11], type="class")
	prediction_quality_val=predict(model_1, validationData[,1:11], type="class")
	prediction_quality_test=predict(model_1, testData[,1:11], type="class")
	model_2 = svm(trainingData[,1:11],Z, kernel = 'polynomial',type= 'C-classification' )
	prediction_types_train=predict(model_2, trainingData[,1:11], type="class")
	prediction_types_val=predict(model_2, validationData[,1:11], type="class")
	prediction_types_test=predict(model_2, testData[,1:11], type="class")
	training_set_accuracy = sum(prediction_types_train==trainingData$type)*100/length(prediction_types_train)
	validation_set_accuracy = sum(prediction_types_val==validationData$type)*100/length(prediction_types_val)
	test_set_accuracy = sum(prediction_types_test==testData$type)*100/length(prediction_types_test)

	training_set_accuracy_quality = sum(prediction_quality_train==trainingData$quality)*100/length(prediction_quality_train)
	validation_set_accuracy_quality  =sum(prediction_quality_val==validationData$quality)*100/length(prediction_quality_val)
	test_set_accuracy_quality  = sum(prediction_quality_test==testData$quality)*100/length(prediction_quality_test)