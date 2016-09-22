# classify the patients into the two different classes, based on a single physical attribute only
data = read.csv("T-61_3050_pima_indian_diabete_data.csv", header = FALSE)
for(i in 1:8)
{
	training_set=data[1:100,]
	validation_set=data[101:768,]
	feature = training_set[,i]
	result = training_set[,9]
	A = cbind(data.frame(feature),data.frame(result))
	M = aggregate(. ~ result, data=A, FUN=mean)
	m_0 = M[1,2]
	m_1 = M[2,2]
	V = aggregate(. ~ result, data=A, FUN=var)
	v_0 = V[1,2]
	v_1 = V[2,2]
	P=as.data.frame(table(result))
	N = P[1,2] + P[2,2]
	P_0 = P[1,2] / (P[1,2] + P[2,2])
	P_1 = P[2,2] / (P[1,2] + P[2,2])
	val_feature = validation_set[,i]
	val_result = validation_set[,9]
	P_C_0 = P_0*exp(-(val_feature-m_0)^2/(2*v_0))/sqrt(2*pi*v_0)
	P_C_1 = P_1*exp(-(val_feature-m_1)^2/(2*v_1))/sqrt(2*pi*v_1)
	prediction = ifelse(P_C_0>P_C_1,0,1)
	accuracy = 100*sum(ifelse(prediction==val_result,1,0))/length(val_result)
	print(paste("Accuracy for feature ",i," is ",accuracy))
	print(paste("Parameters ",m_0,m_1,sqrt(v_0),sqrt(v_1),P_0,P_1))
}
