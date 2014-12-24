setwd("C:\\Users\\Avva_2\\Desktop\\ML\\Exercise 6\\T-61_3050_autompgdata")
data = read.csv("auto-mpg.csv", header = FALSE)
X = apply(data, 2, function(y) y - mean(y)) 
	coVar = cov(X)
	eigenVect = eigen(coVar)$vectors
	eigenVals = eigen(coVar)$values
	Z = X%*%eigenVect[,1:2]
	plot(Z,xlab="Component 1", ylab = "Component 2",pch=20, cex=2, col="orange")
for( k in 1 : dim(X)[2])
{
	Z = X%*%eigenVect[,1:k]
	error = sum(eigenVals[1:k])/sum(eigenVals)
	print(error)
}