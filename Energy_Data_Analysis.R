################# Part 1 #############################

#Acquiring data
the.data <- as.matrix(read.table("Energy20.txt "))
the.data

#Subset
my.data <- the.data[sample(1:671,350),c(1:6)]
my.data



#Assigning column data to variables

X1 = my.data[,1]
X2 = my.data[,2]
X3 = my.data[,3]
X4 = my.data[,4]
X5 = my.data[,5]
Y = my.data[,6]


#Scatter plot of X against Y

plot(X1,Y, main = "Scatter plot of X1 and Y")
abline(lm(Y~X1),col ="Red")
plot(X2,Y, main = "Scatter plot of X2 and Y")
abline(lm(Y~X2),col ="Red")
plot(X3,Y, main = "Scatter plot of X3 and Y")
abline(lm(Y~X3),col ="Red")
plot(X4,Y, main = "Scatter plot of X4 and Y")
abline(lm(Y~X4),col ="Red")
plot(X5,Y, main = "Scatter plot of X5 and Y")
abline(lm(Y~X5),col ="Red")

#Histograms of columns
hist(X1)
hist(X2)
hist(X3)
hist(X4)
hist(X5)
hist(Y)
################# Part 2 #############################


#Checked for sqrt, log transformation and log transform seemed better
#hist(log(X1))
#hist(log(X2))
#hist(log(Y))

#Applying log on X1, X2 and Y to reduce skewness
X1 = log(X1)
X2 = log(X2)
Y = log(Y)

# Applying linear scaling to X1, X2, X3, X4 and Y

scaledX1 = (X1 - min(X1))/(max(X1)-min(X1))
scaledX2 = (X2 - min(X2))/(max(X2)-min(X2))
scaledX3 = (X3 - min(X3))/(max(X3)-min(X3))
scaledX4 = (X4 - min(X4))/(max(X4)-min(X4))
scaledY = (Y - min(Y))/(max(Y)-min(Y))

scaledData = cbind(scaledX1, scaledX2, scaledX3, scaledX4, scaledY)

write.table(scaledData,"Rajeshkumar-transformed.txt")

################# Part 3 #############################

source("AggWaFit718.R")

#WAM
fit.QAM(scaledData[,c(1:4,5)],"WAMoutput.txt", "WAMstats.txt")

#WPM for P = 0.5
fit.QAM(scaledData[,c(1:4,5)],"PMoutput.txt", "PMstats.txt", g=PM05, g.inv = invPM05)

#WPM for P = 5
fit.QAM(scaledData[,c(1:4,5)],"PM5output.txt", "PM5stats.txt", g=PM5, g.inv = invPM5)

#OWA
fit.OWA(scaledData[,c(1:4,5)],"OWAoutput.txt", "OWAstats.txt")

#choquet
fit.choquet(scaledData[,c(1:4, 5)], "Choquetoutput2.txt", "Choquetstats2.txt")


################# Part 4 #############################
#Predicting new Y based on X1 = 17, X2 = 39, X3 = 4, X4 = 77

#Transforming the data similar to previous scaling
newX1 = log(17)
newX1 = (newX1 - min(X1))/(max(X1)-min(X1))
newX2 = log(39)
newX2 = (newX2 - min(X2))/(max(X2)-min(X2))
newX3 = (4 - min(X3))/(max(X3)-min(X3))
newX4 = (77 - min(X4))/(max(X4)-min(X4))

PredictSet = c(newX1, newX2, newX3, newX4)
choquetWeights = c(0.219175498128903,0,0.219175498128903,
                   0.315080630847907,0.947498408799821,0.545973379661499,
                   0.947498408799875,0.347431549324234,0.47510559260446,
                   0.347431549324234,0.47510559260446,0.69358116518173,
                   1.00000000001103,0.693581165181768,1.00000000001106)
#Precting Y
PredictedCho_Y = choquet(PredictSet, choquetWeights)

#Transforming back to get Predicted value in original range
newY = (PredictedCho_Y * (max(Y) - min(Y))) + min(Y)
PredictedCho_Y = exp(newY)

################# Part 5 #############################
dataset = read.table("Rajeshkumar-transformed.txt")

lm_X1 = dataset[,1]
lm_X2 = dataset[,2]
lm_X3 = dataset[,3]
lm_X4 = dataset[,4]
lm_Y = dataset[,5]

#Applying lm() to subset
fitlm = lm(formula = lm_Y~ lm_X1+lm_X2+lm_X3+lm_X4, data=dataset)

#Summary
summary(fitlm)

#Display linear model to find coefficients for prediction model
fitlm

#Choquet integral predictions of Y for full subset and rescaling it to original range

Data_Predicted_Y = array(1:350)
for (i in 1:350) {
  currentSet = c(dataset[i,1],dataset[i,2],dataset[i,3],dataset[i,4])
  currentY=choquet(currentSet,choquetWeights)
  newY = (currentY * (max(Y) - min(Y))) + min(Y)
  Data_Predicted_Y[i] = exp(newY)
}


#Linear model predictions of Y and rescaling it to original range

lm_predicted_Y = -0.10889 + (lm_X1 * 0.28392) + (lm_X2* -0.03222) + (lm_X3*0.63385) + (lm_X4*0.35771)
lm_predicted_Y = (lm_predicted_Y * (max(Y) - min(Y))) + min(Y)
lm_predicted_Y = exp(lm_predicted_Y)

#Plots of Original Y and predicted Y

hist(my.data[,6], main = "Original Y values")
hist(lm_predicted_Y, main="Linear Model - Predicted Y")
hist(Data_Predicted_Y, main="Choquet integral - Predicted Y")

