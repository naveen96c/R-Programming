the.data <- as.matrix(read.table("C://Users//61484//Desktop//Real world//Assesment 2//Energy20.txt ")) #assign the data to matrix

setwd("C:/Users/61484/Desktop/Real world/Assesment 2")

my.data <- the.data[sample(1:671,350),c(1:6)]

write.csv(my.data,"C:\\Users\\61484\\Desktop\\Real world\\Assesment 2\\Sample.csv", row.names = FALSE)

sample_Data =read.csv("C://Users//61484//Desktop//Real world//Assesment 2//sample.csv")
sample_Data

#DataFrame
data_Frame = data.frame(my.data)
cor(data_Frame[,1],data_Frame[,6])
cor(data_Frame[,2],data_Frame[,6])
cor(data_Frame[,3],data_Frame[,6])
cor(data_Frame[,4],data_Frame[,6])
cor(data_Frame[,5],data_Frame[,6])


#Scatter plot
plot(data_Frame[,1], data_Frame[,6],  xlab = 'X1 in Celcius' , ylab = 'Energy Consumption in Wh', main = 'Temperature in kitchen area VS Energy use of appliances', col= 'red')
plot(data_Frame[,2], data_Frame[,6], xlab = 'X2 in %' , ylab = 'Energy Consumption in Wh', main = 'Humidity in kitchen area VS Energy use of appliances', col='orange')
plot(data_Frame[,3], data_Frame[,6], xlab = 'X3 in Celcius' , ylab = 'Energy Consumption in Wh', main = 'Temperature outside VS Energy use of appliances', col='blue')
plot(data_Frame[,4], data_Frame[,6], xlab = 'X4 in %' , ylab = 'Energy Consumption in Wh', main = 'Humidity outside VS Energy use of appliances', col='green')
plot(data_Frame[,5], data_Frame[,6], xlab = 'X5 in km' , ylab = 'Energy Consumption in Wh', main = 'Visibility VS Energy use of appliances', col='black')

#Histogram

H1= hist(data_Frame[,1], xlab = 'X1 in Celsius', main='Temperature in kitchen area', col= "red", border="black")
H2= hist(data_Frame[,2], xlab = 'X2 in percentage', main= 'Humidity in kitchen area', col= "yellow", border="white")
H3= hist(data_Frame[,3], xlab = 'X3 in Celsius', main= 'Temperature outside', col= "orange", border="white")
H4= hist(data_Frame[,4], xlab = 'X4 in percentage', main= 'Humidity outside', col= "Blue", border="white")
H5= hist(data_Frame[,5], xlab = 'X5 in Km', main= 'Visibility', col= "Green", border="white")
Y= hist(data_Frame[,6], xlab = 'Energy Consumption in Wh', main = 'Energy use of applications', col= "grey", border="black")

#transform the data

TX1 =  log(data_Frame[,1]+1, max(data_Frame[,1]+1))
#plotNormalHistogram(residuals(TX1))
hist(TX1)


TX2 = log(data_Frame[,2]+1, max(data_Frame[,2]+1))
hist(TX2)


TX3 =log(data_Frame[,3]+1, max(data_Frame[,3]+1))
hist(TX3)


TX4 =log(data_Frame[,4]+1, max(data_Frame[,4]+1))
hist(TX4)


TX5 =  log(data_Frame[,5]+1, max(data_Frame[,5])+1)
hist(TX5)


TX6 =  log(data_Frame[,6]+1 , max(data_Frame[,6])+1)
hist(TX6)


transform_Array = cbind(TX1,TX2,TX3,TX4,TX6)

write.table(transform_Array,"C:\\Users\\61484\\Desktop\\Real world\\Assesment 2\\Naveen-transformed.txt")

source("AggWaFit718.R")

fit.QAM(transform_Array[,c(1:4,5)],'WAMOutput.txt','WAMstat.txt')
fit.QAM( transform_Array[,c(1:5)] ,'WPMoutput.txt','WPMstat.txt', g=PM05 , g.inv = invPM05)
fit.QAM( transform_Array[,c(1:5)] ,'WPMoutput(p=5).txt','WPMstat(p=5).txt', g=PM_5 , g.inv = invPM_5)
fit.OWA(transform_Array[,c(1:5)], 'OWAoutput.txt' , 'OWAstats.txt')
fit.choquet( transform_Array[,c(1:5)],'choquetOutput.txt','choquetStat.txt')

#model for prediction
modeltransform_X1 = log(17 + 1, max(data_Frame[,1]) + 1)
modeltransform_X2 = log(39 + 1, max(data_Frame[,2]) + 1)
modeltransform_X3 = log(4  + 1, max(data_Frame[,3]) + 1)
modeltransform_X4 = log(77 + 1, max(data_Frame[,4]) + 1)

 
choquet_Weights= c(0,
                   0,
                   0,
                   0.450096561419675,
                   0.450096561419675,
                   0.450096561419675,
                   0.450096561419675,
                   0,
                   0.402750171696363,
                   0,
                   0.579867107943164,
                   0.999999999994411,
                   0.999999999986134,
                   0.999999999994411,
                   0.999999999968544)

predicted_Data = choquet(c(modeltransform_X1,modeltransform_X2,modeltransform_X3,modeltransform_X4),choquet_Weights)
predicted_Data
predicted_Value = (max(data_Frame[,6])+1) ^(predicted_Data)
predicted_Value
mean(data_Frame[,6])

#linear regerssion
#model creation
transformarray_Dataframe = data.frame(transform_Array)
linear_Regression = lm( TX6 ~ TX1 + TX2 + TX3 + TX4, transformarray_Dataframe)
summary(linear_Regression)

#creating columns
a = data.frame(rbind(c(modeltransform_X1, modeltransform_X2, modeltransform_X3, modeltransform_X4)))
colnames(a)= c('TX1','TX2','TX3','TX4')

#predict the value
linearpredicted_Values = (max(data_Frame[,6])+1)^predict(linear_Regression,a)
linearpredicted_Values

#predicted values for 350 data
linearpredicted_Values_b = (max(data_Frame[,6])+1)^predict(linear_Regression,b)
linearpredicted_Values_b

#model choquet
input = cbind(TX1,TX2,TX3,TX4)
model_choquet <- as.matrix(1:350,ncol=1,nrow=350)
for(i in 1:350)
{
  model_choquet[i,1] = choquet(input[i,],choquet_Weights)
}
model_choquet
predicted_Value_ = (max(data_Frame[,6])+1) ^(model_choquet)
predicted_Value_

#ggplotx
library("ggplot2")
ggplot() +
  geom_line( data = data.frame( norml =  data_Frame[,6]) , aes( x = c(1:350)  , y =  norml , col = 'yellow'  ) ) +
  geom_line( data = data.frame( pred = linearpredicted_Values_b   ) , aes( x = c(1:350)  , y =  pred , col = 'red' )  ) +
  geom_line( data = data.frame( chopred =  predicted_Value_ )  , aes( x = c(1:350) , y = chopred , col = 'blue'  )  )+
  scale_color_identity(name='Legend',breaks=c('yellow','red','blue'),labels=c('Y','Linear_Model','Choquet values'),guide = 'legend')




