library(corrplot) #for visualisation of correlation
library(lattice) #for visualisation
library(ggplot2) #for visualisation
library(caTools) #for splittind data into testing and training data
library(dplyr) #manipulating dataframe
library(plotly) #converting ggplot to plotly
library(MASS) #Includes the Boston dataset

housing <- Boston

numberOfNA <- length(which(is.na(housing)==T))
if(numberOfNA>0) {
  housing <- housing[complete.cases(housing),]
}

set.seed(123)
split <- sample.split(housing,SplitRatio = 0.75) #assigns booleans to a new coloumn based on the split ratio
train <- subset(housing,split==TRUE)
test <- subset(housing,split==FALSE)

str(housing)

head(housing)

summary(housing)

par(mfrow = c(1, 4))
boxplot(housing$crim, main='crim',col='Sky Blue')
boxplot(housing$zn, main='zn',col='Sky Blue')
boxplot(housing$rm, main='rm',col='Sky Blue')
boxplot(housing$black, main='black',col='Sky Blue')

corrplot(cor(housing))

dropList <- c('chas','rad','crim','zn','black')
#We drop chas and rad because they are non numeric
#We drop crim, zn and black because they have lot of outliers
housingplot <- housing[,!colnames(housing) %in% dropList]
splom(housingplot,col = 'Sky Blue')

# Fitting Simple Linear regression
# . is used to fit predictor using all independent variables
lm.fit1 <- lm(medv~.,data=train)
summary(lm.fit1)

lm.fit2 <- lm(medv~.-age-indus+I(lstat^2),data=train)
summary(lm.fit2)

lm.fit3 <- lm(medv~.-indus-age-zn+rm*lstat-black+rm*rad+lstat*rad,data=train)


residuals <- data.frame('Residuals' = lm.fit3$residuals)
res_hist <- ggplot(residuals, aes(x=Residuals)) + geom_histogram(color='black', fill='skyblue') + ggtitle('Histogram of Residuals')
res_hist

plot(lm.fit3, col='Sky Blue')

test$predicted.medv <- predict(lm.fit3,test)
pl1 <-test %>% 
  ggplot(aes(medv,predicted.medv)) +
  geom_point(alpha=0.5) + 
  stat_smooth(aes(colour='black')) +
  xlab('Actual value of medv') +
  ylab('Predicted value of medv') +
  theme_bw()

ggplotly(pl1)
