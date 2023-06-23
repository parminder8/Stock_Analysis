library(DBI)
library(corrplot)
library(caret)
library(gridExtra)
library(ggpubr)

library(doMC)

registerDoMC(cores=4)
getwd()
setwd("C:/Users/parmi/OneDrive/Documents/Major_v01")
getwd()
dataSet <- read.csv("abc1.csv",header = TRUE,sep = ',')
colnames(dataSet)

head(dataSet,10)
dim(dataSet)

table(unlist(lapply(dataSet,class)))
plot.ts(dataSet[,c(2)])
plot.ts(dataSet["FTSE"])


dataSet <- as.numeric(dataSet[,c("FTSE")])
names(dataSet) <- 1:length(dataSet)
df <- cbind(read.table(text = names(dataSet)),dataSet)
x = df$V1; y=df$dataSet

Model = lm(y~x)
summary(Model)

r2 <- cor(fitted(Model),y)^2
summary(Model)$r.squared
print(r2)


paste('y =', coef(Model)[[2]],'* x','+',coef(Model)[[1]])

tendency = coef(Model)[[2]] * x + coef(Model)[[1]]; print(tendency)

steps=25
x_in_fut <- (length(x)+1):(length(x)+steps)

forecastedVal = coef(Model)[[2]] * x_in_fut + coef(Model)[[1]]; print(forecastedVal)


res <- stack(data.frame(Observed = c(y,forecastedVal),
                        Predicted = c(tendency,forecastedVal)))
res <- cbind(res,x=rep(c(x,x_in_fut),2))
require("lattice")
g1 <- xyplot(values ~x,data=res,grou = ind,auto.key = TRUE,grid = TRUE,
             type=c("p","l"))

library(gridExtra)
grid.arrange(g1,nrow = 1)

