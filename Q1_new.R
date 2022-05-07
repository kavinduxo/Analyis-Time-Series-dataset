#packages to install
install.packages("factoextra")
install.packages("caret")
install.packages("tidyverse")
install.packages("ggcorrplot")
install.packages("plotmo")
install.packages("keras")
install.packages("kableExtra")
install.packages("modelr")
install.packages("psych")
install.packages("Rmisc")
install.packages("yardstick")
install.packages("NbClust")


#libraries using
library(caret)
library(tidyverse)
library(leaps)
library(ggplot2)
library(reshape2)
library(MASS)
library(ggcorrplot)
library(corrplot)
library(plotmo)
library(keras)
library(kableExtra)
library(modelr)
library(psych)
library(Rmisc)
library(gridExtra)
library(scales)
library(rpart)
library(yardstick)
library(cluster)
library(NbClust)
library(factoextra)
library(readxl)

#loading xlsx data file
wineDataset <- read_excel("S:\\UoW\\YR3\\ML\\cw\\Whitewine_v2.xlsx")

#remove white spaces from column names
names(wineDataset) <- gsub(" ", "_", names(wineDataset))

# get column name
colnames(wineDataset)

#demo dataset
wineDataset

#datatypes of the columns
sapply(wineDataset, class)

#summary of dataset
summary(wineDataset)

oldpar = par(mfrow = c(2,6))
for ( i in 1:11 ) {
  boxplot(wineDataset[[i]])
  mtext(names(wineDataset)[i], cex = 1, side = 1, line = 2)
}
par(oldpar)

pairs(wineDataset[, -grep("quality", colnames(wineDataset))])

oldpar = par(mfrow = c(2,6))
for ( i in 1:12 ) {
  truehist(wineDataset[[i]], xlab = names(wineDataset)[i], col = 'lightgreen',
           main = paste("Average =", signif(mean(wineDataset[[i]]),3)))
}

par(oldpar)

outliers = c()
for ( i in 1:11 ) {
  stats = boxplot.stats(wineDataset[[i]])$stats
  bottom_outlier_rows = which(wineDataset[[i]] < stats[1])
  top_outlier_rows = which(wineDataset[[i]] > stats[5])
  outliers = c(outliers , top_outlier_rows[ !top_outlier_rows %in% outliers ] )
  outliers = c(outliers , bottom_outlier_rows[ !bottom_outlier_rows %in% outliers ] )
}

mod = lm(quality ~ ., data = wineDataset)
cooksd = cooks.distance(mod)
plot(cooksd, pch = "*", cex = 2, main = "Influential Obs by Cooks distance")
abline(h = 4*mean(cooksd, na.rm = T), col = "red")

coutliers = as.numeric(rownames(wineDataset[cooksd > 4 * mean(cooksd, na.rm=T), ]))
outliers = c(outliers , coutliers[ !coutliers %in% outliers ] )

cleanWhiteDat = wineDataset[-outliers, ]
oldpar = par(mfrow=c(2,10))
for ( i in 1:12 ) {
  truehist(cleanWhiteDat[[i]], xlab = names(cleanWhiteDat)[i], col = 'lightgreen', 
           main = paste("Average =", signif(mean(cleanWhiteDat[[i]]),3)))
}

boxplot(cleanWhiteDat)$out
for ( i in 1:11 ) {
  boxplot(cleanWhiteDat[[i]])
  mtext(names(cleanWhiteDat)[i], cex = 1, side = 1, line = 2)
}

