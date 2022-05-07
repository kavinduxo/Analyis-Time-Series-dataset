# install the package
install.packages("ggstatsplot")
install.packages("readxl")
install.packages("factoextra")
install.packages("NbClust")

# Load the package
library(ggstatsplot)
library(readxl)
library(factoextra)
library(NbClust)

#
#-------------Question 01---------------
#  

# import Excel file into R
wineDataset <- read_excel("S:\\UoW\\YR3\\ML\\cw\\Whitewine_v2.xlsx")
names(wineDataset) <- gsub(" ", "_", names(wineDataset))

# get column name
colnames(wineDataset)

#demo dataset
wineDataset

#scaling the dataframe
df_scaled <- scale(wineDataset)

#output scale data
df_scaled



# Create a boxplot of the dataset, outliers are shown as two distinct points
boxplot(wineDataset)$out

# find outliers and store them
outliers1 <- boxplot(wineDataset$fixed_acidity, plot = FALSE)$out
outliers2 <- boxplot(wineDataset$volatile_acidity, plot = FALSE)$out
outliers3 <- boxplot(wineDataset$citric_acid, plot = FALSE)$out
outliers4 <- boxplot(wineDataset$residual_sugar, plot = FALSE)$out
outliers5 <- boxplot(wineDataset$chlorides, plot = FALSE)$out
outliers6 <- boxplot(wineDataset$free_sulfur_dioxide, plot = FALSE)$out
outliers7 <- boxplot(wineDataset$total_sulfur_dioxide, plot = FALSE)$out
outliers8 <- boxplot(wineDataset$density, plot = FALSE)$out
outliers9 <- boxplot(wineDataset$pH, plot = FALSE)$out
outliers10 <- boxplot(wineDataset$sulphates, plot = FALSE)$out
outliers11 <- boxplot(wineDataset$alcohol, plot = FALSE)$out

# restore dataset for new varibale
nill_outliers_winedataset <- wineDataset
nill_outliers_winedataset = nill_outliers_winedataset[,!(names(nill_outliers_winedataset) %in% c("quality"))]

while (length(outliers4) > 0 || length(outliers6) > 0 || length(outliers7) > 0 || length(outliers9) > 0) {

    # remove outliers from new duplicated dataset
    if (length(outliers1) > 0) {
        nill_outliers_winedataset <- nill_outliers_winedataset[-which(nill_outliers_winedataset$fixed_acidity %in% outliers1), ]
    }
    if (length(outliers2) > 0) {
        nill_outliers_winedataset <- nill_outliers_winedataset[-which(nill_outliers_winedataset$volatile_acidity %in% outliers2), ]
    }
    if (length(outliers3) > 0) {
        nill_outliers_winedataset <- nill_outliers_winedataset[-which(nill_outliers_winedataset$citric_acid %in% outliers3), ]
    }
    if (length(outliers4) > 0) {
        nill_outliers_winedataset <- nill_outliers_winedataset[-which(nill_outliers_winedataset$residual_sugar %in% outliers4), ]
    }
    if (length(outliers5) > 0) {
        nill_outliers_winedataset <- nill_outliers_winedataset[-which(nill_outliers_winedataset$chlorides %in% outliers5), ]
    }
    if (length(outliers6) > 0) {
        nill_outliers_winedataset <- nill_outliers_winedataset[-which(nill_outliers_winedataset$free_sulfur_dioxide %in% outliers6), ]
    }
    if (length(outliers7) > 0) {
        nill_outliers_winedataset <- nill_outliers_winedataset[-which(nill_outliers_winedataset$total_sulfur_dioxide %in% outliers7), ]
    }
    # if (length(outliers8) > 0) {
    #     nill_outliers_winedataset <- nill_outliers_winedataset[-which(nill_outliers_winedataset$density %in% outliers8), ]
    # }
    if (length(outliers9) > 0) {
        nill_outliers_winedataset <- nill_outliers_winedataset[-which(nill_outliers_winedataset$pH %in% outliers9), ]
    }
    if (length(outliers10) > 0) {
        nill_outliers_winedataset <- nill_outliers_winedataset[-which(nill_outliers_winedataset$sulphates %in% outliers10), ]
    }
    # if (length(outliers11) > 0) {
    #     nill_outliers_winedataset <- nill_outliers_winedataset[-which(nill_outliers_winedataset$alcohol %in% outliers11), ]
    # }

    outliers1 <- boxplot(nill_outliers_winedataset$fixed_acidity, plot = FALSE)$out
    outliers2 <- boxplot(nill_outliers_winedataset$volatile_acidity, plot = FALSE)$out
    outliers3 <- boxplot(nill_outliers_winedataset$citric_acid, plot = FALSE)$out
    outliers4 <- boxplot(nill_outliers_winedataset$residual_sugar, plot = FALSE)$out
    outliers5 <- boxplot(nill_outliers_winedataset$chlorides, plot = FALSE)$out
    outliers6 <- boxplot(nill_outliers_winedataset$free_sulfur_dioxide, plot = FALSE)$out
    outliers7 <- boxplot(nill_outliers_winedataset$total_sulfur_dioxide, plot = FALSE)$out
    outliers8 <- boxplot(nill_outliers_winedataset$density, plot = FALSE)$out
    outliers9 <- boxplot(nill_outliers_winedataset$pH, plot = FALSE)$out
    outliers10 <- boxplot(nill_outliers_winedataset$sulphates, plot = FALSE)$out
    outliers11 <- boxplot(nill_outliers_winedataset$alcohol, plot = FALSE)$out
}

boxplot(nill_outliers_winedataset)$out
  
oldpar1 = par(mfrow = c(2,6))
for ( i in 1:11 ) {
  boxplot(nill_outliers_winedataset[[i]])
  mtext(names(nill_outliers_winedataset)[i], cex = 1, side = 1, line = 2)
}
par(oldpar1)  

#
#-------------Question 02---------------
#  

#standarize new dataset with the help of scale func in R
new_df <- scale(nill_outliers_winedataset)
new_df


#NBclust method
res<-NbClust(nill_outliers_winedataset, diss=NULL, distance = "euclidean", min.nc=2, max.nc=6, 
             method = "kmeans", index = "all")
res$All.index

# Elbow method
fviz_nbclust(new_df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")