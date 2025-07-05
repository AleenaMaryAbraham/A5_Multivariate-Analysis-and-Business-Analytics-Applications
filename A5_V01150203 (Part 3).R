# Function to auto-install and load packages
install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
}

# List of packages to install and load
packages <- c("cluster", "FactoMineR", "factoextra", "pheatmap")
install_and_load(packages)

#C) Do multidimensional scaling and interpret the results. 

icecream_df<-read.csv('C:/Users/Aleena Mary Abraham/OneDrive/Desktop/SCMA632_2025/DATA/icecream.csv',header=TRUE)
dim(icecream_df)

names(icecream_df) 

ice<-subset(icecream_df,select = -c(Brand)) 
distance_matrix<-dist(ice) 

mds_result<-cmdscale(distance_matrix,k=2) 

plot(mds_result[,1],mds_result[,2],pch=16,xlab="Dimension1",ylab="Dimension2",main="MDS plot") 
