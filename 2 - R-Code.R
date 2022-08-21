library(factoextra)
library(ggplot2)
library(RColorBrewer)

#Reading Base Data File 
data <- read.csv('F:\\ISB\\2 - Term 2 - (18-22 June) - H\\ML - Unsupervised 1\\MLUL - Assignment 2 - 17 July\\Test Wine.csv')

#Exploring the dataset 
dim(data)
head(data)
str(data)
summary(data)

#Changing Group Cultivars to Factors (instead of integers)
data$Wine.Cultivars <- as.factor(data$Wine.Cultivars)
table(data$Wine.Cultivars)

#Creating data without wine cultivars for performing PCA
df <- data[,-1]
head(df)

#Scaling the dataframe for performing PCA
scale(df)

#Performing PCA on standardised wine data
df <- princomp(df, cor=TRUE, scores=TRUE, covmat=NULL)
head(df)
str(df)

#Eigenvectors 
df$loadings 

#For understanding % Variation covered 
summary(df)


#Screeplot
plot(df, type='l')
fviz_eig(df)
biplot(df, scale=0, col=c('light grey','red'),xlab='First Component',
       ylab='Second Component')


#Combining PCs to original data
head(PC_data)
PC_data <- as.data.frame(df$scores)
PC_data <- cbind(data,PC_data)
write.csv(PC_data,file='F:\\ISB\\2 - Term 2 - (18-22 June) - H\\ML - Unsupervised 1\\MLUL - Assignment 2 - 17 July\\Wine PCA Data.csv')

head(PC_data)

ggplot(PC_data, aes(Comp.1,Comp.2, col=Wine.Cultivars, fill=Wine.Cultivars)) +
  geom_point(shape=21, col='black') + 
  stat_ellipse(geom='polygon', col='black', alpha=0.5)

summary(df, loading=TRUE)

#Correlation
hm <- cor(data[,-1],PC_data[,15:20])
hm
?heatmap
heatmap (hm, Rowv=NA, Colv=NA, col= colorRampPalette(brewer.pal(8, "Blues"))(256))


#------------------------------------------------------------------------------------------------

#Clustering for all 13 chemicals

chem_data <- data[,-1]
str(chem_data)

chem_data <- scale(chem_data)
head(chem_data)

#Plotting Dendrogram
d <- dist(chem_data, method = "euclidean")
fit <- hclust(d, method = "ward.D2")
plot(fit, main = "Hierarchial Clustering Dendrogram for ALL Chemicals", labels = FALSE, hang=-1)

#Visually inspect the Dendrogram and decide optimal clusters based on vertical distance 
rect.hclust(fit, k=3, border="red")

#Visualizing the Clusters
chem_groups <- cutree(fit,k=3) 
fviz_cluster(list(data=chem_data, cluster = chem_groups), labelsize = 1, ellipse.alpha=0)

chem_mem<-as.matrix(chem_groups)
table(chem_mem)

#--------------------------------------------------------------------------------------

#Clustering for PCAs

str(PC_data)

pca_data <- PC_data[,15:16]
pca_data <- scale(pca_data)
head(pca_data)

#Plotting Dendrogram
d <- dist(pca_data, method = "euclidean")
fit <- hclust(d, method = "ward.D2")
plot(fit, main = "Hierarchial Clustering Dendrogram for 2 PCs", labels = FALSE, hang=-1)

#Visually inspect the Dendrogram and decide optimal clusters based on vertical distance 
rect.hclust(fit, k=3, border="red")

#Visualizing the Clusters
pc_groups <- cutree(fit,k=3) 
fviz_cluster(list(data=pca_data, cluster = pc_groups), labelsize = 1, ellipse.alpha=0)

pc_mem<-as.matrix(pc_groups)
table(pc_mem)



colnames(chem_mem)[1] <- "Chem Membership"
colnames(pc_mem)[1] <- "PC Membership"
final <- cbind(PC_data,chem_mem,pc_mem)

str(final)

write.csv(final,file='F:\\ISB\\2 - Term 2 - (18-22 June) - H\\ML - Unsupervised 1\\MLUL - Assignment 2 - 17 July\\Wine PCA Data.csv')



#-----------------------------------------
