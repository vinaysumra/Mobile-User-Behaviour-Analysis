
user_data <- read.csv(file.choose())

################    K-NN Algorithm     ############################################# 

str(user_data)
View(user_data)

user_data <- user_data[, -1]  

user_data$Device.Model <- as.numeric(as.factor(user_data$Device.Model))
user_data$Operating.System <- as.numeric(as.factor(user_data$Operating.System))
user_data$Gender <- as.numeric(as.factor(user_data$Gender))

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

user_data_norm <- as.data.frame(lapply(user_data[, -ncol(user_data)], normalize))

user_data_norm$User.Behavior.Class <- user_data$User.Behavior.Class


set.seed(42)
train_indices <- sample(1:nrow(user_data_norm), 0.7 * nrow(user_data_norm))
train_data <- user_data_norm[train_indices, ]
test_data <- user_data_norm[-train_indices, ]
train_labels <- train_data$User.Behavior.Class
test_labels <- test_data$User.Behavior.Class


train_data <- train_data[, -ncol(train_data)]
test_data <- test_data[, -ncol(test_data)]


install.packages("class") 
library(class)
test_pred <- knn(train = train_data, test = test_data, cl = train_labels, k = 27)

#Print the matrix
install.packages("gmodels") 
library(gmodels)
CrossTable(x = test_labels, y = test_pred, prop.chisq = FALSE)

################################## k- mean #########################################

set.seed(6)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(user_data, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab ='Number of cluster',
     ylab = 'wcss')

#Fitting K-mean to the dataset
set.seed(29)
kmeans= kmeans(x = user_data, centers = 6,
               iter.max=500)
y_kmeans = kmeans$cluster
y_kmeans

x = kmeans$cluster
x

getwd()
write.csv(x,"cluster.csv")

install.packages("cluster")
library(cluster)
clusplot(user_data,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('clusters of User behavior'),
         xlab = 'App Usage Time (min/day)',
         ylab = 'Data Usage (MB/day)')

###################  hierarchical clustering ##########################

colnames(user_data) <- make.names(colnames(user_data))
user_data <- user_data[c(4,8)]

dendrogram <- hclust(d = dist(user_data, method = 'euclidean'), method = 'ward.D')
plot(dendrogram, main = 'Dendrogram', xlab = 'Users', ylab = 'Euclidean distances')

hc <- hclust(d = dist(user_data, method = 'euclidean'), method = 'ward.D')
y_hc <- cutree(hc, k = )  

library(cluster)
clusplot(user_data,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = 'Clusters of user behavior',
         xlab = 'App Usage Time (min/day)',
         ylab = 'Data Usage (MB/day)')


