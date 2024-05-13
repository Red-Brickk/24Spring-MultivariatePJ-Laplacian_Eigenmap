install.packages("Rdimtools")

# 3D scatter plot See 3D scatter plot of the original iris data(take the first 3 variables.)


library(Rdimtools)
library(plotly)
## use iris data
data(iris)
summary(iris)
pairs(~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width+Species, data = iris, main = "基本數據探索")
set.seed(100)
subid = sample(1:150,150)
X     = as.matrix(iris[subid,1:4])
lab   = as.factor(iris[subid,5])
df_3d <- as.data.frame(X)
df_3d$lab <- lab
fig_3d <- plot_ly(df_3d, x=~Sepal.Length, y=~Sepal.Width,z=~Petal.Length,color=~lab,size = c(0.8),colors = c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951'))
fig_3d <- fig_3d %>% add_markers()
fig_3d <- fig_3d %>% layout(title = '3D scatter plot of original iris dataset')
fig_3d


# paired 2D scatter plots See 2D scatter plots for all 6 pairs of variables.


varnames <- colnames(df_3d)
fig_list <- list()
count <- 1
for(i in 1:3){
  for (j in (i+1):4){
    fig_2d <- plot_ly(df_3d, x=df_3d[,i], y=df_3d[,j],color=~lab,size = c(0.8),colors = c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951'))
    fig_2d <- fig_2d %>% add_markers()
    fig_2d <- fig_2d %>% layout(title = 'scatter plot of original iris dataset.') 
    fig_list[[count]] <- fig_2d
    count <- count+1
    rm(fig_2d)
  }
}
# Combine all figures into one subplot
combined_fig <- subplot(style(fig_list[[1]], showlegend=FALSE),
                        style(fig_list[[2]], showlegend=FALSE),
                        style(fig_list[[3]], showlegend=FALSE),
                        style(fig_list[[4]], showlegend=FALSE),
                        style(fig_list[[5]], showlegend=FALSE),
                        style(fig_list[[6]], showlegend=FALSE),
                        nrows = 3, margin=0.05)
# Display the combined figure
combined_fig # NOTE! x and y axes need to be added to add readability
# 為每張子圖添加軸標題
fig_list[[1]] <- style(fig_list[[1]], showlegend=T) %>% layout(
  xaxis = list(title = "Sepal.Length"),
  yaxis = list(title = 'Sepal.Width')
)

fig_list[[2]] <- style(fig_list[[2]], showlegend=T) %>% layout(
  xaxis = list(title = 'Sepal.Length'),
  yaxis = list(title = 'Petal.Length')
)

fig_list[[3]] <- style(fig_list[[3]], showlegend=T) %>% layout(
  xaxis = list(title = 'Sepal.Length'),
  yaxis = list(title = 'Petal.Width')
)

fig_list[[4]] <- style(fig_list[[4]], showlegend=T) %>% layout(
  xaxis = list(title = 'Sepal.Width'),
  yaxis = list(title = 'Petal.Length')
)

fig_list[[5]] <- style(fig_list[[5]], showlegend=T) %>% layout(
  xaxis = list(title = 'Sepal.Width'),
  yaxis = list(title = 'Petal.Width')
)

fig_list[[6]] <- style(fig_list[[6]], showlegend=T) %>% layout(
  xaxis = list(title = 'Petal.Length'),
  yaxis = list(title = 'Petal.Width')
)
fig_list[[1]]
fig_list[[2]]
fig_list[[3]]
fig_list[[4]]
fig_list[[5]]
fig_list[[6]]






# Laplacian Eigen map
#Use "proportion" to construct neighborhood map


## try different levels of connectivity
#proportion=1
for (i in 1){
  le1 <- do.lapeig(X, type=c("proportion",i), weighted=FALSE)
  df_le1 <- as.data.frame(le1$Y)
  colnames(df_le1) <- c("dim1","dim2")
  df_le1$lab <- lab
  fig_le1 <- plot_ly(df_le1, x=~dim1, y=~dim2,color=~lab,size = c(0.8),colors = c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951'))
  fig_le1 <- fig_le1 %>% add_markers()
  fig_le1 <- fig_le1 %>% layout(title = 'Embedded dimension 1 and 2,proportion=1', plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Embedded dim 1'), 
                                yaxis = list(title = 'Embedded dim 2'), legend = list(title=list(text='<b> Species of Iris </b>')))
  print(fig_le1)  
}
#proportion=0.05
for (i in 0.05){
  le1 <- do.lapeig(X, type=c("proportion",i), weighted=FALSE)
  df_le1 <- as.data.frame(le1$Y)
  colnames(df_le1) <- c("dim1","dim2")
  df_le1$lab <- lab
  fig_le1 <- plot_ly(df_le1, x=~dim1, y=~dim2,color=~lab,size = c(0.8),colors = c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951'))
  fig_le1 <- fig_le1 %>% add_markers()
  fig_le1 <- fig_le1 %>% layout(title = 'Embedded dimension 1 and 2,proportion=0.05', plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Embedded dim 1'), 
                                yaxis = list(title = 'Embedded dim 2'), legend = list(title=list(text='<b> Species of Iris </b>')))
  print(fig_le1)  
}
#proportion=0.1
for (i in 0.1){
  le1 <- do.lapeig(X, type=c("proportion",i), weighted=FALSE)
  df_le1 <- as.data.frame(le1$Y)
  colnames(df_le1) <- c("dim1","dim2")
  df_le1$lab <- lab
  fig_le1 <- plot_ly(df_le1, x=~dim1, y=~dim2,color=~lab,size = c(0.8),colors = c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951'))
  fig_le1 <- fig_le1 %>% add_markers()
  fig_le1 <- fig_le1 %>% layout(title = 'Embedded dimension 1 and 2,proportion=0.1', plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Embedded dim 1'), 
                                yaxis = list(title = 'Embedded dim 2'), legend = list(title=list(text='<b> Species of Iris </b>')))
  print(fig_le1)  
}
#proportion=0.3
for (i in 0.3){
  le1 <- do.lapeig(X, type=c("proportion",i), weighted=FALSE)
  df_le1 <- as.data.frame(le1$Y)
  colnames(df_le1) <- c("dim1","dim2")
  df_le1$lab <- lab
  fig_le1 <- plot_ly(df_le1, x=~dim1, y=~dim2,color=~lab,size = c(0.8),colors = c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951'))
  fig_le1 <- fig_le1 %>% add_markers()
  fig_le1 <- fig_le1 %>% layout(title = 'Embedded dimension 1 and 2,proportion=0.3', plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Embedded dim 1'), 
                                yaxis = list(title = 'Embedded dim 2'), legend = list(title=list(text='<b> Species of Iris </b>')))
  print(fig_le1)  
}

#proportion=0.5
for (i in 0.5){
  le1 <- do.lapeig(X, type=c("proportion",i), weighted=FALSE)
  df_le1 <- as.data.frame(le1$Y)
  colnames(df_le1) <- c("dim1","dim2")
  df_le1$lab <- lab
  fig_le1 <- plot_ly(df_le1, x=~dim1, y=~dim2,color=~lab,size = c(0.8),colors = c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951'))
  fig_le1 <- fig_le1 %>% add_markers()
  fig_le1 <- fig_le1 %>% layout(title = 'Embedded dimension 1 and 2,proportion=0.5', plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Embedded dim 1'), 
                                yaxis = list(title = 'Embedded dim 2'), legend = list(title=list(text='<b> Species of Iris </b>')))
  print(fig_le1)  
}
#proportion=0.7
for (i in 0.7){
  le1 <- do.lapeig(X, type=c("proportion",i), weighted=FALSE)
  df_le1 <- as.data.frame(le1$Y)
  colnames(df_le1) <- c("dim1","dim2")
  df_le1$lab <- lab
  fig_le1 <- plot_ly(df_le1, x=~dim1, y=~dim2,color=~lab,size = c(0.8),colors = c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951'))
  fig_le1 <- fig_le1 %>% add_markers()
  fig_le1 <- fig_le1 %>% layout(title = 'Embedded dimension 1 and 2,proportion=0.7', plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Embedded dim 1'), 
                                yaxis = list(title = 'Embedded dim 2'), legend = list(title=list(text='<b> Species of Iris </b>')))
  print(fig_le1)  
}


#Use "knn(k nearest neighbor)" to construct neighborhood map.

## try different levels of connectivity
#knn=10
for (k in seq(10,10,5)){
  le1 <- do.lapeig(X, type=c("knn",k), weighted=FALSE)
  df_le1 <- as.data.frame(le1$Y)
  colnames(df_le1) <- c("dim1","dim2")
  df_le1$lab <- lab
  fig_le1 <- plot_ly(df_le1, x=~dim1, y=~dim2,color=~lab,size = c(0.8),colors = c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951'))
  fig_le1 <- fig_le1 %>% add_markers()
  fig_le1 <- fig_le1 %>% layout(title = paste('Embedded dimension 1 and 2, k=',k), plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Embedded dim 1'), 
                                yaxis = list(title = 'Embedded dim 2'), legend = list(title=list(text='<b> Species of Iris </b>')))
  print(fig_le1)  
}
#knn=30
for (k in seq(10,30,5)){
  le1 <- do.lapeig(X, type=c("knn",k), weighted=FALSE)
  df_le1 <- as.data.frame(le1$Y)
  colnames(df_le1) <- c("dim1","dim2")
  df_le1$lab <- lab
  fig_le1 <- plot_ly(df_le1, x=~dim1, y=~dim2,color=~lab,size = c(0.8),colors = c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951'))
  fig_le1 <- fig_le1 %>% add_markers()
  fig_le1 <- fig_le1 %>% layout(title = paste('Embedded dimension 1 and 2, k=',k), plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Embedded dim 1'), 
                                yaxis = list(title = 'Embedded dim 2'), legend = list(title=list(text='<b> Species of Iris </b>')))
  print(fig_le1)  
}
#knn=50
for (k in seq(10,50,5)){
  le1 <- do.lapeig(X, type=c("knn",k), weighted=FALSE)
  df_le1 <- as.data.frame(le1$Y)
  colnames(df_le1) <- c("dim1","dim2")
  df_le1$lab <- lab
  fig_le1 <- plot_ly(df_le1, x=~dim1, y=~dim2,color=~lab,size = c(0.8),colors = c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951'))
  fig_le1 <- fig_le1 %>% add_markers()
  fig_le1 <- fig_le1 %>% layout(title = paste('Embedded dimension 1 and 2, k=',k), plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Embedded dim 1'), 
                                yaxis = list(title = 'Embedded dim 2'), legend = list(title=list(text='<b> Species of Iris </b>')))
  print(fig_le1)  
}
#knn=75
for (k in seq(10,75,5)){
  le1 <- do.lapeig(X, type=c("knn",k), weighted=FALSE)
  df_le1 <- as.data.frame(le1$Y)
  colnames(df_le1) <- c("dim1","dim2")
  df_le1$lab <- lab
  fig_le1 <- plot_ly(df_le1, x=~dim1, y=~dim2,color=~lab,size = c(0.8),colors = c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951'))
  fig_le1 <- fig_le1 %>% add_markers()
  fig_le1 <- fig_le1 %>% layout(title = paste('Embedded dimension 1 and 2, k=',k), plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Embedded dim 1'), 
                                yaxis = list(title = 'Embedded dim 2'), legend = list(title=list(text='<b> Species of Iris </b>')))
  print(fig_le1)  
}
#knn=100
for (k in seq(10,100,10)){
  le1 <- do.lapeig(X, type=c("knn",k), weighted=FALSE)
  df_le1 <- as.data.frame(le1$Y)
  colnames(df_le1) <- c("dim1","dim2")
  df_le1$lab <- lab
  fig_le1 <- plot_ly(df_le1, x=~dim1, y=~dim2,color=~lab,size = c(0.8),colors = c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951'))
  fig_le1 <- fig_le1 %>% add_markers()
  fig_le1 <- fig_le1 %>% layout(title = paste('Embedded dimension 1 and 2, k=',k), plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Embedded dim 1'), 
                                yaxis = list(title = 'Embedded dim 2'), legend = list(title=list(text='<b> Species of Iris </b>')))
  print(fig_le1)  
}
#knn=150
for (k in seq(10,150,10)){
  le1 <- do.lapeig(X, type=c("knn",k), weighted=FALSE)
  df_le1 <- as.data.frame(le1$Y)
  colnames(df_le1) <- c("dim1","dim2")
  df_le1$lab <- lab
  fig_le1 <- plot_ly(df_le1, x=~dim1, y=~dim2,color=~lab,size = c(0.8),colors = c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951'))
  fig_le1 <- fig_le1 %>% add_markers()
  fig_le1 <- fig_le1 %>% layout(title = paste('Embedded dimension 1 and 2, k=',k), plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Embedded dim 1'), 
                                yaxis = list(title = 'Embedded dim 2'), legend = list(title=list(text='<b> Species of Iris </b>')))
  print(fig_le1)  
}

#Use "enn(epsilon nearest neighbor)" to construct neighborhood map.

## try different levels of connectivity
#e=1
for (e in seq(1,1,1)){
  le1 <- do.lapeig(X, type=c("enn",e), weighted=FALSE)
  df_le1 <- as.data.frame(le1$Y)
  colnames(df_le1) <- c("dim1","dim2")
  df_le1$lab <- lab
  fig_le1 <- plot_ly(df_le1, x=~dim1, y=~dim2,color=~lab,size = c(0.8),colors = c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951'))
  fig_le1 <- fig_le1 %>% add_markers()
  fig_le1 <- fig_le1 %>% layout(title = paste('Embedded dimension 1 and 2, enn, e=',e), plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Embedded dim 1'), 
                                yaxis = list(title = 'Embedded dim 2'), legend = list(title=list(text='<b> Species of Iris </b>')))
  print(fig_le1)  
}
#e=3
for (e in seq(1,3,1)){
  le1 <- do.lapeig(X, type=c("enn",e), weighted=FALSE)
  df_le1 <- as.data.frame(le1$Y)
  colnames(df_le1) <- c("dim1","dim2")
  df_le1$lab <- lab
  fig_le1 <- plot_ly(df_le1, x=~dim1, y=~dim2,color=~lab,size = c(0.8),colors = c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951'))
  fig_le1 <- fig_le1 %>% add_markers()
  fig_le1 <- fig_le1 %>% layout(title = paste('Embedded dimension 1 and 2, enn, e=',e), plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Embedded dim 1'), 
                                yaxis = list(title = 'Embedded dim 2'), legend = list(title=list(text='<b> Species of Iris </b>')))
  print(fig_le1)  
}
#e=5
for (e in seq(1,5,1)){
  le1 <- do.lapeig(X, type=c("enn",e), weighted=FALSE)
  df_le1 <- as.data.frame(le1$Y)
  colnames(df_le1) <- c("dim1","dim2")
  df_le1$lab <- lab
  fig_le1 <- plot_ly(df_le1, x=~dim1, y=~dim2,color=~lab,size = c(0.8),colors = c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951'))
  fig_le1 <- fig_le1 %>% add_markers()
  fig_le1 <- fig_le1 %>% layout(title = paste('Embedded dimension 1 and 2, enn, e=',e), plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Embedded dim 1'), 
                                yaxis = list(title = 'Embedded dim 2'), legend = list(title=list(text='<b> Species of Iris </b>')))
  print(fig_le1)  
}
#e=7
for (e in seq(1,7,1)){
  le1 <- do.lapeig(X, type=c("enn",e), weighted=FALSE)
  df_le1 <- as.data.frame(le1$Y)
  colnames(df_le1) <- c("dim1","dim2")
  df_le1$lab <- lab
  fig_le1 <- plot_ly(df_le1, x=~dim1, y=~dim2,color=~lab,size = c(0.8),colors = c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951'))
  fig_le1 <- fig_le1 %>% add_markers()
  fig_le1 <- fig_le1 %>% layout(title = paste('Embedded dimension 1 and 2, enn, e=',e), plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Embedded dim 1'), 
                                yaxis = list(title = 'Embedded dim 2'), legend = list(title=list(text='<b> Species of Iris </b>')))
  print(fig_le1)  
}
#e=10
for (e in seq(1,10,1)){
  le1 <- do.lapeig(X, type=c("enn",e), weighted=FALSE)
  df_le1 <- as.data.frame(le1$Y)
  colnames(df_le1) <- c("dim1","dim2")
  df_le1$lab <- lab
  fig_le1 <- plot_ly(df_le1, x=~dim1, y=~dim2,color=~lab,size = c(0.8),colors = c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951'))
  fig_le1 <- fig_le1 %>% add_markers()
  fig_le1 <- fig_le1 %>% layout(title = paste('Embedded dimension 1 and 2, enn, e=',e), plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Embedded dim 1'), 
                                yaxis = list(title = 'Embedded dim 2'), legend = list(title=list(text='<b> Species of Iris </b>')))
  print(fig_le1)  
}
#e=15
for (e in seq(1,15,1)){
  le1 <- do.lapeig(X, type=c("enn",e), weighted=FALSE)
  df_le1 <- as.data.frame(le1$Y)
  colnames(df_le1) <- c("dim1","dim2")
  df_le1$lab <- lab
  fig_le1 <- plot_ly(df_le1, x=~dim1, y=~dim2,color=~lab,size = c(0.8),colors = c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951'))
  fig_le1 <- fig_le1 %>% add_markers()
  fig_le1 <- fig_le1 %>% layout(title = paste('Embedded dimension 1 and 2, enn, e=',e), plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Embedded dim 1'), 
                                yaxis = list(title = 'Embedded dim 2'), legend = list(title=list(text='<b> Species of Iris </b>')))
  print(fig_le1)  
}
# KNN with k=10 for the first three dimensions
le_k10 <- do.lapeig(X, type=c("knn",10), weighted=FALSE, ndim=3)
df_le_k10 <- as.data.frame(le_k10$Y)
df_le_k10$lab <- lab

# ENN with e=1 for the first three dimensions
le_e1 <- do.lapeig(X, type=c("enn",1), weighted=FALSE, ndim=3)
df_le_e1 <- as.data.frame(le_e1$Y)
df_le_e1$lab <- lab

# Plotting for KNN k=10 (dim2 vs dim3)
fig_k10_dim23 <- plot_ly(df_le_k10, x=~V2, y=~V3, color=~lab, 
                         colors=c('#4AC6B7', '#1972A4', '#965F8A'), 
                         marker=list(size=10)) %>%
  layout(title="KNN k=10: Dim2 vs Dim3", 
         xaxis=list(title="Dim 2"), 
         yaxis=list(title="Dim 3"))

# Plotting for ENN e=1 (dim2 vs dim3)
fig_e1_dim23 <- plot_ly(df_le_e1, x=~V2, y=~V3, color=~lab, 
                        colors=c('#4AC6B7', '#1972A4', '#965F8A'), 
                        marker=list(size=10)) %>%
  layout(title="ENN e=1: Dim2 vs Dim3", 
         xaxis=list(title="Dim 2"), 
         yaxis=list(title="Dim 3"))

# Exclude Setosa for focused analysis on versicolor and virginica
X_no_setosa <- X[lab != "setosa",]
lab_no_setosa <- lab[lab != "setosa"]

# Laplacian Eigenmap without setosa
le_no_setosa <- do.lapeig(X_no_setosa, type=c("knn",10), weighted=FALSE, ndim=2)
df_le_no_setosa <- as.data.frame(le_no_setosa$Y)
df_le_no_setosa$lab <- lab_no_setosa

# Plotting for knn=10 non-setosa species
fig_no_setosa <- plot_ly(df_le_no_setosa, x=~V1, y=~V2, color=~lab, 
                         colors=c('#1972A4', '#965F8A'), 
                         marker=list(size=10)) %>%
  layout(title="Non-Setosa Species: Laplacian Eigenmap knn=10", 
         xaxis=list(title="Dim 1"), 
         yaxis=list(title="Dim 2"))
# Plotting for enn=1 non-setosa species
le_no_setosa <- do.lapeig(X_no_setosa, type=c("enn",1), weighted=FALSE, ndim=2)
df_le_no_setosa <- as.data.frame(le_no_setosa$Y)
df_le_no_setosa$lab <- lab_no_setosa
fig_no_setosa <- plot_ly(df_le_no_setosa, x=~V1, y=~V2, color=~lab, 
                         colors=c('#1972A4', '#965F8A'), 
                         marker=list(size=10)) %>%
  layout(title="Non-Setosa Species: Laplacian Eigenmap enn=1", 
         xaxis=list(title="Dim 1"), 
         yaxis=list(title="Dim 2"))


# Print or view the plots
fig_k10_dim23
fig_e1_dim23
fig_no_setosa
#pca分析
# 載入需要的套件
library(ggplot2)

# 載入Iris數據集
data(iris)

# 進行PCA分析
iris.pca <- prcomp(iris[,1:4], center = TRUE, scale. = TRUE)
summary(iris.pca)

# 提取主成分分數
scores <- iris.pca$x

# 繪製PCA圖
ggplot(data = as.data.frame(scores), aes(x = PC1, y = PC2, color = iris$Species)) +
  geom_point(alpha = 0.8, size = 3) +
  ggtitle("PCA of the Iris dataset") +
  xlab("Principal Component 1") +
  ylab("Principal Component 2") +
  theme_minimal() +
  scale_color_manual(values =c('#4AC6B7', '#1972A4', '#965F8A'))
#MDS分析
# 載入需要的套件
library(stats)
library(ggplot2)

# 載入Iris數據集
data(iris)

# 進行MDS分析
dist_matrix <- dist(iris[, 1:4])  # 計算距離矩陣
mds_result <- cmdscale(dist_matrix, k = 2)  # 執行MDS


# 轉換為data.frame格式，準備繪圖
mds_df <- as.data.frame(mds_result)
mds_df$Species <- iris$Species

# 繪製MDS圖
ggplot(mds_df, aes(x = V1, y = V2, color = Species)) +
  geom_point(alpha = 0.8, size = 3) +
  ggtitle("MDS of the Iris dataset") +
  xlab("Dimension 1") +
  ylab("Dimension 2") +
  theme_minimal() +
  scale_color_manual(values = c('#4AC6B7', '#1972A4', '#965F8A'))
