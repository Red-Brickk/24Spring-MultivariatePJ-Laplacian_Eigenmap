# 24Spring Multivariate Project: Laplacian_Eigenmap

Course project repository for STAT 519100:Applied Multivariate Analysis in NTHU.

We want to investigate the algorithm, learn its theoretical basis, and apply it to real world problems. 



## Time line

| Task           | From To     | People           | Feedback |
| -------------- | ----------- | ---------------- | -------- |
| Do Mathematics | 0405 - 0428 | 元鴻、暐晉       |          |
| Coding         | 0405 - 0428 | 旭晴、翌翔、曉汀 |          |
|                |             |                  |          |
|                |             |                  |          |
| Make Slide?    |             |                  |          |
| Write Report?  |             |                  |          |
| Presentation   |             |                  |          |



## Materials

1. On Laplacian Eigenmaps for Dimensionality Reduction - Juan Orduz

https://www.youtube.com/watch?v=U31TIICsHiA

A good lecture illustrating the algorithm. The lecturer's slide is also in the folder. 

2. Laplacian Eigenmaps,Course Slide from CMU School of Computer Science. Source:

https://www.cs.cmu.edu/~aarti/Class/10701/slides/Lecture21_1.pdf

3. Laplacian Eigenmaps, Course Slide from San Jose State University. Source:

https://www.sjsu.edu/faculty/guangliang.chen/Math253S20/lec12laplacian.pdf

4. Laplacian Eigenmaps for Dimensionality Reduction and Data Representation(2003)
5. Laplacian Eigenmaps and Spectral Techniques for Embedding and Clustering(2001)
6. Color image segmentation using Laplacian eigenmaps(2009)
7. Kernelized Supervised Laplacian Eigenmap for Visualization and Classification of Multi-Label Data(2021) (Supervised LE, and many fascinating visualizations)

## Dataset

1. Swiss roll data (by Han-Ming Wu).

```
Swiss roll data Description

Response: angle
Axis Variables: x, y, z	
Noise Variables: v1,...,v10 ~Normal (0,1)
n=500.

see http://isomap.stanford.edu/
```

source: http://www.hmwu.idv.tw/KSIR/RegData/SwissRoll/index.htm

2. Swiss roll data (self generated)

source: 

```{r}
# Generating Swiss Roll Data
n <- 2000
t <- runif(n,0,10)
z <- runif(n,0,10)
swiss <- data.frame(x=t*cos(t),y=t*sin(t),z=z, t = t)
save(swiss,file = "my_Swiss.Rda")
```

3. Toy binary images (horizontal and vertical bars)

source

```{r}
get_img <- function(left,up,bar_w,bar_l){
  if (left<1 | up<1 | left+bar_w>40 | up+bar_l>40){
    return(NULL)
  }
  img <- matrix(0,nrow=40,ncol=40)
  img[up:(up+bar_l),left:(left+bar_w)] = 1
  return(img)
}
bar_w <- 10
bar_l <- 2
n <- 100
horizontal_list <- list()
for(i in 1:n){
  left <- as.integer(((40-bar_w)*runif(1, 0, 1)+1))
  up <- as.integer((40-bar_l)*runif(1, 0, 1)+1)
  img <- get_img(left, up,bar_w, bar_l)
  img <- matrix(img, nrow = 1)  
  horizontal_list[[i]] <- img
}
save(horizontal_list, file="horizontal_bars.Rda")
```

4. MNIST written digits dataset

The format is:

```
label, pix-11, pix-12, pix-13, ...
```

And the script to generate the CSV file from the original dataset is included in this dataset.

source:

https://git-disl.github.io/GTDLBench/datasets/mnist_datasets/

5. CIFAR image classification dataset

The CIFAR-10 dataset consists of 60000 32x32 colour images in 10 classes, with 6000 images per class. There are 50000 training images and 10000 test images.

source:

[CIFAR-10 and CIFAR-100 datasets (toronto.edu)](http://www.cs.toronto.edu/~kriz/cifar.html)



## Output



## Log

04.01 create github repository, post some learning materials.

04.05 first group meeting.

朋友們晚上好！以下是本次討論的紀要：
1. 介紹了算法的基本特性、算法過程、應用場景、有待回答的理論問題
2. 提出建議，在實現算法時，可以嘗試多組不同參數，對比其保留局部鄰域信息的能力
3. 提出建議，在實現算法時，可以嘗試用不同的Kernel構造W矩陣
4. 在降維之後還要不要聚類 (Clustering)？（注意，Slides裡面的彩虹圖是既有label、Speech Recognition則未經聚類，作者只是手動並劃分區域。）可以在代碼實現之後再看。若希望集中注意力於本算法，則不聚類；若有餘力且希望展現應用價值，則聚類。
5. 今後每週若無特殊情況會進行Weekly Meeting，我會在每週日晚調查下一周大家有空的時間，感謝配合~
6. 請元鴻、暐晋兩位同學負責Mathematics的部分，關注兩篇Paper中的Justification，期待能在Presentation中向同學們介紹，在Report中闡釋；請旭晴、曉汀、翌翔三位同學負責Application部分，關注Paper中的Examples部分的Coding實現，若有可能，也可找其它數據集呈現，期待能在Presentation中向同學們展現算法的威力。進一步的分工請大家再商討。
6. 第一次Deadline定在4月28日，請大家關注！



## 