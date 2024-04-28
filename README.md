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
8. "What's the intuition behind a Laplacian matrix? "Excellent answer by **[Muni Sreenivas Pydi](https://www.quora.com/profile/Muni-Sreenivas-Pydi)** · 

https://www.quora.com/Whats-the-intuition-behind-a-Laplacian-matrix-Im-not-so-much-interested-in-mathematical-details-or-technical-applications-Im-trying-to-grasp-what-a-laplacian-matrix-actually-represents-and-what-aspects-of-a-graph-it-makes-accessible/answer/Muni-Sreenivas-Pydi

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

6. Brown Corpus Cooccurrence embedding.

Source: https://songxia-sophia.medium.com/word-embedding-of-brown-corpus-using-python-ec09ff4cbf4f

Referring to Xia Song's elegant guide, and edit the code to meet our own need. 

Generated a 300×600 matrix, with row for the first 300 most commonly appeared words, and the 600 columns for left neighbor (window width = 2) cooccurrence and right neighbor (window width = 2) cooccurrence. 

```python
c_words_left = []
c_words_right = []
for v_word in V:
    left_2_words = []
    right_2_words = []
    positions = [x for x, n in enumerate(filter_words) if n == v_word] # locate each word of V in filter_words
    for i in positions:
        if i ==0:
            right_2_word = filter_words[1:3]
        elif i == 1:
            left_2_word = filter_words[0]
            right_2_word = filter_words[1:3]
        else:
            left_2_word = filter_words[(i-2):i]
            right_2_word = filter_words[(i+1):(i+3)]

        left_2_uniq = ls_uniq(left_2_word)
        left_2_words = left_2_words + left_2_uniq
        right_2_uniq = ls_uniq(right_2_word)
        right_2_words = right_2_words + right_2_uniq

    left_words_count = dict(collections.Counter(left_2_words))
    right_words_count = dict(collections.Counter(right_2_words))
    window_count = len(positions)
    for l_word in left_words_count:
        if l_word in C:
            lword_fre = left_words_count[l_word]
            Pr_l = lword_fre/window_count
            c_words_left.append((v_word, l_word, lword_fre, window_count, Pr_l))
    for r_word in right_words_count:
      if r_word in C:
          rword_fre = right_words_count[r_word]
          Pr_r = rword_fre/window_count
          c_words_right.append((v_word, r_word, rword_fre, window_count, Pr_r))
cwords_left = pd.DataFrame(c_words_left)
cwords_left.columns = ['V_Word','C_Word','Cword_Count(Left)','Window_Count','Pr_cw']
cwords_right = pd.DataFrame(c_words_right)
cwords_right.columns = ['V_Word','C_Word','Cword_Count(Right)','Window_Count','Pr_cw']
```

```python
# 使用 pivot_table 来生成矩阵
left_matrix = cwords_left.pivot_table(index='V_Word', columns='C_Word', values='Cword_Count(Left)', fill_value=0)
right_matrix = cwords_right.pivot_table(index='V_Word', columns='C_Word', values='Cword_Count(Right)', fill_value=0)
neighbor_embd = pd.concat([left_matrix, right_matrix],axis=1)
neighbor_embd.to_csv("corpus_neighboring_embedding_matrix.csv")
```

For further information, see

https://colab.research.google.com/drive/1l75_g6T4Z7j-otc4Scl2d8l9I7AT28L0#scrollTo=He21jnZniRNi



## Implementation

1. Made by hand?
2. Use R packages......

Ref: https://search.r-project.org/CRAN/refmans/Rdimtools/html/nonlinear_LAPEIG.html

0411update

曉汀：I have tried the Rdimtools package, do.lapeig() function. That's not bad! Some illustration can be found in "Useful Code" folder. 

## Useful Code

0410trylap.Rmd: implement do.lapeig function to iris and my_Swiss dataset, successfully embed the data into a lower dim space, preserving local neighborhood.



## Output

0421 更新了讀論文所學習到的一些數學知識，歡迎大家補充和指正！



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



04.14 second group meeting

朋友們晚上好！以下是本次討論的紀要：

1. coding小隊介紹了已經找到/構建出來的數據集：1. Iris；2.Swiss Roll，3. Toy（Horizontal and Vertical Bar）4. Corpus.
2. 介紹了計算L的兩種方法：do.lapeig函數和手嗑法（比較清楚地知道運行過程）
3. 數學小隊推薦了Quora上的介紹文章、以及來自SJSU的Slide（都已經傳到github上，大家可以查看）
4. 介紹了Laplace這一工具的物理背景：對離散數據先求梯度，再求散度；介紹了Heat Kernel的含義；介紹了Beltrami算子，但好像不是特別重要，可以暫時擱置；介紹了分群任務的數學評價標準，以及其與原距離保持任務的聯繫。

Note：

1. 考慮到現在暫時不了解報告順序，數學和coding兩個小隊可以先完成基礎任務，確保能較好地完成展示任務，再求report的進階
2. 程式碼中，請都使用do.lapeig函數計算L矩陣及其特征值特征向量，提高代碼的通用性
3. coding小隊前期在摸索，工作多有重疊，接下來可以有更細緻的分工。



0421 third group meeting

朋友們晚上好！以下是本次討論的紀要：
1. 數學小隊綜合簡報與論文內容，已經開始撰寫報告。
2. coding小隊介紹了本週工作，包括iris數據集，corpus數據集的分析。
3. 疑難問題：第一維為什麼只分出1-2個點？Epsilon較小時為何特徵分解會失敗？
可以考慮scale完之後再做enn，刪除若干離群點再做lapeig
4. 另外，可以考慮研究一下do.lapeig函數求得究竟是誰的特徵分解，是L還是D^{-1}L?
5. 確定使用Canva製作簡報，本週開始但暫時不規定進度，待下週會後再確定時間線。
祝大家期中考試順利~



0428 fourth meeting

朋友們晚上好！以下算是本次討論的紀要：

1. 數學小隊繼續寫Project部分內容，大體已完成，關於Laplacian Beltrami算子，計劃只做簡單介紹。

2. Coding小隊，Iris數據集部分，已經做了比較詳盡的分析；Corpus數據集部分，對各參數做了比較及評述，Swissroll數據集部分，對knn和enn方法做了比較，對k和t的取值進行了比較；相關內容放在github倉庫的output文件夾、useful code文件夾中，大家可參考取用。

3. 已經解決的問題：

   若出現非全連通圖，或全連通但子圖間連邊權重非常小的圖，0所對特征空間的維度會大於1，使得LE方法第一維退化或eigen decomposition失敗。

   在存在離群點、離群簇時，e較小的enn容易壞掉（非全連通），e較大的enn又失去局部性。knn則能保持子圖間仍有連通性。但是，若使用熱核且t值較小，仍有可能壞掉（非全連通）。由此，對此類數據集，還是應該考慮採用knn，不用熱核。

4. 解決不了的問題：do.lapeig到底在做誰的特征分解……

4. 新的想法

   LE方法與MDS方法重在“找到保持距離關係的低維表示”，PCA、Factor Analysis等方法重在"找到變異最大的變量組合/找到可解釋各變量共通變異數的隱含因子"，目的不同。LE方法所得的各維度，未必能做出易懂的解讀，除非流形的結構顯著且流形上各維度的變化有意義。

   目標函數可以看作是連通點之間較遠距離的懲罰，又可以認為是相連點間有著“引力”的作用。

5. 待解決的疑難問題

   是否能找到一量化指標，評價LE方法距離關係的保持效果？

   在使用LE方法得到原始數據的低維表示後，給到新的數據，如何再對其進行低維表示？

6. 時間線：

5月5號之前完成report部分，5號會後，分工，分別做Latex整合和PPT，爭取在12號之前完成PPT，若有機會試講為最佳。