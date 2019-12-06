Class 8 machine learning
================
Hongji Jiang
10/24/2019

K-means Clustering
------------------

``` r
 # Generate some example data for clustering
tmp <- c(rnorm(30,-3), rnorm(30,3))
x <- cbind(x=tmp, y=rev(tmp))
plot(x)
```

![](class8_files/figure-markdown_github/unnamed-chunk-1-1.png)

Q. How many points are in each cluster? Q. What ‘component’ of your result object details - cluster size? - cluster assignment/membership? - cluster center?

``` r
k <- kmeans(x, centers=2, nstart=20)
k$size
```

    ## [1] 30 30

``` r
k$cluster
```

    ##  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2
    ## [39] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2

``` r
k$centers
```

    ##           x         y
    ## 1 -2.796261  3.204081
    ## 2  3.204081 -2.796261

Plot x colored by the kmeans cluster assignment and add cluster centers as blue points

``` r
plot(x, col = k$cluster)
points(k$centers,col = 12,pch = 15)
```

![](class8_files/figure-markdown_github/unnamed-chunk-3-1.png)

Hierarchical clustering in R
----------------------------

The 'hclust()' function requires a distance matrix as input You can get this from 'dist()' function

``` r
# First we need to calculate point (dis)similarity
#   as the Euclidean distance between observations
dist_matrix <- dist(x)
# The hclust() function returns a hierarchical
#  clustering model
hc <- hclust(d = dist_matrix)
# the print method is not so useful here
hc
```

    ## 
    ## Call:
    ## hclust(d = dist_matrix)
    ## 
    ## Cluster method   : complete 
    ## Distance         : euclidean 
    ## Number of objects: 60

``` r
plot(hc)
abline(h=6,col='red')
```

![](class8_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
cutree(hc,h=6)
```

    ##  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 3 4 4 4 4 4 4 4
    ## [39] 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4

``` r
 # Step 1. Generate some example data for clustering
x <- rbind(
  matrix(rnorm(100, mean=0, sd = 0.3), ncol = 2),   # c1
  matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2), # c2
  matrix(c(rnorm(50, mean = 1, sd = 0.3),           # c3
           rnorm(50, mean = 0, sd = 0.3)), ncol = 2))
colnames(x) <- c("x", "y")
# Step 2. Plot the data without clustering
plot(x)
```

![](class8_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
# Step 3. Generate colors for known clusters
#         (just so we can compare to hclust results)
col <- as.factor( rep(c("c1","c2","c3"), each=50) )
plot(x, col=col)
```

![](class8_files/figure-markdown_github/unnamed-chunk-6-2.png)

``` r
kmeans(x,centers=3,nstart =20)
```

    ## K-means clustering with 3 clusters of sizes 47, 50, 53
    ## 
    ## Cluster means:
    ##            x           y
    ## 1 0.95271660  1.00879674
    ## 2 1.03712771 -0.01828684
    ## 3 0.09789529  0.06285915
    ## 
    ## Clustering vector:
    ##   [1] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2
    ##  [38] 3 3 3 3 3 3 3 3 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 2 1 1
    ##  [75] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 2 1 1 1 2 2 2 2 2 2 2 2 2 2 2
    ## [112] 3 1 2 2 2 3 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 3 2 2 2 2 3 2 2 2 2 2 2
    ## [149] 2 2
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 7.416274 7.036417 7.353961
    ##  (between_SS / total_SS =  73.1 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"

``` r
#clustering
hc <- hclust(dist(x))

#draw tree
plot(hc)
abline(h=2,col='red')
```

![](class8_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
#cut the tree into clusters/grps
grps <- cutree(hc,h=2)
grps
```

    ##   [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 2 1 1 1 1 1 1 1 1 1 1 2
    ##  [38] 1 1 2 1 1 1 1 1 1 1 1 1 1 2 2 3 3 3 3 2 2 3 3 3 3 2 3 2 3 3 1 2 3 3 2 2 2
    ##  [75] 2 3 2 3 3 2 3 3 3 3 3 2 3 3 3 2 3 3 3 3 3 3 2 3 3 3 2 2 2 2 2 2 2 2 2 2 2
    ## [112] 1 1 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2
    ## [149] 2 2

Plot the data colored by ther hcluster result with

``` r
plot(x,col = grps)
```

![](class8_files/figure-markdown_github/unnamed-chunk-9-1.png)

How many

``` r
table(grps)
```

    ## grps
    ##  1  2  3 
    ## 51 66 33

Cross tablulate ie compare the clustering result with known answers

``` r
table(grps,col)
```

    ##     col
    ## grps c1 c2 c3
    ##    1 46  1  4
    ##    2  4 16 46
    ##    3  0 33  0

``` r
mydata <- read.csv("https://tinyurl.com/expression-CSV",
                        row.names=1)
head(mydata)
```

    ##        wt1 wt2  wt3  wt4 wt5 ko1 ko2 ko3 ko4 ko5
    ## gene1  439 458  408  429 420  90  88  86  90  93
    ## gene2  219 200  204  210 187 427 423 434 433 426
    ## gene3 1006 989 1030 1017 973 252 237 238 226 210
    ## gene4  783 792  829  856 760 849 856 835 885 894
    ## gene5  181 249  204  244 225 277 305 272 270 279
    ## gene6  460 502  491  491 493 612 594 577 618 638

``` r
dim(mydata)
```

    ## [1] 100  10

Principal component anylysis
============================

``` r
## lets do PCA
pca <- prcomp(t(mydata), scale=TRUE)

attributes(pca)
```

    ## $names
    ## [1] "sdev"     "rotation" "center"   "scale"    "x"       
    ## 
    ## $class
    ## [1] "prcomp"

``` r
plot(pca$x[,1],pca$x[,2])
```

![](class8_files/figure-markdown_github/unnamed-chunk-14-1.png)

``` r
pca.var <- pca$sdev^2
#1 is one significant figure
pca.var.per <- round(pca.var/sum(pca.var)*100,1)
pca.var.per
```

    ##  [1] 92.6  2.3  1.1  1.1  0.8  0.7  0.6  0.4  0.4  0.0

``` r
barplot(pca.var.per, main="Scree Plot",
        xlab="Principal Component", ylab="Percent Variation")
```

![](class8_files/figure-markdown_github/unnamed-chunk-15-1.png)

``` r
plot(pca$x[,1], pca$x[,2], 
     col= c('red','red','red','red','red','blue','blue','blue','blue','blue'))
```

![](class8_files/figure-markdown_github/unnamed-chunk-16-1.png)

``` r
x <- read.csv("UK_foods.csv")

dim(x)
```

    ## [1] 17  5

``` r
head(x)
```

    ##                X England Wales Scotland N.Ireland
    ## 1         Cheese     105   103      103        66
    ## 2  Carcass_meat      245   227      242       267
    ## 3    Other_meat      685   803      750       586
    ## 4           Fish     147   160      122        93
    ## 5 Fats_and_oils      193   235      184       209
    ## 6         Sugars     156   175      147       139

``` r
rownames(x) <- x[,1]
x <- x[,-1]
head(x)
```

    ##                England Wales Scotland N.Ireland
    ## Cheese             105   103      103        66
    ## Carcass_meat       245   227      242       267
    ## Other_meat         685   803      750       586
    ## Fish               147   160      122        93
    ## Fats_and_oils      193   235      184       209
    ## Sugars             156   175      147       139

``` r
dim(x)
```

    ## [1] 17  4

``` r
barplot(as.matrix(x), beside=T, col=rainbow(nrow(x)))
```

![](class8_files/figure-markdown_github/unnamed-chunk-18-1.png)

``` r
barplot(as.matrix(x),  col=rainbow(nrow(x)))
```

![](class8_files/figure-markdown_github/unnamed-chunk-19-1.png)

``` r
pairs(x, col=rainbow(10), pch=1)
```

![](class8_files/figure-markdown_github/unnamed-chunk-20-1.png)

``` r
pca <- prcomp(t(x))
summary(pca)
```

    ## Importance of components:
    ##                             PC1      PC2      PC3       PC4
    ## Standard deviation     324.1502 212.7478 73.87622 4.189e-14
    ## Proportion of Variance   0.6744   0.2905  0.03503 0.000e+00
    ## Cumulative Proportion    0.6744   0.9650  1.00000 1.000e+00

``` r
plot(pca$x[,1],pca$x[,2])
```

![](class8_files/figure-markdown_github/unnamed-chunk-22-1.png)

``` r
pca$x
```

    ##                  PC1         PC2         PC3           PC4
    ## England   -144.99315    2.532999 -105.768945  2.842865e-14
    ## Wales     -240.52915  224.646925   56.475555  7.804382e-13
    ## Scotland   -91.86934 -286.081786   44.415495 -9.614462e-13
    ## N.Ireland  477.39164   58.901862    4.877895  1.448078e-13
