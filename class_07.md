Untitled
================
Hongji Jiang
10/22/2019

R Markdown
----------

Source my functions from last day

``` r
source("http://tinyurl.com/rescale-R")
```

``` r
rescale(1:10)
```

    ##  [1] 0.0000000 0.1111111 0.2222222 0.3333333 0.4444444 0.5555556 0.6666667
    ##  [8] 0.7777778 0.8888889 1.0000000

``` r
rescale(c(1,10,5,NA,6))
```

    ## [1] 0.0000000 1.0000000 0.4444444        NA 0.5555556

Write a function to find where there are NA elements in two input vectors First make some simple input where I know the answer

``` r
x<-c(1,2,NA,3,NA)
y<-c(NA,3,NA,3,4)
```

Looked online and found the **is.na()** function

``` r
is.na(x)
```

    ## [1] FALSE FALSE  TRUE FALSE  TRUE

and the **which()** function tells us where the TRUE values are

``` r
which(is.na(x))
```

    ## [1] 3 5

``` r
is.na(x)
```

    ## [1] FALSE FALSE  TRUE FALSE  TRUE

``` r
is.na(y)
```

    ## [1]  TRUE FALSE  TRUE FALSE FALSE

The AND function requires two input TRUE to give a true output

``` r
is.na(x) & is.na(y)
```

    ## [1] FALSE FALSE  TRUE FALSE FALSE

Taking the **sum()** of TRUE FALSE vector will tell me how many TRUE elements I have

``` r
sum(is.na(x)&is.na(y))
```

    ## [1] 1

``` r
sum(TRUE,TRUE,FALSE)
```

    ## [1] 2

Now turn it into function

``` r
both_na <- function(x,y){
  sum(is.na(x)&is.na(y))
}
```

``` r
x <-  c(NA, NA, NA)
y1 <- c( 1, NA, NA)
y2 <- c( 1, NA, NA, NA,NA,NA)
both_na(x,y2)
```

    ## [1] 5

``` r
x2<-c(NA,NA)
both_na(x2,y2)
```

    ## [1] 5

``` r
plot(1:10,col=c('red','blue'))
```

![](class_07_files/figure-markdown_github/unnamed-chunk-14-1.png)

``` r
y3<-c(1,NA,NA,NA,NA,NA,NA)
length(x)
```

    ## [1] 3

``` r
length(y3)
```

    ## [1] 7

``` r
both_na2 <- function(x,y){
  if(length(x) != length(y)){
    stop("Input x and input y should have the same length")
  }
  sum(is.na(x)&is.na(y))
}
```

``` r
grade0 <- function(x){
  has_NA = FALSE
  for(i in x){
    if(is.na(i)){
      has_NA = TRUE
      x <- setdiff(x, i)
    }
  }
  if(has_NA){
    return(mean(x))
  }
  else
    return(mean(x[-which.min(x)]))
}
```

``` r
# student 1
s1 <- c(100, 100, 100, 100, 100, 100, 100, 90) # student 2
s2 <-  c(100, NA, 90, 90, 90, 90, 97, 80)
```

``` r
grade <- function(x){
  if(any(is.na(x))){
    warning("Student is missing a homework")
  }
  mean(x[which.min(x)],na.rm=TRUE)
}
```

``` r
url <- "https://tinyurl.com/gradeinput"
hw <- read.csv(url, row.names = 1)
```

``` r
apply(hw, 1, grade)
```

    ## Warning in FUN(newX[, i], ...): Student is missing a homework

    ## Warning in FUN(newX[, i], ...): Student is missing a homework

    ## Warning in FUN(newX[, i], ...): Student is missing a homework

    ## Warning in FUN(newX[, i], ...): Student is missing a homework

    ##  student-1  student-2  student-3  student-4  student-5  student-6  student-7 
    ##         73         64         69         73         75         77         74 
    ##  student-8  student-9 student-10 student-11 student-12 student-13 student-14 
    ##         76         77         72         66         70         76         76 
    ## student-15 student-16 student-17 student-18 student-19 student-20 
    ##         65         74         63         87         68         68
