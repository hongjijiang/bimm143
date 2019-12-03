#' ---
#' title: "Class 05"
#' author: "Hongji Jiang"
#' output: github_document
#' ---

#Class5 Data Visualization

x <- rnorm(1000)

#some summary stats
mean(x)
sd(x)

summary(x)
boxplot(x)

hist(x)

rug(x)

# Section 2 scaterplots
weight <- read.table("bimm143_05_rstats/weight_chart.txt",header = TRUE)
plot(weight$Age, weight$Weight, type = 'o',pch = 15,cex = 1.5,lwd = 2,ylim=c(2,10))

mouse <- read.delim("bimm143_05_rstats/feature_counts.txt",header = TRUE)
head(mouse)

par(mar=c(5,11,4,2))
barplot(mouse$Count,names.arg = mouse$Feature,horiz = TRUE,las =1)


hist(c(rnorm(10000),rnorm(10000)+4))

male <- read.delim("bimm143_05_rstats/male_female_counts.txt",header = TRUE)
barplot(male$Count,col=rainbow(10),names.arg = male$Sample,las = 2,ylab = "Counts")
barplot(male$Count,col=c("blue2","red2"),names.arg = male$Sample,las = 2,ylab = "Counts")
ud <- read.delim("bimm143_05_rstats/up_down_expression.txt",header = TRUE)
nrow(ud$Gene)
plot(ud$Condition1, ud$Condition2, col=ud$State, 
     xlab="Expression condition 1", ylab="Expression condition 2")
