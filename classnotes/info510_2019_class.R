a <- "Hello"
a = "Hello"
print(a)
a <- "2"
b <- a * 2
a <- c(1:100)
a
cat(a)
a <- "Hello"
b <- "world"
cat(paste(a,b, sep = ""))
c <- c(a,b)
str(c)
a <- 2
b <- "2"
class(a)
class(b)
a * 2
b * 2
2
2.0
as.numeric(b) * 2
??as.numeric()
?cat()
x <- c(1:100)
x[99] <- 999

l <- list()
l[[1]] <- "Hello"
l[[2]] <- 2
l[[3]] <- c(1:1000)
l[[3]][99]
l <- list(a = "Hello", b=2)
l[[1]]
l[["a"]]
f <- factor()

length(l)
x <- c(5000:6000)
length(x)

for (i in 1:length(x)) {
  x[i] <- x[i] + 49
  #cat(a, sep="\n")
}
x+49
x <- x +49

data("iris")
head(iris)
dim(iris)
row.names(iris)
row.names(iris) <- as.numeric(row.names(iris)) + 49
names(iris)
iris[1,1]

dim(iris)[2]

unique(iris$Species)
levels(iris$Species)
as.character(iris$Species)

split_data <- split(iris, f = iris$Species)

mean(split_data[["setosa"]]$Petal.Width)

for ( i in 1:length(split_data)) {
  df <- split_data[[i]]
  pw <- df$Petal.Width
  x <- mean(pw)
  cat(names(split_data)[i],x, "\n")
}

myfunction <-
  function ( x, col_name = "Petal.Width") {
    return( mean(x[[col_name]]) )
}

result <- myfunction(split_data[[1]])
result_vector <- vector()

for (i in 1:length(split_data)) {
  result <- myfunction(split_data[[i]], "Sepal.Length")
  result_vector[i] <- result
}

result_list <- sapply(split_data, myfunction)

name_vector <- names(iris)
name_vector == "Sepal.Length"

numbers <- c(1:1000)
numbers [numbers > 500]

which( name_vector == "Sepal.Length")

df <- split_data[[1]]
View(df)
df[,3]
df[,which(names(iris) == "Petal.Width")]

v <- c(1,2,3, NA, 5, 6)
v[!is.na(v)]

iris

write.table(iris,file = gzfile("myiris.txt.gz"), row.names = F, quote = F, sep="\t")
getwd()
setwd("/home/malay/Downloads")
getwd()
myiris <- read.table("myiris.txt.gz", header = T)

a=c(1,2)
b=c(3,4)
c=c(5,6)
m <- data.frame()
rbind(a,b,c)
cbind(a,b,c)

set.seed(12345)
r <- rnorm(50)
mean(r)
hist(r))
plot(density(r)
     
plot(iris)

library(RColorBrewer)
nicecolors <- brewer.pal(3, "Pastel2")
pdf(file = "test.pdf", height = 8.5, width = 8.5)
par(mfrow=c(1,2),pty="s")
plot(iris$Sepal.Length, iris$Sepal.Width,xlab = "Sepal length",
     ylab = "Sepal width", main="Some data",
     ylim = c(0, max(iris$Petal.Length)),
     pch=16,col=nicecolors[1]
     )
points(iris$Sepal.Length, iris$Petal.Length, col=nicecolors[2],pch=16)
legend("topleft", legend=c("Sepal width", "Petal length"), 
       col=c(nicecolors[1],nicecolors[2]), pch=16)
plot(iris$Sepal.Length, iris$Sepal.Width,xlab = "Sepal length",
     ylab = "Sepal width", main="Some data",
     ylim = c(0, max(iris$Petal.Length)),
     pch=16,col=nicecolors[1]
)
dev.off()

#png()
#svg()
#tiff()

subset_iris <- iris[, 1:4]
pdf ("looping_plot.pdf", height=11, width=8.5)
par(mfrow=c(4,4))
for (i in 1:dim(subset_iris)[2]) {
  for (j in 1:dim(subset_iris)[2]) {
    plot(subset_iris[,i], subset_iris[,j])
  }
}
dev.off()
x <- myiris$Sepal.Length[1:50]
y <- myiris$Sepal.Width[1:50]

plot(x,y)
o <- order(x)
lines(x[o],y[o])

hist(x)
plot(density(x))

boxplot(iris$Sepal.Width)
summary(x)

iris$Species == "virginica"
mean_vir <- mean(iris[iris$Species == "virginica", ]$Sepal.Length)
mean_set <- mean(iris[iris$Species == "setosa", ]$Sepal.Length)
t.test(iris[iris$Species == "virginica", ]$Sepal.Length,
       iris[iris$Species == "setosa", ]$Sepal.Length,
       alternative = "two.sided")
boxplot(iris[iris$Species == "virginica", ]$Sepal.Length,
        iris[iris$Species == "setosa", ]$Sepal.Length)

data(mtcars)
counts <- table(mtcars$gear)
barplot(counts, horiz = T)
counts <- table(mtcars$gear,mtcars$cyl)
barplot(counts, beside = T)
transform_iris <- t(iris)

# Mean sepal length for the each species

l <- split (iris, iris$Species)
means_sepal <- sapply(l, function(x) {
  mean(x$Sepal.Length)
})
means_petal <- sapply(l, function(x) {
  mean(x$Petal.Length)
})
barplot(means)
df_barplot <- rbind (means_petal, means_sepal)
barplot(df_barplot,beside = T,legend=rownames(df_barplot))

plot(iris$Sepal.Length, iris$Petal.Length)
cor_test <- cor.test(iris$Sepal.Length, 
                     iris$Petal.Length,
                     alternative = "two.sided", 
                     method="pearson")
cor_test
summary(cor_test)

plot(iris$Petal.Length, iris$Sepal.Length)
m <- lm(iris$Sepal.Length~iris$Petal.Length)
abline(m)

b <- barplot(df_barplot[1,])
sd_sepal_length <- sapply(l, function(x) {
  sd(x$Sepal.Length)
})

barplot(df_barplot[1,], ylim=c(0,6.5))
arrows(x0=b[,1],y0=df_barplot[1,] + sd_sepal_length,
       x1=b[,1],y1=df_barplot[1,] - sd_sepal_length,
       code = 3, angle=90)

#====================
# RNASEQ
#====================

counts <- read.table("~/info510/rnaseq_data/nagalakshmi_count_table.txt", 
                     header = T)
rownames(counts) <- counts$gene
counts <- counts[, -1]
boxplot(counts)
counts$sum <- apply(counts,MARGIN = 1, FUN = sum)
counts <- counts[counts$sum !=0, ]
counts <- counts[, -5]
counts_median_normalized <- counts
medians <- sapply(counts,median)
for (i in 1:dim(counts)[2]) {
  counts_median_normalized[,i] <- counts[,i] / medians[i]
}
#counts_median_normalized <- counts / sapply(counts, median)
boxplot(counts_median_normalized, outline = F)

# upper quartile normalization
uq <- sapply(counts,function(x) {
  quantile(x[x !=0], 0.75) }
  )
counts_uq <- counts
for (i in 1:dim(counts)[2]) {
  counts_uq[,i] <- counts[,i] / uq[i]
}
boxplot(counts_uq, outline = F)


###################
# Reading file
###################
setwd("~/info510/")
data <- read.table("pnas_expression.txt",header=T)

######### Reading excel file ################
library(readxl)
read_xlsx()
###############################

############## Reading commadline ##########

args <- commandArgs(trailingOnly = T)
args[1]
read.table(args[1])

#################### Differential expression
setwd("~/info510/")
data <- read.table("pnas_expression.txt",header=T)
rownames(data) <- data$ensembl_ID
data <- data[,-1]
data <- data[, -8]
names(data) <- c("C1","C2","C3","T1","T2","T3","T4")

## EdgeR ###
library(edgeR)
groups <- c( rep("Y",3), rep("N",4))
cds <- DGEList(data, group=groups)
cds <- calcNormFactors(cds)
d <- cds@.Data[[2]]
normalization_factors <- d$norm.factors
design <- model.matrix(~groups)
y <- estimateDisp(cds,design)
fit <- glmQLFit(y, design)
qlf <- glmQLFTest(fit, coef=2)

topTags(qlf)

# Deseq2
library(DESeq2)
coldata <- data.frame(condition=c(rep("C",3), rep("T",4)))
dds <- DESeqDataSetFromMatrix(data, colData = coldata, design = ~condition)
dds <- estimateSizeFactors(dds)
sizeFactors(dds)
dds <- DESeq(dds)
results <-results(dds)
write.table(results, file="deseq_result.txt")
plotMA(results)

# PCA

data
data <- data + 1
data <- log2(data)
data <- t(data)
View(data)
d.pca <- prcomp(data)
plot(d.pca)
summary(d.pca)
str(d.pca)
d.pca$x
plot(d.pca$x[,1], d.pca$x[,2], col=c("black","black","black",
                                     rep("red",4)))
