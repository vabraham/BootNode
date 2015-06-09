# clear workspace
rm(list=ls())

# print system information
R.version
Sys.info()

# install non-core packages
install.packages(c('rbenchmark', 'randomForest', 'rpart', 'foreach', 'doParallel'))

# load packages
library(rbenchmark)
library(randomForest)
library(rpart)
library(foreach)
library(doParallel)
library(compiler)

# function from http://dirk.eddelbuettel.com/blog/2011/04/12/
k <- function(n, x=1) for (i in 1:n) x=1/{1+x}

# parallel process function from http://www.r-bloggers.com/a-brief-foray-into-parallel-processing-with-r/
kp <- function(iters, n) {
  #setup parallel backend to use n processors
  cl<-makeCluster(n)
  registerDoParallel(cl)  
  #start time
  #strt<-Sys.time()
  #loop
  ls<-foreach(icount(iters)) %dopar% { 
    to.ls<-rnorm(1000000)
    to.ls<-summary(to.ls)
    #to.ls
  } 
  #print(Sys.time()-strt)
  stopCluster(cl)
}

# create random matrix
mat1 <- matrix(data = rexp(200, rate = 10), nrow = 3000, ncol = 3000)
mat2 <- matrix(data = rexp(200, rate = 10), nrow = 3000, ncol = 3000)

# prepare data set from UCI Repository
# see: http://archive.ics.uci.edu/ml/datasets/Credit+Approval
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data"
mydata <- read.csv(url, header=F)
mydata$V2 <- as.numeric(mydata$V2)
mydata$V14 <- as.numeric(mydata$V14)

# run benchmark
results_rro <- benchmark(rf=randomForest(V16 ~ ., data=mydata, ntree=1000),
                     rp=rpart(V16 ~ ., data=mydata),
                     mm=mat1%*%mat2,
                     fun=k(1000000, x=1),
                     pfun=kp(10,4),  # set the proper number of cores prior to running
                     replications=20
)

results_rro