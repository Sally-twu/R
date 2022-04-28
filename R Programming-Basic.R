#1.2(a)
sessionInfo()
#1.2(b)
date()
Sys.time()
#1.2(c)
list.dirs()
list.files()
dir()
?list.dirs
#1.2(d)
home <- getwd()
setwd("C:/Users/Default")
setwd(home)
#1.2(e)
dir()
#1.2(f)
ls()
rm(list = ls())
#1.2(g)
.libPaths()
#1.2(h)
version
R.Version()
#1.2(i)
install.packages('seriation')
library(seriation)
packageVersi/n("seriation")
#1.2(j)
?"&"
my.dist<-function(x1=0,y1=0,x2,y2){
  dist <- sqrt((x1-x2)^2+(y1-y2)^2)
  dist
}
my.dist(2,3,1,2)
#---------------------------------------------------------------
#1.7(a)
rep(c("A","B","C","D","E"),c(5,4,3,2,1))
#1.7(b)
seq1 <- letters[seq(from=2,to=26,by=2)]
seq2 <- letters[seq(from=1,to=26,by=2)]
c(seq1,seq2)
#1.7(c)
library(MASS)
fractions((-1)^seq(2,21,1)*1/seq(from=1,to=20,by=1))
#1.7(d)
c(2:6,3:6,4:6,5:6,6)
#1.7(e)
seq1 <- month.abb[seq(1,12,2)]
seq2 <- month.abb[seq(2,12,2)]
c(seq1,seq2)
#-----------------------------------------------------------------
#1.20
factor <- cut(0:1,breaks = 10)
table(factor)
hist(u, 10, right=T, plot = FALSE)
#----------------------------------------------------------------
#1.24(a)
set.seed(1234567)
n <- 60
ID <- sample(1:n)
gender <- sample(c("男","女"),n,replace=T)
consent <- sample(c("非常不同意","不同意","普通","同意","非常同意"),
                  n,replace=T,prob=c(0.1,0.1,0.2,0.4,0.3))
table(factor(gender))
#1.24(b)
consent <- as.factor(consent)
levels(consent) <- c("非常不同意","不同意","普通","同意","非常同意")
consent<-ordered(consent)
#1.24(c)
survey.df <- data.frame(ID,gender,consent)
colnames(survey.df )<-c("ID","gender","consent")
head(survey.df,n=5)
str(survey.df)
#1.24(d)
which(consent <= "不同意")
sum(consent <= "不同意")
#1.24(e)
score <- function(consent) {
  if (consent == "非常不同意") {
    return(1)
  } else if (consent == "不同意") {
    return(2)
  } else if (consent == "普通") {
    return(3)
  } else if (consent == "同意") {
    return(4)
  } else {
    return(5)
  }
}
survey.df$score = sapply(survey.df$consent, FUN = score)
mean(survey.df$score)
#-----------------------------------------------------
#1.29(a)
scores <- c(30,49,95,NA,54,NA,61,85,51,22,0,0)
gender <- c('m','f','f','m','f','m','f','m','m','f','f','m')
gender.f <- factor(gender)
table(gender.f)
#1.29(b)
scores.max <- max(scores,na.rm = T)
scores.min <- min(scores,na.rm = T)
cat("最低分:",scores.min,"最高分:",scores.max)
#1.29(c)
scores.mean <- mean(scores,na.rm = T)
scores.sd <- sd(scores,na.rm = T)
cat("平均:",scores.mean,"標準差:",scores.sd)
(scores.mean.fm <- tapply(scores,gender.f,mean,na.rm = T))
#1.29(d)
scores = scores+10
scores[is.na(scores)]<-0
scores[scores>100]<-100
scores
#1.29(e)
which(scores>60)
length(which(scores>60))
#-----------------------------------------------------
#1.31(a)
library(readxl)
R.score <- read_excel("data/R-score.xlsx",sheet = "工作表1",na="NA",cell_rows(2:14))
colnames(R.score)<-c("NO.","系級","學號","姓名","小考1","小考2","小考3","作業","期末考","點名")
head(R.score,5)
#1.31(b)
cat("小考1平均:",mean(R.score$小考1),
    "小考2平均:",mean(R.score$小考2),
    "小考3平均:",mean(R.score$小考3),
    "作業平均:",mean(R.score$作業),
    "期末考平均:",mean(R.score$期末考))
cat("小考1標準差:",sd(R.score$小考1),
    "小考2標準差:",sd(R.score$小考2),
    "小考3標準差:",sd(R.score$小考3),
    "作業標準差:",sd(R.score$作業),
    "期末考標準差:",sd(R.score$期末考))
#1.31(c)
R.score$學期成績 = 0.1*R.score$小考1+0.15*R.score$小考2+
  0.15*R.score$小考3+0.2*R.score$作業+0.4*R.score$期末考
result <- data.frame(R.score$學號,R.score$學期成績)
result
#------------------------------------------------------
#2.20(a)
student.id <- paste("student",1:50,sep =".")
p <- dnorm(seq(-3,3,length=101))
my.p <- p/sum(p)
set.seed(123456)
score <- sample(0:100,length(student.id),replace = T,prob=my.p)
sum(score<60)/50
#2.20(b)
which(score == max(score))
#2.20(c)
score = sqrt(score)*10
cat("平均數:",mean(score),"標準差:",sd(score))
#-------------------------------------------------------
#2.48
ComputeWeight <- function(){
  gender <- readline("請輸入你的性別: ")
  height <- readline("請輸入你的身高(公分): ")
  height <- as.numeric(height)
  if(gender=='男'){
    weight<-(height-80)*0.7
  }else{
    weight<-(height-70)*0.6
  }
  cat('你的標準體重為:',weight)
}
ComputeWeight()
#------------------------------------------------------
#6.19
correlation <- function(n,x,y){
  
}