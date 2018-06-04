
# 整理資料 --------------------------------------------------------------------

data <- read.csv("federalist.csv")
a <- which(data$author=="HAMILTON")
b <- which(data$author=="MADISON")
c <- which(data$author=="HAMILTON OR MADISON")
newdata <- data[c(a,b,c),]
install.packages("tm")
library(tm)
library(magrittr)
library("NLP")
library(slam)
library(dplyr)
newdata$text%<>%as.character()
#先把不要的髒東西抓出來
IC <- Corpus(VectorSource(newdata$text))
writeLines(as.character(IC[[1]]))
str(IC)
#清除標點符號#
d.corpus <- tm_map(IC, removePunctuation)
#清除數字#
d.corpus <- tm_map(IC, removeNumbers)
#轉換大小寫#
d.corpus <- tm_map(IC, tolower)
#清除指定文字#
d.corpus <- tm_map(IC, removeWords,c("<c>","<p>","<l>","<S>","<Q>"))

#建立text matrix
d.corpus <- tm_map(d.corpus, PlainTextDocument)
d.corpus <- Corpus(VectorSource(d.corpus))
tdm <- TermDocumentMatrix(d.corpus, control =list(weighting=weightTfIdf,tolower=FALSE))
class(tdm)
tdm <- as.matrix(tdm)

# 分類 ----------------------------------------------------------------------


