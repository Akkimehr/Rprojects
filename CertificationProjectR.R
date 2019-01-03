# CERTIFICATION PROJECT

# DATA ANALYSIS AND PREPROCESSING
# 1.
p = read.csv("projectdata.csv")
# 2.
str(p)
# 3.
p$description <- as.character(p$description)
# 4. 
str(p)
# 5.
summary(p)
# 6.
apply(p , 2 , function(x) length(unique(x)))
# 7.
median <- median(p$startprice , na.rm = TRUE)
# 8.
p$priceclass <- ifelse(p$startprice > median , 1 , 0)
# 9.
str(p)
# 10.
cols = c("biddable", "sold", "priceclass")
for(i in cols){
  p[,i]=as.factor(p[,i])
}
# 11.
apply(p[, c(3,8)] , 2 , max , na.rm = TRUE)
# 12.
apply(p[, c(4:7,9,10,12)] , 2 , max , na.rm = TRUE)
# 13.
lapply(p[, c(3,8)] , function(x) median(x , na.rm = TRUE))
# 14.
lapply(p[, c(3,8)] , function(x) sd(x , na.rm =TRUE))
# 15.
tapply(p$startprice , p$sold , median)
# 16 .
tapply(p$startprice , p$biddable , median)


# DATA MANIPULATION AND EDA
# 17.
library(dplyr)
# 18. 
select(p , startprice , condition , sold)
# 19.
select(p , c("biddable":"sold"))
# 20.
p <- mutate(p , var1 = storage/startprice)
# 21.
arrange(p , startprice)
# 22.
arrange(p , condition)
# 23.
arrange(p , biddable , sold)
# 24.
notbought <- filter(p , sold == 0)
# 25.
arrange(notbought , condition , sold)
# 26.
summarise(p , minimum = min(startprice) , average = mean(startprice) , maximum = max(startprice) , SD = sd(startprice) , IQR = IQR(startprice))
# 27.
summarise(p , minimum = min(storage) , average = mean(storage) , maximum = max(storage) , SD = sd(storage) , IQR = IQR(storage))


# DATA VISUALIZATION
# 28.
library(ggplot2)
# 29. 
ggplot(p , aes(x = storage , y = startprice)) + geom_point()
# 30.
ggplot(p , aes(x = storage , y = startprice , col = sold)) + geom_point()
# 31.
ggplot(p , aes(x = storage , y = startprice , col = sold)) + geom_point() + geom_smooth()
# 32.
ggplot(p , aes(x = startprice)) + geom_histogram()
# 33.
ggplot(p , aes(x = startprice)) + geom_histogram(binwidth = 200)
# 34.
ggplot(p , aes(x = startprice)) + geom_histogram(aes(y = ..density..) , binwidth = 200)
# 35.
ggplot(p , aes(x = startprice , fill = "#377EB8")) + geom_histogram(aes(y = ..density..) , binwidth = 200)
# 36.
ggplot(p , aes(x = sold , fill = biddable)) + geom_bar()
# 37.
ggplot(p , aes(x = sold , fill = biddable)) + geom_bar(position = "stack")
# 38.
ggplot(p , aes(x = sold , fill = biddable)) + geom_bar(position = "fill")
# 39.
ggplot(p , aes(x = sold , fill = biddable)) + geom_bar(position = "dodge")
# 40.
ggplot(p, aes(x = storage , y = startprice)) + geom_point()
# 41.
ggplot(p, aes(x = storage , y = startprice)) + facet_grid(sold ~ .) + geom_point()
# 42.
ggplot(p, aes(x = storage , y = startprice)) + facet_grid(. ~ biddable) + geom_point()
# 43.
ggplot(p, aes(x = storage , y = startprice)) + facet_grid(sold ~ biddable) + geom_point()


# TEXT ANALYTICS
# 44.
library(stringr)
library(tm)
# 45.
r1 = as.character(p$description)
set.seed(100)
sample = sample(r1 , (length(r1)))
# 46.
p_VS = VectorSource(p$description)
p_corpus = VCorpus(p_VS)
# 47.
p_corpus = tm_map(p_corpus , removePunctuation)
# 48.
p_corpus = tm_map(p_corpus , tolower)
p_corpus = tm_map(p_corpus , PlainTextDocument)
# 49.
P_corpus = tm_map(p_corpus , removeNumbers)
# 50.
p_corpus = tm_map(p_corpus , stripWhitespace)
# 51.
p_corpus = tm_map(p_corpus , removeWords , stopwords("english"))
# 52.
p_corpus = tm_map(p_corpus , stemDocument)
# 53.
p_DTM = DocumentTermMatrix(p_corpus)
# 54.
p_DF = data.frame(as.matrix(p_DTM) , StringAsFactors = FALSE)
# 55.
library(RColorBrewer)
library(wordcloud)
p_WC = wordcloud(p_corpus , random.order = TRUE)
# 56.
p_WC = wordcloud(p_corpus , random.order = FALSE)
# 57.
p_WC = wordcloud(p_corpus , random.order = FALSE , min.freq = 5)
 

# TEXT ANALYTICS ON UNSOLD IPADS
# 58.
notsoldipads = subset(p , sold == 0)
n1 = as.character(notsoldipads$description)
set.seed(100)
sample2 = sample(n1 , (length(n1)))
# 59.
notsoldipads_VS = VectorSource(notsoldipads)
corpus2 = VCorpus(notsoldipads_VS)
corpus2 = tm_map(corpus2 , removePunctuation)
corpus2 = tm_map(corpus2 , tolower)
corpus2 = tm_map(corpus2 , PlainTextDocument)
corpus2 = tm_map(corpus2 , removeNumbers)
corpus2 = tm_map(corpus2 , stripWhitespace)
corpus2 = tm_map(corpus2 , removeWords , stopwords("english"))
corpus2 = tm_map(corpus2 , stemDocument)
# 60.
frequencies2 = DocumentTermMatrix(corpus2)
# 61.
corpus2_DF = data.frame(as.matrix(frequencies2) , StringAsFactors = FALSE)
corpus2_WC = wordcloud(corpus2 , random.order = TRUE)
corpus2_WC = wordcloud(corpus2 , random.order = FALSE)
corpus2_WC = wordcloud(corpus2 , random.order = FALSE , min.freq = 5)


# TEXT ANALYTICS ON SOLD IPADS
# 62.
p_sold = subset(p , sold == 1)
# 63.
p1 = as.character(p_sold$description)
set.seed(100)
samples3 = sample(p1 , (length(p1)))
# 64.
p_sold_VS = VectorSource(p_sold)
corpus3 = VCorpus(p_sold_VS)
corpus3 = tm_map(corpus3 , removePunctuation)
corpus3 = tm_map(corpus3 , tolower)
corpus3 = tm_map(corpus3 , PlainTextDocument)
corpus3 = tm_map(corpus3, removeNumbers)
corpus3 = tm_map(corpus3 , stripWhitespace)
corpus3 = tm_map(corpus3 , removeWords , stopwords("english"))
corpus3 = tm_map(corpus3 ,stemDocument)
# 65.
corpus3_DTM = DocumentTermMatrix(corpus3)
# 66.
corpus3_DF = data.frame(as.matrix(corpus3_DTM) , stringsAsFactors = FALSE)
# 67.
corpus3_WC = wordcloud(corpus3 , random.order = TRUE)
corpus3_WC = wordcloud(corpus3 , random.order = FALSE)
corpus3_WC = wordcloud(corpus3 , random.order = FALSE , min.freq = 5)
