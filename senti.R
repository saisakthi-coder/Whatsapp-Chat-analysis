
print(getwd())
install.packages("rlang")
print(getwd())
library(syuzhet)
library(ggplot2)
library(tm)
texts=readLines(file.choose())
print(texts)
#This function returns a matrix with the sentiment counts 
#for each of the 10 sentiment categories provided by the NRC lexicon.
sentiment=get_nrc_sentiment(texts)
print(sentiment)
text=cbind(texts,sentiment)
TotalSentiment=data.frame(colSums(text[,c(2:11)]))
TotalSentiment
names(TotalSentiment)="count"
TotalSentiment
names(TotalSentiment)="count"
TotalSentiment=cbind("sentiment"=rownames(TotalSentiment), TotalSentiment)
rownames(TotalSentiment)=NULL
ggplot(data=TotalSentiment,aes(x=sentiment,y=count))+geom_bar(aes(fill=sentiment),stat ="identity")
+theme(legend.position="none")+xlab("sentiment")+ylab("Total Count")+ggtitle("Total Sentimental Score")
install.packages("ggplot2")
