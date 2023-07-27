install.packages("rwhatsapp")
install.packages("ggplot2")

install.packages("lubridate")
install.packages("ggimage")
install.packages("tidytext")
install.packages("stopwords")
install.packages("tidyverse")
install.packages("tidymodels")
install.packages("cli")
install.packages("scales")
library(rwhatsapp)
library(ggplot2); theme_set(theme_minimal()) #actually you can set any ggplot2 themes in here
library(lubridate)
library(ggimage)
library(tidytext)
library(stopwords)
library(tm)
stopwords("english")



library(tidyverse)
library(tidymodels)

# Load Data -----------------------------------------------------------------------------------

#import and check structure of data
chat <- rwa_read("text.txt") %>% filter(!is.na(author)) 

# Number of messages
#reorder func is to sort in descending order
chat %>%
        mutate(day = date(time)) %>% #new column day created
        count(author) %>%
        ggplot(aes(x = reorder(author, n), y = n, fill=author)) +
        geom_bar(stat = "identity") +
        ylab("") + xlab("") +
        coord_flip() +
        ggtitle("Number of messages")



# Messages per day

chat %>%
        mutate(day = date(time)) %>%
        count(day) %>%
        ggplot(aes(x = day, y = n)) +
        geom_bar(stat = "identity") +
        ylab("") + xlab("") +
        ggtitle("Messages per day")



# Most often used words

to_remove <- c(stopwords("english"),
               "media",
               "omitted",
               "ref",
               "dass",
               "schon",
               "mal",
               "android.s.wt",'ahhh','hmm','kk','k','aa','Aa','Ehh', 'aahhh', 'enn', 'nee','aa�', 'aaa','njn','ee',
               'avide','eyy','avide','apo','appo','ipo','okey','oru','nale','ath','ind','oke','onnum','aahh','pole',
               'nthaa','illaa','athe','ivide','poyi','ini','nalla','alla','alle','https','oo','the', 'enik','inne',
               'ithe','inn','ippo','good','onnum','and')        


# Most often used words !
#unnest_tokekens = split each message into words
chat %>%
        unnest_tokens(input = text,
                      output = word) %>%
        filter(!word %in% to_remove) %>%
        count(author, word, sort = TRUE) %>%
        group_by(author) %>%
        top_n(n = 6, n) %>%
        ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
        geom_col(show.legend = FALSE) +
        ylab("") +
        xlab("") +
        coord_flip() +
        facet_wrap(~author, ncol = 2, scales = "free_y") +
        scale_x_reordered() +
        ggtitle("Most often used words")        

# Important words used
#gsub - remove words that  ends with .com
#Replace words that starts with gag to 9gag
#tf-idf --> term freq - inverse doc freq
#bind_tf_idf --> count the tfidf score for each word
chat %>%
        unnest_tokens(input = text,
                      output = word) %>%
        select(word, author) %>%
        filter(!word %in% to_remove) %>%
        mutate(word = gsub(".com", "", word)) %>%
        mutate(word = gsub("^gag", "9gag", word)) %>%
        count(author, word, sort = TRUE) %>%
        bind_tf_idf(term = word, document = author, n = n) %>%
        filter(n > 10) %>%
        group_by(author) %>%
        top_n(n = 6, tf_idf) %>%
        ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
        geom_col(show.legend = FALSE) +
        ylab("") +
        xlab("") +
        coord_flip() +
        facet_wrap(~author, ncol = 2, scales = "free_y") +
        scale_x_reordered() +
        ggtitle("Important words used")

# Lexical Diversity - measure of no of unique words
#summarize - compute the no of distinct words
#geom_col - bar plot
#geom_text - to add labels in the bar
chat %>%
        unnest_tokens(input = text,
                      output = word) %>%
        filter(!word %in% to_remove) %>%
        group_by(author) %>%
        summarise(lex_diversity = n_distinct(word)) %>%
        arrange(desc(lex_diversity)) %>%
        ggplot(aes(x = reorder(author, lex_diversity),
                   y = lex_diversity,
                   fill = author)) +
        geom_col(show.legend = FALSE) +
        scale_y_continuous(expand = (mult = c(0, 0, 0, 500))) +
        geom_text(aes(label = scales::comma(lex_diversity)), hjust = -0.1) +
        ylab("unique words") +
        xlab("") +
        ggtitle("Lexical Diversity") +
        coord_flip()  

#emoji - !
library("tidyr")
chat %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y")  +
  ggtitle("Most often used emojis")
       

#emoji -2 
library("ggimage")
emoji_data <- rwhatsapp::emojis %>% 
  mutate(hex_runes1 = gsub("\\s.*", "", hex_runes),
         emoji_url = paste0("https://abs.twimg.com/emoji/v2/72x72/", tolower(hex_runes1), ".png"))

chat %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  left_join(emoji_data, by = "emoji") %>% 
  ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  geom_image(aes(y = n + 20, image = emoji_url)) +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  ggtitle("Most often used emojis") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

#emoji 3 - !
library("tidyr")
chat %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y")  +
  ggtitle("Most often used emojis")


# unique words
o_words <- chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author != "Altrin VIT") %>% 
  count(word, sort = TRUE) 

chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author == "Altrin VIT") %>% 
  count(word, sort = TRUE) %>% 
  filter(!word %in% o_words$word) %>% # only select words nobody else uses
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Unique words of Altrin")

# R program to illustrate
# Generating word cloud

# Installing the required packages
install.packages("tm")		 # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator
install.packages("RColorBrewer") # color palettes

# Load the packages
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

# To choose the text file
text = readLines(file.choose())

# VectorSource() function
# creates a corpus of
# character vectors
docs = Corpus(VectorSource(text))

# Text transformation
toSpace = content_transformer(
  function (x, pattern)
    gsub(pattern, " ", x))
docs1 = tm_map(docs, toSpace, "/")
docs1 = tm_map(docs, toSpace, "@")
docs1 = tm_map(docs, toSpace, "#")
strwrap(docs1)

# Cleaning the Text
docs1 = tm_map(docs1, content_transformer(tolower))
docs1 = tm_map(docs1, removeNumbers)
docs1 = tm_map(docs1, stripWhitespace)

# Build a term-document matrix
dtm = TermDocumentMatrix(docs)
m = as.matrix(dtm)
v = sort(rowSums(m),
         decreasing = TRUE)
d = data.frame(word = names(v),
               freq = v)
head(d, 10)

# Generate the Word cloud
wordcloud(words = d$word,
          freq = d$freq,
          min.freq = 1,
          max.words = 200,
          random.order = FALSE,
          rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))
