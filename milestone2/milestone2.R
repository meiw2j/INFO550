#read in data
library(RCurl)
x <- getURL("https://raw.githubusercontent.com/skhan890/GIT-tutorial/master/1001_nights_analects.csv")
y<- read.csv(text = x, stringsAsFactors = FALSE)
names(y)
library(tidytext)
library(tidyverse)

#unnest text to words
y %>% 
  count(title,type)

y_words <- y %>%
  group_by(title) %>%
  mutate(linenumber=row_number()) %>%
  unnest_tokens(word,text) %>%
  ungroup()

#inner join with sentiment table
sentiments_count <- y_words %>%
  inner_join(get_sentiments("bing"), c=("word"))%>%
  count(word, sentiment)

Top_word <- sentiments_count %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word=reorder(word,n))

#plot the by sentiment
plot<- ggplot(Top_word, aes(x=word,y=n, fill=sentiment))+
  geom_col(show.legend = FALSE)+facet_wrap(~sentiment, scales = "free")+coord_flip()

plot
