library(tidyverse)
library(tictoc)
library(tidytext)
library(wordcloud)




# single word -------------------------------------------------------------
weed_papers <- read_csv("Small Data Files/weeds_paper_chris_assess.csv")

weed_papers$combined <- paste(weed_papers$title, weed_papers$description, weed_papers$authkeywords, weed_papers$publicationName)

weed_papers$combined <- weed_papers$combined%>% str_replace_all(., "[[:punct:]]", "") %>% tolower()

weed_paper_words<-unnest_tokens(weed_papers,word,combined)

word_freq <- weed_paper_words %>% count(word, sort = TRUE)

word_freq<-word_freq %>% anti_join(filter(stop_words), by="word")

word_freq %>% 
  top_n(20) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()

# #word cloud
# word_freq %>% with(wordcloud(word, n, max.words = 50, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2")))






# ngrams2 ---------------------------------------------------------------

weed_papers <- read_csv("Small Data Files/weeds_paper_chris_assess.csv")

weed_papers$combined <- paste(weed_papers$title, weed_papers$description, weed_papers$authkeywords, weed_papers$publicationName)


weed_papers$combined <- weed_papers$combined%>% str_replace_all(., "[[:punct:]]", "") %>% tolower()

species_remove<-data.frame(word = c("lolium perenne","avena fatua","chenopdium album", "cirsium vulgare", "brassica oleracea"))

weed_paper_words2<-unnest_tokens(weed_papers,word,combined, token = "ngrams", n = 2)

weed_paper_words2<- weed_paper_words2 %>% anti_join(species_remove, by = "word")

weeds_separated2 <- weed_paper_words2 %>% separate(word, c("word1", "word2"), sep = " ") %>% 
  anti_join(stop_words, by = c(word1 = "word")) %>% 
  anti_join(stop_words, by = c(word2 = "word"))

word_freq2 <- weeds_separated2 %>% unite(word, word1, word2, sep = " ") %>% count(word, 
                                                                                sort = TRUE)


word_freq2 %>% 
  top_n(20) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()

# #word cloud
# word_freq2 %>% with(wordcloud(word, n, max.words = 50, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2")))



# ngrams 3 and comparing good and bad  --------------------------------------------------
weed_papers <- read_csv("Small Data Files/weeds_paper_chris_assess.csv")

weed_papers$combined <- paste(weed_papers$title, weed_papers$description, weed_papers$authkeywords, weed_papers$publicationName)


weed_papers$combined <- weed_papers$combined%>% str_replace_all(., "[[:punct:]]", "") %>% tolower()


weed_paper_words2<- unnest_tokens(weed_papers,word,combined, token = "ngrams", n = 3)

weeds_separated2 <- weed_paper_words2 %>% separate(word, c("word1", "word2", "word3"), sep = " ") %>% 
  anti_join(stop_words, by = c(word1 = "word")) %>% 
  anti_join(stop_words, by = c(word2 = "word")) %>%
  anti_join(stop_words, by = c(word3 = "word")) 

word_freq2 <- weeds_separated2 %>% unite(word, word1, word2, word3, sep = " ") 

word_freq_good <- word_freq2 %>% filter(GoodBad=="good")%>% count(word, sort = TRUE) %>% mutate(status="good")
word_freq_bad <-  word_freq2 %>% filter(GoodBad!="good")%>% count(word, sort = TRUE) %>% mutate(status="bad")

word_freq_goodbad<-bind_rows(word_freq_bad,word_freq_good)


word_freq_goodbad %>%  mutate(word = reorder(word, n))%>%
  slice_max(order_by=n, n=40)  %>%  
  ggplot(aes(word, n, fill=status)) + geom_bar(position="stack", stat="identity") + xlab(NULL) + coord_flip()

word_freq_good %>%  mutate(word = reorder(word, n))%>%
  slice_max(order_by=n, n=20)  %>%  
  ggplot(aes(word, n, fill=status)) + geom_bar(position="stack", stat="identity") + xlab(NULL) + coord_flip()

word_freq_bad %>%  mutate(word = reorder(word, n))%>%
  slice_max(order_by=n, n=20)  %>%  
  ggplot(aes(word, n, fill=status)) + geom_bar(position="stack", stat="identity") + xlab(NULL) + coord_flip()



# proportions ngrams2 ----------------------------------------------------------------

weed_papers <- read_csv("Small Data Files/weeds_paper_chris_assess.csv")

weed_papers$combined <- paste(weed_papers$title, weed_papers$description, weed_papers$authkeywords, weed_papers$publicationName)

weed_papers$combined <- weed_papers$combined%>% str_replace_all(., "[[:punct:]]", "") %>% tolower()

species_remove<-data.frame(word = c("lolium perenne","avena fatua","chenopdium album"))

weed_paper_words<- unnest_tokens(weed_papers,word,combined, token = "ngrams", n = 2)

weed_paper_words<- weed_paper_words %>% anti_join(species_remove, by = "word")

weeds_separated <- weed_paper_words %>% separate(word, c("word1", "word2"), sep = " ") %>% 
  anti_join(stop_words, by = c(word1 = "word")) %>% 
  anti_join(stop_words, by = c(word2 = "word"))

word_freq <- weeds_separated %>% unite(word, word1, word2, sep = " ") 

word_freq_good <- word_freq %>% filter(GoodBad=="good")%>% group_by(weed_searched) %>% count(word, sort = TRUE) %>%rename("good"=n)
word_freq_bad <-  word_freq %>% filter(GoodBad!="good")%>%  group_by(weed_searched) %>% count(word, sort = TRUE) %>%rename("bad"=n)

word_freq_goodbad<-left_join(word_freq_bad,word_freq_good)

word_prop<- word_freq_goodbad %>% mutate(prop=bad/good) %>% arrange(-prop)

word_prop %>% 
  slice_max(order_by=prop, n=5)  %>%  mutate(word = reorder(word, prop))%>%
  ggplot(aes(word, prop, fill=weed_searched)) + geom_bar(position="stack", stat="identity") + xlab(NULL) + coord_flip()


# ngrams 1 proportions ----------------------------------------------------


weed_papers <- read_csv("Small Data Files/weeds_paper_chris_assess.csv")

weed_papers$combined <- paste(weed_papers$title, weed_papers$description, weed_papers$authkeywords, weed_papers$publicationName)

weed_papers$combined <- weed_papers$combined%>% str_replace_all(., "[[:punct:]]", "") %>% tolower()


species_remove<-data.frame(word = c("lolium","perenne","avena", "fatua","chenopodium","album", "de", "la", "oat", "italian"))

weed_paper_words<-unnest_tokens(weed_papers,word,combined)

weed_paper_words<- weed_paper_words %>% anti_join(species_remove, by = "word")


word_freq_good <- weed_paper_words %>% filter(GoodBad=="good") %>% count(word, sort = TRUE) %>%rename("good"=n)
word_freq_bad <-  weed_paper_words %>% filter(GoodBad !="good"& GoodBad != "french") %>% count(word, sort = TRUE) %>%rename("bad"=n)

word_freq_goodbad<-left_join(word_freq_bad,word_freq_good)

word_prop<- word_freq_goodbad %>% mutate(prop=good/bad) %>% arrange(-prop)

word_prop %>% 
  slice_max(order_by=prop, n=35)  %>%  mutate(word = reorder(word, prop))%>%
  ggplot(aes(word, prop)) + geom_bar(position="stack", stat="identity") + xlab(NULL) + coord_flip()

