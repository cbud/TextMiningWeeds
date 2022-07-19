#Chris Buddenhagen 2022-07-14
#Bibliometric analysis

#Vignette source information: https://www.bibliometrix.org/vignettes/Introduction_to_bibliometrix.html #see also package help

# Load packages
library(bib2df)
library(bibliometrix)
library(tidyverse)
library(rscopus)

#Not synching large files on github
#dir()
# [1] "Large Data Files"      "Outputs"               "README.md"             "Small Data Files"     
# [5] "TextMiningWeeds.Rproj" "Weed_text_mining.r"  

#Note it looks like it is better to download from one recognized source through an API for bibliometrix package
#GW<-convert2df(path2file, dbsource = "scopus", format = "endnote")
#results <- biblioAnalysis(GW, sep = ";")
#Note it looks like it is better to download from one recognized source through an API for bibliometrix package
#GW<-convert2df(path2file, dbsource = "scopus", format = "endnote")
#path2file<-"Small Data Files/Good weed references.bib"
#GW<-bib2df(path2file, separate_names = TRUE)
#path2file<-"Small Data Files/Some problem reference examples.bib"
#BW<-bib2df(path2file, separate_names = TRUE)


#import data
good_papers<- bib2df("Small Data Files/Good weed references.bib", separate_names = TRUE)
bad_papers<- bib2df("Small Data Files/Some problem reference examples.bib", separate_names = TRUE)
before_clean <- bind_rows(good_papers, bad_papers)
                         

#adds lowercase title, abstract, and keywords to new column 
before_clean$combined <- paste(tolower(before_clean$TITLE), tolower(before_clean$ABSTRACT), tolower(before_clean$KEYWORDS))

#remove duplicates
cleaning<-before_clean %>%
  distinct(paste(tolower(TITLE)), .keep_all=TRUE) %>% 
  select(-"paste(tolower(TITLE))") %>% #remove temporary column
  mutate(keep="good") #create keep column

#add duplicates back in, with keep = duplicate
cleaning<- before_clean %>% left_join(cleaning, by = names(.))
cleaning$keep[is.na(cleaning$keep)] <- "duplicate"

#create function to remove rows based on input words
remove_word <- function(input) {
  for (i in 1:length(input)) {
    removed <- cleaning[grep(input[i], cleaning$combined), ]  #subset rows based on input word
    
    removed <- removed %>%
      mutate(keep = paste0(ifelse(keep == "good", "", paste0(keep, ", ")), input[i])) #replace keep with input word + any other exclusion words
    
    cleaning <-
      cleaning %>% anti_join(removed, by = "TITLE") %>% full_join(removed, by = names(.)) #adds subset rows back onto main weed paper list
  }
  return(cleaning)
}            

#uses the function to remove papers that contain any of the input words. You can run this bit separately with different terms.
after_clean<-remove_word(list("insect pests", "cover crop", "medicine",
                              "food", "carnivore", "domesticated", "wild animal"))

#possible others: folk, chemistry, herbal, essential oils

#calculates erroneous removals
missed_bad <- after_clean %>% filter(keep=="good") %>% anti_join(good_papers)
excluded_good <- after_clean %>% filter(keep!="good") %>% merge(good_papers)

#shows summary of papers excluded
after_clean%>% 
  group_by(keep) %>%
  summarise(n()) %>% 
  arrange(-.[2]) #arrange by 2nd column, descending


paste(nrow(missed_bad), "missed bad papers")
paste(nrow(excluded_good), "excluded good papers")


