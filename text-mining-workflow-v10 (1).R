# Workflow of phases and steps as described in Pham et al. Text mining to support abstract screening for systematic reviews: A workflow approach
# The R codes were adopted from Jelena Jovanovic, LASI'18 Workshop on Text mining for learning content analysis 
# https://github.com/jeljov/Text_Mining_at_LASI18

# Codes were written in R using RStudio. Recommended latest version of RStudio and R.
# See "Humans and computing resources required for implementing the workflow" in the online Appendix of the article.

# Required packages for Figure 1 in the main paper.  
# Figure.1.packages = c("dplyr", "tidyr", "caret", "rpart", "e1071", "DMwR", "quanteda", "kernlab", "randomForest", "nnet",
# "rpart.plot", "klaR", "irlba", "doSNOW","text2vec","glmnet","udpipe","topicmodels","purr")

# install.packages(Figure.1.packages) # To only install once for R installation (and update). To attach the package libraries for each R session.

# rm(list = ls()) # clean up all previous objects

# Step 0. Attach the library 
library(readr)             # tools to read rectangular data (like 'csv', 'tsv', and 'fwf')
library(dplyr)             # tools for working with R data frames
library(tidyr)             # tools to tidy R codes
library(caret)             # Classification And REgression Training
library(rpart)             # Recursive PARTitioning for building Classification and Regression Trees
library(e1071)             # tools for support vector machines and many other topics
library(DMwR)              # data mining tools 
library(kernlab)           # tools for kernel-based machine learning methods for classification,regression
library(quanteda)          # tools for managing and analyzing textual data
library(randomForest)      # tools for building random forest for classification
library(irlba)             # tools for Fast Truncated Singular Value Decomposition and Principal Components Analysis 
library(stringr)           # tools for character manipulation
library(doSNOW)            # parallel processing  
library(text2vec)          # text analysis and natural language processing (e.g., dist2)
# library(nnet)              # neural network model
library(udpipe)            # NLP language models
library(topicmodels)       # topic modeling, including Latent Dirichlet Allocation model
library(purrr)             # tools for working with functions and vectors

# Initialization
pos.label="INCLUDE" # coding of screening results
neg.label="EXCLUDE"

###################################################################################
# Step 1. Import citations (Figure 1)
## Import input data of abstract text and results of abstract screening by human reviewers into R 

## Input data: Comma-separated values (CSV) file with column names: id, title, abstract, status. See the file sample and some explanations below.
## column "id": numerical id of abstracts. Each abstract appears twice, as it was reviewed by pairs of reviewers. 
## columns "title", "abstract": title and abstract of each abstract
## column "status": results of abstract screening by human reviewers, such as "INCLUDED" or "EXCLUDED"  
## For status, use uppercase in the coding of "INCLUDED" or "EXCLUDED"; otherwise, please find and replace the hard coding categories above.
## In our practice, we put the protocol as the first record in the corpus as it contains all the relevant terms for abstract screening
 
raw.file="/home/ba/ranking/csvfiles/sr t2d final.csv"   
raw.ta=read.csv(file=raw.file,header=TRUE,stringsAsFactors=FALSE) 

## check and remove abstracts with missing status
jid=which(raw.ta$status=="") # retrieve row number of abstracts with missing screening results
raw.ta=raw.ta[-jid,]         # remove these abstracts

## The input data is organized as screening results (after reconciliation) from two reviewers
## Each abstract is represented by two rows, for the first and second reviewers, but the screening results are the same after reconciliation
selected.ta= seq(from=1, to=nrow(raw.ta) - 1, by=2)  

## keep data from one reviewer only in the working TA's database
wkta=raw.ta[selected.ta,]
rm(raw.ta) # remove unused R datasets from the R space

# coding the text categories of the screening results into R categories
wkta[,4] = factor(wkta[,4],levels=c(pos.label,neg.label)) # machine-learning models to predict the first category of included abstracts

summary(wkta[,4]) 

### merge titles and abstracts into text for analysis, retain variables in wkta
wkta=wkta %>% mutate(text=paste(title,abstract,sep=". ")) %>% dplyr::select(id,title,abstract,text,status)
dim(wkta)

###################################################################################
# Step 2. Pre-process text (Figure 1) 
###################################################################################
## Take a look at a specific abstract and observe the "copyright" info
display_id=662067 # this abstract is with copyright info
wkta$text[wkta$id==display_id] # display th etext, see Copyright info at the end of the text

## A regular expression is a special text string for describing a search pattern.
## See for example a quick reference and an online testing tool for building your regular expressions
## https://www.regexbuddy.com/regex.html
## https://regex101.com/

##  Regular expression to locate "Copy right" information in an abstract. 
## Note: in R, you need to double the "\\" in the specification of the regular expression.
copyright.pattern = "\\bCopyright\\b(.)*" # match the word and any characters beyond to the end of the abstract

## remove all "copyright" info from text   
jx = wkta$text %>% purrr::map(sub,pattern=copyright.pattern,replacement="",ignore.case=TRUE) # tidyR format
jy = sub(pattern=copyright.pattern,replacement="",x=wkta$text,ignore.case=TRUE) # old format

jx[wkta$id==display_id] # check if copyright info in abstract 662067 is gone
wkta$text=unlist(jx) # remove [[]] to [] # now all copyright info were removed

############################################################################################### 
# Step 3. Construct features for singular value decomposition: 1-, 2, 3-gram phrases (Figure 1)
###############################################################################################

## Tokenize the text while removing numbers, punctuation marks, symbols, and so on
svd.tokens <- quanteda::tokens(x = wkta$text, what = "word", remove_numbers = TRUE,remove_punct = TRUE,
                     remove_symbols = TRUE, remove_hyphens= FALSE, ngrams=1:3,
                     concatenator="_") # including single words, and two-word and three-word word-phrases
## if needed, see "tokenization" in the Glossary, online Appendix

str(svd.tokens) # check the volume of tokens

## remove tokens with 1 or 2 characters only as they rarely bear any meaning
svd.tokens <- tokens_keep(x = svd.tokens, min_nchar = 3)

## to lower letter 
svd.tokens <- tokens_tolower(svd.tokens)

## remove stopwords (if needed, see "pre-process text" in Glossary, online Appendix)
head(stopwords(), n = 20) # display the first 20 stopwords in a total of 175 words
svd.tokens <- tokens_remove(svd.tokens, stopwords())

## Perform stemming on the tokens (if needed, see "pre-process text" in Glossary, online Appendix)
svd.tokens <- tokens_wordstem(svd.tokens, language = "english")

## create Document-Feature Matrix with Term frequencies (TF) - Inverse Document Frequencies (IDF) as feature weights  
## if needed, see "feature construction" and related terms in Glossary, online Appendix
svd.dfm <- dfm(x = svd.tokens, tolower = FALSE) %>% dfm_tfidf(scheme_tf = "prop")  

dim(svd.dfm) #  number of terms from the corpus, typically a few million terms

##################################################################################
## Step 4. Select features (Figure 1)
## Step 4a. Select features based upon selected threshold of feature weights
## Step 4b. Use singular value decomposition for further dimension reduction (see Glossary, online Appendix)
## Step 4c. Retain 300 features which are linear combinations of the selected features
##################################################################################

## Step 4a. Select features based upon selected threshold of feature weights
### Examine the distribution of the feature weights. 
features.wts=colSums(svd.dfm) # each feature weight is the sum of the weights of the feature across abstracts
summary(features.wts) # inspect the distribution of weights of features in the corpus

### set threshold to keep only features with significant feature weights in the corpus (see Table 2 in the main paper)
## keep features with weight above the quantile of 0.7, 0.8, 0.9 of the feature weight distribution (Table 2)
threshold.p = 0.90  
threshold.wts <- quantile(features.wts, probs = threshold.p) 
threshold.wts
to_keep <- which(features.wts > threshold.wts) # indices of selected features
### keep features with weights above threshold of weight distribution in the document-feature matrix
svd.dfm1 <- dfm_keep(svd.dfm, pattern = names(to_keep), valuetype = "fixed", verbose = TRUE) # matrix with TF-IDF for SVD

### inspect the number of vocabulary
nc.retained.features=dim(svd.dfm1) # ~702K terms for case study 1 

topfeatures(svd.dfm1,n=50) # inspect top features with highest feature weights

jx=tail(topfeatures(svd.dfm1, n =ncol(svd.dfm1)), n=100) # inspect the terms that are at the tail of the distribution
attributes(jx)$names 

## Step 4b. Use Singular Value Decomposition (SVD, if needed, see Glossary, online Appendix) for dimension reduction
### Obtain 300 linear combinations of selected features (see supporting evidence for the value of 300 in Table 2)

start.time <- Sys.time() # this is time consuming of up to 8 hours for a threshold of 70%
# cat(" HERE IS WHERE WE START: ", format(Sys.time(), "%a %b %d %X %Y"), "\n")
my.cluster <- makeCluster(spec=6, type = "SOCK") # parallel processing
registerDoSNOW(my.cluster)

sr.svd <- irlba(t(svd.dfm1), # it is transposed as SVD requires Term-Document Matrix as an input 
                nv = 300,  # number of singular vectors to estimate
                maxit = 600) # maxit is recommended to be twice larger than nv 

stopCluster(my.cluster)
svd.time=Sys.time() - start.time ## keep track of this.

## cat("Computing time for SVD: ", Sys.time() - start.time, "\n")
## system("echo \"All done\" | mailx -s \'Complete SVD \' ba.pham@theta.utoronto.ca")

### Reduced Document-Features Matrix for ML classification  
sr.svd.X = sr.svd$v # Reduced DFM of 'n' abstracts x 300 features
dim(sr.svd.X)
## X matrix from SVD at differenct cutoff of feature weights
# saveRDS(sr.svd.X, "sr_svd70.RData") # threshold of 70%
# saveRDS(sr.svd.X, "sr_svd80.RData") # threshold of 80%
# saveRDS(sr.svd.X, "sr_svd90.RData") # threshold of 90%
# sr.svd.X <- readRDS("sr_svd70.RData")
###########################################################################################################################
## Steps 2-4. Build additional feature matrix to supplement the feature matrix from Singular Value Decomposition (Figure 1)
## Step 2. Natural language processing of abstracts to extract nouns and verbs
## Step 3. Build document-feature matrix of lemmatized nouns and verbs
## Step 4a. Conduct topic modeling with Latent Dirichlet Allocation 
## Step 4b. Obtain the posterior topic distributions of abstracts to be used as feature matrix with 300 topics 
###########################################################################################################################
## Step 2 - use the *udpipe* R package
## https://github.com/bnosac/udpipe
my.dir="/home/ba/ranking/csvfiles" # storage
language.model="/home/ba/ranking/csvfiles/english-ud-2.0-170801.udpipe" 
## Load the appropriate language model (the one for English language)
tagger <- udpipe_download_model("english", model_dir = my.dir)
tagger <- udpipe_load_model(file = language.model)
## Annotate the text of the abstracts using the loaded model (tagger).
## This will produce several linguistic annotations for each word, including the appropriate POS tags and lemmas

start.time <- Sys.time()    
my.cluster <- makeCluster(spec=6, type = "SOCK") # parallel processing
registerDoSNOW(my.cluster)

abstract_annotated <- udpipe_annotate(tagger, wkta$text, doc_id =wkta$id) # this step is time consuming, about 2 hours

stopCluster(my.cluster)

## To be able to use the udpipe object easily, we'll transform it into a data frame, see Jelena Jovanovic, LASI'18
abst.ann.df <- as.data.frame(abstract_annotated) # Notes: udpipe provides words and their lemma's  

## Save the object to have it available for later
saveRDS(abst.ann.df, "abst_ann_df.RData")
# abst.ann.df <- readRDS("abst_ann_df.RData")

## and remove the large udpipe object, to release memory
remove(abstract_annotated)

abstract.annotated.time=Sys.time() - start.time ## keep track of computing time

## check the total numbers of nouns and verbs in the annotated data
summary(factor(abst.ann.df$upos))

## use the annotated df of the abstracts to build topic modeling at the document level
## See instructions at https://bnosac.github.io/udpipe/docs/doc6.html
## Build Latent Dirichlet Allocation model with nouns and verbs - see  
## Fiona Martin and Mark Johnson. More Efﬁcient Topic Modelling Through a Noun Only Approach. 
## Proceedings of Australasian Language Technology Association Workshop 2015.
dtf <- subset(abst.ann.df, upos %in% c("NOUN", "VERB"))  
dtf$lemma=tolower(dtf$lemma) # fix instances we detected that the lemma of words are not in lower case

dtf1 <- document_term_frequencies(dtf, document = "doc_id", term = "lemma") # topic modeling is at the abstract level "doc_id"

## Create a document/term/matrix for building a topic model
lda.dtm <- document_term_matrix(x = dtf1)
jterms=colnames(lda.dtm)

deleted.terms=grep("[-0-9<>/#%.=\\*\\?]",jterms,value=TRUE) # fix instances where weird terms are detected as nouns in udpipe
lda.dtm1= dtm_remove_terms(lda.dtm, terms=deleted.terms)
 
jterms=colnames(lda.dtm1)
jterms.len = nchar(jterms)
jterms.short.terms=jterms.len > 3 
deleted.terms=jterms[jterms.short.terms==FALSE]
lda.dtm2= dtm_remove_terms(lda.dtm1, terms=deleted.terms) # delete nouns or verbs with 1-3 characters with little meaning

## Remove nouns with low frequencies and remove abstracts without nouns or verbs
lda.dtm3 <- dtm_remove_lowfreq(lda.dtm2, minfreq = 5) 

dim(lda.dtm)
dim(lda.dtm1)
dim(lda.dtm2)
dim(lda.dtm3)
head(dtm_colsums(lda.dtm3))

## identify removed abstracts through the steps above
jj = wkta$id %in% as.integer(rownames(lda.dtm3))
lda.deleted.abstract.ids=wkta$id[jj==FALSE] # abstracts with limited content are not used in ML training
length(lda.deleted.abstract.ids)
head(lda.deleted.abstract.ids)

lda.dtm=lda.dtm3 
rm(lda.dtm1,lda.dtm2,lda.dtm3) # remove unused document-term matrices

# testing method="VEM" # Use this estimation method as the Bayesian method takes over 4 days without convergence (versus 14 hours for VEM)
lda.control.lst.test=list(verbose=5000) # this is more for the Bayesian method, now we use VEM method for estimation, so this is a place holder only
start.time <- Sys.time()
my.cluster <- makeCluster(spec=4, type = "SOCK") # SOCK stands for socket cluster, parallel processing
registerDoSNOW(my.cluster)
tm.lda.test <- LDA(x= lda.dtm, k = 300, method = "VEM", control = lda.control.lst.test) # set 300 topics 
# Notes: the Bayesian approach to estimation ran for 4 days without convergence - we used the VEM estimation
stopCluster(my.cluster)
lda.time.test= Sys.time() - start.time # this step is time consuming, about 14 hours of run-time
# tm.lda <- readRDS("tm_lda.RData")
saveRDS(tm.lda.test, "tm_lda_test.RData")
## extract the posterior distributions of topics for each abstract in the corpus
sr.lda.X.test <- posterior(tm.lda.test)$topics
dim(sr.lda.X.test)

sr.lda.X=sr.lda.X.test # 300 topics

saveRDS(sr.lda.X.test, "sr_lda_X_test.RData") #  
# sr.lda.X.test <- readRDS("sr_lda_X_test.RData")

##################################################################################################################################
## Steps 2-5. Build additional feature matrix and distance matrix to supplement those from Singular Value Decomposition (Figure 1)
## Clinical Concept Embeddings
## Run this section for Systematic review of clinical studies 
## Step 2. Pre-process text - Annotate abstracts using the biomedical semantic annotator RysannMD
##         Obtain Unique Medical Language System Clinical Concept Unique Identifiers UMLS CUI's for each abstract
##     See John Cuzzola, Jelena Jovanovic, Ebrahim Bagheri. RysannMD: A biomedical semantic annotator balancing speed and accuracy. 
##     Journal of Biomedical Informatics 71 (2017) 91–109
## Step 3. Construct features as UMLS CUI's and obtain vector representations of CUI's using pre-trained word vectors for CUI's
##     See Beam et al. Clinical Concept Embeddings Learned from Massive Sources of Multimodal Medical Data. 
## Step 4. Obtain vector representations of abstracts as weighted average of vector representations of CUI's, weighting on CUI frequency 
## Step 5. Calculate the distance between abstracts as the minimum amount of distance that the embedded CUI'ss of one abstract need to
##         “travel” to reach the embedded CUI's of another abstract.
##         Kusner M, Sun Y, Kolkin N, Weinberger K. From Word Embeddings To Document Distances. 
##         Proceedings of the 32 nd International Conference on Machine Learning, Lille, France, 2015.
######################################################################################################################################

## Step 2. Pre-process text - Annotate abstracts using the biomedical semantic annotator RysannMD
#### Create Dataset from the directory containing RysannMD outputs, each abstract is an annotated file
rysannmd_folder<-"/home/ba/ranking/rysannmd/MD.TA.all" # each abstract is a text file in this folder
rys_data <- data.frame(read_folder(rysannmd_folder)) %>% transmute(id, text)
# remove ".MD.title.abstract.txt" from id's in the first column of the dataset
rys_data[,1]=rys_data[,1] %>% map(sub,pattern=".MD.title.abstract.txt",replacement="") %>% as.integer()
# extract cui's and replace the text in each abstract in the dataset rys_data by a list of CUI's
start.time=Sys.time()
cui_dd <- rys_data %>% extr_cuis(certainty.threshold=0.5) # keep CUI's with high certainty of correct annotation - see function "extr_cuis" below
cui.time=Sys.time() - start.time # keep track of computing time

rm(rys_data)

## Step 3. Construct features as CUI's and obtain vector representations of CUI's using pre-trained CUI vectors from project cui2vec
## Create dtm
cui.dtm <- dfm(cui_dd$text, tolower = FALSE)
rownames(cui.dtm)=cui_dd$id
abstract.cuis=colnames(cui.dtm) # CUI's in abstracts

## Load the pre-trained vector representation of CUI from project cui2vec with 500 dimensions
cui2vec.file <- "/home/ba/ranking/csvfiles/cui2vec_pretrained.csv" # each row consists of a CUI and 500 variables
cui2vec.dd=read.csv(file=cui2vec.file,header=TRUE) 
colnames(cui2vec.dd)[1]="cui" #  name the column of CUI's as "cui"

## Match CUI's from the abstracts to CUI's from the pretrained dataset cui2vec  
cuis.to.keep <- intersect(abstract.cuis, cui2vec.dd$cui)
## check the 'level' of matching
length(cuis.to.keep)/length(abstract.cuis)
## 44% of CUIs from our abstracts have their vectors in cui2vec - low coverage

## Create a new DTM that will keep only those common CUI's - TF
cui.dtm1 <- dfm_keep(cui.dtm, pattern=cuis.to.keep, valuetype="fixed", verbose=TRUE)
abstract.cuis=colnames(cui.dtm1) # CUI's in abstracts and in pre-trained cui2vec dataset 

## Likewise, from cui2vec, select CUI's that are in the corpus abstracts
cuis.to.keep.indices <- which(cui2vec.dd$cui %in% cuis.to.keep) # rows in the cui2vec dataset
cui2vec.dd1=cui2vec.dd[cuis.to.keep.indices,]
dim(cui2vec.dd1)

## Order the columns in the cui2vec.dd1 dataset to be the same as in the columns of the cui.dtm1 
jj = cui2vec.dd1[,-1] # drop the column containing CUI's names
jj = t(jj) 
colnames(jj)=cui2vec.dd1$cui 
jj = jj[,abstract.cuis] # ordering
jj[1:5,1:5] # check
abstract.cuis[1:5]  # check
cui2vec.dd1=jj; 
rm(jj);

## Step 4. Obtain vector representations of abstracts as weighted average of vector representations of CUI's, weighting on CUI frequency 
start.time <- Sys.time()                                                  
sr.cui.X <- data.frame() 
for(i in 1:nrow(cui.dtm1)) {                                                       # this is time consuming,about 10 hours
  abst.tf <- as.matrix(cui.dtm1)[i,]  # DTM 
  abst.matrix <- abst.tf * t(cui2vec.dd1) # weighted cloud of CUIs in 500d space
  abst.mapped <- apply(abst.matrix, 2, mean)  # the central point of the cloud
  sr.cui.X <- as.data.frame(rbind(sr.cui.X, abst.mapped)) # store abstract representation in 500d
}
colnames(sr.cui.X) <- paste0("V",1:ncol(sr.cui.X)) # V1:V500
rownames(sr.cui.X) <- rownames(cui.dtm1) # abstract names
cui.time=Sys.time() - start.time # Time difference  

sr.cui.X[1:5,1:5] # check
dim(sr.cui.X) #### matrix of n abstracts and 500 vectors

saveRDS(sr.cui.X, "sr_cui_X.RData")
# Load the saved object
# sr.cui.X <- readRDS("sr_cui_X.RData")

## Step 5. Calculate the distance between abstracts as the minimum amount of distance that the embedded words of one abstract need to
##         “travel” to reach the embedded words of another abstract.
## Create a Relaxed Word Mover Distance (RWMD) object by specifying 2 input parameters:
## - word vector matrix with words given in rows and dimensions of the embedding space in columns; rows should have word names.
## - the method to be used for computing the distance between word vectors
cui.rwmd.model = RWMD$new(wv = t(cui2vec.dd1), method = "cosine") # Notes: transpose is needed here

## Now, we use the RWMD object and our DTM to compute WMD distances between 
# each document pair. However, before that, we need to normalize TFs in
# the DTM matrix (required by the WMD algorithm; see the original paper)

start.time <- Sys.time()                                                    # this step is time consuming, about 23 hours
cui.dtm1.norm <- dfm_weight(cui.dtm1, scheme = "prop")
sr.cui.dist = dist2(x = cui.dtm1.norm, method = cui.rwmd.model, norm = 'none')
dim(sr.cui.dist) ##### WMD matrix for abstracts
cui.rwmd.time=Sys.time()-start.time

saveRDS(sr.cui.dist, "sr_cui_dist.RData")
# sr.cui.dist <- readRDS("sr_cui_dist.RData")
###############################################################################################################
## Steps 2-5. Build additional feature matrix and distance matrix to supplement those from Singular Value Decomposition (Figure 1)
## Word Embeddings: Global Vectors for Word Representation
## Run this section for Systematic review of non-clinical studies (e.g., SRs of health services research methods, SLR of computing topics)
## Step 2. Pre-process text to extract words from abstracts
## Step 3. Construct features as words and obtain vector representations of words using pre-trained word vectors from Glove
##          Jeffrey Pennington, Richard Socher, Christopher D. Manning. GloVe: Global Vectors for Word Representation  
##          https://nlp.stanford.edu/projects/glove/
## Step 4. Obtain vector representations of abstracts as weighted average of vector representations of words, weighting on word frequency 
## Step 5. Calculate the distance between abstracts as the minimum amount of distance that the embedded words of one abstract need to
##         “travel” to reach the embedded words of another abstract.
##         Kusner M, Sun Y, Kolkin N, Weinberger K. From Word Embeddings To Document Distances. 
##         Proceedings of the 32 nd International Conference on Machine Learning, Lille, France, 2015.
###############################################################################################################
## Step 2. Pre-process text to extract words from abstracts
glove.tokens <- tokens(x = wkta$text, what = "word", remove_numbers = TRUE,remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens= FALSE, ngrams=1) # words

## remove tokens with 1 or 2 characters only as they rarely bear any meaning
glove.tokens <- tokens_keep(x = glove.tokens, min_nchar = 3)

## to lower letter 
glove.tokens <- tokens_tolower(glove.tokens)

## remove stopwords.
glove.tokens <- tokens_remove(glove.tokens, stopwords())
## Note that we are not stemming the tokens since words in the GloVe model were not stemmed, and we need to match against those words.  

## Create DTM
glove.dtm <- dfm(glove.tokens, tolower=FALSE)

## Extract words (features) from the DTM since we need to match these against the words in the pre-trained GloVe model
abstract.words <- colnames(glove.dtm)
## ... and examine them
head(abstract.words, n = 20)
tail(abstract.words, n = 20)

## Notice the presence of words ending with "'s" (such as "kaiser's"). Replace such words with their version without "'s" 
end.with.s <- str_detect(abstract.words, "(\\w+)'s$")
end.with.s <- abstract.words[which(end.with.s)]
words.no.s <- str_replace(end.with.s, "(\\w+)'s", "\\1")
## Replace, in the tokens object, tokens that end with "'s" with their 'cleaned' version
glove.tokens <- tokens_replace(glove.tokens, pattern = end.with.s, replacement = words.no.s) 

## Step 3. Construct features as words and obtain vector representations of words using pre-trained word vectors from Glove
## Create again dtm
glove.dtm <- dfm(glove.tokens, tolower = FALSE)

## Create again a vector of vocabulary terms
abstract.words <- colnames(glove.dtm)

## Load the pre-trained GloVe word vectors of 840 billions terms and a vectorized space of 300 dimensions
glove.840B.300d.file <- "/home/ba/ranking/csvfiles/glove.840B.300d.txt"
start.time <- Sys.time()
g840B.300d <- scan(file = glove.840B.300d.file, what="", sep="\n")
glove.load.time=Sys.time() - start.time

## What we have read - g840B.300d - is in fact a huge character vector, consisting of millions of entries
## Each entry is given as a string that consists of 301 items delimited by a space: 
## the 1st item is a word and the rest (300 items) are the estimated values of the 300 dimensions of that word

## Create a data frame out of the large vector read from the file
## (get_word_vectors_df() is defined in the UtilityFunctions.R script)
g840B.300d.df <- get_word_vectors_df(g840B.300d, verbose = TRUE)
dim(g840B.300d.df)
# save this large object
saveRDS(g840B.300d.df, "g840B_300d_df.RData")
# Load the saved object
# g840B.300d.df <- readRDS("g840B_300d_df.RData")

## Remove unused objects to release memory
remove(g840B.300d)

## Take the words from the GloVe model - we need these words to match them against the features (words) from the corpus DTM
glove.words <- colnames(g840B.300d.df)

## Match words from the abstracts to words from Glove  
words.to.keep <- intersect(abstract.words, glove.words)
## check the 'level' of matching
length(words.to.keep)/length(abstract.words)
## 71% of words from our DTM have their vectors in GloVe

## Let's briefly inspect words from abstracts that are not in GloVe
setdiff(abstract.words, glove.words)[1:50]
## 30% missing of words, mainly words pertaining to KS methods

## Create a new DTM that will keep only those words (columns) - TF
glove.dtm1 <- dfm_keep(glove.dtm, pattern=words.to.keep, valuetype="fixed", verbose=TRUE)

## Likewise, from GloVe, select word vectors that will be used for building a feature set, that is, words present in abstracts
glove.to.keep.indices <- which(glove.words %in% words.to.keep)
g840B.300d.df1 <- g840B.300d.df[,glove.to.keep.indices]

## Order the columns (words) in the reduced g840B_300d_df1, to be the same as in the reduced glove_dtm1
g840B.300d.df1 <- g840B.300d.df1[,colnames(glove.dtm1)]

## Before proceeding, remove large objects that are no longer needed
remove(g840B.300d.df, glove.tokens, glove.words, abstract.words, glove.dtm)

## Step 4. Obtain vector representations of abstracts as weighted average of vector representations of words, weighting on word frequency 
## Compute feature values for each abstract as the (coordinate-wise) TF-weighted mean value across all the word vectors.
##
## Note that after the above reduction of DTM and GloVe to the common set of 
## features (words), the two matrices have the same number of columns.
## Now, we will take each abstract (row) from the DTM and multiply it with the transposed 
## GloVe matrix, thus, in fact weighting word vectors in GloVe with the post-specific 
## TF weights of the corresponding words. As the result, we will get a matrix of
## TF-weighted word vectors (words in rows, dimensions in columns) for each abstract. 
## Next, we take the mean value (across words) for each dimension (columns), to obtain
## a new feature vector for each abstract; these vectors have the same number of features 
## as there are dimensions in the GloVe model (300). This way, we are, in fact,  
## translating the existing feature space (words in DTM) into a new feature space 
## (dimensions of the GloVe word vectors).

start.time <- Sys.time()                                                    # this step is time consuming

sr.glove.X <- data.frame() 
for(i in 1:nrow(glove.dtm1)) {
  abst.tf <- as.matrix(glove.dtm1)[i,]  # DTM 
  abst.matrix <- abst.tf * t(g840B.300d.df1) # weighted cloud of words in 300d space
  abst.mapped <- apply(abst.matrix, 2, mean)  # the central point of the cloud
  sr.glove.X <- as.data.frame(rbind(sr.glove.X, abst.mapped)) # store document representation in 300d
}
colnames(sr.glove.X) <- paste0("V",1:ncol(sr.glove.X))
dim(sr.glove.X) #### matrix of n abstracts and 300 vectors

glove.time=Sys.time() - start.time # Time difference of 4.600928 mins

saveRDS(sr.glove.X, "sr_glove_X.RData")
# Load the saved object
# sr.glove.X <- readRDS("sr_glove_X.RData")

## Step 5. Calculate the distance between abstracts as the minimum amount of distance that the embedded words of one abstract need to
##         “travel” to reach the embedded words of another abstract.
## Create a Relaxed Word Mover Distance (RWMD) object by specifying 2 input parameters:
## - word vector matrix with words given in rows and dimensions of the embedding space in columns; rows should have word names.
## - the method to be used for computing the distance between word vectors
rwmd.model = RWMD$new(wv = t(g840B.300d.df1), method = "cosine")

## Now, we use the RWMD object and our DTM to compute WMD distances between 
# each document pair. However, before that, we need to normalize TFs in
# the DTM matrix (required by the WMD algorithm; see the original paper)

start.time <- Sys.time()                                                    # this step is time consuming
glove.dtm1.norm <- dfm_weight(glove.dtm1, scheme = "prop")
sr.glove.dist = dist2(x = glove.dtm1.norm, method = rwmd.model, norm = 'none')
dim(sr.glove.dist) ##### WMD matrix for abstracts
glove.rwmd.time=Sys.time()-start.time

saveRDS(sr.glove.dist, "sr_glove_dist.RData")

#############################################################
# Step 5. Quantify citation similarity (Figure 1)
#############################################################
 
start.time <- Sys.time()
my.cluster <- makeCluster(spec=6, type = "SOCK") # parallel processing
registerDoSNOW(my.cluster)
sr.svd.dist = dist2(x=sr.svd.X, method="cosine") # symmetric matrix of pairwise distances between pairs of citations # SA - Threshold - Repeat
# sr.lda.dist = dist2(x=sr.lda.X, method="cosine")
stopCluster(my.cluster)
Sys.time() - start.time

dim(sr.svd.dist) # Distance matrix to be used for candidate selection for screening in the workflow steps 

dim(sr.lda.dist)

##################################################################################
# Workflow function - Phases 1 and 2 (Figure 1)  
##################################################################################
## inputs:  (see Table 2 in the main paper)
##   m.x - Document-feature matrix from SVD 
##   m.distance: List of 3 matrices of dimension n x n of pairwise distances between n abstracts: svd, lda, and text2vec
##   dd - dataset with columns "id" and "status" of abstracts. Specify status categories in pos.label and neg.label variables below.
##   l.seeds: initial list of ID's of seed studies for prioritizing abstracts for screening by human reviewers
##   n.rounds: maximum number of rounds of screening by human reviewers (e.g., 20 rounds, see Table 2 in the paper)
##   n.initial: Minimum sample size of the initial train dataset, such as 600 abstracts when the corpus of abstracts is represented by 300 features (see Table 2)
##   pick.init: The size k of k nearest-neighbors of an eligible abstract used to build the initial train dataset (e.g., k=8, see Table 2)
##   pick.ml: The size k of k nearest-neighbors of an eligible abstract used to iterate the loop in the Workflow Phase 2 (e.g., k=15, see Table 2)

## outputs: (see Table 2A in the online Appendix)
##      ll.candidates: list of lists of rows of abstracts in the corpus to be screened by human reviewers
##      d.results: Step-specific results of the execution of the workflow (see examples in the Appendix of the paper)
##      rf.svd: final random forest model with feature representations from singular value decomposition SVD

workflow.phase1and2 <- function(m.x, m.distance, dd, l.seeds, n.rounds, n.initial=600, pick.init=8, pick.ml=15) {
  
  system("echo \"Initialization\" | mailx -s \'Workflow starts \' ba.pham@theta.utoronto.ca") # tracking workflow steps over some running hours
  
  ## initialization
  ll.candidates=NULL # list of lists of candidate abstracts for screening by human reviewers 
  
  d.results= data.frame(matrix(NA,nrow=4*n.rounds,ncol=14)) # panel data to record step-specific results from the workflow history
  colnames(d.results)=c("phase","round","n.seeds","n.candidates","n.eligibles","percent",
                        "precision","recall","f1","accuracy","tp","fp","fn","tn") # see Table 2A of online Appendix

  cum.candidate.rows=NULL  # row indices of abstracts that have been screened by human reviewers. The indices refer to the rows of the corpus dataset
  initial.phase.done = FALSE # flag to indicate the initial phase that accumulates the training dataset is done
  train.level.done=FALSE # flag to indicate the two iterations of ML phase is done
  metric="Sens" # metric to maximize in cross-validation of ML models: 1) maximizing sensitivity for all cross-validated models and 2) maximizing ROC for the final model
  curr.eligibles=unlist(lapply(l.seeds,indx.lkup.all,dd=dd)) # look up the rows in the corpus of the initially eligible abstracts, see function "indx.lkup.all" below 
  cum.eligibles.rows=NULL # cumulative list of all seeds across iterations 
  c.round = 1 # round denotes the number of times the workflow interacts with human reviewers for abstract screening
  
  while(c.round<n.rounds) { # rounds denote the number of times the workflow asks human reviewers to screen batches of selected abstracts
    if(!initial.phase.done) { # Start Phase 1 to gather the training data by iterative steps 6-9, Figure 1)
      ## Step 6 - Prioritize citations using 3 distance matrices: SVD, feature embeddings and topic modeling LDA,(Figure 1) ##################
      curr.candidate.rows = similar.abstracts(distance.matrices=m.distance,seed.id=curr.eligibles,pick=pick.init) # Step 6 - identify abstracts similar to an eligible abstract
      curr.candidate.rows=c(curr.candidate.rows,curr.eligibles) # include the seeds into current candidates
      curr.candidate.rows=unique(curr.candidate.rows) # remove duplicates
      duplicates = curr.candidate.rows %in% cum.candidate.rows # identify duplications with already screened abstracts
      curr.candidate.rows =curr.candidate.rows[duplicates==FALSE] # only candidates that are yet to be screened
      if(length(curr.candidate.rows)==0) {
        cat("Initial phase to generate training data: cannot generate new candidates \n")
        initial.phase.done=TRUE
      } # no new candidates
      else { 
        cum.candidate.rows=c(cum.candidate.rows,curr.candidate.rows) # update the list of all candidates
        cum.candidate.rows=unique(cum.candidate.rows) # remove duplicates
        cum.candidate.rows=na.omit(cum.candidate.rows) # remove missing row numbers, if necessary
        cum.eligibles.rows=c(cum.eligibles.rows,curr.eligibles) # update the list of all eligible abstracts
        cum.eligibles.rows=unique(cum.eligibles.rows) # remove duplicates
        
        ## recording step-specific results of steps 6-7 (Figure 1)
        n.seeds=length(curr.eligibles) # record the number of eligible abstracts in this round of human screening
        curr.status=dd$status[cum.candidate.rows] # results of screening by human reviewers   
        numerator.prevalence=length(cum.candidate.rows[curr.status==pos.label]) # record the number of predicted eligible abstracts
        denomerator.prevalence=length(cum.candidate.rows) # record the number of screened abstracts
        
        recording(d.results,"initial",c.round,c.round,n.seeds,denomerator.prevalence,numerator.prevalence,
                  rep(NA,4),rep(NA,4))  # recording the step-specific results into the workflow panel data, see sample of the panel in the Appendix   
        
        ## Step 7 - Screen citations (Figure 1) #####################
        ll.candidates[[c.round]]=curr.candidate.rows # record the set of abstracts to be screened by human reviewers  
        
        curr.status=dd$status[curr.candidate.rows] # results of screening by human reviewers        
        curr.eligibles=curr.candidate.rows[curr.status==pos.label] # identify eligible citations as seeds for the next round of iteration
        duplicates=curr.eligibles %in% cum.eligibles.rows # identify duplications in the cumulative list of eligible abstracts
        curr.eligibles=curr.eligibles[duplicates==FALSE] # Step 9 - remove duplicates. Newly identified eligible abstracts are used in the next iteration of steps 6-9
        
        if(length(curr.eligibles)==0) {
          cat("Initial phase to generate training data: cannot generate new seeds \n")
          initial.phase.done=TRUE
        } # no new eligible abstracts
        
        if(length(cum.candidate.rows)>n.initial) { # step 8 (Figure 1)
          cat("Initial phase to generate training data: completed \n")
          initial.phase.done=TRUE
          k=c.round+1 # set up for fitting ML models - index to the row of the workflow panel
        } # accumulate enough training data
      }

      ## prepare for another round of human screening, if needed
      c.round=c.round+1
      
    } # end if(!initial.phase.done)
     
    if(initial.phase.done && !train.level.done) { # start the Workflow Phase 2 in Figure 1 ##################
      
      system("echo \"ML modeling\" | mailx -s \'Workflow in ML phase \' ba.pham@theta.utoronto.ca") # tracking workflow steps over some running hours
      
      ## Step 7. Screen citations by human reviewers (Figure 1) ######################################
      curr.dd = dd[cum.candidate.rows,]  # current training dataset, given the screening results of the list of cumulative candidates  
      
      ### collect step-specific statistics from the training dataset
      numerator.prevalence=length(curr.dd$status[curr.dd$status==pos.label]) # record the number of eligible abstracts
      denomerator.prevalence=nrow(curr.dd) # record the number of screened abstracts in the training dataset
      
      ## Step 10. Assemble training data (Figure 1) ####################################################
      curr.m.x=m.x[cum.candidate.rows,] # assemble the X matrix of features from the SVD method
      curr.m.xy=data.frame(curr.m.x) %>% mutate(status=curr.dd$status) # assemble training dataset, including features and screening results
      
      ## Step 11. Training random forest models (Figure 1) #################################################
      rf.svd=rf.model(mdata=curr.m.xy, metric=metric) 
      ## Step 12. Predict eligible abstracts (Figure 1) #################################################
      pred.rf.raw.svd.corpus <- predict(rf.svd, newdata = m.x, type="raw") # corpus level
      rf.confusion.m <- confusionMatrix(data = pred.rf.raw.svd.corpus, reference = dd$status, positive=pos.label) # evaluate model performance
      m.rf.eval <- get_eval_measures(rf.confusion.m) # calculate performance measures 
      jtp = rf.confusion.m$table
      m.rf.abcd <- c(jtp[1,1],jtp[1,2],jtp[2,1],jtp[2,2]) # Extract TP, FP, FN, TN
      ### recording the results of the RF classifier into workflow panel data
      m.round=d.results[k-1,"round"]+2 # each fitted RF involves 2 sets of candidates for screening - see below
      recording(d.results,"rf.svd",k,m.round,NA,denomerator.prevalence,numerator.prevalence, m.rf.eval,m.rf.abcd)  
      k= k + 1 # next rows   
      cat("fitting rf.svd \n")
      
      ## Step 12 - Prepare predicted eligible abstracts for screening (Figure 1) #####################
      curr.candidates.rows = which(pred.rf.raw.svd.corpus == pos.label) # identify rows with predicted eligibles in the corpus
      duplicates=curr.candidates.rows %in% cum.candidate.rows # identify duplications with already screened abstracts
      curr.candidate.rows = curr.candidate.rows[duplicates==FALSE] # remove duplicates
      
       ## Step 12. Do we have predicted eligible abstracts to be screened by human reviewers?
      if(length(curr.candidate.rows)==0) {
        train.level.done = TRUE # iterate until no predicted eligible abstracts are possible
        c.round = n.rounds # no more iteration  
      }
      else {
        ll.candidates.index=length(ll.candidates) + 1 # Increment the list of screened abstracts
        ll.candidates[[ll.candidates.index]]=curr.candidate.rows # Step 6 - record the set of predicted eligible abtracts to be screened by human reviewers   
        
        # Step 7 - use of the screened results to identify eligible abstracts and look for similar abstracts to those newly identified eligible abstracts
        curr.dd=dd[curr.candidates.rows,] 
        j.status=curr.dd$status # get the screened results from the screening candidates
        j.eligibles <- j.status == pos.label # identify eligible abstracts among the screening candidates
        j.id <- curr.dd$id[j.eligibles == TRUE] # obtain abstract ID's of the newly identified eligible abstracts  
        
        curr.eligibles=which(dd$id %in% j.id) # look up the rows in the corpus - this set becomes the subjects for the next iteration of steps 10-14 and 6-7
        duplicates=curr.eligibles %in% cum.eligibles.rows # identify duplications in eligible abstracts
        curr.eligibles=curr.eligibles[duplicates==FALSE] # remove duplicates
        
        if(length(curr.eligibles)==0) { # Step 14 - Can we still identify newly identified eligible abstracts?
          train.level.done = TRUE # iterate until no new eligible abstracts are possible
          c.round = n.rounds # no more iteration
        }
        else {
          # update the list of screened abstracts
          cum.candidate.rows=c(cum.candidate.rows,curr.candidate.rows) # update the list of all candidates in terms of rows in the corpus dataset
          cum.candidate.rows=unique(cum.candidate.rows) # remove duplicates
          cum.candidate.rows=na.omit(cum.candidate.rows) # remove missing row numbers 
          cum.eligibles.rows=c(cum.eligibles.rows,curr.eligibles) # update the list of all eligibles that have been identified 
          cum.eligibles.rows=unique(cum.eligibles.rows) # remove duplicates      
          
          # Step 6 - Identify abstracts similar to the newly identified eligibles
          curr.candidate.rows = similar.abstracts(distance.matrices=m.distance,seed.id=curr.eligibles,pick=pick.ml) # identify abstracts similar to newly identified eligibles
          curr.candidate.rows=c(curr.candidate.rows,curr.eligibles) # include the seeds into current candidates
          curr.candidate.rows=unique(curr.candidate.rows) # remove duplicates
          cum.candidate.rows=na.omit(cum.candidate.rows) # remove missing row numbers
          duplicates = curr.candidate.rows %in% cum.candidate.rows # identify duplications with already screened citations
          curr.candidate.rows=curr.candidate.rows[duplicates==FALSE] # remove duplicates
          
          ## Step 7 - abstracts to be manually screened
          ll.candidates.index=length(ll.candidates) + 1 # index to the next set of abstracts for screening by human reviewers
          ll.candidates[[ll.candidates.index]]=curr.candidate.rows # record the set of abtracts to be screened by human reviewers   
          ## prepare for another round of human screening, if needed        
          c.round=c.round+1 # each Workflow Phase 2 involves two rounds of manual screening        
          
          # Step 10 - Prepare the screening results for updating the training dataset
          curr.dd=dd[curr.candidates.rows,] 
          j.status=curr.dd$status # get the screened results from the screening candidates
          j.eligibles <- j.status == pos.label # identify eligible abstracts among the screening candidates
          j.id <- curr.dd$id[j.eligibles == TRUE] # obtain abstract ID's of predicted eligibles  
          
          curr.eligibles=which(dd$id %in% j.id) # look up the rows in the corpus dataframe - this set becomes the new seeds 
          duplicates=curr.eligibles %in% cum.eligibles.rows # identify duplications in seeds
          curr.eligibles=curr.eligibles[duplicates==FALSE] # remove duplicates
          
          # Step 10 - Update the list of screened abstracts and the list of eligible abstracts identified up to this point
          cum.candidate.rows=c(cum.candidate.rows,curr.candidate.rows) # update the list of all candidates in terms of rows in the corpus dataset
          cum.candidate.rows=unique(cum.candidate.rows) # remove duplicates
          cum.candidate.rows=na.omit(cum.candidate.rows) # remove missing row numbers 
          cum.eligibles.rows=c(cum.eligibles.rows,curr.eligibles) # update the list of all eligibles that have been identified 
          cum.eligibles.rows=unique(cum.eligibles.rows) # remove duplicates   
          
          ## prepare for another round of human screening, if needed        
          c.round=c.round+1 # each ML iteration involves two rounds of manual screening        
          
        } # end if(length(curr.eligibles)==0)
      } # end of if(length(curr.candidate.rows)==0)
      
    } # end if(initial.phase.done && !train.level.done) 
    

  } # end while(c.round<n.rounds)
    
  # tracking messages
  if(!initial.phase.done) {cat("Initial phase to generate training data: Not completed \n")}
  if(!train.level.done) {cat("ML phase - training level: Not completed \n")}
  
  d.results=d.results %>% dplyr::filter(!is.na(round)) # remove blank rows in the workflow panel data
  output=list(candidates=ll.candidates, results=d.results, rf.svd=rf.svd) # compile outputs, the final prediction model is the Random Forest with SVD feature representation
  
  return(output)
} # end function

### Functions called by the Workflow functions

## select abstracts similar to the seed abstracts for screening, given a distance matrix
## return the rows of abstracts in the corpus dataset for human screening
## requires a distance matrix, list of seed studies and the number k fo the k-nearest neighbors
similar.abstracts.one = function(distance.matrix=NULL,seed.id=seed.id,pick=25) {
  # cat("In similar abstracts - seed.id: ",seed.id,"\n" )
  m.colnames=colnames(distance.matrix) # row and column names of the distance matrix must be sequenced from 1 to number of abstracts
  list.pick=NULL
  nn=length(seed.id)
  pick = pick + 1 # skip the first one, take from 2 to pick + 1, as the abstracts that are closest to the seed
  for(i in c(1:nn)) {
    jtemp = distance.matrix[seed.id[i],] # take distances of each seed abstract relative to others
    jord=order(jtemp,decreasing=FALSE) # line up the more similar abstracts to the seed
    jextract.colnames=m.colnames[jord]
    current.pick=jextract.colnames[2:pick]
    list.pick=c(list.pick,current.pick)
  }
  list.pick=unique(list.pick)
  list.pick=as.integer(list.pick)
  return(list.pick) # return a list of rows in the main corpus database  
}

## select abstracts similar to the seed abstracts for screening, given "n" distance matrices
similar.abstracts = function(distance.matrices=distance.matrices,seed.id=seed.id,pick=25) {
  nn = length(distance.matrices) # expected distance matrices from SVD, LDA and feature embeddings
  list.pick=NULL # list of candidate abstracts for human screening
  for (i in c(1:nn)) {
    curr.matrix=distance.matrices[[i]]
    curr.pick = similar.abstracts.one(distance.matrix=curr.matrix,seed.id=seed.id,pick=pick)
    # cat("in here ", i, "candidates", curr.pick, "\n")
    list.pick=c(list.pick,curr.pick)
  }
  return(unique(list.pick)) # return a list of rows in the main corpus database  
}        

## look up the row of an abstract ID  
indx.lkup.all=function(x,dd) {which(dd$id==x)} # look up the row of an abstract ID from the input dataframe 

### Fit the Random forest model through Cross-Validation (CV)
### Input data is a dataframe with 300 predictors, the last column is labeled as "status", denoting the screening results, and is the response variable
### Output is the fitted RF model - Specify "ROC" or "Sens"
rf.model <- function(mdata, metric="ROC") { # or metric="Sens"
  cv.cntrl.rf <- trainControl(method = "cv", number = 10, sampling='smote', search = "grid", 
                              summaryFunction=twoClassSummary, classProbs = TRUE,verboseIter=FALSE) # set up cross-validation parameters, see package 'caret'
  max.n.leaves = as.integer(ncol(mdata)/10) # preferrably a number much smaller than the number of features such as 300 for svd and lda or 500 for text2vec
  rf.grid <-  expand.grid(mtry = as.integer(seq(from = 1, to = max.n.leaves, length.out = 20))) # possible values for the # of predictors of decision trees in the RF
  set.seed(seed) # fix value for the stream of pseudo-random number generator
  ## Create a cluster to work on logical cores;
  assign("last.warning", NULL, envir = baseenv()) # clear messages
  unregister() # clear any remaining registered data on parallel processing
  my.cluster <- makeCluster(spec=6, type = "SOCK") # specify the number of clusters for parallel processing
  
  registerDoSNOW(my.cluster) # start the parallel processing
  
  m.rf.cv <- train(status ~ ., data=mdata, method = "rf", ntree=500, metric=metric, 
                   tuneGrid = rf.grid, trControl=cv.cntrl.rf, maximize=TRUE) # fit the RF model through CV

  stopCluster(my.cluster) # end parallel processing
  assign("last.warning", NULL, envir = baseenv()) # clear messages
  unregister() # clear any remaining registered data on parallel processing
  return(m.rf.cv) 
}

## Recording specific results of the workflow in a dataframe (see Table 2A for sample output)
recording = function(d.results,phase,k,round,n.seeds,n.candidates,n.eligibles,eval,table) {
  jj=d.results # obtain a copy of the current dataframe of step-specific results 
  jj[k,"phase"]=phase # worflow phases: initial phase and ML phase, which specifies RF, SVM, Ensemble of the two
  jj[k,"round"]=round # the number of times the Workflow function interacts with human reviewers to help screening citations for the training dataset
  jj[k,"n.seeds"]=n.seeds # number of seeds used in the near-neighboring procedure to identify candidates for screening by human reviewers
  jj[k,"n.candidates"]=n.candidates # number of candidate citations required screening by human reviewers
  jj[k,"n.eligibles"]=n.eligibles # number of eligible citations from the current round of screening
  jj[k,"percent"]=round(100*n.eligibles/n.candidates) # percent eligible citation from the current round of screening
  jj[k,c("precision","recall","f1","accuracy")]=eval # RF or SVM performance measures
  jj[k,c("tp","fp","fn","tn")]=table # breakdown of cells in the 2x2 tables

  eval.parent(substitute(d.results<-jj)) # call by reference https://www.r-bloggers.com/call-by-reference-in-r/ to put the updated results in the d.results
}

### The function extracts some basic evaluation metrics from the model evaluation object 
### produced by the confusionMatrix() function of the caret package
get_eval_measures <- function(model_eval) {
  metrics <- c("Precision", "Recall", "F1", "Accuracy")
  eval_measures <- model_eval$byClass[metrics[1:3]]
  eval_measures <- c(eval_measures, model_eval$overall[metrics[4]])
  eval_measures
}

### Ensemble classification with positive is identified if at least one classifier is positive
ensemble23 = function(l1,l2,l3=NULL,positives=pos.label,negatives=neg.label) { # l1, l2, l3 are the raw predictions from the classifiers
  jout = rep(negatives,length(l1)) # "INCLUDE" if either classifier labeled as "INCLUDE"
  ja=l1 == positives # e.g., RF
  jb=l2 == positives # SVM
  if(length(l3)==0) {
    jc=ja | jb
  }
  else {
    jc=l3 == positives # XGBT
    jc=ja|jb|jc
  }
  jout[jc]=positives # if any classifier is "INCLUDE"
  jout=factor(jout,levels=c(positives,negatives))
  return(jout)
}

## function to convert uncertainty values (some in scientific format e.g., 0.76E-4) into numeric values
scientific_format_to_decimal=function(x) {
  indx=regexpr("[E]",x,ignore.case=TRUE)[1]
  if(indx>=1) {
    num=substr(x,start=1,stop=indx-1);  
    n.exp=substr(x,start=indx+1, stop=nchar(x)); 
    y=as.numeric(num)*10^as.numeric(n.exp); 
  } else y=as.numeric(x)
  return(y)
}

### Extract Concept Unique Identifiers (CUI) from the output of the Annotator RysannMD for each abstract
### See John Cuzzola, Jelena Jovanovic, Ebrahim Bagheri. RysannMD: A biomedical semantic annotator balancing speed and accuracy. 
### Journal of Biomedical Informatics 71 (2017) 91–109
### Input: annotated text from RysannMD and a threshold of uncertainty

extr_cuis <- function(rys_data,certainty.threshold=0.5) {
  ### UMLS CUI coding pattern: UMLS(cui)\":\"C0035647;  
  cui.pattern="UMLS\\(cui\\)\\\":\\\"C[0-9]{7}" # search patterns - noted the "\\(" for the escape of the special character "("
  partial.cui.pattern=16 # number of trailing characters that need to be removed after matching pattern
  
  ### Uncertainty estimate of CUI - Pattern: "uncertainty\":6.1185281268738E-5
  uncertainty.pattern="uncertainty\\\":[0-9\\.E-]*"
  partial.uncertainty.pattern=14  
  op=rys_data
  for(i in c(1:nrow(op))) {
    # extract cui's 
    abst.cuis= rys_data$text[i] %>% str_extract_all(cui.pattern) %>% unlist() %>% str_remove(substr(cui.pattern,start=1,stop=partial.cui.pattern))
    len1=length(abst.cuis)
    
    # extract uncertainty and convert to numeric using a procedure in the UtilityFunctions.R
    abst.uncertainty=rys_data$text[i] %>% str_extract_all(uncertainty.pattern) %>% unlist() %>% str_remove(substr(uncertainty.pattern,start=1,stop=partial.uncertainty.pattern))
    len2=length(abst.uncertainty)
    uncertainty=abst.uncertainty %>% map(scientific_format_to_decimal) %>% unlist()
    certainty = 1-uncertainty # vector of certainty estimates
    
    if(len1!=len2) {
      cat("Not matching for list of CUI's and list of uncertainty estimates for abstract: ",i,"\n")
      absts="NA"
    } else {
      jindx=certainty>=certainty.threshold
      absts=abst.cuis[jindx==TRUE] %>% str_c(collapse=" ") # only kept the cui's with high certainty
    }
    op$text[i]= absts
  }
  return(op)
}

## The function creates a data frame out of the word vectors 
## that originate from a pre-trained GloVe model (m_glove)
get_word_vectors_df <- function(m_glove, verbose = FALSE) {
  
  # initialize space for values and the names of each word in the model
  n_words <- length(m_glove)
  vals <- list()
  names <- character(n_words)
  
  # loop through to gather values and names of each word
  for(i in 1:n_words) {
    if (verbose) {
      if(i %% 5000 == 0) {print(i)}
    }
    this_vec <- m_glove[i]
    this_vec_unlisted <- unlist(strsplit(this_vec, " "))
    this_vec_values <- as.numeric(this_vec_unlisted[-1])  
    this_vec_name <- this_vec_unlisted[1]
    
    vals[[i]] <- this_vec_values
    names[i] <- this_vec_name
  }
  
  # convert the list to a data frame and attach the names
  glove_df <- data.frame(vals)
  names(glove_df) <- names
  
  glove_df
}

## The function computes harmonic mean for the given input vector
harmonicMean <- function(values, precision=2000L) {
  require("Rmpfr")
  valMed <- median(values)
  as.double(valMed - log(mean(exp(-mpfr(values, prec = precision) + valMed))))
}

## The function reads all files from the given folder (infolder) 
## into a data frame and returns the created data frame
read_folder <- function(infolder) {
  data_frame(file = dir(infolder, full.names = TRUE)) %>%
    mutate(text = map(file, read_lines)) %>%
    transmute(id = basename(file), text) %>%
    unnest(text)  # text is a list-column; unnest transforms each element of the list into a row
}

# clearance of previous parallel cluster data in the system
# https://stackoverflow.com/questions/25097729/un-register-a-doparallel-cluster
unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

### End of the Workflow algorithm with Steps 1-11

# Set up for workflow execution - 
# NOTES: These steps are messy right now because they were modified as I tested things out. It will be a few lines once the testing is done

# abstracts with limited content are not used in ML training, for SR type 1 diabetes, n=68
dim(wkta) # 16375 x 5, 16375 - 68 = 16307
length(lda.deleted.abstract.ids)
lda.deleted.abstract.rows=unlist(lapply(lda.deleted.abstract.ids,indx.lkup.all,dd=wkta)) # look up the rows of the deleted abstract ID's
length(lda.deleted.abstract.rows)

# corpus data frame
jj.dd = wkta %>% dplyr::select(id, abstract, status)
dim(jj.dd) # 16375 x 3
jj.dd1= jj.dd[-lda.deleted.abstract.rows,]
dim(jj.dd1) # 16307 x 3

# X matrices
dim(sr.svd.X) # 16375 x 300
length(wkta$id) # matching 16375
sr.svd.X1=sr.svd.X[-lda.deleted.abstract.rows,] # SA - Threshold - Repeat
dim(sr.svd.X1) # 16307 x 300

sr.lda.X1=sr.lda.X.test # LDA
dim(sr.lda.X1) # 16307 x 300 

dim(sr.cui.X) # 16376   500
j.sr.cui.X.ids=as.integer(rownames(sr.cui.X))
jjone=j.sr.cui.X.ids %in% wkta$id
summary(jjone)
j.sr.cui.X.ids[jjone==FALSE] # ID is 662239
which(j.sr.cui.X.ids==662239) # row 178
jj.sr.cui.X=sr.cui.X[-178,]
jj.guest=as.integer(rownames(jj.sr.cui.X))==wkta$id
summary(jj.guest) # equal
sr.cui.X.old=sr.cui.X
sr.cui.X=jj.sr.cui.X # 16375 x 500
sr.cui.X1=sr.cui.X[-lda.deleted.abstract.rows,]
dim(sr.cui.X1) # 16307 x 500

# distance matrices
dim(sr.svd.dist) # 16375 16375 # SA - Threshold - Repeat
sr.svd.dist1=sr.svd.dist[-lda.deleted.abstract.rows,-lda.deleted.abstract.rows]
dim(sr.svd.dist1) # 16307 x 16307

# sr.lda.dist1 = dist2(x=sr.lda.X1, method="cosine")
dim(sr.lda.dist1) #  16307 16307

dim(sr.cui.dist) # 16376 x 16376 
sr.cui.dist.old=sr.cui.dist
sr.cui.dist=sr.cui.dist[-178,-178]
dim(sr.cui.dist) # 16375 16375
sr.cui.dist1=sr.cui.dist[-lda.deleted.abstract.rows,-lda.deleted.abstract.rows]
dim(sr.cui.dist1) # 16307 x 16307 - this is Word Mover Distance

# remove titles only
jtitles.only=which(jj.dd1$abstract=="") # remove rows of titles only 
length(jtitles.only) # 1993 titles only

# corpus data frame
jj.dd2=jj.dd1[-jtitles.only,c(1,3)]
dim(jj.dd2) # 14314 x 2
 
# X matrices
sr.svd.X2=sr.svd.X1[-jtitles.only,]  # SA - Threshold - Repeat
dim(sr.svd.X2) # 14314 x 300
sr.lda.X2=sr.lda.X1[-jtitles.only,]
dim(sr.svd.X2) # 14314 x 300
sr.cui.X2=sr.cui.X1[-jtitles.only,]
dim(sr.cui.X2) # 14314 x 500

# distance matrices
sr.svd.dist2=sr.svd.dist1[-jtitles.only,-jtitles.only] # SA - Threshold - Repeat
dim(sr.svd.dist2) # 14314 x 14314
sr.lda.dist2=sr.lda.dist1[-jtitles.only,-jtitles.only]
dim(sr.lda.dist2) # 14314 x 14314
sr.cui.dist2=sr.cui.dist1[-jtitles.only,-jtitles.only]
dim(sr.cui.dist2) # 14314 x 14314

## Set the row and column names to the sequential order of how the abstracts were arranged in the input file
rownames(sr.svd.dist2)=colnames(sr.svd.dist2)=seq(from=1, to=nrow(sr.svd.dist2),by=1) # SA - Threshold - Repeat
rownames(sr.lda.dist2)=colnames(sr.lda.dist2)=seq(from=1, to=nrow(sr.lda.dist2),by=1)
rownames(sr.cui.dist2)=colnames(sr.cui.dist2)=seq(from=1, to=nrow(sr.cui.dist2),by=1)

## list of seed abstracts 
seed.abstract.ids= c(100000,678127,664273,667053,662836)
jj.seeds.rows=unlist(lapply(seed.abstract.ids,indx.lkup.all,dd=jj.dd2)) # look up the rows of the abstract ID's

## lists of matrices of X and distances
colnames(sr.svd.X2) <- paste0("V",1:ncol(sr.svd.X2)) # SA - Threshold - Repeat
colnames(sr.lda.X2) <- paste0("V",1:ncol(sr.lda.X2)) 
colnames(sr.cui.X2) <- paste0("V",1:ncol(sr.cui.X2)) 

j.m.x=list(svd=sr.svd.X2, text2vec=sr.cui.X2) # use 2 feature representations # SA - Threshold - Repeat
# j.m.x=list(svd=sr.svd.X2, text2vec=sr.cui.X2, lda=sr.lda.X2) # use 3 feature representations

j.m.distance=list(svd=sr.svd.dist2,lda=sr.lda.dist2,text2vec=sr.cui.dist2) # SA - Threshold - Repeat
j.m.distance1=list(svd=sr.svd.dist2,text2vec=sr.cui.dist2)
## 

## Main analysis
start.time.wf <- Sys.time() 
wf.test=workflow.phase1and2(m.x=sr.svd.X2, m.distance=j.m.distance,dd=jj.dd2,l.seeds=seed.abstract.ids,n.rounds=20,n.initial=600,pick.init=8,pick.ml=15)
wf.time12=Sys.time() - start.time.wf
# 
