# make a shortcut from home directory to our group folder 
# file.symlink(from = '/nfs/HealthandResourcePolicy-data', to = 'data')

setwd("~/healthandresourcepolicy/data/Task 1/corpus")

# clear workspace
rm (list =ls())
cat("\014")

# trying to make a document corpus (corpus is a set of documents, 
# document is each a unit in the corpus)
# refs: 
# https://tutorials.quanteda.io/import-data/multiple-files/
# Welbers et al. 2017 

# install.packages("readtext")
library(readtext)
# install.packages("quanteda")
library(quanteda)

# test based on Welbers p. 248 
filepath <- ("/nfs/HealthandResourcePolicy-data/Task 1/corpus/for_R")
# approach from quanteda github 
# ref: https://github.com/quanteda/quanteda_tutorials/blob/master/content/import-data/multiple-files.en.Rmarkdown

# test based on https://tutorials.quanteda.io/import-data/multiple-files/
# information to include in file name: number_org_lang_year_doctype

# test for pdfs 
#test_data <- readtext(paste0(filepath, "/*.pdf"), 
  # docvarsfrom = "filenames", 
  # docvarnames = c("document", "language"),
  # sep = "_") # pdf works for better quality pdfs

# test for word docs 
#test_data <- readtext(paste0(filepath, "/*.docx"), 
  # docvarsfrom = "filenames", 
  # docvarnames = c("document", "language"),
  # sep = "_") 

# test for text documents (this one seems to work the best for our files)
test_data <- readtext(paste0(filepath, "/*.txt"), 
                      docvarsfrom = "filenames", 
                      docvarnames = c("doc_number", "organization", "language","year", "doc_type" ),
                      dvsep = "_") 
str(test_data)
names(test_data) 
encoding(test_data) 

# form a corpus 
# ref: https://tutorials.quanteda.io/basic-operations/corpus/corpus/
test_corpus <- corpus(test_data)
summary(test_corpus) 

# keep corpus as an original reference copy -- do not edit directly 
# make timeline based on quanteda quickstart code 
tokenInfo <- summary(test_corpus)
if (require(ggplot2))
ggplot(data=tokenInfo, aes(x = year, y = Tokens, group = 1)) + geom_line() + geom_point() +
  scale_x_continuous(labels = c(seq(1789, 2017, 12)), breaks = seq(1789, 2017, 12)) +
  theme_bw()

# explore documents
kwic(test_corpus, "santé") # shows "keywords-in-context"

# build out stopwords 
# ref: http://docs.quanteda.io/reference/stopwords.html 
head(stopwords("french")) # built in stopwords 

# manually removing stop words
# ref: https://github.com/quanteda/quanteda/issues/937
stopwords1<-c("a", "plus", "i", "mise", "o", "the", "d’un", "d’une", "entre", "dont","of", 
                "b", "ainsi", "comme", "si", "non", "and", "e", "afin", "á", "r", "x", "tous", 
                "f", "ii", "an", "peu", "donc", "page","p", "in", "rév", "lors","etc")

# corpus --> tokens 
test_tokens <- tokens(test_corpus, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = FALSE, ngrams = 3)

# tokens --> dfm 
# note: if we want a to use bi or trigrams in our analysis we set that when we tokenize and then make than into a dfm 
test_dfm <- dfm(test_tokens, tolower = TRUE, remove = c(stopwords("french"), stopwords1), remove_punct = TRUE)
topfeatures(test_dfm) # picking up errors: i_i_i etc. 

dfm_select(test_dfm, pattern = "é", valuetype = "regex") %>% topfeatures()

nfeat(test_dfm) # number of features

# make a feature co-occurence matrix https://tutorials.quanteda.io/basic-operations/fcm/fcm/
test_dfm_trim <- dfm_trim(test_dfm, min_termfreq = 100)
nfeat(test_dfm_trim) # number of features
test_fcm <- fcm(test_dfm_trim) # r keeps failing when I try to do this
topfeatures(test_fcm)

feat <- names(topfeatures(test_fcm, 50))
test_fcm_trim_select <- fcm_select(test_fcm, pattern = feat)
dim(test_fcm_trim_select)

size <- log(colSums(dfm_select(test_dfm, feat)))
set.seed(144)
textplot_network(test_fcm_trim_select, min_freq = 0.8, vertex_size = size / max(size) * 3)

freq <- textstat_frequency(test_dfm) # biggest issue seems to be e with accent
head(freq, 200)

