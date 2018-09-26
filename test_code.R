# make a shortcut from home directory to our group folder 
file.symlink(from = '/nfs/HealthandResourcePolicy-data', to = 'data')

setwd("/nfs/HealthandResourcePolicy-data/Task 1/corpus")

# clear workspace
rm (list =ls())
cat("\014")

# trying to make a document corpus (corpus is a set of documents, 
# document is each a unit in the corpus)
# refs: 
# https://tutorials.quanteda.io/import-data/multiple-files/
# Welbers et al. 2017 

install.packages("readtext")
library(readtext)
install.packages("quanteda")
library(quanteda)

# test based on Welbers p. 248 
filepath <- ("~/Dropbox (MIT)/Research Projects/SESYNC/text_analysis/corpus/files")
# approach from quanteda github 
# https://github.com/quanteda/quanteda_tutorials/blob/master/content/import-data/multiple-files.en.Rmarkdown

# test based on https://tutorials.quanteda.io/import-data/multiple-files/
# information to include in file name: documentname_number_org_lang_year_doctype

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

encoding(test_data) # not all UTF-8, try to fix this

# https://stackoverflow.com/questions/9511281/handling-special-characters-e-g-accents-in-r
test_data$text <- iconv(test_data$text, to="UTF-8")
encoding(test_data)

# https://stringr.tidyverse.org/reference/str_conv.html
# install.packages("stringr")
library(stringr)
str_conv(test_data$text, "UTF-8")

encoding(test_data)

# form a corpus (per https://tutorials.quanteda.io/basic-operations/corpus/corpus/)
test_corpus <- corpus(test_data)
summary(test_corpus) # should we take out the document names? it's making the summary pretty illegible 

# keep corpus as an original reference copy -- do not edit directly 

# make timeline based on quanteda quickstart code 
tokenInfo <- summary(test_corpus)
if (require(ggplot2))
  ggplot(data=tokenInfo, aes(x = year, y = Tokens, group = 1)) + geom_line() + geom_point() +
  scale_x_continuous(labels = c(seq(1789, 2017, 12)), breaks = seq(1789, 2017, 12)) +
  theme_bw()

# explore documents
kwic(test_corpus, "santé") # shows "keywords-in-context"

head(stopwords("french")) # built in stopwords 
# http://docs.quanteda.io/reference/stopwords.html  -- test this multiple times 

# corpus --> tokens 
test_tokens <- tokens(test_corpus, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = FALSE, ngrams = 3)
head(test_tokens[[1]], 100) # this line doesn't work anymore 

# test removing stopwords 
tokens_remove(test_tokens, stopwords("french")) # split up english and french documents
head(test_tokens[[1]], 100) 

# tokens --> dfm 
# note: if we want a to use bi or trigrams in our analysis we set that when we tokenize and then make than into a dfm 
test_dfm <- dfm(test_tokens, tolower = TRUE, remove = stopwords("french"), remove_punct = TRUE)
topfeatures(test_dfm)

dfm_select(test_dfm, pattern = "é", valuetype = "regex") %>% topfeatures()

freq <- textstat_frequency(test_dfm) # biggest issue seems to be e with accent
head(freq, 50)

