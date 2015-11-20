#-----------------------------------------------------------------------------------------------------
# This r script is final product of Data Science Capstone Class, we may also this work submit to the 
# Yelp dataset challenge Round 6. The script include function to generate semantic & n grams data 
# for star prediction. 

#------------------------------------------------------------------------------------------------------

# review<-'yelp_academic_dataset_review.json'
# review <- stream_in(file(review))
# save(review,file='review.RData')

# business.file <- 'yelp_academic_dataset_business.json'
# business <- stream_in(file(business.file))
# save(business,file='business.RData')
# rm(business)

# checkin<- 'yelp_academic_dataset_checkin.json'
# checkin <- stream_in(file(checkin))
# save(checkin,file='checkin.RData')
# rm(checkin)

# tip<- 'yelp_academic_dataset_tip.json'
# tip <- stream_in(file(tip))
# save(tip,file='tip.RData')
# rm(tip)

# user<- 'yelp_academic_dataset_user.json'
# user <- stream_in(file(user))
# save(user,file='user.RData')
# rm(user)

####################################################################################################
#Can you add any external data that might improve your predictions?
#In this project we used as external data the dictionary that is provided by Hu and Liu (2004) and Liu et al. (2005). 
# https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#datasets (last downloaded)
#It consists two lists, both of several thousand positive and negative terms. (Opinion-Lexicon-English)

positive_words <- readLines("positive-words.txt")
positive_words <- positive_words[!str_detect(positive_words, "ˆ;")]
positive_words <- positive_words[1:length(positive_words)]
positive_words <- stemDocument(positive_words, language = "english")
positive_words <- positive_words[!duplicated(positive_words)]

negative_words <- readLines("negative-words.txt")
negative_words <- negative_words[!str_detect(negative_words, "ˆ;")]
negative_words <- negative_words[1:length(negative_words)]
negative_words<- stemDocument(negative_words, language = "english")
negative_words<- negative_words[!duplicated(negative_words)]

#--------------------------------------------------------------------------------------------------------------------------------
# list of Englsh adverbs list(last downloaded)
# Rated manually: Positive  4.24 and negative 1.75
# source: http://www.free-db.com/tag/English
adverbs<-read.csv("d1a98f1482e9594047030cfae6953031_22_csv_download.csv") 


inputfunction<-function(text){ 
  corpus <- VCorpus(VectorSource(text))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, PlainTextDocument)
  
  trigram_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
  tdm_tri <- TermDocumentMatrix(corpus, control = list(tokenize = trigram_tokenizer))
  triGrams_m <- as.matrix(removeSparseTerms(tdm_tri, 0.99999925))
  triGrams_freq <- sort(rowSums(triGrams_m), decreasing = TRUE)
  triGramsDF <- data.frame(word = names(triGrams_freq), freq = as.integer(triGrams_freq))
  triGramsDF
}

adverbs<-inputfunction(adverbs)

# Extracting positive adverbs
adv<-adverbs$word %in% positive_words
positive_adverbs<-adverbs[adv,]
len1<-nrow(positive_adverbs)
star<-c(rep(4.25, len1))
positive_adverbs<-cbind(positive_adverbs , star)
save(positive_adverbs,file='positive_adverbs.RData')
load('positive_adverbs.RData')

adv<-adverbs_n$word %in% negative_words
negative_adverbs<-adverbs_n[adv,]
len1<-nrow(negative_adverbs)
star<-c(rep(1.75, len1))
negative_adverbs<-cbind(negative_adverbs , star)
save(negative_adverbs,file='negative_adverbs.RData')
load('negative_adverbs.RData')

# It was nessecary for one gram predction to add also negating verbs, this I have created manually.
# The only ARTIFCAL RATES I HAVE DONE IN THE PROJECT BELOW SELECTIVE VALUES
negating_verbs<-read.csv("negating a verb1.csv")

####################################################################################################7###


sentiment_star_rate<-function(data_main){ 
  # Partition the main data from yelp academic dataset review file.
  nRow_data <- nrow(data_main)
  k<-7 # Partition of the file in 7 different data frame
  partition_size<- as.integer(nRow_data/k)
  
  spliting_data<- function(data, k) {for (i in 1:k ) 
    j <- (i-1)*partition_size+1
    l <- i*partition_size
    partition_data <- data[j:l, ]
    partition_data
  }
  
  review1<-spliting_data(data_main,1)
  review2<-spliting_data(data_main,2)
  review3<-spliting_data(data_main,3)
  review4<-spliting_data(data_main,4)
  review5<-spliting_data(data_main,5)
  review6<-spliting_data(data_main,6)
  review7<-spliting_data(data_main,7)
  
  
  sentiment<-function(data){   
    #Extracted  review text, star review and review id. 
    streview<-data.frame(data[ ,4], data[ ,6], data[ ,2])
    names(streview)<-c("stars","text", "user_id")
    streview<- streview[ streview$stars==s, ]
    
    # Removal of the  extra space, punctuation, lower and uppercase and the text articles.
    text<-streview[ ,2]
    textfun <- function(text, lowercase=TRUE, numbers=TRUE, punctuation=TRUE, spaces=TRUE){
      text=iconv(text,to="ASCII",sub="")
      if (lowercase)
        text=tolower(text)
      if (numbers)
        text=removeNumbers(text)
      if (punctuation)
        text=removePunctuation(text)
      if (spaces) 
        (text=stripWhitespace(text)) 
      text
    }
    # n-gram tokenization for each review text.
    text<-textfun(text) 
    options(mc.cores=1)
    NgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = p, max = q ,delimiters = " \\r\\n\\t.,;:\"()?!"))
    dtm.streview.ngram<- DocumentTermMatrix(VCorpus(VectorSource(text), readerControl=list(language="en")), control=list(tokenize = NgramTokenizer, 
                                                                                                                         wordLengths=c(3,Inf), tolower=FALSE,stemming=TRUE,bounds=list(global=c(5,Inf)),stopwords=TRUE )) 
    user_id<-streview[ ,3]
    rownames(dtm.streview.ngram) <-user_id 
    
    doc.freq <- rollup(dtm.streview.ngram,2,FUN=sum)
    is.empty <- as.vector(doc.freq ==0)
    dtm.streview.ngram <- dtm.streview.ngram[!is.empty,]
    # Implementation of the Latent Dirichlet model
    lda_out <- LDA(dtm.streview.ngram, 2)
    result<-terms(lda_out, 150)
    
    word.result <- data.frame(word=colnames(result),word.freq=matrix(result), stringsAsFactors = FALSE)
    
    words<-word.result$ word.freq
    freq1<-table(words)
    data1<-sapply(freq1, MARGIN=1, FUN=sum)
    freq1 <- sort(rowSums(as.matrix(data1)), decreasing=TRUE)
    wf1 <- data.table(terms=names(freq1), freq=freq1)
    len1<-nrow(wf1)
    star<-c(rep(s, len1))
    data<-cbind(wf1 , star)
    #data<-data[data$terms   %in% dictationary,]; # ENABLE when you want to work with ONE_GRAM_SEMANTIC_DATA
    data
  }
  
  
  sentiment_star_rate1<-sentiment(review1)
  sentiment_star_rate2<-sentiment(review2)
  sentiment_star_rate3<-sentiment(review3)
  sentiment_star_rate4<-sentiment(review4)
  sentiment_star_rate5<-sentiment(review5)
  sentiment_star_rate6<-sentiment(review6)
  sentiment_star_rate7<-sentiment(review7)
  
  
  sentiment_star_rate<-rbind(sentiment_star_rate1, sentiment_star_rate2, sentiment_star_rate3, 
                             sentiment_star_rate4, sentiment_star_rate5, sentiment_star_rate6, 
                             sentiment_star_rate7)
  
  
  sentiment_star_rate
  
}

s<-2 # Stars assigned as (1:5), depend which star you desire to calculate
p<-3 #min selected words (1-unigram, 2-bigrams, 3-threegrams, 4-quadragrams)
q<-3 #max selected words (1-unigram, 2-bigrams, 3-threegrams, 4-quadragrams, 5-pentagrams, 6-hexagrams)

# ENABLE when you want to work with ONE_GRAM_SEMANTIC_ANALYSIS
#dictationary<-positive_words     # "positive_words"  or "negative_words"
#-----------------------------------------------------------------------------------------------------------------------------#

# ONE GRAM SEMANTIC ANALYSIS DATA
negative_sentiment_onegram_star1<-sentiment_star_rate(review)
negative_sentiment_onegram_star2<-sentiment_star_rate(review)
positive_sentiment_onegram_star4<-sentiment_star_rate(review)
positive_sentiment_onegram_star5<-sentiment_star_rate(review)

sentiment_onegram<-rbind(negative_sentiment_onegram_star1,negative_sentiment_onegram_star2,
                         positive_sentiment_onegram_star4,positive_sentiment_onegram_star5, 
                         positive_adverbs, negative_adverbs, negating_verbs)

save(sentiment_onegram,file='sentiment_onegram.RData')

#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

# THREE GRAMS SEMANTIC ANALYSIS DATA

sentiment_threegram_star1<-sentiment_star_rate(review)
sentiment_threegram_star2<-sentiment_star_rate(review)
sentiment_threegram_star4<-sentiment_star_rate(review)
sentiment_threegram_star5<-sentiment_star_rate(review)

sentiment_threegram_star1<- sentiment_threegram_star1[!duplicated(sentiment_threegram_star1$terms)]
sentiment_threegram_star2<- sentiment_threegram_star2[!duplicated(sentiment_threegram_star2$terms)]
sentiment_threegram_star4<- sentiment_threegram_star4[!duplicated(sentiment_threegram_star4$terms)]
sentiment_threegram_star5<- sentiment_threegram_star5[!duplicated(sentiment_threegram_star5$terms)]

s<-5 # Stars assigned as (1:5), depend which star you desire to calculate
semantic_match_3grams<-function(input, semantic_onegram_data) { 
  freq1<-table(input)
  data<-sapply(freq1, MARGIN=1, FUN=sum)
  freq1 <- sort(rowSums(as.matrix(data)), decreasing=TRUE)
  wf1<- data.table(terms=names(freq1), freq=freq1)
  len1<-nrow(wf1)
  star<-c(rep(s, len1))
  data<-cbind(wf1 , star)
  data<- data[!duplicated(data$terms)]
  v1=rnorm(len1)
  attach(data)
  data<-data.frame(v1, terms,terms, freq, star )
  data$terms<- as.character(data$terms) 
  detach(data)
  rows <- apply(data[ ,2:3],  1, function(x) any(sapply(semantic_onegram_data, grepl, x=x)))
  final<-data[rows, ]
  final
}
#--------------------------------------------------------------------------------------------------------------#
# MATCHING THREE GRAMS SEMANTIC STAR ONE
#gram one negative for star ONE matching three grams star ONE semantic terms
names(gram_one_negativeS1)<-c("terms","freq", "star")
names(gram_one_negativeS1)<-c("terms","freq", "star")
gram_one_negativeS1<-rbind(neg_sentiment_star1_rate, negative_adverbs, negating_verbs)
gram_one_negativeS1<- gram_one111[!duplicated(gram_one111$terms)]
save(gram_one_negativeS1,file='gram_one_negativeS1.RData')
#---------------------------------------------------------------------------------------------------------------#
matched_sentiment_threegram_star1<-semantic_match_3grams(sentiment_threegram_star1$terms, gram_one_negativeS1$terms )
save(matched_sentiment_threegram_star1,file='matched_sentiment_threegram_star1.RData')
#================================================================================================================#
# MATCHING THREE GRAMS SEMANTIC STAR TWO
#gram one negative for star TWO matching three grams star TWO semantic terms
names(gram_one_negativeS2)<-c("terms","freq", "star")
names(gram_one_negativeS2)<-c("terms","freq", "star")
gram_one_negativeS2<-rbind(neg_sentiment_star2_rate, negative_adverbs, negating_verbs)
gram_one_negativeS2<- gram_one111[!duplicated(gram_one111$terms)]
save(gram_one_negativeS2,file='gram_one_negativeS2.RData')
#------------------------------------------------------------------------------------------------------------------#

matched_sentiment_threegram_star2<-semantic_match_3grams(sentiment_threegram_star2$terms, gram_one_negativeS2$terms)
save(matched_sentiment_threegram_star2,file='matched_sentiment_threegram_star2.RData')
#===================================================================================================================#
# MATCHING THREE GRAMS SEMANTIC STAR FOUR
#gram one positive for star FOUR matching three grams star FOUR semantic terms
names(pos_sentiment_star4_rate)<-c("terms","freq", "star")
names(positive_adverbs)<-c("terms","freq", "star")
gram_one_positiveS4<-rbind(pos_sentiment_star4_rate, positive_adverbs)
gram_one_positiveS4<- gram_one_positiveS4[!duplicated(gram_one_positiveS4$terms)]
save(gram_one_positiveS4,file='gram_one_positiveS4.RData')
#--------------------------------------------------------------------------------------------------------------------#

matched_sentiment_threegram_star4<-semantic_match_3grams(sentiment_threegram_star4$terms, gram_one_positiveS4$terms)
save(matched_sentiment_threegram_star4,file='matched_sentiment_threegram_star4.RData')

#====================================================================================================================#

# MATCHING THREE GRAMS SEMANTIC STAR FIVE
#gram one positive for star FIVE matching three grams star FIVE semantic terms
names(pos_sentiment_star5_rate)<-c("terms","freq", "star")
names(positive_adverbs)<-c("terms","freq", "star")
gram_one_positiveS5<-rbind(pos_sentiment_star5_rate, positive_adverbs)
gram_one_positiveS5<- gram_one_positiveS5[!duplicated(gram_one_positiveS4$terms)]
save(gram_one_positiveS5,file='gram_one_positiveS5.RData')
#---------------------------------------------------------------------------------------------------------------------#

matched_sentiment_threegram_star5<-semantic_match_3grams(sentiment_threegram_star5$terms, gram_one_positiveS5$terms )
save(matched_sentiment_threegram_star5,file='matched_sentiment_threegram_star5.RData')

#======================================================================================================================#
sentiment_threegram_star<-rbind(matched_sentiment_threegram_star1, matched_sentiment_threegram_star2,
                                matched_sentiment_threegram_star4, matched_sentiment_threegram_star5)

save(sentiment_threegram_star,file='sentiment_threegram_star.RData')
load( 'sentiment_threegram_star.RData')

#=======================================================================================================================#
#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

# FOUR SIX GRAMS SEMANTIC ANALYSIS DATA
sentiment_four_six_gram_star1<-sentiment(review)
sentiment_four_six_gram_star2<-sentiment(review)
sentiment_four_six_gram_star3<-sentiment(review)
sentiment_four_six_gram_star4<-sentiment(review)
sentiment_four_six_gram_star5<-sentiment(review)

sentiment_four_six_grams<-rbind(sentiment_four_six_gram_star1,sentiment_four_six_gram_star2,
                                sentiment_four_six_gram_star3,sentiment_four_six_gram_star4,
                                sentiment_four_six_gram_star5)

save(sentiment_four_six_grams,file='sentiment_four_six_grams.RData')

load("sentiment_onegram.RData")
load("sentiment_four_six_grams.RData")
load('sentiment_threegram_star.RData')

#################################################################################################################################

