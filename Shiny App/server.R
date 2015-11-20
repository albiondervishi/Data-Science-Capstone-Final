require(shiny)
library(parallel)
library(RWeka)
library(lda)
library(plyr)
library(topicmodels)
library(caret)
library(slam)
library(igraph)
library(wordcloud)
library("RColorBrewer")
library(segmented)
library(grid)
require(stringr)
require(R.utils)
require(tidyr)
library("igraphtosonia")
library(RWekajars)
library(qdapDictionaries)
library(googleVis)
library(mallet)
library(RGraphics)
library(gridExtra)
library(NLP)
library(tm)
library(stringr)
library("jsonlite")
library(data.table)
library(googleVis)

load("sentiment_four_six_grams.RData")
load("sentiment_onegram.RData")
load("sentiment_threegram_star.RData")



shinyServer(function(input, output) {
  
  dataInput <- reactive(inputfunction(input$entry))
  
  inputfunction<-function(text){ 
    corpus <- VCorpus(VectorSource(text))
    corpus<- tm_map(corpus, removeNumbers)
    corpus<- tm_map(corpus, removePunctuation)
    corpus<- tm_map(corpus, stripWhitespace)
    corpus<- tm_map(corpus, content_transformer(tolower))
    corpus<- tm_map(corpus, PlainTextDocument)
    
    n_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 6))
    tdm_one_six <- TermDocumentMatrix(corpus, control = list(tokenize = n_tokenizer))
    one_six_Grams<- as.matrix(removeSparseTerms(tdm_one_six, 0.999925))
    one_six_Grams <- sort(rowSums(one_six_Grams), decreasing = TRUE)
    one_six_Grams <- data.frame(word = names(one_six_Grams), freq = as.integer(one_six_Grams))
    d<-sentiment_four_six_grams[sentiment_four_six_grams$terms   %in%  one_six_Grams$ word];
    
    one_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
    tdm_one <- TermDocumentMatrix(corpus, control = list(tokenize = one_tokenizer))
    one_Grams<- as.matrix(removeSparseTerms(tdm_one, 0.999925))
    one_Grams <- sort(rowSums(one_Grams), decreasing = TRUE)
    one_Grams <- data.frame(word = names(one_Grams), freq = as.integer(one_Grams))
    sentiment_star_rate<-sentiment_onegram[sentiment_onegram$terms  %in% one_Grams$ word, ];
    
    three_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
    tdm_three <- TermDocumentMatrix(corpus, control = list(tokenize = three_tokenizer))
    three_Grams<- as.matrix(removeSparseTerms(tdm_three, 0.999925))
    three_Grams <- sort(rowSums(three_Grams), decreasing = TRUE)
    three_Grams <- data.frame(word = names(three_Grams), freq = as.integer(three_Grams))
    
    sentiment<-sentiment_threegram_star$terms.1 %in% three_Grams$word
    three_semantic_rate<-sentiment_threegram_star[sentiment, ]
    
    starate<-mean(rbind(d$star, sentiment_star_rate$star,  three_semantic_rate$star))
    
    star_approx<- function(LD){for (i in 1:length(LD))
      if( is.na(LD[i])) { } 
      else if (LD[i] <1.6) { LD <- 1
      } else if (LD[i] <2.6) { LD <- 2
      } else if (LD[i] <3.6) { LD <- 3 
      } else if (LD[i] <4.5) { LD<- 4
      } else if (LD[i] >=4.5) { LD <- 5
      } 
      LD
    }
    
    stars<-star_approx(starate)
    stars
    
  }
  
  
  output$view <- renderGvis({
    M <- matrix(nrow=dataInput(),ncol=dataInput())
    M[col(M)==row(M)] <- 1:dataInput()
    dat <- data.frame(X=1:dataInput(), M)
    
    gvisScatterChart(dat, 
                     options=list(
                       title=" ",
                       legend="none",
                       pointSize=77, 
                       vAxis="{gridlines:{color:'none', count:0}}",
                       hAxis="{gridlines:{color:'none', count:0}}",
                       series="{
                              0: { pointShape: 'star', color:'gold' },
                              1: { pointShape: 'star',color:'gold' },
                              2: { pointShape: 'star',color:'gold' },
                              3: { pointShape: 'star', color:'gold' },
                              4: { pointShape: 'star',color:'gold' },
                              
                              }"))
  })
  
  
})