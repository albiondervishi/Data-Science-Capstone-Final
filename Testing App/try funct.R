library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)
library(gridExtra)
library(dplyr)
library(lattice)
library(ggplot2)
library(cluster)
library(MASS)

application_test<-read.csv("app_testing.csv")


matching_funtion<- function(dataframe){
  df <- dataframe[, sapply(dataframe, is.numeric)]
  vdiff <- function(x){
    y <- outer(x, x, "==")
    min(abs(y[lower.tri(y)]))
  }
  return(apply(df, 1, vdiff))
}


matching<-matching_funtion(application_test)
matching<-as.factor(matching)
application_test<-cbind(application_test, matching )

summary(application_test)



library(klaR)
partimat(matching ~ ., data=application_test,method="lda")

app_test1<-t.test(application_test$Real.Star, application_test$Test.Star )

########################################################################################


star_approximation<-function( LDL5) {   
  star1<- function(LDL5){for (i in 1:length(LDL5))  
    if( is.na(LDL5[i])) { }  
    else if (LDL5[i] ==2 | LDL5[i] ==1 ) { LDL5 <-1
    }   
    LDL5
  }  
  star_one<-sapply(LDL5, star1)
  star2<- function(LDL5){for (i in 1:length(LDL5))  
    if( is.na(LDL5[i])) { } 
    else if (LDL5[i] ==3 | LDL5[i] ==1 | LDL5[i] ==2) { LDL5 <- 2
    }  
    LDL5
  }  
  star_two<-sapply(LDL5, star2)
  star3<- function(LDL5){for (i in 1:length(LDL5))  
    if( is.na(LDL5[i])) { } 
    else if (LDL5[i] ==4| LDL5[i] ==2 | LDL5[i] ==3) { LDL5 <- 3
    }   
    LDL5
  }
  star_three<-sapply(LDL5, star3)
  star4<- function(LDL5){for (i in 1:length(LDL5))  
    if( is.na(LDL5[i])) { } 
    else if (LDL5[i] ==4 | LDL5[i] ==3 | LDL5[i] ==5) { LDL5<- 4
    }  
    LDL5
  }
  star_four<-sapply(star_two, star4)
  star5<- function(LDL5){for (i in 1:length(LDL5))  
    if( is.na(LDL5[i])) { } 
    else if ( LDL5[i] ==4 |LDL5[i] ==5) { LDL5 <- 5
    }  
    LDL5
  } 
  star_five<-sapply(star_one, star5)
  star_aprox<-c(star_three,  star_four, star_five)
  star_aprox
}



Real.Star_approx<-star_approximation(application_test$ Real.Star)
Test.Star_approx<-star_approximation(application_test$ Test.Star)
aplication_test_approx<-cbind(Real.Star_approx, Test.Star_approx)
aplication_test_approx<-as.data.frame(aplication_test_approx)

matching_funtion<- function(dataframe){
  df <- dataframe[, sapply(dataframe, is.numeric)]
  vdiff <- function(x){
    y <- outer(x, x, "==")
    min(abs(y[lower.tri(y)]))
  }
  return(apply(df, 1, vdiff))
}

matching_approx<-matching_funtion(aplication_test_approx)
matching_approx<-as.factor(matching_approx)
aplication_test_approx<-cbind(aplication_test_approx, matching_approx)

summary(aplication_test_approx)

par(mfrow=c(1,2))
partimat(matching_approx ~ ., data=aplication_test_approx, method="lda")
partimat(matching ~ ., data=application_test,method="lda")




app_test_approx<-t.test(aplication_test_approx$Test.Star_approx,  aplication_test_approx$Real.Star_approx )

################

ctrl <- trainControl(method = "cv")
model_lda <- train(matching ~ ., method = "rf", trControl = ctrl, data=application_test)


star_four<-sapply(LDL5, star4)
star5<- function(LDL5){for (i in 1:length(LDL5))  
  if( is.na(LDL5[i])) { } 
  else if ( LDL5[i] ==4 |LDL5[i] ==5) { LDL5 <- 5
  }  
  LDL5
} 
star_five<-sapply(LDL5, star5)
