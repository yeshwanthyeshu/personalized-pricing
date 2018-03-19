# personalised pricing using amazon:
# source of data: http://jmcauley.ucsd.edu/data/amazon/
# STEP 1: GETTING RECOMMENDATIONS
#############################################################################
# setting the working directory

#############################################################################
# loading the data
data <- read.csv('amazon.csv',stringsAsFactors = FALSE)

#############################################################################
## extracting the reviews column
user_rev <- data$customer_reviews
#############################################################################
# DATA MINING FOR USER REVIEWS:
library(tm)
library(qdap)

## function to clean the reviews with tm package
basic_clean <- function(user_rev){
  user_rev <- removePunctuation(user_rev)
  user_rev <- removeNumbers(user_rev)
  user_rev <- stripWhitespace(user_rev)
  return(user_rev)
}
user_rev <- basic_clean(user_rev)

# to convert all special charecters with their transalation
user_rev <- iconv(user_rev, to = "ASCII//TRANSLIT")

# to remove all stopwords in engilsh
user_rev <- removeWords(user_rev,stopwords('en'))

# function to clean reviews with qdap library
clean_qdap <- function(user_rev){
  user_rev <- bracketX(user_rev)
  user_rev <- replace_number(user_rev)
  user_rev <- replace_abbreviation(user_rev)
  user_rev <- replace_contraction(user_rev)
  user_rev <- replace_symbol(user_rev)
  user_rev <- removeWords(user_rev,stopwords('en'))
  user_rev <- gsub("\\W*\\b\\w\\b\\W*", " ", user_rev)
  user_rev <- removePunctuation(user_rev)
  return(user_rev)
}
user_rev <- clean_qdap(user_rev)

# to convert all capital to lower case
user_rev <- tolower(user_rev)

# cleaning by converting to Vector Corpus
rev_sr <- VectorSource(user_rev)
rev_cp <- VCorpus(rev_sr)

# function to clean the corpus
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
  return(corpus)
}
rev_cl <- clean_corpus(rev_cp)

#############################################################################
### BIWORDS FREQUENCY:
### extracting the feature words i.e bi words 
library(RWeka)

# making the tokenizer function
tokenizer <- function(x) 
  NGramTokenizer(x, Weka_control(min = 2, max = 2))

rev_f_tdm <- TermDocumentMatrix(
  rev_cl,
  control = list(tokenize = tokenizer)
)

# making a sparse terms matrix
sparsmat <- removeSparseTerms(rev_f_tdm,sparse = 0.99)
rev_f_m <- as.matrix(sparsmat)
rev_freq <- rowSums(rev_f_m)
rev_freq <- sort(rev_freq,decreasing = TRUE)
head(rev_freq)

# taking all biwords generated
totalbiwords <- totalbiwords <- rownames(rev_f_m)

# taking only high frequency biwords
topbiwords <- names(head(rev_freq,20))

# calculating the indeces of these top biwords in rev_f_m matrix
indtopbiwords <- which(totalbiwords %in% topbiwords)

# getting the matrix for these topbiwords
topmat <- rev_f_m[indtopbiwords,]
transtopmat <- t(topmat)
# transtopmat is products and their bi key words present or not

#############################################################################
# extracting the products and the ratings data frames
products <- subset(data, select = c(uniq_id,product_name,
                                    price,customer_reviews))

# adding column of prodit percentage of that product
products$profit_percent <- round(runif(nrow(products),4.8,6.0),2)


# making the prices to correct form i.e removing first three to six characters
prices_correct_form <- as.numeric(substr(products$price,3,6))
products$price <- prices_correct_form

# replacing NA with mean of the price
mean_price <- mean(products$price,na.rm = TRUE)
products$price[which(is.na(products$price))] <- mean_price 


ratings <- subset(data,select = c(uniq_id,average_review_rating))

#############################################################################
### ratings data frame processing
# extracting the actual rating for a product in numeric form
ratings$average_review_rating <- as.numeric(substr(ratings$average_review_rating,1,3))

# add user_id columns to the ratings, as users column is not present in data set
# we will assume 100 users 
users <- sample(1:100,nrow(ratings),replace = TRUE)
ratings$user_id <- users

# changing the colnames 
colnames(ratings) <- c("product_id","rating","user_id")

## finding all na is rating column in ratings data frame and replace with 2
which(is.na(ratings$rating))

#############################################################################
## method : User review based score recommendations
# this method recommends by  calculating square
# first user recommendations using his score for each bi key word

ratings_without_na <- ratings
ratings_without_na[which(is.na(ratings_without_na$rating)),]$rating <- 0
which(is.na(ratings_without_na))

## makign score_ratings i.e each user with review given product with 1 else 0
library(data.table)
score_ratings <- dcast(ratings_without_na, product_id~user_id, value.var = "rating", na.rm=FALSE)

for( i in 1: ncol(score_ratings)){
  score_ratings[which(is.na(score_ratings[,i]) == TRUE),i] <- 0
}

score_ratings <- score_ratings[,-1]

uniq_users <- length(unique(ratings$user_id))
score_result = matrix(0,20,uniq_users) # here 20 is top bi key words and unique users are 100

############################## to do:
## Calculate dot product for User Profiles
## for a user a product is liked then the products bi keywords are summed
## how much a user likes a bi key word
for (c in 1:ncol(score_ratings)){
  for (i in 1:ncol(transtopmat)){
    score_result[i,c] <- sum( (transtopmat[,i])
                              * (score_ratings[,c]) ) #ratings per genre
  }
}

#############################################################################
## making function for recommended products by score:
recommend_score <- function(value){
  scorelist <- c()
  for(i in 1:nrow(transtopmat)){
    tempscore <- sum(transtopmat[i,] * score_result[,value])
    scorelist <- c(scorelist,tempscore)
  }
  scoredf <- data.frame(scorelist , c(1:nrow(transtopmat)))
  colnames(scoredf) <- c('score','productId')
  attach(scoredf)
  sortscoredf <- scoredf[order(-score),]
  ind <- (head(sortscoredf)$productId)
  # recommendations
  recommended_products_score <- products[ind,]
  recommended_products_score
}

#recom<- recommend_score(1)
## RECOMMENDATIONS ARE COMPLETED:

#############################################################################
## STEP: MAKING DISCOUNT ON THE RECOMMENDED PRODUCTS BASED ON USER PREVIOUS
## PURCHASES.

 
#######################################################################
## Making function for the discount prices:
  
# making the threshold
treshold_percentage <- 5

#############################

personlized_price <- function(val){
  recom <- recommend_score(val)
  ## getting the products purchased by userid : 'val'
  produsts_by_val_index <- which(ratings$user_id == val)
  products_by_val <- products[produsts_by_val_index,]
  ## products_by_val are the products purchased or rated by userid 'val

    
  #checking whether to give discout or not
  library(dplyr)
  products_by_val <- mutate(products_by_val, moreprofit = price*(profit_percent -5)*0.01)
  totalfromuserval <- sum(products_by_val$moreprofit)
  ## procuts by the user val is ready
  
  ## framing the base condition i.e to check whether to give recommendations or not
  ## if totalfromuserval is > 0  we can give discouts or else no
  if(totalfromuserval > 0){
    for( i in 1:nrow(recom)){
      if(recom$profit_percent[i] < treshold_percentage){
        recom$discoutprice[i] <- recom$price[i] 
      }else{
        recom$discoutprice[i] <- recom$price[i]*(1-((recom$profit_percent[i]-treshold_percentage)*0.01))
      }
    }
    
  }else{
    print("We already have been ultimately dicounted that user")
    print("no additional discout to be given")
  }
  
  recom
}
out <- personlized_price(2)
out
#####################################################################
## its time for some UI type result:
visualizerecommend()
# function to visualize UI shiny
visualizerecommend <- function(){
  
  library(shiny)
  
  
  # Define UI for application that plots features of movies
  ui <- fluidPage(
    
    # Sidebar layout with a input and output definitions
    sidebarLayout(
      
      # Inputs
      sidebarPanel(
        n_total <- 100,
        # Text instructions
        HTML(paste("Enter a value i.e userid between 1 and",n_total,"to get recommmends")),
        
        # Numeric input for sample size
        numericInput(inputId = "n",
                     label = "User id:",
                     min = 1,
                     max = 100,
                     value = 2,
                     step = 1)
        
      ),
      
      # Output: Show data table
      mainPanel(
        DT::dataTableOutput(outputId = "recommendstable")
      )
    )
  )
  
  
  server <- function(input, output) {
    
    # Create data table
    output$recommendstable <- DT::renderDataTable({
      req(input$n)
      out <- personlized_price(input$n)
      out$customer_reviews <- NULL
      DT::datatable(data = out, 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    })
    
  }
  
  # Create a Shiny app object
  shinyApp(ui = ui, server = server)
  
}
