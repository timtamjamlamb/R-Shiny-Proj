library(shiny)
library(stopwords)
library(tokenizers)
library(SnowballC)
library(tidyverse)
library(syn)
library(wordcloud)
library(tm)

set.seed(308958428)
data <- read.csv("~/R/data399/w6p/personal.csv")
data = data[-c(3, 37,62), ]

server <- function(input, output) {

  ### naive code
  data[data$class == 1, ] <- data[c(sample(1:nrow(data[data$class == 1, ]), table(data$class)[2], replace = F), rep(NA, nrow(data[data$class == 1, ])-table(data$class)[2])), ]
  data = na.omit(data)
  init_me <- (table(data$class) / sum(table(data$class)))[1]
  init_class <- (table(data$class) / sum(table(data$class)))[2]
  init_instructor <- (table(data$class) / sum(table(data$class)))[3]
  calc_Probs <- function(tokens) {
    counts <- table(unlist(tokens)) + 1
    (counts/sum(counts))
  }
  me_data <- subset(data, class == 1)
  class_data <- subset(data, class == 2)
  instructor_data <- subset(data, class == 3)
  me_probs <- calc_Probs(me_data$word)
  class_probs <- calc_Probs(class_data$word)
  instructor_probs <- calc_Probs(instructor_data$word)


  classify_sentiment <- function(input) {
    test <- unlist(tokenize_words(input,
                                  lowercase = TRUE,
                                  strip_punct = TRUE,
                                  strip_numeric = TRUE,
                                  stopwords = stopwords::stopwords("en")))

    me_pred <- init_me * ifelse(is.na(prod(me_probs[test])), 1, prod(me_probs[test]))
    class_pred <- init_class * ifelse(is.na(prod(class_probs[test])), 1, prod(class_probs[test]))
    instructor_pred <- init_instructor * ifelse(is.na(prod(instructor_probs[test])), 1, prod(instructor_probs[test]))

    if ( me_pred > class_pred & me_pred > instructor_pred) {
      paste("The classmates felt this way about the course")
    } else if ( class_pred > me_pred & class_pred > instructor_pred) {
      paste("The classmates believed other classmates felt this way about the course")
    } else if ( instructor_pred > me_pred & instructor_pred > class_pred) {
      paste("The classmates believed the instructors felt this way about the course")
    } else {
      paste("NA, also this probably isn't a word or No classmates felt this way.")
    }
  }
  # 25 words list
  word_function <- function(word = "happy"){
    x = syns(word, n_words = 50)
    result <- lapply(x, function(x) grep("\\s", x))
    for (i in result){
      x = x[[1]][-i]
    }
    x = x[1:25]
    x
  }


  #dictionary of word and word list.
  rv <- reactiveValues(word = "happy",
                       list = word_function("happy"))

  observeEvent (input$action , {
    rv$word <- input$word
    rv$list <- word_function(input$word)
  })




  ### naive print

  output$naive_title <- renderText({

    if(!grepl("[0-9]", rv$word) == TRUE) {
      paste("This is what the naive bayes predicted about your word: ", rv$word, sep = '')
    } else {
      paste("This is what the naive bayes predicted about your word: ", rv$word, sep = '')
    }
  })


  output$naive <- renderText({

    if(!grepl("[0-9]", rv$word) == TRUE) {
      classify_sentiment(rv$word)
    } else {
      paste("Wait this isn't a word")

    }
  })


  ### synonyms print
  output$syns_title <- renderText({
    if(!grepl("[0-9]", rv$word) == TRUE) {
      paste("Here are 25 synonyms of your word: ", rv$word, sep = '')
    }
    else{
      paste("READ ABOVE")
    }
  })

  output$syns <- renderText({
    if (anyNA(rv$list) == TRUE){
      paste("We could not find any related words. Whoops")
    } else {
      paste(rv$list, sep = 'asdfasdfadsf')
    }
  })


  output$wordcloudplot_title <- renderText({
    if(!grepl("[0-9]", rv$word) == TRUE) {
      paste("Here is a word cloud of 25 synonyms of your word: ", rv$word, sep = '')
    }
    else{
      paste("WHERE IS IT?")
    }
  })

  ### Word cloud print
  output$wordcloudplot <- renderPlot({
    if(!grepl("[0-9]", rv$word) == TRUE) {

      random = floor(runif(length(rv$list), 1,10))

      d <- data.frame(word = rv$list,freq=random)

      wordcloud(words = d$word, freq = d$freq,
                random.order=FALSE,
                rot.per=0,
                scale=c(2,2),
                colors=palette.colors(palette = "Okabe-Ito"))

    }
  })

  output$troll <- renderText({
    if(!grepl("[0-9]", rv$word) == FALSE) {
      paste('Your face when you thought you could enter a non word. TROLOLOLOL')
    }
  })

  output$photo <- renderImage({
    if(!grepl("[0-9]", rv$word) == FALSE) {
      list(
        src = file.path("Troll.png"),
        contentType = "image/jpeg",
        width = 300,
        height = 300,
        alt = "TROLOLOLOLOLOL"
      )
    }else{
      NA
    }
  })

  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })




}
