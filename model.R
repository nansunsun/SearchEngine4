
getFig = function(data, plotType, param, remove = 0, removeKeyword = c()){

  
  ########pre-processing
  
  title = data$title
  df <- data.frame(title)
  textdata <- df[df$title, ]
  textdata = gsub("[[:digit:]]", "", textdata)
  textdata = gsub("http\\w+", "", textdata)
  textdata = gsub("[ \t]{2,}", "", textdata)
  textdata = gsub("^\\s+|\\s+$", "", textdata)
  try.error = function(x)
  {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  textdata = sapply(textdata, try.error)
  textdata = textdata[!is.na(textdata)]
  names(textdata) = NULL
  
  textdata = removeWords(textdata, stopwords("english"))
  if(length(removeKeyword) != 0)
    textdata = removeWords(textdata, removeKeyword)
  
  corpus = Corpus(VectorSource(textdata))
  if(remove > 0){
    freq_term = findFreqTerms(DocumentTermMatrix(corpus), 
      ifelse(remove * length(title)<2, 2, remove * length(title)), Inf)
    textdata = removeWords(textdata, freq_term)
    corpus = Corpus(VectorSource(textdata))
  }
  else
    freq_term = c()
  
  review_dtm_tfidf <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))
  freq = data.frame(sort(colSums(as.matrix(review_dtm_tfidf)), decreasing=TRUE))
  colnames(freq) = "tfidf"
  
  ########end processing
  fig = NULL
  
  
  if(plotType == "bar"){
    fig = renderPlot({
      sub_freq = data.frame(freq[1:param,])
      rownames(sub_freq) = rownames(freq)[1:param]
      names(sub_freq) = names(freq)
      textSize = ifelse(param <=50, 12, 9)
      
      p <-ggplot(sub_freq, aes(x = reorder(rownames(sub_freq), tfidf), tfidf))
      p + geom_bar(stat = "identity") + coord_flip() + labs(y = "tf-idf", x = "") + 
        theme(axis.text=element_text(size=textSize))
    })
  }
  else if(plotType == "authorBar"){
    fig = renderPlot({
      authorTable = data.frame(table(data$author))
      authorTable = authorTable[order(authorTable$Freq, decreasing = T),]
      authorTable = authorTable[1:param,]
      
      p <-ggplot(authorTable,  aes(x = reorder(Var1, Freq), Freq))
            
      if(param <= 50)
        ySize = element_text(size = 12)
      else if(param <= 100)
        ySize = element_text(size = 9)
      else
        ySize = element_blank()
      
      p + geom_bar(stat = "identity") + coord_flip() + labs(y = "# of posts", x = "") + 
              theme(axis.text.x=element_text(size = 9), axis.text.y=ySize)
       
     })
    
  }

  else if(plotType == "bigramBar"){
    fig = renderPlot({
      title_frame <- data_frame(sourse = rep("Board", length(textdata)), text = textdata)
      title_bigrams <- title_frame %>%
        count_bigrams()
      
      pairs = data.frame(cbind(paste(title_bigrams$word1, "->", title_bigrams$word2), title_bigrams$n))
      names(pairs) = c("term", "n")
      pairs$term = as.character(pairs$term)
      pairs$n = as.integer(as.character(pairs$n))
      sub_pairs = pairs[1:param,]
      
      textSize = ifelse(param <=50, 12, 9)
      
      p <-ggplot(sub_pairs, aes(x = reorder(term, n), n))
      p + geom_bar(stat = "identity") + coord_flip() + labs(y = "# of occurance", x = "") +
        theme(axis.text.x=element_text(size=9), axis.text.y=element_text(size = textSize))

    })
  }
  
  else if(plotType == "tfCloud"){
    fig = renderPlot({
      wordcloud(corpus, max.words = param, colors=brewer.pal(1, "Dark2"), scale = c(5, 1))
    })
  }
  
  else if(plotType == "tfidfCloud"){
    fig = renderPlot({
      wordcloud(rownames(freq), freq[,1], max.words = param, colors=brewer.pal(1, "Dark2"), scale = c(5, 1))
    })
  }
  
  else if(plotType == "bCloud"){
    fig = renderPlot({
      title_frame <- data_frame(sourse = rep("Board", length(textdata)), text = textdata)
      title_bigrams <- title_frame %>%
        count_bigrams()

      if(nrow(title_bigrams) == 0 || max(title_bigrams$n) < param)
        stop("option too large. Try a smaller # of pairs.")
        
        title_bigrams %>%
          filter(n >= param,
               !str_detect(word1, "\\d"),
               !str_detect(word2, "\\d")
               )%>%
            visualize_bigrams()
      
    })
  }
  
  else if(plotType == "sCloud"){
    fig = renderPlot({
      title_frame <- data_frame(sourse = rep("Board", length(textdata)), text = textdata)
      raw_title <- title_frame %>% unnest_tokens(word, text)
      
      tidy_title <- raw_title %>%
        count(sourse, word, sort = TRUE)
      
      sent = raw_title %>%
        inner_join(get_sentiments("nrc"))
      
      sent %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(max.words = param, title.size = 2, scale=c(5,1))
    })
  }
  
  else if(plotType == "tCloud"){
    fig = renderPlot({
      title_frame <- data_frame(post = 1:length(textdata), text = textdata)
      tidy_title <- title_frame %>% unnest_tokens(word, text)
      
      text_word_counts <- tidy_title %>%
        count(post, word, sort = TRUE)
      
      text_words <- text_word_counts %>%
        bind_tf_idf(post, term_col = word, n_col = n)
      
      text_dtm <- text_words %>%
          cast_dtm(post, word, n)
      
      lda <- LDA(text_dtm, k = param, control = list(seed = 1234))
      title_topics <- tidy(lda, matrix = "beta")
      
      top_terms <- title_topics %>%
        group_by(topic) %>%
        top_n(20, beta) %>%
        ungroup() %>%
        arrange(topic, -beta)
      
      top_terms%>%
        acast(term ~ topic, value.var = "beta", fill = 0) %>%
        comparison.cloud(max.words = 100, title.size = 2, scale=c(5,1))
    })
  }
  
  else if(plotType == "termDep"){
    fig = renderTable({
      word_pool = data_frame(Post = 1:length(textdata), text = textdata) %>%
         unnest_tokens(term, text)
      
      word_pool
      
      word_cors <- word_pool %>%
        group_by(term) %>%
        filter(n() >= 20) %>%
        pairwise_cor(term, Post, sort = TRUE)
      
      word_cors = data.frame(word_cors)
      
      if(param == ""){
        print(1)
        word_cors[1:10,]
      }
        
      else{
        print(2)
        word_cors[word_cors$item1 == param,]
      }
        
      
    })
  }
  
  if(is.null(fig))
    fig = renderText({"Figure can't be generated, check you're data / options."})
  out = list(fig, freq_term)
  names(out) = c("figure", "removed")
  return (out)


}


###################
# Functions
###################

count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}


visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 6) +
    theme_void()
}
