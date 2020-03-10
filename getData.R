getData = function(file){
  data = read.csv(paste("./temp/", file, ".csv", sep = ""), sep = ",", header = T)
  names(data) = c("id", "title", "likes", "comment", "time", "author", "content","link")
  data$title = iconv(as.character(data$title), "latin1", "UTF-8")
  charlikes = as.character(data$likes)
  data$likes = as.integer(gsub(",", "", charlikes))
  charComment = as.character(data$comment)
  data$comment = as.integer(gsub(",", "", charComment))
  data$time = as.integer(data$time)
  data$author = iconv(as.character(data$author), "latin1", "UTF-8")
  data$content = iconv(as.character(data$content), "latin1", "UTF-8")
  data$link = paste0('twitter.com', data$link)
  
  return (data)
}

getDate = function(time){
  return (paste(substr(time, 1, 4), "-", substr(time, 5, 6), "-", 
    substr(time, 7, 8), sep = ""))
}
