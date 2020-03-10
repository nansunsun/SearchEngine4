
mydata = NULL
curdata = NULL
first = TRUE
out = NULL

library(shiny)
source("library.R")
source("getData.R")
source("model.R")

library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(janeaustenr)
library(dplyr)
library(stringr)
library(stringi)
library(tidytext)
library(reshape2)
library(tidyr)
library(igraph)
library(ggraph)
library(topicmodels)
library(devtools)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(janeaustenr)
library(dplyr)
library(stringr)
library(stringi)
library(tidytext)
library(reshape2)
library(tidyr)
library(igraph)
library(ggraph)
library(topicmodels)
library(devtools)

####################################################################
shinyServer(function(input, output, session) {
  
  output$cover = renderText({"Cybersecurity: search and visulisation"})
  vec = c("none", list.files("./temp/"))
  vec = gsub(".csv", "", vec)
  names(vec) = vec
  names(vec)[1] = "select board"
  updateSelectInput(session, 'board', choices = vec, selected = 'none')
  
  # load data
  observeEvent(input$board, {
    if(input$board == "none"){
      return()
    }
    
    first <<- TRUE
    
    isolate({
      withProgress({
        setProgress(message = "Processing...")
        mydata <<- getData(input$board)
        curdata <<- mydata

      })
    })

    updateSummary()
    updateDate()
    updateKeyword()
  }) 
  
  output$filter = renderUI({
    if(input$board != "none")
      checkboxInput('filter', "Filter", value = F)
  })
  
  # update curdata by filter condition
  readCond = function(){
    curdata <<- mydata
    
    #read keywords
    if(length(input$keyword) !=0 && input$keyword != ""){
      keywordVec = strsplit(input$keyword, ";")[[1]]
      
      if(input$kyCase){
        if(input$keywordUnion == "u"){
          vec = FALSE
          for(i in 1:length(keywordVec))
            vec = vec | grepl(keywordVec[i], curdata$title)
          
          curdata <<- subset(curdata, vec)
        }
          else{
            for(i in 1:length(keywordVec))
              curdata <<- subset(curdata, grepl(keywordVec[i], title))
          }
      }
      else{
        if(input$keywordUnion == "u"){
          vec = FALSE
          for(i in 1:length(keywordVec))
            vec = vec | grepl(tolower(keywordVec[i]), tolower(curdata$title))
          
          curdata <<- subset(curdata, vec)
        }
        else{
          for(i in 1:length(keywordVec))
            curdata <<- subset(curdata, grepl(tolower(keywordVec[i]), tolower(title)))
        }
      }


    }
    
    #read exclude
    if(length(input$exclude) != 0 && input$exclude != ""){
      excludeVec = strsplit(input$exclude, ";")[[1]]
      vec = FALSE
      if(input$exCase){
        for(i in 1:length(excludeVec))
          vec = vec | grepl(excludeVec[i], curdata$title)
      }
      else{
        for(i in 1:length(excludeVec))
          vec = vec | grepl(tolower(excludeVec[i]), tolower(curdata$title))
      }

      curdata <<- subset(curdata, !vec)
    }
    
    #read author, date, likes, comment
    if(length(input$dates) != 0){
      date = as.integer(gsub("-", "", input$dates, perl = F))
      curdata <<- subset(curdata, (input$author == "" | input$author == author) &
                    (time >= date[1] & time <= date[2]) &
                    (likes >= input$likes[1] & likes <= input$likes[2]) & 
                    (comment >= input$comment[1] & comment <= input$comment[2])
                  )
    }
    else
      curdata <<- subset(curdata, (input$author == "" | input$author == author))
    
    updateSummary()
  }
  
  # activate readCond
  observeEvent(c(input$author, input$keyword, input$dates, input$likes, input$comment, 
    input$keywordUnion, input$exclude, input$kyCase, input$exCase), {

    if(first)
      first <<- FALSE
    else
      readCond()
  })
  
  
  observeEvent(input$filter,{
    if(input$filter){
      updateKeyword()
      updateDate()
    }
    else{
      output$dates = renderUI({NULL})
      output$authorUI = renderUI({NULL})
      output$keywordUI = renderUI({NULL})
      output$excludeUI = renderUI({NULL})
      output$union = renderUI({NULL})
      output$likes = renderUI({NULL})
      output$comment = renderUI({NULL})
      output$keywordCase = renderUI({NULL})
      output$excludeCase = renderUI({NULL})
    }

      
  })
  
  updateDate = function(){

      output$dates = renderUI({

        if(nrow(curdata)==0 || length(input$filter) == 0||!(input$filter))
          return ()
        start = getDate(as.character(min(curdata$time)))
        end = getDate(as.character(max(curdata$time)))
        
        dateRangeInput("dates", label = "Date range", start = start, end = end,
          min = getDate(as.character(min(mydata$time))), max = getDate(as.character(max(mydata$time))))
        
      })
    
      if( length(input$filter) != 0&& input$filter){
        output$likes = renderUI({
          min = min(curdata$likes)
          max = max(curdata$likes)
          
          sliderInput("likes", label = " Likes", min = min, 
              max = max, value = c(min, max), step = 10)
        })
      
        output$comment = renderUI({
          min = min(curdata$comment)
          max = max(curdata$comment)
          
          sliderInput("comment", label = " # of comments", min = min, 
              max = max, value = c(min, max), step = 10)
        })
      }
  }
  
  updateKeyword = function(){
    if(length(input$filter) != 0 && input$filter){
      output$authorUI = renderUI({
        textInput("author", label = "Search author", value = "")
      })
      
      output$keywordUI = renderUI({
        textInput("keyword", label = 'Search keywords (use " ; " to separate)', value = "")
      })
      
      output$keywordCase = renderUI({
        checkboxInput("kyCase", label = 'Case sensitive', value = F)
      })
      
      output$union = renderUI({
        radioButtons('keywordUnion', NULL,
                     c(Union='u', intersect='i'), selected = 'u', inline = TRUE)
      })
      
      output$excludeUI = renderUI({
        textInput("exclude", label = 'Exclude keywords (use " ; " to separate)', value = "")
      })
      
      output$excludeCase = renderUI({
        checkboxInput("exCase", label = 'Case sensitive', value = F)
      })
    }
    else{
      output$authorUI = renderUI({NULL})
      output$keywordUI = renderUI({NULL})
      output$union = renderUI({NULL})
      output$excludeUI = renderUI({NULL})
      output$keywordCase = renderUI({NULL})
      output$excludeCase = renderUI({NULL})
    }

  }
  

  updateSummary = function(){
    isolate({
      withProgress({
        setProgress(message = "Processing...")
        output$head = renderTable({
          if(nrow(curdata) != 0)
            curdata[1:min(10, nrow(curdata)),c(-1,-7)]
        })
        
        output$sumTable = renderTable({
          if(nrow(curdata) != 0){
            p = data.frame(unclass(summary(curdata$likes)), check.names = FALSE, stringsAsFactors = FALSE)
            c = data.frame(unclass(summary(curdata$comment)), check.names = FALSE, stringsAsFactors = FALSE)
            names(p) = "Likes"
            names(c) = "comments"
            cbind(p, c)
          }
        }, spacing = 'l', rownames = T)
        
        output$boxplot = renderPlot({
          if(nrow(curdata) != 0)
          boxplot(curdata$likes, curdata$comment, names = c("Likes", "comments"), outline = F,
            main = "# of Likes & comments")
        })
        
        
        if(nrow(curdata) > 1)
        output$histogram = renderPlot({
          
            
            diff = as.Date(as.character(max(curdata$time)), "%Y%m%d") - 
              as.Date(as.character(min(curdata$time)), "%Y%m%d")
            if(diff > 6*30)
              breaks = "months"
            else if(diff > 30)
              breaks = "weeks"
            else
              breaks = "days"
          
            hist(as.Date(as.character(curdata$time), "%Y%m%d"), breaks = breaks,
              main = paste("# of posts over time, by", breaks), xlab = "Date", ylab = "", freq = T)
          
        })
        else
          output$histogram = renderPlot({NULL})
        
        if(first)
          output$pic = renderImage({
            filename <- normalizePath(file.path('./www',
                                    paste(input$board, ".png", sep = "")))
          
            list(src=filename, height = 100, width = 100)
          }, deleteFile = FALSE)
        
        output$size = renderText({
          return (paste("Total # of posts:", nrow(curdata)))
        })
        
        output$sumAuthor = renderText({
          if(length(input$keyword) != 0)
           return (paste("Author:", ifelse(input$author == "", "not specified", input$author)))
          else
            return (return ("Author(s): not specified"))
        })
        
        output$sumKeyword = renderText({
          if(length(input$keyword) != 0)
            return (paste("Keyword(s):", ifelse(input$keyword == "", "not specified", input$keyword)))
          else
            return ("Keyword(s): not specified")
        })
        
        output$sumExclude = renderText({
          if(length(input$exclude) != 0)
            return (paste("Exclude(s):", ifelse(input$exclude == "", "not specified", input$exclude)))
          else
            return ("Exclude(s): not specified")
        })
        
        output$time = renderText({
          if(length(input$dates) != 0)
            return (paste("Date range:\n", input$dates[1], "~", input$dates[2]))
          else
            return ("Date range: not specified")
        })
        
      })
    })
    

  }
  
  
  
    
  output$opt = renderUI({

    if(input$plot == "tfCloud" || input$plot == "tfidfCloud" || input$plot == "sCloud" 
      || input$plot == "bar" || input$plot == "bigramBar")
      sliderInput("param", label = "# of words to plot", min = 10, 
        max = 100, value = 20)
      
    else if(input$plot == "bCloud")
      sliderInput("param", label = "Minimum frequency of word-pairs frequency", min = 1, 
        max = 100, value = 10)
    
    else if(input$plot == "tCloud")
      sliderInput("param", label = "# of topics to cluster", min = 1, 
        max = 10, value = 4)
    
    else if(input$plot == "authorBar"){
      max = length(unique(curdata$author))
      sliderInput("param", label = "# of Authors to plot", min = min(10L, max), 
        max = max, value = min(10L, max), step = 10)
    }
    
    else if(input$plot == "termDep"){
      textInput("param", label = "Specify a term to search")
    }
      
    else
      return (NULL)
      
  })
  
  observeEvent(input$removeFreq, {
    output$removeFreqPercent = renderUI({
      if(input$removeFreq)
        sliderInput("freqPercent", "Remove terms that are in at least ? portion of the posts",
          min = 0, max = 1, value = 0.01)
    })
  })
  
  observeEvent(input$reload, {
    if(!is.null(mydata)){
      curdata <<- mydata
      updateDate()
      updateKeyword()
      updateSummary()
    }

    
  })
  
  
  observeEvent(input$draw, {

    output$removed = renderText({NULL})
    
    if(is.null(curdata)|| nrow(curdata) ==0)
      return()
    
    isolate({
      withProgress({
        setProgress(message = "Processing...")

        if(input$removeFreq){
          out <<- getFig(curdata, plotType = input$plot, param = input$param, remove = input$freqPercent, 
            strsplit(input$removeKeyword, ";")[[1]])
          output$removed = renderText({paste("Words removed:", paste(out$removed, collapse = ", "), sep = " ")})
         }
          
        else
          out <<- getFig(curdata, plotType =input$plot, param = input$param, 
            removeKeyword = strsplit(input$removeKeyword, ";")[[1]])
        
        if(input$plot == "termDep"){
          output$termDepTable = out$figure
          updateTabsetPanel(session, "mainTab", "tableTab")
        }
          
        else{
          output$fig = out$figure
          updateTabsetPanel(session, "mainTab", "plotTab")
        }

      })
    })
    

  })
  

  # download results
  
  output$saveData <- downloadHandler(
    
    filename = function() {
      paste(input$board, "/_", Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(curdata, file)
    }
  )
  

})
