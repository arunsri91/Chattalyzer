
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  
  output$plot <- renderPlot({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    WappChat <- readLines(input$file1$datapath)
    #header = input$header,
    #sep = input$sep,
    #quote = input$quote)
    #############DTM AND BOW BUILDING#############################
    require(text2vec)||install.packages(text2vec)
    require(data.table)||install.packages(data.table)
    require(stringr)||install.packages(stringr)
    require(tm)||install.packages(tm)
    require(RWeka)||install.packages(RWeka)
    require(tokenizers)||install.packages(tokenizers)
    require(slam)||install.packages(slam)
    require(wordcloud)||install.packages(wordcloud)
    require(ggplot2)||install.packages(ggplot2)
    require("ggmap")||install.packages("ggmap")
    require("leaflet")||install.packages("leaflet")
    require("plotGoogleMaps")||install.packages("plotGoogleMaps")
    require("text2vec")||install.packages("text2vec")
    require("ggmap")||require("ggmap")
    require("gsubfn")||require("gsubfn")
    require("dplyr")||install.packages("dplyr")
    require(memoise)||install.packages(memoise)
    require(ggplot2)||install.packages(ggplot2)
    require(lubridate)||install.packages(lubridate)
    require(reshape2)||install.packages(reshape2)
    require(tm)||install.packages(tm)
    require(SnowballC)||install.packages(SnowballC)
    require(wordcloud)||install.packages(wordcloud)
    require(RColorBrewer)||install.packages(RColorBrewer)
    require(stringr)||install.packages(stringr)
    require(syuzhet)||install.packages(syuzhet)
    require(dplyr)||install.packages(dplyr)
    library(text2vec)
    library(data.table)
    library(stringr)
    library(tm)
    library(RWeka)
    library(tokenizers)
    library(slam)
    library(wordcloud)
    library(ggplot2)
    library("ggmap")
    library("leaflet")
    library("plotGoogleMaps")
    library("text2vec")
    require("ggmap")
    require("gsubfn")
    library("dplyr")
    library(memoise)
    ################### SENTIMENT ANALYSIS############################
    library(ggplot2)
    library(lubridate)
    #install.packages("Scale")
    #library(Scale)
    library(reshape2)
    library(tm)
    library(SnowballC)
    library(wordcloud)
    library(RColorBrewer) 
    library(stringr)
    #install.packages("syuzhet")
    #install.packages("syuzhet")
    library(syuzhet) 
    library(dplyr ) 
      wappDf = data.frame(NULL)
      #wappDft
      #l=5
      
      for (l in 1:length(WappChat))
      {
        text = WappChat[l]
        
        split = strsplit(text,": ")  # Notice \\? in the pattern
        #length(split[[1]])
        if (length(split[[1]]) == 2 )
        {
          datestmp = split[[1]][1]
          
          message = gsub("[^[:alnum:][:space:]]","",split[[1]][2])
          message = gsub("Ã|Ã¢|â|ð|à|²|¹|à|³³|Â","",message)
        }
        else if ((length(split[[1]])) == 1)
        {
          datestmp =""
          
          message = gsub("[^[:alnum:][:space:]]","",split[[1]][1])
          message=gsub("Ã|Ã¢|â|ð|à|²|¹|à|³³","",message)
          
        }
        else if ((length(split) ==0))
        {
          datestmp=""
          
          message=""
        }
        
        
        if (datestmp != "")
        {
          date = gsub("\\[","",strsplit(datestmp,',')[[1]][1])
          name = gsub("[^[:alnum:][:space:]]","",strsplit(datestmp,']')[[1]][2])
          name = gsub("Ã|Ã¢|â|ð|à|à|Â","",name)
          name = gsub("\\s","",name)
          #name = strsplit(datestmp,']')[[1]][2]
        }
        if (datestmp == '')
        {
          date = "Not Available"
          name = "Not Available"
        }
        wappDft = data.frame(name,date,message,stringsAsFactors = F)
        wappDf = rbind(wappDf,wappDft)
      }
      wappDf_new <- data.frame(NULL)
      wappDf_new <- wappDf[!duplicated(wappDf), ]
     
    
    if (input$disp == "Chatcount"){
      par(mar = c(0,0,0,0))
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      
      text(x = 0.5, y = 0.55, paste("Welcome to Whatsapp Chatter Analytics. \n",
                                      length(wappDf_new$message), "  Messages Uploaded & Ready for Analysis \n Please Select  on different Radio Boxes under insights and  \n  Click on Track! to get the actual insights"), 
           cex = 1.2, col = "blue", family="serif", font=2, adj=0.5)
      
      #text(x = 0.34, y = 0.9, paste("    Currently it contains : ",length(wappDf_new$message),
       #                             "Chat Messages From The Whatsapp Group "), 
        #   cex = 1.5, col = "gray30", family="serif", font=2, adj=0.5)
    }
    else if(input$disp == "UserStat") {
      
      #setProgress(message = "Pulling Top 15 Active Users.........")
      summarized_users <- wappDf_new %>% 
        group_by(wappDf_new$name) %>%
        summarise(number = n())
      head(summarized_users)
      
      active_users <-  summarized_users[order(summarized_users$number,decreasing = TRUE),]
      active_users <- subset(active_users,active_users$`wappDf_new$name` != "Not Available")
      Atleast_1post <- length(active_users$`wappDf_new$name`)
      active_users_df <- as.data.frame(active_users)
      if(nrow(active_users)>=15){
        top_15_user <- active_users_df[1:15,]
      }else{
        top_15_user <- active_users_df[1:nrow(active_users),]
      } 
      top_15contrib <- round(col_sums(as.matrix(top_15_user$number)) / colSums(as.matrix(active_users_df$number)),2)
      active_users_df$inactive <-  ifelse(active_users_df$number<=5,1,0)
      #head(active_users_df)
      #tail(active_users_df)
      temp_inactive <- round(colSums(as.matrix(active_users_df$inactive)) / nrow(active_users_df),2)
      display_text<-paste("=>A total of ",nrow(active_users_df),"users have been involved in the Conversation. <=\n","=>Among all the Users, ",temp_inactive*100,
                          "% of Users Have been very much inactive by not posting \n for more than 5 times in the entire whatsapp chat and the rest \n",
                          (1 - temp_inactive)*100, "% of population have posted more than 5 times. <=\n => Also the Top N (here N is = ",nrow(top_15_user),") Users contributed to ",
                          (top_15contrib)*100,"% of the overall chat conversation\n", "while the rest ",nrow(active_users_df)-nrow(top_15_user),
                          "Users contribute to ", (1 - top_15contrib)*100,"% of the conversations <=\n",
                          "                                                                \n",
                          "                                                                \n",
                          " Assumptions - This insight works well only if number of users in \n",
                          " the group is atleast 15 or above  \n",
                          "=>Top N- Can be 15 if users > 15 in group or \n",
                          "Equal To Number of Users if Total Users is Less than 15<=\n")
      
      
      par(mar = c(0,0,0,0))
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      
      text(x = 0.6, y = 0.6,display_text, 
           cex = 1.2, col = "blue", family="Arial", font=2, adj=0.5)
    }
    else if (input$disp =="Active_Days_Post_Count_Order") {
      
      withProgress({
        setProgress(message = "Pulling Top 15 Active Days.....")
      summarized_date <- wappDf_new %>% 
        group_by(wappDf_new$date) %>%
        summarise(number = n())
      head(summarized_date)
      
      date_post <-  summarized_date[order(summarized_date$number,decreasing = TRUE),]
      date_post <- subset(date_post,date_post$`wappDf_new$date`!="Not Available")
      date_post_df <- as.data.frame(date_post)
      
      top_15_active_days <- date_post_df[1:15,]
      colnames(top_15_active_days) <- c("Days","Posts_Count")
      #barplot(top_10$Count,main="Page Views", horiz=TRUE,names.arg=top_10$ContactName,las=1)
      top_15_active_days$Days<-reorder(top_15_active_days$Days,-top_15_active_days$Posts_Count)
      #############MOST ACTIVE DAYS IN WHAATSAPP GROUP######################
      #output$plot <- renderplot({
      top_day <- as.array(as.character(top_15_active_days$Days))
      top_cnt <- as.array(as.numeric(top_15_active_days$Posts_Count))
      dist_text <- paste("Most Active day of the Group  is ",top_day[1]," with ",top_cnt[1]," Posts")
      (ggplot(top_15_active_days,aes(Days,Posts_Count),colour='red')+
         geom_bar(stat="identity")+
           ggtitle("MOST 15 ACTIVE DAYS BASED ON NO OF POST", subtitle = dist_text)+
         theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)))
      })
      #return(top_15_active_days)
    }
      else if (input$disp =="Active_Days_Chronological_Order") {
        
        withProgress({
          
          summarized_date <- wappDf_new %>% 
            group_by(wappDf_new$date) %>%
            summarise(number = n())
          head(summarized_date)
          
          date_post <-  summarized_date[order(summarized_date$number,decreasing = TRUE),]
          date_post <- subset(date_post,date_post$`wappDf_new$date`!="Not Available")
          date_post_df <- as.data.frame(date_post)
          
          #plot(as.data.frame(active_users))
          #?subset()
          #install.packages("plot_ly")
          top_15_active_days <- as.data.frame(NULL)
          top_15_active_days <- date_post_df[1:15,]
          colnames(top_15_active_days) <- c("Days","Count")
          top_15_active_days$Days <- as.character(top_15_active_days$Days)
          #top_15_active_days$Days <- as.Date(top_15_active_days$Days,"%d/%m/%Y")
          
          top_15_active_days$Datetemp <- as.Date(top_15_active_days$Days,"%d/%m/%Y")
          top_15_active_days$Datetemp <- as.character(top_15_active_days$Datetemp)
          top_15_active_days$Datetemp <- str_replace_all(top_15_active_days$Datetemp,"-","")
          top_15_active_days <- top_15_active_days[order(top_15_active_days$Datetemp,decreasing = F),]
          
          
          #barplot(top_10$Count,main="Page Views", horiz=TRUE,names.arg=top_10$ContactName,las=1)
          top_15_active_days$Days<-reorder(top_15_active_days$Days,as.numeric(top_15_active_days$Datetemp))
          #############MOST ACTIVE DAYS IN WHAATSAPP GROUP######################
          #plot(top_15_active_days,type="b")
          
          ggplot(top_15_active_days,aes(Days,Count),colour='red')+
            geom_bar(stat="identity")+
            ggtitle("MOST ACTIVE DAYS",subtitle = "Top 15 Active Days pulled and arranged in Chronological Order")+
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
          
          
          
        })
        #return(top_15_active_days)
      }
    else if (input$disp =="ActiveUsers") {
      withProgress({
        
      setProgress(message = "Pulling Top 15 Active Users.........")
      summarized_users <- wappDf_new %>% 
        group_by(wappDf_new$name) %>%
        summarise(number = n())
      #head(summarized_users)
      
      active_users <-  summarized_users[order(summarized_users$number,decreasing = TRUE),]
      active_users <- subset(active_users,active_users$`wappDf_new$name` != "Not Available")
      Atleast_1post <- length(active_users$`wappDf_new$name`)
      active_users_df <- as.data.frame(active_users)
      top_15 <- active_users_df[1:15,]
      colnames(top_15) <- c("ContactName","Posts_Count")
      top_15$ContactName<-reorder(top_15$ContactName,-top_15$Posts_Count)
      top_per <- as.array(as.character(top_15$ContactName))
      top_post <- as.array(as.character(top_15$Posts_Count))
      #top_per[1]
      dis_text <- paste("Most Active Person is ",top_per[1]," with ",top_post[1],
                        "posts ")
      ggplot(top_15,aes(ContactName,Posts_Count),colour='red')+
        geom_bar(stat="identity")+
        ggtitle("ACTIVE USERS",subtitle = dis_text)+
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
      })
      
    }
      else if (input$disp =="InactiveUsers") {
        withProgress({
          
          setProgress(message = "Pulling Top 20 InActive Users.........")
          summarized_users <- wappDf_new %>% 
            group_by(wappDf_new$name) %>%
            summarise(number = n())
          head(summarized_users)
          
          active_users <-  summarized_users[order(summarized_users$number,decreasing = FALSE),]
          #head(active_users)
          active_users <- subset(active_users,active_users$`wappDf_new$name` != "Not Available")
          ########### This displays the number of users who had posted atleast once inside group#
          Atleast_1post <- length(active_users$`wappDf_new$name`)
          active_users_df <- as.data.frame(active_users)
          inactive_users <-  summarized_users[order(summarized_users$number,decreasing = FALSE),]
          inactive_users <- subset(inactive_users,inactive_users$`wappDf_new$name` != "Not Available")
          inactive_users_df <- as.data.frame(inactive_users)
          inactive_users_new <- subset(inactive_users_df,inactive_users_df$number <=5)
          #head(inactive_users)
          
          summarized_inusers <- inactive_users_new %>% 
            group_by(inactive_users_new$number) %>%
            summarise(number = n())
          
          inactive_user_bucket <- as.data.frame(summarized_inusers)
          colnames(inactive_user_bucket) <- c("No_of_Posts","No_Of_Users")
          ggplot(inactive_user_bucket,aes(No_of_Posts,No_Of_Users),colour='red')+
            geom_bar(stat="identity")+
            ggtitle("INACTIVE USERS ",subtitle = "No of Inactive Users Who have Posted a min of 1 post and Max of 5 posts")+
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1))
          
        })
        
      }
    else if (input$disp =="FrequentWords") {
      
      library("wordcloud")
      library("tm")
      l <- iconv(wappDf_new$message, to='ASCII//TRANSLIT')
      #create corpus
      lc <- Corpus(VectorSource(l))
      
      #clean up
      
      lc <- tm_map(lc, content_transformer(tolower)) 
      lc <- tm_map(lc, removePunctuation)
      lc <- tm_map(lc, function(x)removeWords(x,stopwords()))
      lc <- tm_map(lc,removeWords,stopwords("en"))
      lc <- tm_map(lc,removeWords,c('photojpg','attached','contact','admin','add','message',
                                    'going','learning','walking','talking','talk','walk',
                                    'again','find','disha','nayi','website','day','must',
                                    'another','doesnt','years',
                                    'can','when','she','he','him','her','them','may','has',
                                    '91aa','make','been','would','has','might','pls','does','you',
                                    'how','have','what','yes','no','your','their','etc','than','his','please',
                                    'also','any','from','much','about','anyone','doing','why','where','too',
                                    'dont','its','should','some','why','try','only','lot','number','group',
                                    'very','more','even','take','different','sure','most','know','now','give',
                                    'got','other','added','changed','true','many','well','same','lot','get',
                                    'all','like','thats','our','both','were','new','see','here','used','both',
                                    'never','did','had','back','need','needs','done','around','asd','year',
                                    'days','keep','let','who','just','long','after','name','things','sometimes',
                                    'understand','image','thanks','thank','one','help','other','others','thing',
                                    'above','below','which','able','use','near','every','share','come',
                                    'dear','since','pages','always','stop','omitted','someone','something',
                                    'want','deleted','way','right','feel','think','suggest','using','nice',
                                    'time','phone','out','stay','start','details','free','still','important',
                                    'small','few','idea','life','ask','great','yrs','say',
                                    'said','delay','senior','special','because','cure','read',
                                    'work','once','words','touch','hand','body','friends',
                                    'cant','wont','tell','friends','little','issues','yourself',
                                    'themself','check','words','two','three','four','five',
                                    'six','seven','eight','helpful','play','anything','home','old',
                                    'first','early','while','those','input','video','better','part',
                                    'cant','cannot','put','wrong','aba','available','makes',
                                    'left','own','based','really','change','mother','address',
                                    'being','age','giving','give','forward','taking','months',
                                    'bemer','through','wish','else','giving','look','works',
                                    'before','says','during','though','head','actually','month',
                                    'looking','person'))
      library(RColorBrewer)
      
      pal2 <- brewer.pal(8,"Dark2")
      wordcloud(lc,min.freq=2,max.words=75, random.order=F,random.color = T, colors=pal2)   
      
    }
    
    else if (input$disp =="Rarewords") {
      verbatimTextOutput("Under construction")
    }
    else if (input$disp =="Cluster") {
      
    }
    else if (input$disp =="Emotion") {
      
      
      #texts <- readLines("_chat.txt")
      texts <- wappDf_new$message
      docs <- Corpus(VectorSource(texts))
      
      #clean our chat data
      trans <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
      docs <- tm_map(docs, trans, "/")
      docs <- tm_map(docs, trans, "@")
      docs <- tm_map(docs, trans, "\\|")
      docs <- tm_map(docs, content_transformer(tolower))
      docs <- tm_map(docs, removeNumbers)
      docs <- tm_map(docs, removeWords, stopwords("en"))
      docs <- tm_map(docs, removeWords, c("â€ž","â€â"))
      #docs <- tm_map(docs, removewords,  )
      docs <- tm_map(docs, removePunctuation)
      docs <- tm_map(docs, stripWhitespace)
      #docs <- tm_map(docs, stemDocument)
      
      #create the document term matrix
      dtm <- TermDocumentMatrix(docs)
      mat <- as.matrix(dtm)
      v <- sort(rowSums(mat),decreasing=TRUE)
      
      #Data frame
      data <- data.frame(word = names(v),freq=v)
      withProgress({
        setProgress(message="Analyzing Emotions........")
        Sentiment <- get_nrc_sentiment(texts)
        
      })
      #dim(Sentiment)
      text_df <- as.data.frame(texts)
      
      text <- cbind(texts,Sentiment)
      
      #count the sentiment words by category
      TotalSentiment <- data.frame(colSums(text[,c(2:9)]))
      names(TotalSentiment) <- "count"
      TotalSentiment <- cbind("sentiment" = rownames(TotalSentiment), TotalSentiment)
      rownames(TotalSentiment) <- NULL
      
      sentiment_array <- TotalSentiment[order(TotalSentiment$count,decreasing = T),]
      temp_array <- as.array(as.character(sentiment_array$sentiment))
      #temp_array[1]
      new_display_text <- paste("THe Top Three Emotion across the Discussion is ",toupper(temp_array[1]) ,
                                " followed by " , toupper(temp_array[2]), " followed by ", 
                                toupper(temp_array[3]))
      #head(TotalSentiment)
      #total sentiment score of all texts
      ggplot(data = TotalSentiment, aes(x = sentiment, y = count)) +
        geom_bar(aes(fill = sentiment), stat = "identity") +
        theme(legend.position = "none") +
        #geom_label(data = new_display_text)+
        xlab("Emotion") + ylab("Total Count") + ggtitle("Emotion Analysis",subtitle=new_display_text)
      
      
      
    }
    else if (input$disp =="Sentiment") {
      
      #withProgress()
      #texts <- readLines("_chat.txt")
      texts <- wappDf_new$message
      docs <- Corpus(VectorSource(texts))
      
      #clean our chat data
      trans <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
      docs <- tm_map(docs, trans, "/")
      docs <- tm_map(docs, trans, "@")
      docs <- tm_map(docs, trans, "\\|")
      docs <- tm_map(docs, content_transformer(tolower))
      docs <- tm_map(docs, removeNumbers)
      docs <- tm_map(docs, removeWords, stopwords("en"))
      docs <- tm_map(docs, removeWords, c("â€ž","â€â"))
      #docs <- tm_map(docs, removewords,  )
      docs <- tm_map(docs, removePunctuation)
      docs <- tm_map(docs, stripWhitespace)
      #docs <- tm_map(docs, stemDocument)
      
      #create the document term matrix
      dtm <- TermDocumentMatrix(docs)
      mat <- as.matrix(dtm)
      v <- sort(rowSums(mat),decreasing=TRUE)
      
      #Data frame
      data <- data.frame(word = names(v),freq=v)
      withProgress({
        setProgress(message="Analyzing Sentiments......")
        Sentiment <- get_nrc_sentiment(texts)
      
      })
      #dim(Sentiment)
      text_df <- as.data.frame(texts)
      #dim(text_df)
      #head(Sentiment)
      text <- cbind(texts,Sentiment)
      #head(text)
      #text[2386,]
      #write.csv(text,"Sentiments_classified.csv")
      #count the sentiment words by category
      TotalSentiment <- data.frame(colSums(text[,c(2:9)]))
      names(TotalSentiment) <- "count"
      TotalSentiment <- cbind("sentiment" = rownames(TotalSentiment), TotalSentiment)
      rownames(TotalSentiment) <- NULL
      nrc_data <- Sentiment
      temp_sent <- NULL
      temp_sent <- colSums(prop.table(nrc_data[, 9:10]))
      temp_sentdf <- as.data.frame(temp_sent)
      temp_sentd <- c("negative","positive")
      temp_sentdf2 <- as.data.frame(temp_sentd)
      new_df <- cbind(temp_sentdf,temp_sentdf2)
      sored_df <- new_df[order(new_df$temp_sent,decreasing = T),]
      display_text <- paste("This Group chat is ",round(as.numeric(temp_sent[1]),2), " % Negative and  ", 
                            round(as.numeric(temp_sent[2]),2), " % Positive",
                            " and overall ",toupper(as.character(sored_df[1,2])) , " Sentiment is Prevalent")
      #paste("the most common one is ",temp_sent[1])
      anger <- which(Sentiment$anger>0)
      texts[anger]
      barplot(
        sort(colSums(prop.table(nrc_data[, 9:10]))), 
        horiz = TRUE, 
        cex.names = 0.7, 
        las = 1, 
        sub = display_text,
        main = "Sentiment Analysis", xlab="Percentage"
      )
      
    }
    
  }
  )
  # output$DateText <- renderPlot(
  #   {
  #     WappChat <- readLines(input$file1$datapath)
  #     #header = input$header,
  #     #sep = input$sep,
  #     #quote = input$quote)
  #     #############DTM AND BOW BUILDING#############################
  #     require(text2vec)||install.packages(text2vec)
  #     require(data.table)||install.packages(data.table)
  #     require(stringr)||install.packages(stringr)
  #     require(tm)||install.packages(tm)
  #     require(RWeka)||install.packages(RWeka)
  #     require(tokenizers)||install.packages(tokenizers)
  #     require(slam)||install.packages(slam)
  #     require(wordcloud)||install.packages(wordcloud)
  #     require(ggplot2)||install.packages(ggplot2)
  #     require("ggmap")||install.packages("ggmap")
  #     require("leaflet")||install.packages("leaflet")
  #     require("plotGoogleMaps")||install.packages("plotGoogleMaps")
  #     require("text2vec")||install.packages("text2vec")
  #     require("ggmap")||require("ggmap")
  #     require("gsubfn")||require("gsubfn")
  #     require("dplyr")||install.packages("dplyr")
  #     require(memoise)||install.packages(memoise)
  #     require(ggplot2)||install.packages(ggplot2)
  #     require(lubridate)||install.packages(lubridate)
  #     require(reshape2)||install.packages(reshape2)
  #     require(tm)||install.packages(tm)
  #     require(SnowballC)||install.packages(SnowballC)
  #     require(wordcloud)||install.packages(wordcloud)
  #     require(RColorBrewer)||install.packages(RColorBrewer)
  #     require(stringr)||install.packages(stringr)
  #     require(syuzhet)||install.packages(syuzhet)
  #     require(dplyr)||install.packages(dplyr)
  #     library(text2vec)
  #     library(data.table)
  #     library(stringr)
  #     library(tm)
  #     library(RWeka)
  #     library(tokenizers)
  #     library(slam)
  #     library(wordcloud)
  #     library(ggplot2)
  #     library("ggmap")
  #     library("leaflet")
  #     library("plotGoogleMaps")
  #     library("text2vec")
  #     require("ggmap")
  #     require("gsubfn")
  #     library("dplyr")
  #     library(memoise)
  #     ################### SENTIMENT ANALYSIS############################
  #     library(ggplot2)
  #     library(lubridate)
  #     #install.packages("Scale")
  #     #library(Scale)
  #     library(reshape2)
  #     library(tm)
  #     library(SnowballC)
  #     library(wordcloud)
  #     library(RColorBrewer) 
  #     library(stringr)
  #     #install.packages("syuzhet")
  #     #install.packages("syuzhet")
  #     library(syuzhet) 
  #     library(dplyr ) 
  #     wappDf = data.frame(NULL)
  #     #wappDft
  #     #l=5
  #     
  #     for (l in 1:length(WappChat))
  #     {
  #       text = WappChat[l]
  #       
  #       split = strsplit(text,": ")  # Notice \\? in the pattern
  #       #length(split[[1]])
  #       if (length(split[[1]]) == 2 )
  #       {
  #         datestmp = split[[1]][1]
  #         
  #         message = gsub("[^[:alnum:][:space:]]","",split[[1]][2])
  #         message = gsub("Ã|Ã¢|â|ð|à|²|¹|à|³³|Â","",message)
  #       }
  #       else if ((length(split[[1]])) == 1)
  #       {
  #         datestmp =""
  #         
  #         message = gsub("[^[:alnum:][:space:]]","",split[[1]][1])
  #         message=gsub("Ã|Ã¢|â|ð|à|²|¹|à|³³","",message)
  #         
  #       }
  #       else if ((length(split) ==0))
  #       {
  #         datestmp=""
  #         
  #         message=""
  #       }
  #       
  #       
  #       #latitude = strsplit(message,",")[[1]][1]
  #       
  #       #longitude = strsplit(message,",")[[1]][2]
  #       if (datestmp != "")
  #       {
  #         date = gsub("\\[","",strsplit(datestmp,',')[[1]][1])
  #         name = gsub("[^[:alnum:][:space:]]","",strsplit(datestmp,']')[[1]][2])
  #         name = gsub("Ã|Ã¢|â|ð|à|à|Â","",name)
  #         name = gsub("\\s","",name)
  #         #name = strsplit(datestmp,']')[[1]][2]
  #       }
  #       if (datestmp == '')
  #       {
  #         date = "Not Available"
  #         name = "Not Available"
  #       }
  #       wappDft = data.frame(name,date,message,stringsAsFactors = F)
  #       wappDf = rbind(wappDf,wappDft)
  #     }
  #     wappDf_new <- data.frame(NULL)
  #     wappDf_new <- wappDf[!duplicated(wappDf), ]
  #     
  #     
  #     library("wordcloud")
  #     library("tm")
  #     l <- iconv(wappDf_new$message, to='ASCII//TRANSLIT')
  #     #create corpus
  #     lc <- Corpus(VectorSource(l))
  #     
  #     #clean up
  #     
  #     lc <- tm_map(lc, content_transformer(tolower)) 
  #     lc <- tm_map(lc, removePunctuation)
  #     lc <- tm_map(lc, function(x)removeWords(x,stopwords()))
  #     lc <- tm_map(lc,removeWords,stopwords("en"))
  #     lc <- tm_map(lc,removeWords,c('photojpg','attached','contact','admin','add','message',
  #                                   'going','learning','walking','talking','talk','walk',
  #                                   'again','find','disha','nayi','website','day','must',
  #                                   'another','doesnt','years',
  #                                   'can','when','she','he','him','her','them','may','has',
  #                                   '91aa','make','been','would','has','might','pls','does','you',
  #                                   'how','have','what','yes','no','your','their','etc','than','his','please',
  #                                   'also','any','from','much','about','anyone','doing','why','where','too',
  #                                   'dont','its','should','some','why','try','only','lot','number','group',
  #                                   'very','more','even','take','different','sure','most','know','now','give',
  #                                   'got','other','added','changed','true','many','well','same','lot','get',
  #                                   'all','like','thats','our','both','were','new','see','here','used','both',
  #                                   'never','did','had','back','need','needs','done','around','asd','year',
  #                                   'days','keep','let','who','just','long','after','name','things','sometimes',
  #                                   'understand','image','thanks','thank','one','help','other','others','thing',
  #                                   'above','below','which','able','use','near','every','share','come',
  #                                   'dear','since','pages','always','stop','omitted','someone','something',
  #                                   'want','deleted','way','right','feel','think','suggest','using','nice',
  #                                   'time','phone','out','stay','start','details','free','still','important',
  #                                   'small','few','idea','life','ask','great','yrs','say',
  #                                   'said','delay','senior','special','because','cure','read',
  #                                   'work','once','words','touch','hand','body','friends',
  #                                   'cant','wont','tell','friends','little','issues','yourself',
  #                                   'themself','check','words','two','three','four','five',
  #                                   'six','seven','eight','helpful','play','anything','home','old',
  #                                   'first','early','while','those','input','video','better','part',
  #                                   'cant','cannot','put','wrong','aba','available','makes',
  #                                   'left','own','based','really','change','mother','address',
  #                                   'being','age','giving','give','forward','taking','months',
  #                                   'bemer','through','wish','else','giving','look','works',
  #                                   'before','says','during','though','head','actually','month',
  #                                   'looking','person'))
  #     library(RColorBrewer)
  #     
  #     pal2 <- brewer.pal(8,"Dark2")
  #     wordcloud(lc,min.freq=2,max.words=75, random.order=F,random.color = T, colors=pal2)
  #     
  #   }
  # )
  

})
