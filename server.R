library(shiny)
library(psych)
library(data.table)
shinyServer(function(input,output){
  
  last3words <- reactive({
    x <- input$userinput
    x <- tolower(x)
	x <- tolower(x)
    x <- paste("#start#","#start#",x,sep=" ")
    x <- sent_detect(x)
    x <- x[length(x)]
    x <- as.character(unlist(strsplit(x,"[ \r,\n,\t,.,;, :, \", (, ), ?, !]")))
    #x <- x[length(x) : length(x)-1]
    
    cy <- c(length(x)-2:length(x))
    #print(c(x[c(length(x)-2:length(x))+2],c(length(x)-2:length(x))+2))
    x <- x[c(length(x)-2:length(x))+2]
   
    cx <- complete.cases(x)
    x <- x[cx]
    
    x <- generatedwords(x)
    cx <- complete.cases(x)
    x <- x[cx]
    return(x)
    })
  generatedwords <- function(words){
    wordvector <- unique(c(gsub(paste("^",words[1]," ",sep=""),"",blogs[[2]][]$phrase[grep(paste("^",words[1]," ",sep=""),blogs[[2]][]$phrase)]),gsub(paste("^",words[1]," ",sep=""),"",newslist[[2]][]$phrase[grep(paste("^",words[1]," ",sep=""),newslist[[2]][]$phrase)]),gsub(paste("^",words[1]," ",sep=""),"",twitterlist[[2]][]$phrase[grep(paste("^",words[1]," ",sep=""),twitterlist[[2]][]$phrase)])))
    wt_bi <- 0.1
    wt_tri <- 0.2
    wt_quad <- 0.7
    
    bigramratio_blog <- wt_bi*vectorFormationBlog(words[1],2,wordvector)
    trigramratio_blog <- wt_tri*vectorFormationBlog(paste(words[2],words[1],sep=" "),3,wordvector)
    quadgramratio_blog <- wt_quad*vectorFormationBlog(paste(words[3],words[2],words[1],sep=" "),4,wordvector)
    #print(bigramratio_blog)
    #print(trigramratio_blog)
    gramsratio_blog <- bigramratio_blog+trigramratio_blog+quadgramratio_blog
    
    bigramratio_news <- wt_bi*vectorFormationNews(words[1],2,wordvector)
    trigramratio_news <- wt_tri*vectorFormationNews(paste(words[2],words[1],sep=" "),3,wordvector)
    quadgramratio_news <- wt_quad*vectorFormationNews(paste(words[3],words[2],words[1],sep=" "),4,wordvector)
    #print(bigramratio_news)
    #print(trigramratio_news)
    
    gramsratio_news <- bigramratio_news+trigramratio_news+quadgramratio_news
    #print(trigramratio_news)
    bigramratio_twitter <- wt_bi*vectorFormationTwitter(words[1],2,wordvector)
    trigramratio_twitter <- wt_tri*vectorFormationTwitter(paste(words[2],words[1],sep=" "),3,wordvector)
    quadgramratio_twitter <- wt_quad*vectorFormationTwitter(paste(words[3],words[2],words[1],sep=" "),4,wordvector)
    #print(bigramratio_twitter)
    #print(trigramratio_twitter)
    
    gramsratio_twitter <- bigramratio_twitter+trigramratio_twitter+quadgramratio_twitter
    
    
    #print(cbind(gramsratio_blog,gramsratio_news,gramsratio_twitter,))
   # gramframe <- data.frame(cbind(gramsratio_blog,gramsratio_news,gramsratio_twitter))
    maxvector <- apply(cbind(gramsratio_blog,gramsratio_news,gramsratio_twitter),1,max,na.rm=TRUE)
    wordvector_calc <- data.table(selection=wordvector,weight=maxvector)
   setkey(wordvector_calc,selection)
   
   #wordvector_calc<- wordvector_calc[]
   wordvector_calc <- wordvector_calc[order(-wordvector_calc$weight),]
   return(wordvector_calc[1:5]$selection)   
   
   #return("hello")
  }
  vectorFormationBlog <- function(userphrase,gram,bivector){
    #print(userphrase)
    i <- 1
    countgram <- NA
    sumgram <- 0
    while((i+1)<length(bivector))
    {
      count<-blogs[[gram]][][which(blogs[[gram]][]$phrase == paste(userphrase,bivector[i]))]$freq
      #print(paste(userphrase,bivector[i],count))
      if(length(as.character(count))==0)
        count <- 0
      sumgram <- sumgram + count
      countgram <- c(countgram,count)
      i <- i+1
    }
    countgram <- countgram[complete.cases(countgram)]
    if(sumgram == 0)
      sumgram <- 1
    countgram <- countgram/sumgram 
    return(countgram)
  }
  vectorFormationNews <- function(userphrase,gram,bivector){
    #print(userphrase)
    i <- 1
    countgram <- NA
    sumgram <- 0
    while((i+1)<length(bivector))
    {
      count<-newslist[[gram]][][which(newslist[[gram]][]$phrase == paste(userphrase,bivector[i]))]$freq
      #if(gram==4)
      #print(paste(userphrase,bivector[i],count))
      if(length(as.character(count))==0)
        count <- 0
      
      sumgram <- sumgram + count
      
      countgram <- c(countgram,count)
      i <- i+1
    }
    countgram <- countgram[complete.cases(countgram)]
    if(sumgram == 0)
      sumgram <- 1
    countgram <- countgram/sumgram 
    return(countgram)
  }
  vectorFormationTwitter <- function(userphrase,gram,bivector){
    #print(userphrase)
    i <- 1
    countgram <- NA
    sumgram <- 0
    while((i+1)<length(bivector))
    {
      count<-twitterlist[[gram]][][which(twitterlist[[gram]][]$phrase == paste(userphrase,bivector[i]))]$freq
      #print(paste(userphrase,bivector[i],count))
      if(length(as.character(count))==0)
        count <- 0
      
      sumgram <- sumgram + count
      countgram <- c(countgram,count)
      i <- i+1
    }
    countgram <- countgram[complete.cases(countgram)]
    if(sumgram == 0)
      sumgram <- 1
    countgram <- countgram/sumgram 
    return(countgram)
  }
output$textarea.out <- renderPrint({
 last3words()   
})
})

