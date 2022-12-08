library(shiny)
library(fmsb)
library(wordcloud2)
library(grDevices)
library(shinythemes)
library(ggplot2)


data <- read.csv(file.path("data_shiny1.csv"),encoding = "UTF-8",header=T)
data2 <- read.csv(file.path("data_open.csv"),encoding = "UTF-8",header=T)

data[,1] <- as.character(data[,1])
data[,"name"] <- as.character(data[,"name"])
data[,"state"] <- as.character(data[,"state"])
data[,"city"] <- as.character(data[,"city"])
data[,"stars_y"] <- as.integer(data[,"stars_y"])
data[,9]<- as.character(data[,9])
data[,10]<- as.character(data[,10])

data2[,1] <- as.character(data2[,1])
data2[,"name"] <- as.character(data2[,"name"])
data2[,"state"] <- as.character(data2[,"state"])
data2[,"city"] <- as.character(data2[,"city"])
data2[,"stars_y"] <- as.integer(data2[,"stars_y"])


ui <- fluidPage(theme = shinytheme("darkly"),
        titlePanel(fluidRow(HTML("<div style='height: 54.75px;'>"),imageOutput("title"),HTML("</div>"))),
          sidebarLayout(
            sidebarPanel(
              selectInput("Country","The Country choosen:",list("USA"="USA")),
              selectInput("city","Select the City:", choices=list(
                'Arizona'=data[which(data[,"state"]=="AZ"),"city"], 'California'=data[which(data[,"state"]=="CA"),"city"],
                'Delaware'=data[which(data[,"state"]=="DE"),"city"], 'Florida'=data[which(data[,"state"]=="FL"),"city"], 
                'Idaho'=data[which(data[,"state"]=="ID"),"city"], 'Illinois'=data[which(data[,"state"]=="IL"),"city"],
                'Indiana'=data[which(data[,"state"]=="IN"),"city"], 'Louisiana'=data[which(data[,"state"]=="LA"),"city"],
                'Missouri'=data[which(data[,"state"]=="MO"),"city"], 'New Jersey'=data[which(data[,"state"]=="NJ"),"city"], 
                'Nevada'=data[which(data[,"state"]=="NV"),"city"], 'Pennsylvania'=data[which(data[,"state"]=="PA"),"city"],
                'Tennessee'=data[which(data[,"state"]=="TN"),"city"])),
              uiOutput("selectname"), uiOutput("selectid"), 
              actionButton("submit", "Submit", icon("paper-plane"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4", type = "info"), 
              tags$hr(), helpText("If there are any problems, please contact msatheesh@wisc.edu")),
            mainPanel(
              tabsetPanel(type = "tabs", 
                tabPanel("Your Performance Analysis",h2(strong(textOutput("nametitle"))),
                  fluidRow(column(6, fluidRow(column(5,HTML("<div style='height: 37.4px;'>"), imageOutput("image"), HTML("</div>")), 
                    column(7,h4(div(textOutput("revnum"),style = "color:grey, font_style:italic")))), h4(textOutput("genre")),  h4(textOutput("address1")), 
                    h4(textOutput("address2")), conditionalPanel(condition="input.submit",tags$hr())
                  )), 
                  fluidRow(column(6, plotOutput("barplot")),
                           column(6, conditionalPanel(condition="input.submit", h3("One of your top reviews based on Sentiment Analysis")),
                                  textOutput("reviewpos"), conditionalPanel(condition="input.submit", h3("\nOne of your worst reviews based on Sentiment Analysis")),
                                  textOutput("reviewneg")
                                  ))),
                tabPanel("Most hated words", 
                  fluidRow(
                    column(6,align="center", HTML("<div style='height: 85px;'>"),conditionalPanel(condition="input.submit",imageOutput("imagetitle1")),HTML("</div>"),wordcloud2Output('wcpos')),
                    column(6,align="center", HTML("<div style='height: 85px;'>"),conditionalPanel(condition="input.submit",imageOutput("imagetitle2")),HTML("</div>"),wordcloud2Output('wcneg'))
))
))))

server <- function(input,output){
  output$selectname<-renderUI({
    selectInput("name","Resteraunt Name:", sort(data[which(data[,"city"]==input$city),"name"]))
  })
  output$selectid<-renderUI({
    selectInput("nameid","Business ID:", sort(data[which(data[,"city"]==input$city & data[,"name"]==input$name),1]))
  })
  
  id <- eventReactive(input$submit,{
    if(is.na(input$nameid)==0)
      return( input$nameid )
    else
      return( 0 )
  })
  
  output$wcpos<-renderWordcloud2({
    words1<-unlist(strsplit(data[data[,1]==id(),9],split=" "))
    wordpos <- as.data.frame(words1)
    wordtab1<-sort(table(words1),decreasing = TRUE)
    wordpos<-as.data.frame(wordtab1)
    if(nrow(wordpos) <= 10){
      size = 0.4} else if(nrow(wordpos) <= 2){
        size = 0.5} else if(nrow(wordpos) <= 4){
          size = 0.6} else if(nrow(wordpos) <= 8){
            size = 0.7} else if(nrow(wordpos) <= 12 ){
              size = 1} else if(nrow(wordpos) <= 16){
                size = 1.5} else{
                  size = 2}
    return(wordcloud2(wordpos,size=size))
  })
  
  output$wcneg<-renderWordcloud2({
    words2<-unlist(strsplit(data[data[,1]==id(),10],split=" "))
    wordneg <- as.data.frame(words2)
    wordtab2<-sort(table(words2),decreasing = TRUE)
    wordneg<-as.data.frame(wordtab2)
    if(nrow(wordneg) <= 5){
      size = 0.4} else if(nrow(wordneg) <= 1){
        size = 0.5} else if(nrow(wordneg) <= 3){
          size = 0.6} else if(nrow(wordneg) <= 5){
            size = 0.7} else if(nrow(wordneg) <= 8 ){
              size = 1} else if(nrow(wordneg) <= 10){
                size = 1.5} else{
                  size = 2}
    return(wordcloud2(wordneg,size=size))
  })
  
  output$imagetitle1<-renderImage({
    list(src = file.path("images/positive.png"), width=180, height=69, alt = "Hmm, well something is wrong!")
  },deleteFile = FALSE)
  
  output$imagetitle2<-renderImage({
    list(src = file.path("images/dislike.jpg"), width=250, height=89, alt = "Hmm, well something is wrong!")
  },deleteFile = FALSE)
  
  output$nametitle <- renderText({data[data[,1]==id(),"name"]})
  img<-reactive({
    if(id() == 0 )
      return(0)
    else
      return(data[data[,1]==id(),"stars_y"])
  })
  
  output$image<-renderImage({
    list(src = file.path(paste("images/", img(),"stars",".png",sep="")), contentType = 'image/png', width=190.4, height=37.4, alt = "Hmm, well something is wrong!")
  },deleteFile = FALSE)
  
  output$title<-renderImage({
    list(src = file.path("images/title.png"), width=253.75, height=44.75, alt = "Hmm, well something is wrong!")
  },deleteFile = FALSE)
  
  output$businessid<-renderText({
    paste("Business ID:",id(),sep=" ")
  })
  
  output$genre<-renderText({
    paste0("Categories: ", as.character(data[data[,1]==id(),"categories"]))
  })
  
  output$revnum<-renderText({
    paste(data[data[,1]==id(),6],"reviews")
  })
  
  output$address1<-renderText({
    data[data[,1]==id(),"address"]
  })
  output$address2<-renderText({
    if(id()==0)
      return("Hmm, well something is wrong!")
    else
      return(paste(data[data[,1]==id(),"city"],", ",data[data[,1]==id(),"state"],sep=""))
  })
  output$barplot <- renderPlot(
   ggplot()+ geom_bar(aes(data2[data2$business_id==id(),][,18])) + xlab("Star Ratings")+ ylab("Frequency")+ ggtitle("Rating Distribuition") + 
     theme(plot.title = element_text(hjust = 0.5))
    )
  
  output$reviewpos <- renderText({
    if(id()==0)
      return("Hmm, well something is wrong!")
    else
      return(data2[data2$business_id== id() & data2$stars_y== 5,][1, "text"])
  })
  
  output$reviewneg <- renderText({
    if(id()==0)
      return("Hmm, well something is wrong!")
    else
      return(data2[data2$business_id== id() & data2$stars_y== 1,][1, "text"])
  })
}
shinyApp(ui, server)