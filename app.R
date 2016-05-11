#Delaney Moran 
library(shiny)
library(wordcloud)

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(SnowballC))
suppressPackageStartupMessages(library(tm))

bible <-readLines("bible.txt", encoding="UTF-8") %>%
  as.character()

anneogg <- readLines("Anne.txt", encoding="UTF-8") %>%
  as.character()

biobook <- readLines("biobook.txt", encoding="UTF-8") %>%
  as.character()


#clean it up
more_stopwords<- c("said", "thy", "thee", "ye", "will", "thee", "can", "shall", "hath", "one", "now", "dont", "mrs", "im", "also", "thou", "will", "go", "going", "mr", "shalt", "much", "upon", "us", "even", "get", "say", "unto", "went", "saith", "therefore", "just", "hast", "ive", "didnt", "id", "isnt", "got", "o", "oh")

anneogg_names <- c("marilla", "diana", "matthew", "anne", "lynde")

bible <- bible %>% 
  tolower() %>%
  removeNumbers() %>%
  removePunctuation() %>%
  removeWords(c(stopwords("english"), more_stopwords)) %>%
  stemDocument() %>%
  stripWhitespace()

anneogg <- anneogg %>% 
  tolower() %>%
  removeNumbers() %>%
  removePunctuation() %>%
  removeWords(c(stopwords("english"),more_stopwords, anneogg_names)) %>%
  stemDocument() %>%
  stripWhitespace()

biobook <- biobook %>% 
  tolower() %>%
  removeNumbers() %>%
  removePunctuation() %>%
  removeWords(c(stopwords("english"), more_stopwords)) %>%
  stemDocument() %>%
  stripWhitespace()


anneogg2 <- VectorSource(anneogg) %>% 
  Corpus()

bible2 <- VectorSource(bible) %>% 
  Corpus()

biobook2 <- VectorSource(biobook) %>% 
  Corpus()



# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Wordclouds"),
  
  
  # Show a plot of the generated distribution
  sidebarPanel(
    selectInput("selection", "Choose a text:",
                choices = c("Anne of Green Gables", 
                            "The Holy Bible", 
                            "Text Book of Biology, Part 1: Vertebrata"),
                selected = "Anne of Green Gables")
  ),
  mainPanel(
    plotOutput("wordcloud")
  )
)
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  output$wordcloud <- renderPlot({
    text <- switch(input$selection,
                   "Anne of Green Gables" = anneogg2,
                   "The Holy Bible" = bible2,
                   "Text Book of Biology, Part 1: Vertebrata" = biobook2)
    scheme <- switch(input$selection, 
                     "Anne of Green Gables" = "YlGn",
                     "The Holy Bible" = "RdPu",
                     "Text Book of Biology, Part 1: Vertebrata" = "BuGn")
    
    
    wordcloud(text, scale=c(4, 0.5), max.words=25, random.order=FALSE,
              rot.per=0.2, use.r.layout=FALSE, colors=brewer.pal(5, scheme))
  })
})



# Run the application 
shinyApp(ui = ui, server = server)

