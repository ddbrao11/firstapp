
# Run global dependencies.R to load required packages 

#set the path 
setwd("E:/shinyapp")
# getwd()

# =================================================================================================
# ui.R code starts from Here

ui <- shinyUI(
    fluidPage(
      
      titlePanel("UDPipe NLP Workflow - Shiny App"),
      
      sidebarLayout( 
        
        sidebarPanel(  
          
          fileInput("text_file", "Upload Text File for Analysis"),
          
          checkboxGroupInput("check_Group_tags", label = h3("Choose Speech Tags"), 
                             choices = list("adjective (ADJ)" = "AD", "noun(NOUN)" = "NO", "proper noun (PROPN)" = "PN", "adverb (ADV)" = "AB", "verb (VERB)" = "VB"),
                             selected = c("AD","NO","PN")),
          
          
          selectInput("Language", label = h3("Select Language"), 
                      choices = list("English-EWT" = "ewt","English-GUM" = "gum","English-LinES" = "lines","English-ParTUT" = "partut"), 
                      selected = "EnglishEWT"),
          hr(),
          
          fluidRow(column(3, verbatimTextOutput("value"))),
          
          submitButton(text = "Apply Changes", icon("refresh"))),
        
        mainPanel(
          
          tabsetPanel(type = "tabs",
                      
                      tabPanel("Overview",h4(p("How to use this App")),
                               
                               p("To use this app you need a document (e.g., newspaper article etc.) in txt file format.\n\n 
                               To upload the article text, click on Browse in left-sidebar panel and upload the txt file from your local machine. \n\n
                               Choose the parts of Speech tags to perform analysis.Select the english model required from the drop down.
                               Once the file is uploaded and english model is selected, the shinyapp will perform analysis on annotated documents, word clouds, Cooccurances of document in the back-end.", align = "justify")),
                      
                      tabPanel("Annotated Documents", 
                               h4(p("Annotated Documents Table Top 100 Rows")),
                               dataTableOutput('anno_docs'),
                               h4(p("Download Total Annotated Documents")),
                               downloadButton('Annotated.csv', 'Download CSV')),
                      
                      tabPanel("Word Clouds",
                               h3(p("NOUN & VERB - Word Clouds")),
                               p(""),
                               h4(p("Noun Word Cloud")),
                               plotOutput('plot_nouns',height =400, width = 400),
                               h4(p("Verb Word Cloud")),
                               plotOutput('plot_verbs',height =400, width = 400)),
                      
                      tabPanel("Co-occurrences Plot",
                               plotOutput('plot_cooccurance'))
                      
          ) 
        )
      ) 
    )  
  ) # ui.R code ends here

  
# ===============================================================================  
# Server.R file code Starts  
server <-shinyServer(function(input, output) {
  
  InputTextFileData <- reactive({ # read the uploaded text file
    
    if (is.null(input$text_file)) {   
                  return(NULL) } else{
      inputTextLines <- readLines(input$text_file$datapath)
      inputTextLines  =  str_replace_all(inputTextLines, "<.*?>", "")
      return(inputTextLines)
    }
  })
  
  model_file <- reactive({ # load the model file based on 
    
    if (input$Language == "ewt") 
    {
      model_file <- udpipe_load_model("./english-ewt-ud-2.3-181115.udpipe")
      return(model_file)
    }
    if (input$Language == "gum") 
    {
      model_file <- udpipe_load_model("./english-gum-ud-2.3-181115.udpipe") 
      return(model_file)
    }
    if (input$Language == "lines") 
    {
      model_file <- udpipe_load_model("./english-lines-ud-2.3-181115.udpipe") 
      return(model_file)
    }
    if (input$Language == "partut") 
    {
      model_file <- udpipe_load_model("./english-partut-ud-2.3-181115.udpipe")  
      return(model_file)
    }
    
  })
  
  AnnText100 <- reactive({ # Top 100 rows of Annotated documents 
    
        annotated_txt <- udpipe_annotate(model_file(), x = InputTextFileData())
        annotated_txt <- select(as.data.frame(annotated_txt),-sentence)
        head(annotated_txt,100)
      }
  )
  
  
 # Tab 2 - Annotated Documents - Top 100 rows
  output$anno_docs = renderDataTable({ AnnText100()})
  
  AnnTextAll <- reactive({
        annotated_txt <- udpipe_annotate(model_file(), x = InputTextFileData())
        annotated_txt <- as.data.frame(annotated_txt)
        return(annotated_txt)
  })
  
# Download CSV file handling logic
  output$Annotated.csv <- downloadHandler(
    filename = function() {'AnnotatedDocs.csv'},
    
    content = function(file) {write.csv (AnnTextAll(), file)}
  )
  # Tab 3 - wordcloud of Nouns
  output$plot_nouns = renderPlot({
    input_text_value <-  InputTextFileData()
    k <- udpipe_annotate(model_file(), x = input_text_value)
    k <- as.data.frame(k)
    
    all_nouns = k %>% subset(., upos %in% "NOUN") 
    top_nouns = txt_freq(all_nouns$lemma)
    
    wordcloud(words = top_nouns$key, 
              freq = top_nouns$freq, 
              min.freq = 2, 
              max.words = 100,
              random.order = FALSE, 
              colors = brewer.pal(6, "Dark2"))
    
  })
  # Tab 3-  wordcloud of verbs
  output$plot_verbs = renderPlot({
    input_text_value <-  as.character(InputTextFileData())
    m <- udpipe_annotate(model_file(), x = input_text_value, doc_id = seq_along(input_text_value))
    m <- as.data.frame(m)
    
    all_verbs = m %>% subset(., upos %in% "VERB") 
    top_verbs = txt_freq(all_verbs$lemma)
    
    wordcloud(words = top_verbs$key, 
              freq = top_verbs$freq, 
              min.freq = 2, 
              max.words = 100,
              random.order = FALSE, 
              colors = brewer.pal(6, "Dark2"))
  })
  
  # Tab 4 - Top -30 Co-Occurances
  
  output$plot_cooccurance = renderPlot({
    
    x <- udpipe_annotate(model_file(), x = InputTextFileData())
    x <- as.data.frame(x)
    
      checked_tags <- input$check_Group_tags
      for(i in seq_len(length(input$check_Group_tags))){
        if (input$check_Group_tags[i] == "AD"){
          checked_tags[i] <- "ADJ"
        }
        else if (input$check_Group_tags[i] == "NO"){
          checked_tags[i] <- "NOUN"
        }
        else if (input$check_Group_tags[i] == "PN"){
          checked_tags[i] <- "PROPN"
        }
        else if (input$check_Group_tags[i] == "AB"){
          checked_tags[i] <- "ADV"
        }
        else{
          checked_tags[i] <- "VERB"
        }
      }
      co_occur <- cooccurrence(   	# try `?cooccurrence` for parm options
        x = subset(x, x$upos %in% checked_tags), term = "lemma", 
        group = c("doc_id", "paragraph_id", "sentence_id"))  
    
    words_network <- head(co_occur, 30)
    words_network <- igraph::graph_from_data_frame(words_network) # needs edgelist in first 2 colms.
    windowsFonts(devanew=windowsFont("Devanagari new normal"))
    suppressWarnings(ggraph(words_network, layout = "fr") +  
                       
                       geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "orange") +  
                       geom_node_text(aes(label = name), col = "darkgreen", size = 6) +
                       
                       theme_graph(base_family = "Arial Unicode MS") +  
                       theme(legend.position = "none") +
                       
                       labs(title = "Top 30 Co-occurrence at Document Level Plot", subtitle = "Universal Speech of TAGS as chosen"))
  })
}) # server.R code ends here
# =======================================================================================================

shinyApp(ui = ui, server = server) # call the both ui.R and server.R using this line of code
