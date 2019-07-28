shinyUI(
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
) 
