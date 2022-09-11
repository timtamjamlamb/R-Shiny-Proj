ui <- fluidPage(

  titlePanel("Jordan's very interesting and intelligent and good and great project that he did by himself with no help from other classmates"),
  titlePanel("Please enter a word and it will offer you synonyms of your word"),
  titlePanel("and who in the course felt that way"),

  sidebarLayout(
    sidebarPanel(
      helpText("Try these words:"),
      helpText("excited"),
      helpText("nervous"),
      helpText("apprehensive"),
      helpText("interesting"),

      textInput("word", "Please enter your word:", "happy"),
      actionButton("action", "Update View")),

    mainPanel(
      verbatimTextOutput("naive_title"),
      textOutput("naive",container = div),

      verbatimTextOutput("syns_title"),
      textOutput("syns",container = div),

      verbatimTextOutput("wordcloudplot_title"),
      plotOutput(outputId = "wordcloudplot"),
      textOutput("troll",container = div),
      imageOutput("photo"),

      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      )

    )
  )
)
