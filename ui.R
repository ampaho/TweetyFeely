library(ggvis)
library(shiny)

langs <- list("All"="all", "English"="en", "French"="fr" , "German"="ge" , "Italian"="it", "Spanish"="sp")

fluidPage(theme="bootstrap.min.css",
  # Application title
  titlePanel("Sentiment Analysis"),

  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      textInput("source1", "Search on Social Media:", value="#donaldtrump"),

conditionalPanel(
  condition = "input.show_source2 == true",
  textInput("source2", "Compare with:" , value="#hillaryclinton")
),
checkboxInput("show_source2", "Compare"),
      actionButton("plot_feel", "Plot Sentiments"),
      hr(),
      selectInput("lang",
        "Language:", langs)
    ),


    mainPanel(
      verbatimTextOutput("twitter_view"),

      h4("Sentiment Chart"),
      helpText("You can click on each dot to read the social media post."),
      ggvisOutput("plot1"),

      h4("Trends"),
      plotOutput("trends"),

      h4("Sources"),
      plotOutput("plot")
    )
  )
)
