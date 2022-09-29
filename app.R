# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)

# Load additional dependencies and setup functions
# source("global.R")

# Question Bank----


# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "purple",
    ### App Header ----
    dashboardHeader(
      title = "Statistical Interpretations",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Statistical_Interpretations")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("home")
        )
      )
    ),
    ### Sidebar ----
    dashboardSidebar( # Need to have Overview, prereq, game, references.
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Game", tabName = "game", icon = icon("gamepad")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ### Body ----
    dashboardBody(
      tabItems(
        #### Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Statistical Interpretations"), # This should be the full name.
          p("This app will introduce and/or refine skills of interpreting various
            statistics and statistical tests."),
          h2("Instructions"),
          p("Click the button below to review some prerequisites."),
          tags$ol(
            tags$li("Review your knowledge using the prerequisites page before 
                    playing the game"),
            tags$li("Once you feel ready, go to the game tab. Your goal is to 
                    score as high as possible without taking your mountain 
                    climber over the cliff and into the abyss. To do this, 
                    answer as many questions correctly as you can. If you answer
                    too many wrong, your climber will get closer and closer to 
                    the top until they fall off.")
          ),
          ##### Prereq Button ----
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "prereqButton",
              label = "Prerequisites",
              size = "large",
              icon = icon("book"), # Goes to prereq page
              style = "default"
            )
          ),
          ##### Create two lines of space
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Neil J.
            Hatfield and Robert P. Carey, III.",
            br(),
            "We would like to extend a special thanks to the Shiny Program
            Students.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 9/8/2022 by POP.")
          )
        ),
        #### Set up the Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("In order to get the most out of this app, please review the
            following terms"),
          box(
            title = strong("Z-score"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "A standardization..."
          ),
          box(
            title = strong("p-value"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "A p-value measures the probability of obtaining such extreme 
            results as actually observed under the assumption the null 
            hypothesis is true."
          ),
          box(
            title = strong("Confidence Interval"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "A range of plausible values for the value of an unknown parameter.
            A higher confidence level will produce a larger range of values as 
            compared to using a smaller confidence level (ie. a 95% CI will 
            have a smaller range than a 99% CI)."
          ),
          box(
            title = strong("Hypothesis Test"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "A branch of statistical inference which allows for two hypotheses 
            to be tested for significance against each other."
          ),
          box(
            title = strong("Regression"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "A way to interpret the relationship between a independent and 
            dependent variable in order to make predictions about data that has 
            not been previously recorded. Linear Regression follows the general 
            equation form y = mx + b."
          ),
          h3("Interpreting Data and Statistical Tests"),
          p("Use the following dropdowns to learn or review how to correctly 
            interpret various statistics and statistical tests."),
          box(
            title = strong("Interpreting a p-value"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "To interpret a p-value, imagine we are given a p-value of 0.05.
            That means for whatever statistic we tested, there is a probability 
            of 0.05 we computed that statistic by pure chance, and it is instead
            more reasonable to assume the statistic we computed supports an 
            alternative hypothesis."
          ),
          box(
            title = strong("Interpreting Confidence Intervals"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "Say we compute a 95% Confidence Interval to be (0.55, 0.69). To 
            correctly interpret this, we would say the following: ''We are 95% 
            confident the true value is between 0.55 and 0.69.''"
          ),
          box(
            title = strong("Interpreting Regression Equations"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "We are given a linear regression equation to be y = 0.5x + 32. To 
            correctly interpret the slope, we would say ''For every one unit 
            increase in x we would expect y to increase by 0.5.'' To correctly 
            interpret the intercept, we would say ''When x = 0 we would expect 
            y to have a value of 32.''"
          ),
        ),
        #### Note: you must have at least one of the following pages. You might
        #### have more than one type and/or more than one of the same type. This
        #### will be up to you and the goals for your app.
    
        
        #### Game Page ----
        tabItem(
          tabName = "game",
          withMathJax(),
          h2("Cliff Hangar Game"),
          p("Answer the questions provided as accurately as possible; as you get
            questions wrong, your mountain climber will get closer to falling 
            off the cliff! Score as high as possible without falling off!"),
          fluidRow(
            column(
              width = 6,
              wellPanel(
                h3("Question"),
                uiOutput("question"),
                br(),
                radioGroupButtons(
                  inputId = "mc1",
                  label = "Question and scenario will be displayed here.",
                  status = "game",
                  direction = "horizontal",
                  selected = character(0),
                  checkIcon = list(
                    yes = icon("check-square"),
                    no = icon("square-o")
                  ),
                  choices = list(
                    # "Pick the expression below that best addresses the question.",
                    "\\(\\frac{1}{4}\\)",
                    "\\(\\frac{2}{4}\\)",
                    "\\(\\frac{3}{4}\\)",
                    "\\(\\frac{4}{4}\\)"
                  ),
                  width = "100%",
                  justified = FALSE,
                  individual = FALSE
                ),
                br(),
                fluidRow(
                  column(
                    width = 4,
                    bsButton(
                      inputId = "submit",
                      label = "Submit",
                      size = "large",
                      style = "default",
                      disabled = FALSE
                    )
                  ),
                  column(
                    width = 4,
                    uiOutput("mark")
                  )
                ),
                br(),
                bsButton(
                  inputId = "nextQuestion",
                  label = "Next Question",
                  size = "large",
                  style = "success",
                  disabled = TRUE
                ),
                bsButton(
                  "restart",
                  "Restart",
                  size = "large",
                  style = "danger",
                  disabled = FALSE
                )
              )
            ),
            column(
              width = 6,
              uiOutput("correct", align = "center"),
              uiOutput("gameProgressTree", align = "center")
            )
          ),
          uiOutput("math1"),
          uiOutput("math2")
          ),
        #### Set up the References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {

  ## Set up Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "To review, visit the prerequisites page. When you are ready to 
        test yourself, go to the game page and see how far you can get up the 
        mountain withour falling off."
      )
    }
  )
  ## Overview Prereq Button----
  observeEvent(
    eventExpr = input$prereqButton,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "prereqButton",
        selected = "prerequisites"
      )
    })

}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)