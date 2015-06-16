library("shiny")
library("ggvis")

shinyUI(fluidPage(
    titlePanel("OpenFDA data plotter"),
    sidebarLayout(
        sidebarPanel(uiOutput("indicationControl"),
                     selectInput("queryType",
                                 "Display",
                                 list(drugs="drugs",
                                      indications="indications")),
                     dataTableOutput("myTable")),
        mainPanel(
            h1("AE-based indication and drug relationships"),
            p("This uses data from ",
              a("the openfda project", href="https://open.fda.gov/"),
             " to build up an idea of the relationship between drug indication ",
              " and the drugs taken by those patients (or co-indications)."),
            p(strong("Beware, the data have not been checked!")),
            h2("Medications / coindications"),
            uiOutput("frequencyGraph_ui"),
            ggvisOutput("frequencyGraph"),
            h2("Time series"),
            uiOutput("timeGraph_ui"),
            ggvisOutput("timeGraph"),
            p("Visualization built using ",
              a("shiny", href="http://shiny.rstudio.com/"),
              ".")))))
