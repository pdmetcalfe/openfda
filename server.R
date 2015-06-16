library("httr")
library("magrittr")
library("ggvis")
library("shiny")
library("dplyr")

##' Run a query against the openFDA api
##' @param ... the query details
run.fda <- function(...) {
    GET("https://api.fda.gov",
        path="drug/event.json",
        query=list(...))
}

##' Run a query against the openFDA api, extracting the results
##' @param ... the query
query.extract <- function(...) {
    resp <- run.fda(...)
    content(resp)$results
}

##' Build a search criterion for the openFDA api
##' @param field.name the field you're looking for
##' @param value the value you're looking for
build.search <- function(field.name, value) {
    paste0(field.name, ':"', gsub(" ", "+", value), '"')
}

##' Extract named results from the openFDA result set
##' @param src the results
##' @param name the name you're looking for
##' @param prototype a prototype for the value you're looking for
extract.named <- function(src, name, prototype) {
    vapply(src, function(item) {item[[name]][[1]]}, prototype)
}

##' Build a time series
##' @param res the FDA result data set
time.to.df <- function(res) {
    data.frame(time=as.Date(extract.named(res, "time", ""), "%Y%m%d"),
               count=extract.named(res, "count", 0),
               stringsAsFactors=FALSE)
}

##' Count matching indications
##' @param indication the indication to filter on
##' @param term the type of term to count
count.terms <- function(indication, term) {
    query.extract(search=build.search("patient.drug.drugindication",
                      indication),
                  count=term) %>%
                      (function(response) {
                           data.frame(term=extract.named(response, "term", ""),
                                      count=extract.named(response, "count", 0),
                                      stringsAsFactors=FALSE)
                       })
}


indications <- query.extract(count="patient.drug.drugindication.exact") %>%
    extract.named("term", "")


shinyServer(function(input, output, session) {
    output$indicationControl <- renderUI(selectInput("indication",
                                                     "choose indication",
                                                     indications))

    good.indication <- reactive({
        ind <- input$indication
        if (is.null(ind)) {ind <- indications[[1]]}
        gsub(" ", "+", ind)
    })

    drug.counts <- reactive({
        count.terms(good.indication(),
                    "patient.drug.openfda.generic_name.exact")
    })

    ind.counts <- reactive({
        count.terms(good.indication(),
                    "patient.drug.drugindication.exact")
    })

    time.series <- reactive({
        drugs <- drug.counts() %>% head(10)
        ind.query <- build.search("patient.drug.drugindication",
                                  good.indication())
        all.data <- lapply(drugs$term, function(drug) {
            drug.query <- build.search("patient.drug.openfda.generic_name",
                                       drug)
            res <- query.extract(search=paste(ind.query, "AND", drug.query),
                                 count="receivedate") %>%
                                     time.to.df
            res$drug <- rep.int(drug, nrow(res))
            res
        })

        all.data <- do.call(rbind, all.data)
        all.data$drug <- reorder(all.data$drug, all.data$count, sum)
        all.data %>% filter(time > "2005-01-01")
        
    })
    
    data.switch <- reactive({
       switch(input$queryType,
              drugs=drug.counts,
              indications=ind.counts)()
    })

    output$myTable <- renderDataTable({
        data.switch()
    })

    plot.data <- reactive({
        data.switch() %>% head(20)
    })
    
    plot.data %>% 
        ggvis(y=~reorder(term, -count), width=~count) %>%
        layer_rects(height=band(), x=0, fill:="#830051", stroke:="white") %>%
        add_tooltip(function(df) {
            paste0(df[,1], " (", df[,2], ")")
        }) %>%
        add_axis("y", title="") %>%
        add_axis("x", title="") %>%
        bind_shiny("frequencyGraph", "frequencyGraph_ui")

    time.series %>%
        ggvis(x=~time, y=~count, stroke=~drug) %>%
        group_by(drug) %>%
        layer_paths() %>%
            add_tooltip(function(df) {df$drug}) %>%
                bind_shiny("timeGraph", "timeGraph_ui")
                    
})
