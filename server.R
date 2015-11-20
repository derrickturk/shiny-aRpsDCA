# (c) 2015 dwt | terminus data science, LLC

library(shiny)
library(aRpsDCA)

valid.data <- function(frame)
{
    if (is.null(frame))
        FALSE
    else if (ncol(frame) < 3)
        FALSE
    else
        TRUE
}

shinyServer(function (input, output, session) {
    reactive.data <- reactive({
        if (is.null(input$datafile$datapath))
            return(NULL)

        read.delim(file=input$datafile$datapath, sep=input$separator,
          quote=input$quotechar, header=input$headerrow,
          na.strings=input$missingstr, stringsAsFactors=FALSE)
    })

    reactive.wellid <- reactive({
        if (!valid.data(reactive.data()))
            return(NULL)
        as.character(reactive.data()[[input$wellidvar]])
    })

    reactive.time <- reactive({
        if (!valid.data(reactive.data()))
            return(NULL)
        time.data <- reactive.data()[[input$timevar]]
        try <- as.numeric(time.data)
        if (any(!is.na(try)))
            return(try)

        try <- as.Date(time.data, format="%Y-%m-%d")
        if (any(!is.na(try)))
            return(try)

        try <- as.Date(time.data, format="%Y/%m/%d")
        if (any(!is.na(try)))
            return(try)

        try <- as.Date(time.data, format="%m-%d-%Y")
        if (any(!is.na(try)))
            return(try)

        try <- as.Date(time.data, format="%m/%d/%Y")
        if (any(!is.na(try)))
            return(try)

        NULL
    })

    reactive.rate <- reactive({
        if (!valid.data(reactive.data()))
            return(NULL)
        as.numeric(reactive.data()[[input$ratevar]])
    })

    reactive.nwells <- reactive({
        wells <- reactive.wellid()
        if (is.null(wells))
            NULL
        else
            length(unique(wells))
    })

    reactive.wellid.filtered <- reactive({
        if (is.null(reactive.wellid())) {
            NULL
        } else {
            if (is.null(input$selectedwells)) # not viewed yet
                reactive.wellid()
            else
                reactive.wellid()[reactive.wellid() %in% input$selectedwells]
        }
    })

    reactive.rate.filtered <- reactive({
        if (is.null(reactive.rate())) {
            NULL
        } else {
            if (is.null(input$selectedwells)) # not viewed yet
                reactive.rate()
            else
                reactive.rate()[reactive.wellid() %in% input$selectedwells]
        }
    })

    reactive.time.filtered <- reactive({
        if (is.null(reactive.time())) {
            NULL
        } else {
            if (is.null(input$selectedwells)) # not viewed yet
                reactive.time()
            else
                reactive.time()[reactive.wellid() %in% input$selectedwells]
        }
    })

    reactive.nwells.filtered <- reactive({
        wells <- reactive.wellid.filtered()
        if (is.null(wells))
            NULL
        else
            length(unique(wells))
    })

    reactive.valid.data <- reactive({
        !is.null(reactive.wellid()) && !all(is.na(reactive.wellid())) &&
          !is.null(reactive.time()) && !all(is.na(reactive.time())) &&
          !is.null(reactive.rate()) && !all(is.na(reactive.rate()))
    })

    reactive.valid.filter <- reactive({
        !is.null(reactive.wellid.filtered()) &&
        !all(is.na(reactive.wellid.filtered())) &&
        !is.null(reactive.time.filtered()) &&
        !all(is.na(reactive.time.filtered())) &&
        !is.null(reactive.rate.filtered()) &&
        !all(is.na(reactive.rate.filtered()))
    })

    output$decline <- renderPlot({
        if (is.null(reactive.wellid.filtered()))
            return(NULL)
        palette <- rainbow(reactive.nwells.filtered())
        plot(reactive.rate.filtered() ~ reactive.time.filtered(), type="n",
          xlab=input$timevar, ylab=input$ratevar, log="y")
        wellid.factor <- as.factor(reactive.wellid.filtered())
        sapply(levels(wellid.factor), function (well) {
            well.rate <- reactive.rate.filtered()[wellid.factor == well]
            well.time <- reactive.time.filtered()[wellid.factor == well]
            time.order <- order(well.time)
            well.rate <- well.rate[time.order]
            well.time <- well.time[time.order]
            lines(well.rate ~ well.time,
              col=palette[match(well, levels(wellid.factor))])
        })
    })

    output$table <- renderTable({
        if (is.null(reactive.data()))
            return(NULL)
        reactive.data()
    })

    output$variableselection <- renderUI({
        if (!valid.data(reactive.data())) {
            p('Invalid data table.')
        } else {
            var.names <- names(reactive.data())
            tagList(
                selectInput('wellidvar', 'Well ID Column', var.names,
                  var.names[1]),
                selectInput('timevar', 'Time Column', var.names, var.names[2]),
                selectInput('ratevar', 'Rate Column', var.names, var.names[3])
            )
        }
    })

    output$variablesvalid <- renderUI({
        if (valid.data(reactive.data()) && !reactive.valid.data())
            tagList(hr(), p('Invalid data in selected columns.'))
        else
            NULL
    })

    output$wellchooser <- renderUI({
        if (!reactive.valid.data()) {
            NULL
        } else {
            well.names <- sort(unique(reactive.wellid()))
            tagList(h3('Active wells'), selectInput('selectedwells', NULL,
              well.names, multiple=TRUE, selected=well.names, selectize=FALSE,
              size=min(length(well.names), 50)))
        }
    })
})
