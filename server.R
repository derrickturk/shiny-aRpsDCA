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

    reactive.valid.plot <- reactive({
        !is.null(reactive.wellid()) && !all(is.na(reactive.wellid())) &&
          !is.null(reactive.time()) && !all(is.na(reactive.time())) &&
          !is.null(reactive.rate()) && !all(is.na(reactive.rate()))
    })

    output$decline <- renderPlot({
        if (!reactive.valid.plot())
            return(NULL)
        palette <- rainbow(reactive.nwells())
        plot(reactive.rate() ~ reactive.time(), type="n",
          xlab=input$timevar, ylab=input$ratevar, log="y")
        wellid.factor <- as.factor(reactive.wellid())
        sapply(levels(wellid.factor), function (well) {
            well.rate <- reactive.rate()[wellid.factor == well]
            print(well.rate)
            well.time <- reactive.time()[wellid.factor == well]
            time.order <- order(well.time)
            well.rate <- well.rate[time.order]
            print(well.rate)
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
        if (valid.data(reactive.data()) && !reactive.valid.plot())
            tagList(hr(), p('Invalid data in selected columns.'))
        else
            NULL
    })
})
