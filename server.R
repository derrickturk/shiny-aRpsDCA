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
        validate(
            need(!is.null(input$datafile$datapath), "Please upload a file.")
        )

        read.delim(file=input$datafile$datapath, sep=input$separator,
          quote=input$quotechar, header=input$headerrow,
          na.strings=input$missingstr, stringsAsFactors=FALSE)
    })

    reactive.wellid <- reactive({
        validate(
            need(valid.data(reactive.data()),
              "Data file invalid or has too few columns.")
        )
        as.character(reactive.data()[[input$wellidvar]])
    })

    reactive.time <- reactive({
        validate(
            need(valid.data(reactive.data()),
              "Data file invalid or has too few columns.")
        )

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

        validate(
            need(FALSE, "Invalid time column.")
        )
    })

    reactive.rate <- reactive({
        validate(
            need(valid.data(reactive.data()),
              "Data file invalid or has too few columns.")
        )

        as.numeric(reactive.data()[[input$ratevar]])
    })

    reactive.nwells <- reactive({
        wells <- reactive.wellid()
        length(unique(wells))
    })

    reactive.wellid.filtered <- reactive({
        if (is.null(input$selectedwells)) # not viewed yet
            reactive.wellid()
        else
            reactive.wellid()[reactive.wellid() %in% input$selectedwells]
    })

    reactive.rate.filtered <- reactive({
        if (is.null(input$selectedwells)) # not viewed yet
            reactive.rate()
        else
            reactive.rate()[reactive.wellid() %in% input$selectedwells]
    })

    reactive.time.filtered <- reactive({
        if (is.null(input$selectedwells)) # not viewed yet
            reactive.time()
        else
            reactive.time()[reactive.wellid() %in% input$selectedwells]
    })

    reactive.nwells.filtered <- reactive({
        wells <- reactive.wellid.filtered()
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

    reactive.declines <- reactive({
        validate(
            need(reactive.valid.filter(), "Invalid columns or wells selected.")
        )

        if (input$declinetype == 'EXP')
            fit.fn <- best.exponential
        else if (input$declinetype == 'HYP')
            fit.fn <- best.hyperbolic
      # else if (input$declinetype == 'H2E')
      #     fit.fn <- best.hyp2exp
        else
            validate(need(FALSE, 'Invalid decline type.'))

        sapply(sort(unique(reactive.wellid.filtered())), function (well) {
            well.time <-
              reactive.time.filtered()[reactive.wellid.filtered() == well]
            well.rate <-
              reactive.rate.filtered()[reactive.wellid.filtered() == well]
            which.present <- !is.na(well.time) & !is.na(well.rate)
            well.time <- well.time[which.present]
            well.rate <- well.rate[which.present]

            if (length(well.time) <= 2)
                return(NULL)

            time.order <- order(well.time)
            well.time <- well.time[time.order]
            well.rate <- well.rate[time.order]

            if (inherits(well.time, "Date")) {
                well.time <-
                  as.numeric(well.time) - as.numeric(well.time)[1]
            }

            peak.time <- well.time[which.max(well.rate)]
            well.rate <- well.rate[well.time >= peak.time]
            well.time <- well.time[well.time >= peak.time]

            if (length(well.time) <= 2)
                return(NULL)

            result <- fit.fn(well.rate, well.time)
            result$peak.time <- peak.time
            result
        }, simplify=FALSE, USE.NAMES=TRUE)
    })

    output$ratetime <- renderPlot({
        validate(
            need(reactive.valid.filter(), "Invalid columns or wells selected.")
        )

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
              col=palette[match(well, levels(wellid.factor))], lty='dashed')

            well.decline <- reactive.declines()[[well]]
            if (!is.null(well.decline)) {
                well.time.forecast <- as.numeric(well.time)
                well.time.forecast.shift <-
                  well.time.forecast - well.time.forecast[1]
                which.forecast <-
                  which(well.time.forecast.shift >= well.decline$peak.time)
                well.time.forecast.shift <-
                  well.time.forecast.shift[which.forecast]
                well.rate.forecast <- arps.q(well.decline$decline,
                  well.time.forecast.shift)
                lines(well.rate.forecast ~ well.time[which.forecast],
                  col=palette[match(well, levels(wellid.factor))], lty='dotted')
            }
        })
    })

    output$table <- renderTable({
        reactive.data()
    })

    output$declineparams <- renderPrint({
        print(reactive.declines())
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

    output$wellchooser <- renderUI({
        well.names <- sort(unique(reactive.wellid()))
        tagList(h3('Active wells'), selectInput('selectedwells', NULL,
          well.names, multiple=TRUE, selected=well.names, selectize=FALSE,
          size=min(length(well.names), 50)))
    })
})
