# (c) 2015 dwt | terminus data science, LLC

library(shiny)
library(aRpsDCA)

shinyServer(function (input, output, session) {
    reactive.data <- reactive({
        if (is.null(input$datafile$datapath))
            return(NULL)

        read.delim(file=input$datafile$datapath, sep=input$separator,
                  quote=input$quotechar, header=input$headerrow)
    })

    output$decline <- renderPlot({
        if (is.null(reactive.data()))
            return(NULL)
        plot(reactive.data())
    })

    output$table <- renderTable({
        if (is.null(reactive.data()))
            return(NULL)
        reactive.data()
    })
})
