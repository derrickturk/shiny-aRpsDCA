# (c) 2015 dwt | terminus data science, LLC

library(shiny)

input.datafile <- fileInput('datafile', 'Data file for rate and time',
  accept=c('text/plain', 'text/csv', 'text/comma-separated-values',
           'text/tab-separated-values', '.csv', '.tsv', '.txt'))
input.headerrow <- checkboxInput('headerrow', 'Header Row?', TRUE)
input.separator <- radioButtons('separator', 'Separator',
  c(Comma=',', Tab='\t', Semicolon=';', Colon=':', Pipe='|'), ',')
input.quotechar <- radioButtons('quotechar', 'Quote character',
  c(Double='"', Single="'", None=''), '"')

shinyUI(fluidPage(
    titlePanel('aRpsDCA interactive demo'),
    sidebarLayout(
        sidebarPanel(
            input.datafile,
            input.headerrow,
            input.separator,
            input.quotechar,
            hr(),
            p("Here's where we'll use uiOutput to generate drop-downs.")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel('Data', tableOutput('table')),
                tabPanel('Plot', plotOutput('decline')))))))
