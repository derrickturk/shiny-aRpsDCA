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
input.missingstr <- textInput('missingstr', 'Missing data indicator', '#N/A!')

input.declinetype <- selectInput('declinetype', 'Decline type',
  c('Arps exponential' = 'EXP',
    'Arps hyperbolic' = 'HYP'))
    #'Hyperbolic-to-exponential' = 'H2E'))
input.rateunit <- selectInput('rateunit', 'Rate units',
  c('/ day'='day', '/ month'='month', '/ year'='year'))

shinyUI(fluidPage(
    titlePanel('aRpsDCA interactive demo'),
    sidebarLayout(
        sidebarPanel(
            tabsetPanel(
                tabPanel("File Upload",
                    h3('File selection'),
                    input.datafile,
                    hr(),
                    h3('File format options'),
                    input.headerrow,
                    input.separator,
                    input.quotechar,
                    input.missingstr,
                    hr(),
                    h3('Column Selection'),
                    uiOutput('variableselection')),
                tabPanel("Well Selection",
                    uiOutput('wellchooser')),
                tabPanel("Fit Options",
                    h3('aRpsDCA options'),
                    input.declinetype,
                    input.rateunit,
                    uiOutput('maybetimeunit')))),
        mainPanel(
            tabsetPanel(
                tabPanel('Data', tableOutput('table')),
                tabPanel('Declines', plotOutput('declineparams',
                  height='600px')),
                tabPanel('Plot', plotOutput('ratetime', height='600px')))))))
