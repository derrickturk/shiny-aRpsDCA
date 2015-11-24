# aRpsDCA interactive demo
# shiny UI
# (c) 2015 dwt | terminus data science, LLC

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

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
