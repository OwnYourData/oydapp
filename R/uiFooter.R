# footer of the app
# last update:2016-10-06

uiFooter <- function(ns) {
        fluidRow(
                column(1),
                column(8,
                       div(htmlOutput(ns('displayAnalysis')), style='color:white;float:left;width:1px;'),
                       div(htmlOutput(ns('displaySource')), style='color:white;float:left;width:1px;'),
                       div(htmlOutput(ns('displayReport')), style='color:white;float:left;width:1px;'),
                       uiOutput(ns('footerLinks'))),
                column(2,
                       div(style='display:flex; width:200px; margin:-42px; margin-top:0;',
                           div(style='margin:8px;', uiOutput(ns('ctrlTrnsl_langLabel'))),
                           selectInput(ns('lang'), label = NULL, width = '160px',
                                   choices = list("Deutsch" = 'de', "English" = 'en'),
                                   selected = 'de')))
        )
}
