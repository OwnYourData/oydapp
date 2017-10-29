# config menu in the top-right corner
# last update:2016-10-06

uiMenu <- function(ns){
        tabPanel(
                # uiOutput(ns('menuList')),
                HTML(paste0("<div id='versionMenuItem'>Version</div></a></li>",
                             "<li><a href=\"https://github.com/OwnYourData/app-",
                             appName,
                             "/blob/master/README.md\" target=\"_blank\">",
                             "<div id='documentationMenuItem'>Dokumentation</div></a></li>",
                             "<li><a href=\"javascript:void(0);\" onclick=\"$('#oyd-startConfig').modal('show');\">",
                             "<div id='configMenuItem'>Konfiguration</div>")),
                 fluidRow(
                         column(1),
                         column(10,
                                tags$div(class='panel panel-default',
                                         tags$div(class='panel-heading',
                                                  style='padding:0',
                                                  tags$h3(class='panel-title pull-left', 'Version',
                                                          style='font-size:200%;padding:10px 15px'),
                                                  tags$div(class='clearfix')
                                         ),
                                         tags$div(class='panel-body',
                                                  tagList(
                                                          h3(paste('akutelle Version:', currVersion)),
                                                          hr(),
                                                          h3('Versionsverlauf'),
                                                          htmlOutput('versionHistory'),
                                                          hr(),
                                                          p('Support: ',
                                                            a(href='mailto:support@ownyourdata.eu',
                                                              'support@ownyourdata.eu')),
                                                          p('Bugtracking: ',
                                                            a(href=paste0('https://github.com/OwnYourData/app-',
                                                                          appName,
                                                                          '/issues'),
                                                              'Github')),
                                                          p('entwickelt von ',
                                                            a(href='https://www.ownyourdata.eu',
                                                              'https://OwnYourData.eu')),
                                                          p('MIT Lizenz, 2016')
                                                  )
                                         )
                                )
                         )
                 ),
                 fluidRow(
                         column(1),
                         column(10,
                                actionButton(ns('backToApp'),
                                             uiOutput(ns('ctrlTrnsl_backToAppBtn')),
                                             style='margin-bottom:20px')),
                         column(1)
                 )
        )
}
