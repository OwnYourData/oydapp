# layout for the body of the app
# last update:2016-10-06

uiApp <- function(ns){
        fluidRow(
                column(1),
                column(10,
                       tags$div(class='panel panel-default',
                                tags$div(class='panel-heading',
                                         style='padding:0',
                                         tags$h3(class='panel-title pull-left',
                                                 tagList(tags$img(src='app_logo.png', style='width: 58px;margin-left: -70px;margin-top: -30px;margin-bottom: -19px'),
                                                         uiOutput(ns('ctrlTrnsl_appTitle'), inline = TRUE)),
                                                 style='font-size:200%;padding:10px 15px;margin-left:30px'),
                                         introBox(
                                                uiOutput(ns('reportBtn')),
                                                data.step = 2,
                                                data.intro = 'report Help'),
                                         introBox(
                                                uiOutput(ns('sourceBtn')),
                                                data.step = 1,
                                                data.intro = 'sources'),
                                         uiOutput(ns('analysisBtn')),
                                         tags$div(class='clearfix')

                                ),
                                tags$div(class='panel-body',
                                         conditionalPanel(
                                                 condition = "output['oyd-displayAnalysis'] != ''",
                                                 uiAnalysis(ns)
                                         ),
                                         conditionalPanel(
                                                 condition = "output['oyd-displaySource'] != ''",
                                                 uiSource(ns)
                                         ),
                                         conditionalPanel(
                                                 condition = "output['oyd-displayReport'] != ''",
                                                 uiReport(ns)
                                         )
                                )
                       )
                )
        )
}
