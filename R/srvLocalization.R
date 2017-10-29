srvLocalization <- function(input, output, session) {
        allLocalization <- append(oydLocalization, localization)
        tr <- function(text, lang = ''){
                if(lang == '') lang <- input$lang
                sapply(text,
                       function(s) allLocalization[[s]][[lang]],
                       USE.NAMES=FALSE)
        }

        observeEvent(input$lang, {
                language <- input$lang
                session$sendCustomMessage(type='setTranslation',
                                          paste0('versionMenuItem,',
                                                 tr('versionMenuItem')))
                session$sendCustomMessage(type='setTranslation',
                                          paste0('documentationMenuItem,',
                                                 tr('documentationMenuItem')))
                session$sendCustomMessage(type='setTranslation',
                                          paste0('configMenuItem,',
                                                 tr('configMenuItem')))
                session$sendCustomMessage(type='setTranslation',
                                          paste0('configDialogCloseBtn,',
                                                 tr('configDialogCloseBtn')))
                session$sendCustomMessage(type='setTranslation',
                                          paste0('oyd-help,',
                                                 paste('<i class="fa fa-question-circle" aria-hidden="true"></i>',
                                                        tr('menu_help'))))
                output$displayAnalysis <- renderText('.')
                output$displaySource <- renderText('')
                output$displayReport <- renderText('')

                lapply(names(allLocalization)[grepl('ctrlTrnsl_',
                                                    names(allLocalization))],
                       function(ctrl) output[[ctrl]] <- renderUI(tr(ctrl)))
                rv$v <- rv$v + 1
        })

        output$hdrMyAppsLink <- renderUI({
                # fix for "Raspberry Navigation"
                ns <- session$ns
                updateNavbarPage(session, ns('mainPage'), selected = appName)
                if(!is.null(session$userData$piaUrl) &&
                   nzchar(session$userData$piaUrl)){
                        tags$div(
                                tags$a(href=session$userData$piaUrl,
                                       style='color:#777;',
                                       icon('arrow-left')),
                                tags$a(href=session$userData$piaUrl,
                                       style='color:#777;',
                                       tr('my_apps'))
                        )
                } else {
                        tags$div(
                                tags$img(height='25px',
                                         style='margin-top:-5px',
                                         src=oydLogo),
                                'OwnYourData'
                        )
                }
        })

        output$analysisBtn <- renderUI({
                ns <- session$ns
                tags$button(id=ns('buttonAnalysis'), type='button',
                            class='btn btn-default action-button pull-right',
                            style='padding:15px; border:0; border-radius:0; background-color:#45b79e; border-left-color: #45b79e; border-left-width: 1px; border-left-style: solid; color: white;',
                            icon('line-chart'), tr('section_analysis'))
        })
        output$sourceBtn <- renderUI({
                ns <- session$ns
                tags$button(id=ns('buttonSource'), type='button',
                            class='btn btn-default action-button pull-right',
                            style='padding:15px; border:0; border-radius:0; background-color:#f5f5f5; border-left-color: #45b79e; border-left-width: 1px; border-left-style: solid;',
                            icon('cloud-download'), tr('section_source'))
        })
        output$reportBtn <- renderUI({
                ns <- session$ns
                tags$button(id=ns('buttonReport'), type='button',
                            class='btn btn-default action-button pull-right',
                            style='padding:15px; border:0; border-radius:0; background-color:#f5f5f5; border-left-color: #45b79e; border-left-width: 1px; border-left-style: solid;',
                            icon('book'), tr('section_report'))
        })

        output$footerLinks <- renderUI({
                tagList(
                        tags$div(style='float:right;margin:8px;',
                                 tags$a(href='https://www.ownyourdata.eu',
                                        style='margin-right:15px;',
                                        tr('aboutOYD')),
                                 tags$a(href='https://www.ownyourdata.eu/mobile-apps/',
                                        style='margin-right:15px;',
                                        tr('mobileApps')),
                                 tags$a(href='https://www.ownyourdata.eu/feedback/',
                                        style='margin-right:15px;',
                                        tr('sendFeedback'))
                        )
                )
        })

        return(tr)
}
