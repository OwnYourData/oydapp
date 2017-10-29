# basic reactive functions for accessing PIA
# last update: 2016-10-29

baseReactive <- function(input, output, session, tr){
        currApp <- reactive({
                ns <- session$ns
                input$p2next
                input$disconnectPIA
                input$lang
                input$addKeyItem
                input$updateKeyItem
                input$delKeyItem
                rv$v

                shinyBS::closeAlert(session, 'alertPiaStatus')
                app <- list()
                piaMsg <- ''
                pia_url <- session$userData$piaUrl
                app_key <- session$userData$appKey
                app_secret <- session$userData$appSecret
                keyItems <- session$userData$keyItems
                if(is.null(keyItems)) {
                        keyItems <- data.frame()
                }
                if(is.null(pia_url) |
                   is.null(app_key) |
                   is.null(app_secret)) {
                        piaMsg <- tr('missingIncompletePiaData')
                } else {
                        if((nchar(pia_url) > 0) &
                           (nchar(app_key) > 0) &
                           (nchar(app_secret) > 0)) {
                                app <- setupApp(pia_url,
                                                app_key,
                                                app_secret,
                                                keyItems)
                                if(length(app) == 0){
                                        piaMsg <- tr('invalidPiaData')
                                } else {
                                        if(is.na(app$token)){
                                                piaMsg <- tr('invalidPiaData')
                                        }
                                }
                        } else {
                                piaMsg <- tr('missingIncompletePiaData')
                        }
                }

                if(nchar(piaMsg) > 0){
                        shinyBS::createAlert(session, 'piaStatus',
                                    alertId = 'alertPiaStatus',
                                    style = 'warning', append = FALSE,
                                    title = tr('piaConnectionMsgTitle'),
                                    content = piaMsg)
                        app <- list()
                } else {
                        shinyBS::closeAlert(session, 'alertPiaStatus')
                        url <- itemsUrl(app$url, appRepoDefault)
                        retVal <- readRawItems(app, url)
                        if(nrow(retVal) > 0){
                                if(checkItemEncryption(retVal)){
                                        if(nrow(keyItems) == 0){
                                                shinyBS::createAlert(
                                                        session, 'piaStatus',
                                                        alertId = 'alertPiaStatus',
                                                        style = 'warning', append = FALSE,
                                                        title = tr('piaEncryptedMsgTitle', input$lang),
                                                        content = tr('piaEncryptedMsg', input$lang))
                                        }
                                }
                        }
                }
                app
        })

        currData <- reactive({
                # list any input controls that effect currData
                input$modalPiaUrl
                input$modalPiaId
                input$modalPiaSecret
                input$p2next

                app <- currApp()
                retVal <- data.frame()
                if(length(app) > 0) {
                        url <- itemsUrl(app$url, appRepoDefault)
                        retVal <- readItems(app, url)
                }
                retVal
        })

        currDataDateSelectTimestamp <- reactive({
                shinyBS::closeAlert(session, ns('myDataStatus'))
                data <- currData()
                if(nrow(data) > 0){
                        mymin <- as.Date(input$dateRange[1], '%d.%m.%Y')
                        mymax <- as.Date(input$dateRange[2], '%d.%m.%Y')
                        if(mymax > mymin){
                                daterange <- seq(mymin, mymax, 'days')
                                data$dat <- as.Date(as.POSIXct(data$time/1000, origin='1970-01-01'))
                                data <- data[data$dat %in% daterange, ]
                                if(nrow(data) > 0){
                                        data
                                } else {
                                        shinyBS::createAlert(session, ns('dataStatus'),
                                                    alertId = 'myDataStatus',
                                                    style = 'warning', append = FALSE,
                                                    title = 'Keine Daten im gewählten Zeitfenster',
                                                    content = 'Für das ausgewählte Zeitfenster sind keine Daten vorhanden.')
                                        data.frame()
                                }
                        } else {
                                shinyBS::createAlert(session, ns('dataStatus'),
                                            alertId = 'myDataStatus',
                                            style = 'warning', append = FALSE,
                                            title = 'Ungültiges Zeitfenster',
                                            content = 'Im ausgewählten Zeitfenster liegt das End-Datum vor dem Beginn-Datum. Korriege die Eingabe!')
                                data.frame()
                        }
                } else {
                        shinyBS::createAlert(session, ns('dataStatus'),
                                    alertId = 'myDataStatus',
                                    style = 'warning', append = FALSE,
                                    title = 'Keine Website-Daten im Datentresor vorhanden',
                                    content = 'Derzeit sind noch keine Website-Daten im Datentresor gespeichert. Wechsle zu "Datenquellen" und installiere das passende Plugin für deinen Browser!')
                        data.frame()
                }
        })

        checkInconsistencyWrite <- function(repoEncrypted){
                ns <- session$ns
                msg <- ''
                if(repoEncrypted){
                        msg <- tr('checkInconsistencyWriteUnencryptedTxt')
                } else {
                        msg <- tr('checkInconsistencyWriteEncryptedTxt')
                }
                shiny::modalDialog(
                        shiny::span(msg),
                        footer = shiny::tagList(
                                shiny::actionButton(
                                        ns('writeInconsistencyCancelBtn'),
                                        tr('cancelLbl')),
                        shiny::actionButton(
                                        ns('writeInconsistencyBtn'),
                                        tr('okLbl'))),
                        size = 's'
                )
        }

        return(list(currApp=currApp,
                    currData=currData,
                    currDataDateSelectTimestamp=currDataDateSelectTimestamp,
                    checkInconsistencyWrite=checkInconsistencyWrite))
}
