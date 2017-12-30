# Module with all server-side functionality for OYD Apps
# last update: 2017-06-13

srvModule <- function(input, output, session, tr, notify, appStart) {
        # initialization ==================================
        observe(priority = 1000, {
                ns <- session$ns
                userLang <- input$userLang
                if(is.null(userLang)) {
                        userLang <- ''
                }
                if(userLang == 'de'){
                        shiny::updateSelectInput(
                                session,
                                'lang',
                                selected = 'de')
                } else {
                        shiny::updateSelectInput(
                                session,
                                'lang',
                                selected = 'en')
                }
        })

        observe(priority = 900, {
                if(session$userData$initFlag){
                        session$userData$initFlag <- FALSE
                        ns <- session$ns
                        urlParams <-
                                parseQueryString(session$clientData$url_search)
                        urlParamExist <- FALSE
                        if(is.null(urlParams[['PIA_URL']])){
                                session$userData$piaUrl <-
                                        input$store$pia_url
                        } else {
                                session$userData$piaUrl <-
                                        urlParams[['PIA_URL']]
                                urlParamExist <- TRUE
                        }
                        if(is.null(urlParams[['APP_KEY']])){
                                session$userData$appKey <-
                                        input$store$app_key
                        } else {
                                session$userData$appKey <-
                                        urlParams[['APP_KEY']]
                                urlParamExist <- TRUE
                        }
                        if(is.null(urlParams[['APP_SECRET']])){
                                session$userData$appSecret <-
                                        input$store$app_secret
                        } else {
                                session$userData$appSecret <-
                                        urlParams[['APP_SECRET']]
                                urlParamExist <- TRUE
                        }
                        if(urlParamExist){
                                if(is.null(input$store$pia_url) |
                                   is.null(input$store$app_key) |
                                   is.null(input$store$app_secret)){

                                } else {
                                        if((input$store$pia_url == urlParams[['PIA_URL']]) &
                                           (input$store$app_key == urlParams[['APP_KEY']]) &
                                           (input$store$app_secret == urlParams[['APP_SECRET']])){

                                        } else {
                                                shinyStore::updateStore(session, "pia_url",
                                                                        session$userData$piaUrl)
                                                shinyStore::updateStore(session, "app_key",
                                                                        session$userData$appKey)
                                                shinyStore::updateStore(session, "app_secret",
                                                                        session$userData$appSecret)
                                                shiny::showNotification(tr('msgNewConnectionData'))
                                        }
                                }
                        }

                        app <- setupApp(session$userData$piaUrl,
                                        session$userData$appKey,
                                        session$userData$appSecret,
                                        NA)
                        if(length(app) > 0) {
                                shinyBS::closeAlert(session, 'alertPiaStatus')
                                updateTextInput(session, 'modalPiaUrl',
                                                value=session$userData$piaUrl)
                                updateTextInput(session, 'modalPiaId',
                                                value=session$userData$appKey)
                                updateTextInput(session, 'modalPiaSecret',
                                                value=session$userData$appSecret)
                                output$currentToken <- renderUI({
                                        HTML(paste0('<strong>',
                                                    tr('configDialogStep2currentTokenLbl'),
                                                    '</strong><br>',
                                                    app$token,
                                                    '<br><br>'))
                                })
                                session$sendCustomMessage(
                                        type='setPiaUrl',
                                        session$userData$piaUrl)
                                url <- itemsUrl(app$url, appRepoDefault)
                                retVal <- readRawItems(app, url)

                                # key management
                                # check if keyInfo is available in local storage
                                keyInfo <- input$store$oyd_keys
                                if(is.null(keyInfo)){
                                        keyInfo <- ''
                                }
                                if(nzchar(keyInfo)){
                                        # yes (local storage has keyInfo)
                                        # check if keyInfo is encrypted
                                        if(encryptedKeyInfo(keyInfo)){
                                                # yes (keyInfo in local storage is encrypted)
                                                session$userData$openDialog <- 'decryptConfigDialog'
                                                shiny::showModal(decryptConfigDialog())
                                        } else {
                                                # no (keyInfo in local storage is either unencrypted or invalid)
                                                # check if keyInfo contains valid keys
                                                if(validKeyInfo(keyInfo)){
                                                        # yes (keyInfo has valid keys)
                                                        session$userData$keyItems <- parseKeyInfo(keyInfo)
                                                        shiny::showNotification(
                                                                paste(nrow(session$userData$keyItems),
                                                                      tr('msgKeyImport')))
                                                        shiny::showNotification(
                                                                tr('msgUnencryptedKeyInfo'),
                                                                type = 'warning')
                                                        keyList()
                                                        rv$v <- rv$v + 1
                                                        appStart()
                                                } else {
                                                        # no (keyInfo is corrupt or no data at all)
                                                        session$userData$keyItems <- data.frame()
                                                        # available data in PIA for current app?
                                                        if(nrow(retVal) > 0){
                                                                shinyBS::createAlert(
                                                                        session, 'piaStatus',
                                                                        alertId = 'alertPiaStatus',
                                                                        style = 'warning', append = FALSE,
                                                                        title = tr('piaEncryptedDataCorruptKeyInfoTitle'),
                                                                        content = tr('piaEncryptedDataCorruptKeyInfoMsg'))
                                                        }
                                                        keyList()
                                                        rv$v <- rv$v + 1
                                                        appStart()
                                                }
                                        }
                                } else {
                                        # no (local storage has no keyInfo)
                                        # available data in PIA for current app?
                                        if(nrow(retVal) > 0){
                                                # yes (there is data)
                                                # check if data is encrypted
                                                if(checkItemEncryption(retVal)){
                                                        # yes (data is encrypted)
                                                        session$userData$openDialog <- 'decryptDialog'
                                                        shiny::showModal(decryptDialog())
                                                } else {
                                                        # no (data is raw)
                                                        shiny::showNotification(
                                                                tr('msgUnencryptedData'),
                                                                type = 'warning')
                                                        keyList()
                                                        rv$v <- rv$v + 1
                                                        appStart()
                                                }
                                        } else {
                                                # no (no data available yet)
                                                session$userData$openDialog <- 'encryptDialog'
                                                shiny::showModal(encryptDialog())
                                        }

                                }
                        } else {
                                shinyBS::createAlert(
                                        session, 'piaStatus',
                                        alertId = 'alertPiaStatus',
                                        style = 'warning', append = FALSE,
                                        title = tr('piaConnectionMsgTitle'),
                                        content = tr('missingIncompletePiaData'))

                                updateTextInput(
                                        session, 'modalPiaUrl',
                                        value=session$userData$piaUrl)
                                updateTextInput(
                                        session, 'modalPiaId',
                                        value=session$userData$appKey)
                                updateTextInput(
                                        session, 'modalPiaSecret',
                                        value=session$userData$appSecret)
                                output$currentToken <- renderText('')
                        }
                }
        })

        observe(priority = -1000, {
                session$sendCustomMessage(type='finishInit', NA)
                oydLog('App complete')
        })

        # Config Dialog =======================
        shiny::observeEvent(input$startConfig, {
                typeOptions <- c(1:2)
                names(typeOptions) <- c(
                        tr('writeOnlyOption'),
                        tr('readWriteOption'))
                updateSelectInput(session, 'keyType',
                                  choices = typeOptions,
                                  selected = 2)
                keyList()
        })

        shiny::observeEvent(input$collapse, {
                cp <- input$collapse
                cp <- cp[length(cp)]
                shinyBS::updateCollapse(
                        session, 'collapse', open = cp,
                        style = list(
                                'Willkommen' = if(cp == 'Willkommen') 'primary' else 'info',
                                'PIA' = if(cp == 'PIA') 'primary' else 'info',
                                'Keys' = if(cp == 'Keys') 'primary' else 'info',
                                'Fertig' = if(cp == 'Fertig') 'primary' else 'info'))
        })

        output$connectError <- renderUI({
                pia_url <- input$modalPiaUrl
                app_key <- input$modalPiaId
                app_secret <- input$modalPiaSecret
                auth_url <- paste0(pia_url, '/oauth/token')
                # reduce response timeout to 10s to avoid hanging app
                # https://curl.haxx.se/libcurl/c/CURLOPT_CONNECTTIMEOUT.html
                optTimeout <- RCurl::curlOptions(connecttimeout = 10)
                response <- tryCatch(
                        RCurl::postForm(auth_url,
                                        client_id     = app_key,
                                        client_secret = app_secret,
                                        grant_type    = 'client_credentials',
                                        .opts         = optTimeout),
                        error = function(e) { return(as.character(e)) })
                if (is.na(response)) {
                        tr('noPiaResponseError')
                } else {
                        if(jsonlite::validate(response)){
                                ''
                        } else {
                                if(grepl('<url> malformed', response)){
                                        tr('malformedPiaUrl')
                                } else {
                                        if(grepl('error', response,
                                                 ignore.case = TRUE)){
                                                response
                                        } else {
                                                paste(tr('errorLbl'), response)
                                        }
                                }
                        }
                }
        })

        observeEvent(input$p1next, ({
                shinyBS::updateCollapse(session,
                                        'collapse',
                                        open = 'PIA',
                                        style = list(
                                                "Willkommen" = 'info',
                                                'PIA' = 'primary',
                                                'Keys' = 'info',
                                                'Fertig' = 'info'))
        }))

        observeEvent(input$p2prev, {
                shinyBS::updateCollapse(session, 'collapse',
                                        open = 'Willkommen',
                                        style = list(
                                                "Willkommen" = 'primary',
                                                'PIA' = 'info',
                                                'Keys' = 'info',
                                                'Fertig' = 'info'))
        })

        observeEvent(input$disconnectPIA, {
                shinyStore::updateStore(session, 'pia_url', NA)
                shinyStore::updateStore(session, 'app_key', NA)
                shinyStore::updateStore(session, 'app_secret', NA)
                updateTextInput(session, 'modalPiaSecret', value='')
                updateTextInput(session, 'modalPiaId', value='')
                updateTextInput(session, 'modalPiaUrl', value='')
                session$userData$piaUrl <- ''
                session$userData$appKey <- ''
                session$userData$appSecret <- ''
                shinyBS::createAlert(session, 'piaStatus',
                                     alertId = 'alertPiaStatus',
                                     style = 'warning', append = FALSE,
                                     title = tr('piaConnectionMsgTitle'),
                                     content = tr('missingIncompletePiaData'))
        })

        observeEvent(input$p2next, {
                ns <- session$ns
                withBusyIndicatorServer(ns('p2next'), {
                        shinyStore::updateStore(session, "pia_url",
                                                isolate(input$modalPiaUrl))
                        shinyStore::updateStore(session, "app_key",
                                                isolate(input$modalPiaId))
                        shinyStore::updateStore(session, "app_secret",
                                                isolate(input$modalPiaSecret))
                        session$userData$piaUrl <-
                                isolate(input$modalPiaUrl)
                        session$userData$appKey <-
                                isolate(input$modalPiaId)
                        session$userData$appSecret <-
                                isolate(input$modalPiaSecret)

                        token <- getToken(session$userData$piaUrl,
                                          session$userData$appKey,
                                          session$userData$appSecret)
                        if(is.na(token)){
                                shinyBS::createAlert(
                                        session, 'piaStatus',
                                        alertId = 'alertPiaStatus',
                                        style = 'warning', append = FALSE,
                                        title = tr('piaConnectionMsgTitle'),
                                        content=tr('missingIncompletePiaData'))
                        } else {
                                shinyBS::closeAlert(session, 'alertPiaStatus')
                        }
                        output$currentToken <- renderUI({
                                HTML(paste0('<strong>',
                                            tr('configDialogStep2currentTokenLbl'),
                                            '</strong><br>',
                                            token,
                                            '<br><br>'))
                        })
                })
                shinyBS::updateCollapse(session,
                                        'collapse',
                                        open = 'Keys',
                                        style = list(
                                                "Willkommen" = 'info',
                                                'PIA' = 'info',
                                                'Keys' = 'primary',
                                                'Fertig' = 'info'))
        })

        observeEvent(input$p2skip, {
                shinyBS::updateCollapse(session,
                                        'collapse',
                                        open = 'Keys',
                                        style = list(
                                                "Willkommen" = 'info',
                                                'PIA' = 'info',
                                                'Keys' = 'primary',
                                                'Fertig' = 'info'))
        })

        observeEvent(input$p3next, {
                # no need to save key information here: it is applied
                # immediately and import/export is used for persistency
                shinyBS::updateCollapse(session,
                                        'collapse',
                                        open = 'Fertig',
                                        style = list(
                                                "Willkommen" = 'info',
                                                'PIA' = 'info',
                                                'Keys' = 'info',
                                                'Fertig' = 'primary'))
        })

        observeEvent(input$p3skip, {
                shinyBS::updateCollapse(session,
                                        'collapse',
                                        open = 'Fertig',
                                        style = list(
                                                "Willkommen" = 'info',
                                                'PIA' = 'info',
                                                'Keys' = 'info',
                                                'Fertig' = 'primary'))
        })

        observeEvent(input$p3prev, {
                shinyBS::updateCollapse(session,
                                        'collapse',
                                        open = 'PIA',
                                        style = list(
                                                "Willkommen" = 'info',
                                                'PIA' = 'primary',
                                                'Keys' = 'info',
                                                'Fertig' = 'info'))
        })

        observeEvent(input$p4prev, {
                shinyBS::updateCollapse(session,
                                        'collapse',
                                        open = 'Keys',
                                        style = list(
                                                "Willkommen" = 'info',
                                                'PIA' = 'info',
                                                'Keys' = 'primary',
                                                'Fertig' = 'info'))
        })

        observeEvent(input$p4close, {
                shinyBS::toggleModal(session, 'startConfig', toggle = "toggle")
        })

        # Key Handling ====================================
        encryptedKeyInfo <- function(keyInfo){
                inputJSON <- tryCatch(
                        as.data.frame(jsonlite::fromJSON(keyInfo)),
                        error = function(e) { return(data.frame()) })
                if(nrow(inputJSON) == 0){
                        FALSE
                } else {
                        if((nrow(inputJSON) == 1) &
                           (all(c('cipher','nonce') %in% colnames(inputJSON)))){
                                TRUE
                        } else {
                                FALSE
                        }
                }
        }

        validKeyInfo <- function(keyInfo){
                inputJSON <- tryCatch(
                        as.data.frame(jsonlite::fromJSON(keyInfo)),
                        error = function(e) { return(data.frame()) })
                if(nrow(inputJSON) == 0){
                        FALSE
                } else {
                        if(all(c('title', 'repo', 'key', 'read') %in% colnames(inputJSON))){
                                TRUE
                        } else {
                                FALSE
                        }
                }
        }

        parseKeyInfo <- function(keyInfo){
                inputJSON <- tryCatch(
                        as.data.frame(jsonlite::fromJSON(keyInfo)),
                        error = function(e) { return(data.frame()) })
                if(nrow(inputJSON) == 0){
                        data.frame()
                } else {
                        if(all(c('title', 'repo', 'key', 'read') %in% colnames(inputJSON))){
                                inputJSON
                        } else {
                                data.frame()
                        }
                }
        }

        observeEvent(input$userLang, {
                ns <- session$ns
                userLang <- input$userLang
                if(is.null(userLang)) {
                        userLang <- ''
                }
                if(userLang == 'de'){
                        shiny::updateSelectInput(
                                session,
                                'lang',
                                selected = 'de')
                } else {
                        shiny::updateSelectInput(
                                session,
                                'lang',
                                selected = 'en')
                }
                switch(session$userData$openDialog,
                'encryptDialog'={
                        shiny::showModal(encryptDialog(lang = input$userLang))
                },
                'decryptDialog'={
                        shiny::showModal(decryptDialog(lang = input$userLang))
                },
                'decryptConfigDialog'={
                        shiny::showModal(decryptConfigDialog(lang = input$userLang))
                })
        })

        encryptDialog <- function(failed = FALSE, errorMsg = '', lang = 'de'){
                ns <- session$ns
                encryptOptions <- c(1:3)
                names(encryptOptions) <- c(
                        tr('encryptSimpleOption', lang),
                        tr('encryptAppOption', lang),
                        tr('encryptCustomOption', lang))

                shiny::modalDialog(
                        shiny::span(tr('encryptDialogText', lang)),
                        br(),br(),
                        shiny::fluidRow(shiny::column(
                                6,
                                shiny::radioButtons(
                                        ns('encryptOptionType'),
                                        label = tr('encryptOptionsLbl', lang),
                                        choices = encryptOptions,
                                        selected = 2)),
                                shiny::column(
                                        6, shiny::conditionalPanel(
                                                sprintf("input['%s'] == '1'",
                                                        ns('encryptOptionType')),
                                                shiny::passwordInput(
                                                        ns('simpleEncryptPassword'),
                                                        tr('keyLbl', lang)),
                                                shiny::em(tr('simpleEncryptInfo', lang))),
                                        shiny::conditionalPanel(sprintf("input['%s'] == '2'",
                                                ns('encryptOptionType')),
                                                div(
                                                        shiny::passwordInput(
                                                                ns('appEncryptPassword'),
                                                                tr('passwordLbl', lang)),
                                                        shiny::downloadButton(ns('downloadAppKey'),
                                                                              "Download",
                                                                              style = 'height: 35px; margin: 24px;'),
                                                        style='display:flex;'),
                                                shiny::em(tr('appEncryptInfo', lang))),
                                        shiny::conditionalPanel(sprintf("input['%s'] == '3'",
                                                ns('encryptOptionType')),
                                                shiny::em(tr('customEncryptInfo', lang))))),
                        if (failed)
                                shiny::div(shiny::tags$b(
                                        errorMsg,
                                        style = "color: red;")),

                        footer = shiny::tagList(
                                shiny::actionButton(
                                        ns('cancelEncryptBtn'),
                                        tr('closeLbl', lang)),
                                shiny::actionButton(
                                        ns('encryptBtn'),
                                        tr('encryptLbl', lang))),
                        size = 'l'
                )
        }

        output$downloadAppKey <- shiny::downloadHandler(
                filename = 'keyfile.json',
                content = function(file) {
                        keyRecord <- data.frame(
                                title = 'Datentresor',
                                repo = 'eu.ownyourdata',
                                key = raw2str(sodium::keygen()),
                                read = TRUE, stringsAsFactors = FALSE)
                        session$userData$keyItems <- keyRecord

                        if(nzchar(input$appEncryptPassword)){
                                origRaw <- charToRaw(as.character(
                                        jsonlite::toJSON(keyRecord)))
                                key <- sodium::sha256(charToRaw(
                                        input$appEncryptPassword))
                                nonce   <- sodium::random(24)
                                cipher <- sodium::data_encrypt(origRaw,
                                                               key,
                                                               nonce)
                                nonceStr <- paste0(
                                        as.hexmode(as.integer(nonce)),
                                        collapse = '')
                                cipherStr <- paste0(
                                        as.hexmode(as.integer(cipher)),
                                        collapse = '')
                                keyData <- as.character(jsonlite::toJSON(list(
                                        cipher = cipherStr,
                                        nonce = nonceStr), auto_unbox = TRUE))
                                # shinyStore::updateStore(session, 'oyd_keys', keyData)
                        } else {
                                keyData <- as.character(
                                        jsonlite::toJSON(keyRecord))
                        }
                        write(keyData, file)
                }
        )

        observeEvent(input$encryptBtn, {
                switch(as.character(input$encryptOptionType),
                '1'={
                        keyStr <- input$simpleEncryptPassword
                        if(nzchar(keyStr)){
                                keyRecord <- data.frame(
                                        title = 'Datentresor',
                                        repo = 'eu.ownyourdata',
                                        key = raw2str(sodium::sha256(
                                                charToRaw(keyStr))),
                                        read = TRUE, stringsAsFactors = FALSE)
                                session$userData$keyItems <- keyRecord
                                keyList()
                                # re-trigger currApp (necessary for create/update/delete)
                                rv$v <- rv$v + 1
                                appStart()
                                removeModal()
                                session$userData$openDialog <- ''
                        } else {
                                showModal(encryptDialog(failed = TRUE,
                                                        tr('missingSimpleEncryptPassword')))
                        }
                },
                '2'={
                        keyRecord <- session$userData$keyItems
                        if(is.null(keyRecord)) {
                                keyRecord <- data.frame()
                        }
                        if(nrow(keyRecord) > 0){
                                if(nzchar(input$appEncryptPassword)){
                                        origRaw <- charToRaw(as.character(
                                                jsonlite::toJSON(keyRecord)))
                                        key <- sodium::sha256(charToRaw(
                                                input$appEncryptPassword))
                                        nonce   <- sodium::random(24)
                                        cipher <- sodium::data_encrypt(origRaw,
                                                                       key,
                                                                       nonce)
                                        nonceStr <- paste0(
                                                as.hexmode(as.integer(nonce)),
                                                collapse = '')
                                        cipherStr <- paste0(
                                                as.hexmode(as.integer(cipher)),
                                                collapse = '')
                                        keyData <- as.character(jsonlite::toJSON(list(
                                                cipher = cipherStr,
                                                nonce = nonceStr), auto_unbox = TRUE))
                                        shiny::showNotification(paste(nrow(session$userData$keyItems),
                                                                      tr('msgKeyExportBrowser')))
                                } else {
                                        keyData <- as.character(
                                                jsonlite::toJSON(keyRecord))
                                        shiny::showNotification(paste(nrow(session$userData$keyItems),
                                                                      tr('msgKeyExportBrowserUnencrypted')))
                                }
                        } else {
                                keyRecord <- data.frame(
                                        title = appTitle,
                                        repo = app_id,
                                        key = raw2str(sodium::keygen()),
                                        read = TRUE, stringsAsFactors = FALSE)
                                session$userData$keyItems <- keyRecord
                                if(nzchar(input$appEncryptPassword)){
                                        origRaw <- charToRaw(as.character(
                                                jsonlite::toJSON(keyRecord,
                                                                 pretty = pretty)))
                                        key <- sodium::sha256(charToRaw(
                                                input$appEncryptPassword))
                                        nonce   <- sodium::random(24)
                                        cipher <- sodium::data_encrypt(origRaw,
                                                                       key,
                                                                       nonce)
                                        nonceStr <- paste0(
                                                as.hexmode(as.integer(nonce)),
                                                collapse = '')
                                        cipherStr <- paste0(
                                                as.hexmode(as.integer(cipher)),
                                                collapse = '')
                                        keyData <- as.character(jsonlite::toJSON(list(
                                                cipher = cipherStr,
                                                nonce = nonceStr), auto_unbox = TRUE))

                                        shiny::showNotification(paste(nrow(session$userData$keyItems),
                                                                      tr('msgKeyExportBrowser')))

                                } else {
                                        shiny::showNotification(paste(nrow(session$userData$keyItems),
                                                                      tr('msgKeyExportBrowserUnencrypted')))
                                        keyData <- as.character(
                                                jsonlite::toJSON(keyRecord))
                                }
                        }
                        shinyStore::updateStore(session, 'oyd_keys', keyData)
                        keyList()
                        # re-trigger currApp (necessary for create/update/delete)
                        rv$v <- rv$v + 1
                        appStart()
                        removeModal()
                        session$userData$openDialog <- ''
                },
                '3'={
                        shiny::showNotification(
                                tr('msgUnencryptedData'),
                                type = 'warning')
                        keyList()
                        # re-trigger currApp (necessary for create/update/delete)
                        rv$v <- rv$v + 1
                        appStart()
                        removeModal()
                        session$userData$openDialog <- ''
                })
        })

        observeEvent(input$cancelEncryptBtn, {
                shinyStore::updateStore(session, 'oyd_keys', NA)
                session$userData$keyItems <- data.frame()
                shiny::showNotification(
                        tr('msgUnencryptedData'),
                        type = 'warning')
                keyList()
                # re-trigger currApp (necessary for create/update/delete)
                rv$v <- rv$v + 1
                appStart()
                removeModal()
                session$userData$openDialog <- ''
        })

        decryptDialog <- function(failed = FALSE, errorMsg = '', lang = 'de'){
                ns <- session$ns
                shiny::modalDialog(
                        shiny::span(tr('decryptDialogText', lang = lang)),
                        shiny::fluidRow(column(12,
                                               shiny::passwordInput(
                                                       ns('masterKey'),
                                                       tr('keyLbl', lang = lang)))),
                        if (failed)
                                shiny::div(shiny::tags$b(
                                        errorMsg,
                                        style = "color: red;")),

                        footer = shiny::tagList(
                                shiny::actionButton(
                                        ns('cancelDecryptBtn'),
                                        tr('closeLbl', lang)),
                                shiny::actionButton(
                                        ns('decryptBtn'),
                                        tr('decryptLbl', lang))),
                        size = 's'
                )
        }

        observeEvent(input$cancelDecryptBtn, {
                shinyBS::createAlert(
                        session, 'piaStatus',
                        alertId = 'alertPiaStatus',
                        style = 'warning', append = FALSE,
                        title = tr('piaEncryptedMsgTitle'),
                        content = tr('piaEncryptedMsg'))
                keyList()
                # re-trigger currApp (necessary for create/update/delete)
                rv$v <- rv$v + 1
                appStart()
                removeModal()
                session$userData$openDialog <- ''
        })

        observeEvent(input$decryptBtn, {
                keyStr <- input$masterKey
                privateKey <- sodium::sha256(charToRaw(keyStr))
                app <- setupApp(session$userData$piaUrl,
                                session$userData$appKey,
                                session$userData$appSecret,
                                session$userData$keyItems)
                if(checkValidKey(app, appRepoDefault, privateKey)){
                        keyRecord <- data.frame(
                                title = 'Datentresor',
                                repo = 'eu.ownyourdata',
                                key = raw2str(sodium::sha256(
                                        charToRaw(keyStr))),
                                read = TRUE, stringsAsFactors = FALSE)
                        session$userData$keyItems <- keyRecord
                        keyList()
                        # re-trigger currApp (necessary for create/update/delete)
                        rv$v <- rv$v + 1
                        appStart()
                        removeModal()
                        session$userData$openDialog <- ''
                } else {
                        session$userData$openDialog <- 'decryptDialog'
                        shiny::showModal(decryptDialog(
                                failed = TRUE,
                                errorMsg = tr('msgDecryptError', input$lang),
                                lang = input$lang))
                }
        })

        decryptConfigDialog <- function(failed = FALSE, errorMsg = '', lang = 'de'){
                ns <- session$ns
                shiny::modalDialog(
                        shiny::span(tr('decryptDialogText', lang)),
                        shiny::fluidRow(column(12,
                                               shiny::passwordInput(
                                                       ns('masterPassword'),
                                                       tr('passwordLbl', lang)))),
                        if (failed)
                                shiny::div(shiny::tags$b(
                                        errorMsg,
                                        style = "color: red;")),

                        footer = shiny::tagList(
                                shiny::actionButton(
                                        ns('cancelDecryptConfigBtn'),
                                        tr('closeLbl', lang)),
                                shiny::actionButton(
                                        ns('decryptConfigBtn'),
                                        tr('decryptLbl', lang))),
                        size = 's'
                )
        }

        observeEvent(input$cancelDecryptConfigBtn, {
                shinyBS::createAlert(
                        session, 'piaStatus',
                        alertId = 'alertPiaStatus',
                        style = 'warning', append = FALSE,
                        title = tr('piaEncryptedMsgTitle'),
                        content = tr('piaEncryptedMsg'))
                keyList()
                # re-trigger currApp (necessary for create/update/delete)
                rv$v <- rv$v + 1
                appStart()
                removeModal()
                session$userData$openDialog <- ''
        })

        observeEvent(input$decryptConfigBtn, {
                oyd_keys <- input$store$oyd_keys
                errorMsg <- ''
                inputJSON <- tryCatch(
                        as.data.frame(jsonlite::fromJSON(oyd_keys)),
                        error = function(e) { return(data.frame()) })
                if(nrow(inputJSON) == 0){
                        errorMsg <- tr('msgInvaldImport')
                }
                if(errorMsg == ''){
                        if((nrow(inputJSON) == 1) &
                           (all(c('cipher','nonce') %in% colnames(inputJSON)))){
                                cipher <- str2raw(as.character(
                                        inputJSON[1, 'cipher']))
                                nonce <- str2raw(as.character(
                                        inputJSON[1, 'nonce']))
                                key <- sodium::sha256(charToRaw(
                                        input$masterPassword))
                                inputJSON <- tryCatch(
                                        as.data.frame(jsonlite::fromJSON(rawToChar(
                                                sodium::data_decrypt(cipher,
                                                                     key,
                                                                     nonce)))),
                                        error = function(e)
                                        { return(data.frame())})
                                if(nrow(inputJSON) == 0){
                                        errorMsg <- tr('msgInvalidDecryptKey')
                                }
                        } else {
                                errorMsg <- tr('msgInvalidEncryptedKeyImport')
                        }
                }
                if(errorMsg == ''){
                        if(!all(c('title', 'repo', 'key', 'read') %in%
                                colnames(inputJSON))){
                                errorMsg <- tr('msgInvaldImport')
                        }
                }
                if(errorMsg == ''){
                        session$userData$keyItems <- inputJSON
                        updateSelectInput(
                                session,
                                'keyList',
                                choices = session$userData$keyItems$title,
                                selected = NA)
                        shiny::showNotification(paste(nrow(inputJSON),
                                                      tr('msgKeyImport')))
                        keyList()
                        removeModal()
                        session$userData$openDialog <- ''
                        # re-trigger currApp (necessary for create/update/delete)
                        rv$v <- rv$v + 1
                        appStart()
                } else {
                        showModal(decryptConfigDialog(failed = TRUE, errorMsg))
                }
        })

        # initialize list
        keyList <- function(){
                allItems <- session$userData$keyItems
                keyTitles <- vector()
                if(nrow(allItems) > 0){
                        keyTitles <- allItems$title
                } else {
                        keyTitles <- tr('currentlyNoEncryptionText')
                }
                updateSelectInput(
                        session,
                        'keyList',
                        choices = keyTitles,
                        selected = NA)
        }

        # selecting an item in the key list
        observeEvent(input$keyList, {
                selItem <- input$keyList
                if(length(selItem)>1){
                        selItem <- selItem[1]
                        updateSelectInput(session,
                                          'keyList',
                                          selected = selItem)
                }
                if(selItem == tr('currentlyNoEncryptionText')){
                        updateTextInput(session, 'keyTitle',
                                        value = '')
                        updateTextInput(session, 'keyRepo',
                                        value = '')
                        updateTextInput(session, 'keyKeystr',
                                        value = '')
                        updateSelectInput(session, 'keyType',
                                          selected = 2)
                } else {
                        allItems <- session$userData$keyItems
                        selItemName <- selItem
                        selItemRepo <- allItems[allItems$title == selItem,
                                                'repo']
                        selItemKey <- allItems[allItems$title == selItem,
                                               'key']
                        selItemRead <- allItems[allItems$title == selItem,
                                                'read']
                        updateTextInput(session, 'keyTitle',
                                        value = selItemName)
                        updateTextInput(session, 'keyRepo',
                                        value = trimws(as.character(selItemRepo)))
                        updateTextInput(session, 'keyKeystr',
                                        value = trimws(as.character(selItemKey)))
                        if(selItemRead){
                                updateSelectInput(session, 'keyType',
                                                  selected = 2)
                        } else {
                                updateSelectInput(session, 'keyType',
                                                  selected = 1)
                        }
                        # re-trigger currApp (necessary for create/update/delete)
                        rv$v <- rv$v + 1
                }
        })

        # adding an item to the key list
        observeEvent(input$addKeyItem, {
                errMsg <- ''
                itemName <- input$keyTitle
                itemRepo <- input$keyRepo
                itemKey  <- input$keyKeystr
                itemRead <- input$keyType
                itemReadBoolean = FALSE
                if(itemRead == 2){
                        itemReadBoolean = TRUE
                }

                allItems <- session$userData$keyItems
                if(itemName %in% allItems$title){
                        errMsg <- tr('msgNameInUse')
                }
                if(errMsg == ''){
                        if(!grepl("^[0-9a-f]{64}$", itemKey, perl = TRUE)){
                                errMsg <- tr('msgInvalidKey')
                        }
                }
                if(errMsg == ''){
                        initNames <- allItems$title
                        newItem <- c(
                                title = itemName,
                                repo = itemRepo,
                                key = itemKey,
                                read = itemReadBoolean)
                        if(nrow(allItems) > 0){
                                keyItems <- rbind(allItems, newItem)
                        } else {
                                keyItems <- as.data.frame(
                                        t(newItem), stringsAsFactors = FALSE)
                        }
                        session$userData$keyItems <- keyItems
                        updateSelectInput(session,
                                          'keyList',
                                          choices = c(initNames, itemName),
                                          selected = NA)
                        updateTextInput(session, 'keyTitle',
                                        value = '')
                        updateTextInput(session, 'keyRepo',
                                        value = '')
                        updateTextInput(session, 'keyKeystr',
                                        value = '')
                        updateSelectInput(session, 'keyType',
                                          selected = 2)
                }
                if(errMsg != ''){
                        shiny::showNotification(errMsg,
                                                type = 'error')
                }
        })

        observeEvent(input$updateKeyItem, {
                errMsg  <- ''
                selItem <- input$keyList
                itemName <- input$keyTitle
                itemRepo <- input$keyRepo
                itemKey  <- input$keyKeystr
                itemRead <- input$keyType
                itemReadBoolean = FALSE
                if(itemRead == 2){
                        itemReadBoolean = TRUE
                }
                if(is.null(selItem)){
                        errMsg <- tr('msgNoKeySelected')
                }
                if(errMsg == ''){
                        if(!grepl("^[0-9a-f]{64}$", itemKey, perl = TRUE)){
                                errMsg <- tr('msgInvalidKey')
                        }
                }
                if(errMsg == ''){
                        allItems <- session$userData$keyItems
                        initNames <- allItems$title
                        newRowNames <- allItems$title
                        newRowNames[initNames == selItem] <- itemName
                        localKeyItems <- session$userData$keyItems
                        localKeyItems[initNames == selItem, ] <- c(
                                title = itemName,
                                repo = itemRepo,
                                key = itemKey,
                                read = itemReadBoolean)
                        session$userData$keyItems <- localKeyItems
                        updateSelectInput(session,
                                          'keyList',
                                          choices = newRowNames,
                                          selected = NA)
                        updateTextInput(session, 'keyTitle',
                                        value = '')
                        updateTextInput(session, 'keyRepo',
                                        value = '')
                        updateTextInput(session, 'keyKeystr',
                                        value = '')
                        updateSelectInput(session, 'keyType',
                                          selected = 2)
                }
                if(errMsg != ''){
                        shiny::showNotification(errMsg,
                                                type = 'error')
                }
        })

        observeEvent(input$delKeyList, {
                errMsg  <- ''
                selItem <- input$keyList
                if(is.null(selItem)){
                        errMsg <- tr('msgNoKeySelected')
                }
                if(errMsg == ''){
                        session$userData$keyItems <- session$userData$keyItems[
                                session$userData$keyItems$title != selItem, ]
                        updateSelectInput(
                                session,
                                'keyList',
                                choices = session$userData$keyItems$title,
                                selected = NA)
                        updateTextInput(session, 'keyTitle',
                                        value = '')
                        updateTextInput(session, 'keyRepo',
                                        value = '')
                        updateTextInput(session, 'keyKeystr',
                                        value = '')
                        updateSelectInput(session, 'keyType',
                                          selected = 2)
                }
                if(errMsg != ''){
                        shiny::showNotification(errMsg,
                                                type = 'error')
                }
        })

        observeEvent(input$resetKeyItemForm, {
                updateSelectInput(session,
                                  'keyList',
                                  choices = session$userData$keyItems$title,
                                  selected = NA)
                updateTextInput(session, 'keyTitle',
                                value = '')
                updateTextInput(session, 'keyRepo',
                                value = '')
                updateTextInput(session, 'keyKeystr',
                                value = '')
                updateSelectInput(session, 'keyType',
                                  selected = 2)
        })

        observeEvent(input$keyRandom, {
                keyKeyStr <- raw2str(sodium::keygen())
                updateTextInput(session, 'keyKeystr',
                                value = keyKeyStr)
        })

        keyGenerateDialog <- function(failed = FALSE) {
                ns <- session$ns
                shiny::modalDialog(
                        shiny::span(tr('keyGenerateSpan')),
                        shiny::textInput(
                                ns('keyText'),
                                tr('keyLbl')
                        ),
                        shiny::checkboxInput(
                                ns('keyWriteOnly'),
                                tr('writeOnlyLbl')),
                        if (failed)
                                shiny::div(shiny::tags$b(
                                        tr('missingInputError'),
                                        style = "color: red;")),

                        footer = shiny::tagList(
                                shiny::modalButton(tr('cancelLbl')),
                                shiny::actionButton(ns('keyGenerateOkBtn'),
                                                    tr('okLbl'))
                        )
                )
        }

        observeEvent(input$keyGenerate, {
                shiny::showModal(keyGenerateDialog())
        })

        observeEvent(input$keyGenerateOkBtn, {
                ns <- session$ns
                if (!is.null(input$keyText) &&
                    nzchar(input$keyText)) {
                        if(input$keyWriteOnly){
                                keyKeyStr <- raw2str(
                                        sodium::pubkey(sodium::sha256(
                                                charToRaw(input$keyText))))
                                updateTextInput(session, 'keyKeystr',
                                                value = keyKeyStr)
                                updateSelectInput(session, 'keyType',
                                                  selected = 1)
                        } else {
                                keyKeyStr <- raw2str(
                                        sodium::sha256(
                                                charToRaw(input$keyText)))
                                updateTextInput(session, 'keyKeystr',
                                                value = keyKeyStr)
                                updateSelectInput(session, 'keyType',
                                                  selected = 2)
                        }
                        removeModal()
                        session$userData$openDialog <- ''
                } else {
                        showModal(keyGenerateDialog(failed = TRUE))
                }
        })

        exportKeyDialog <- function(failed = FALSE, errorMsg = '') {
                ns <- session$ns
                exportOptions <- c(1:2)
                names(exportOptions) <- c(
                        tr('fileOption'),
                        tr('localStorageOption'))
                typeOptions <- c(1:2)
                names(typeOptions) <- c(
                        tr('writeOnlyOption'),
                        tr('readWriteOption'))
                shiny::modalDialog(
                        title = tr('keyExportTitle'),
                        shiny::fluidRow(shiny::column(
                                6,
                                shiny::radioButtons(
                                        ns('keyExportType'),
                                        label = tr('keyExportTypeLbl'),
                                        choices = exportOptions,
                                        selected = 1)),
                                shiny::column(6,
                                        shiny::conditionalPanel(
                                              sprintf("input['%s'] == '1' && input['%s'] != null",
                                                      ns('keyExportType'), ns('keyList')),
                                              shiny::checkboxInput(
                                                      ns('exportSingleKey'),
                                                      tr('exportSingleKeyTxt')),
                                              shiny::conditionalPanel(
                                                      sprintf("input['%s'] && input['%s'] == '2'",
                                                              ns('exportSingleKey'), ns('keyType')),
                                                      shiny::selectInput(
                                                              ns('keyTypeExportSingleKey'),
                                                              tr('ctrlTrnsl_configDialogStep3KeyType'),
                                                              choices = typeOptions,
                                                              selected = 1)
                                              )
                                        ),
                                        shiny::conditionalPanel(
                                                sprintf("input['%s'] == '2'",
                                                        ns('keyExportType')),
                                                shiny::uiOutput(ns('localStorageKeyInfo')),
                                                shiny::actionButton(
                                                        ns('clearKeyStorage'),
                                                        tr('exportclearKeyStorageBtn'),
                                                        icon('trash'),
                                                        style='margin-top:7px'))
                                        )
                                ),
                        hr(),
                        shiny::div(
                                shiny::checkboxInput(
                                        ns('encryptKeyExport'),
                                        tr('encryptKeyExportLbl')),
                                shiny::passwordInput(
                                        ns('keyExportPassphrase'),
                                        label = NULL),
                                style='display:flex;'),
                        if (failed)
                                shiny::div(shiny::tags$b(
                                        errorMsg,
                                        style = "color: red;")),
                        footer = shiny::tagList(
                                shiny::div(
                                        shiny::modalButton(tr('closeLbl')),
                                        style='float: left;'),
                                shiny::conditionalPanel(
                                        sprintf("input['%s'] == '1'",
                                                ns('keyExportType')),
                                        shiny::downloadButton(ns('downloadKeys'),
                                                              "Download")),
                                shiny::conditionalPanel(
                                        sprintf("input['%s'] == '2'",
                                                ns('keyExportType')),
                                        shiny::actionButton(ns('exportKeys'),
                                                            tr('exportKeysLbl'),
                                                            icon('save')))
                        ),
                        easyClose = TRUE
                )
        }

        output$localStorageKeyInfo <- renderUI({
                input$exportKeys
                input$importKeys
                input$clearKeyStorage
                keyData <- input$store$oyd_keys
                if(is.null(keyData)){
                        keyData <- ''
                }
                kip <- input$keyExportPassphrase
                if(is.null(kip)){
                        kip <- ''
                }
                if(nzchar(keyData)){
                        inputJSON <- tryCatch(
                                as.data.frame(jsonlite::fromJSON(keyData)),
                                error = function(e) { return(data.frame()) })
                        if(nrow(inputJSON) == 0){
                                tr('invalidKeyDataLocalStorageTxt')
                        } else {
                                if((nrow(inputJSON) == 1) &
                                   (all(c('cipher', 'nonce') %in%
                                        colnames(inputJSON)))){
                                        if(nzchar(kip)){
                                                cipher <- str2raw(as.character(
                                                        inputJSON[1, 'cipher']))
                                                nonce <- str2raw(as.character(
                                                        inputJSON[1, 'nonce']))
                                                key <- sodium::sha256(charToRaw(kip))
                                                inputJSON <- tryCatch(
                                                        as.data.frame(jsonlite::fromJSON(rawToChar(
                                                                sodium::data_decrypt(cipher,
                                                                                     key,
                                                                                     nonce)))),
                                                        error = function(e)
                                                        { return(data.frame())})
                                                if(nrow(inputJSON) == 0){
                                                        tr('validKeyDataLocalStorageTxt')
                                                } else {
                                                        paste(nrow(inputJSON),
                                                              tr('numberKeyDataLocalStorageTxt'))
                                                }
                                        } else {
                                                tr('validKeyDataLocalStorageTxt')
                                        }
                                } else {
                                        if(all(c('title', 'repo', 'key', 'read') %in% colnames(inputJSON))){
                                                paste(nrow(inputJSON),
                                                      tr('numberKeyDataLocalStorageTxt'))
                                        } else {
                                                tr('invalidKeyDataLocalStorageTxt')
                                        }
                                }
                        }
                } else {
                        tr('noKeyDataLocalStorageTxt')
                }
        })

        observeEvent(input$clearKeyStorage, {
                shinyStore::updateStore(session, 'oyd_keys', NA)
        })

        observeEvent(input$exportKeyList, {
                df <- session$userData$keyItems
                if(nrow(df) > 0){
                        shiny::showModal(exportKeyDialog())
                } else {
                        shiny::showNotification(tr('msgExportKeyListNoKeys'),
                                                type='error')
                }
        })

        getExportKeyData <- function(pretty = TRUE){
                keyItems <- session$userData$keyItems
                if(input$exportSingleKey){
                        keyItems <- keyItems[keyItems$repo == input$keyRepo, ]
                        if(input$keyTypeExportSingleKey == '1'){
                                my_key <- keyItems[1, 'key']
                                keyItems[1, 'key'] <-
                                        raw2str(sodium::pubkey(str2raw(my_key)))
                                keyItems[1, 'read'] <- FALSE
                        }
                }
                keyData <- ''
                orig <- as.character(jsonlite::toJSON(keyItems,
                                                      pretty = pretty))
                if(input$encryptKeyExport){
                        origRaw <- charToRaw(orig)
                        key <- sodium::sha256(charToRaw(
                                input$keyExportPassphrase))
                        nonce   <- sodium::random(24)
                        cipher <- sodium::data_encrypt(origRaw,
                                                       key,
                                                       nonce)
                        nonceStr <- paste0(
                                as.hexmode(as.integer(nonce)),
                                collapse = '')
                        cipherStr <- paste0(
                                as.hexmode(as.integer(cipher)),
                                collapse = '')
                        keyData <- as.character(jsonlite::toJSON(list(
                                cipher = cipherStr,
                                nonce = nonceStr), auto_unbox = TRUE))
                } else {
                        keyData <- orig
                }
                keyData
        }

        output$downloadKeys <- shiny::downloadHandler(
                filename = function() {
                        ns <- session$ns
                        if(input$exportSingleKey){
                                if(input$keyType == '2'){
                                        if(input$keyTypeExportSingleKey == '1'){
                                                paste0(input$keyRepo, '.write_key.json')
                                        } else {
                                                paste0(input$keyRepo, '.key.json')
                                        }
                                } else {
                                        paste0(input$keyRepo, '.write_key.json')
                                }
                        }  else {
                                paste0(appName, '.allkeys.json')
                        }
                },
                content = function(file) {
                        keyData <- getExportKeyData()
                        write(keyData, file)
                }
        )

        observeEvent(input$exportKeys, {
                keyData <- input$store$oyd_keys
                if(is.null(keyData)){
                        keyData <- ''
                }
                if(nzchar(keyData)){
                        shiny::showModal(exportKeyDialog(failed = TRUE,
                                                         errorMsg = tr('errorExistingKeyDataLocalStorage')))
                } else {
                        keyData <- getExportKeyData(pretty = FALSE)
                        shinyStore::updateStore(session, 'oyd_keys', keyData)
                        shiny::showNotification(paste(nrow(session$userData$keyItems),
                                                      tr('msgKeyExportBrowser')))
                        removeModal()
                        session$userData$openDialog <- ''
                }
        })

        importKeyDialog <- function(failed = FALSE, errorMsg = '') {
                ns <- session$ns
                importOptions <- c(1:2)
                names(importOptions) <- c(
                        tr('fileOption'),
                        tr('localStorageOption'))
                shiny::modalDialog(
                        title = tr('keyImportTitle'),
                        shiny::fluidRow(shiny::column(6,
                                shiny::radioButtons(
                                        ns('keyImportType'),
                                        label = tr('keyImportTypeLbl'),
                                        choices = importOptions,
                                        selected = 1)),
                                shiny::column(6, shiny::conditionalPanel(
                                        sprintf("input['%s'] == '1'",
                                                ns('keyImportType')),
                                        shiny::fileInput(
                                                ns('keyImportFile'),
                                                label = tr('keyImportFileLbl'),
                                                multiple = FALSE)))),
                        hr(),
                        shiny::div(
                                shiny::checkboxInput(
                                        ns('decryptKeyImport'),
                                        tr('decryptKeyImportLbl')),
                                shiny::passwordInput(
                                        ns('keyImportPassphrase'),
                                        label = NULL),
                                style='display:flex;'),
                        if (failed)
                                shiny::div(shiny::tags$b(
                                        errorMsg,
                                        style = "color: red;")),

                        footer = shiny::tagList(
                                shiny::modalButton(tr('closeLbl')),
                                shiny::actionButton(ns('importKeys'),
                                                    tr('importKeysLbl'),
                                                    icon('open'))),
                        easyClose = TRUE
                )
        }

        observeEvent(input$importKeyList, {
                shiny::showModal(importKeyDialog())
        })

        observeEvent(input$importKeys, {
                importData <- switch(as.character(input$keyImportType),
                        '1'={ input$keyImportFile$datapath },
                        '2'={ input$store$oyd_keys })
                errorMsg <- ''
                inputJSON <- tryCatch(
                        as.data.frame(jsonlite::fromJSON(importData)),
                        error = function(e) { return(data.frame()) })
                if(nrow(inputJSON) == 0){
                        errorMsg <- tr('msgInvaldImport')
                }
                if(errorMsg == ''){
                        if(input$decryptKeyImport){
                                if((nrow(inputJSON) == 1) &
                                   (all(c('cipher', 'nonce') %in%
                                    colnames(inputJSON)))){
                                        cipher <- str2raw(as.character(
                                                inputJSON[1, 'cipher']))
                                        nonce <- str2raw(as.character(
                                                inputJSON[1, 'nonce']))
                                        key <- sodium::sha256(charToRaw(
                                                input$keyImportPassphrase))
                                        inputJSON <- tryCatch(
                                                as.data.frame(jsonlite::fromJSON(rawToChar(
                                                        sodium::data_decrypt(cipher,
                                                                             key,
                                                                             nonce)))),
                                                error = function(e)
                                                        { return(data.frame())})
                                        if(nrow(inputJSON) == 0){
                                                errorMsg <- tr('msgInvalidEncryptedKeyImport')
                                        }
                                } else {
                                        errorMsg <- tr('msgInvalidEncryptedKeyImport')
                                }
                         }
                }
                if(errorMsg == ''){
                        if(!all(c('title', 'repo', 'key', 'read') %in%
                               colnames(inputJSON))){
                                errorMsg <- tr('msgInvaldImport')
                        }
                }
                if(errorMsg == ''){
                        if(any(inputJSON$title %in%
                               session$userData$keyItems$title)){
                                errorMsg <- tr('msgDuplicateKeys')
                        }
                }
                if(errorMsg == ''){
                        session$userData$keyItems <-
                                rbind(session$userData$keyItems, inputJSON)
                        updateSelectInput(
                                session,
                                'keyList',
                                choices = session$userData$keyItems$title,
                                selected = NA)
                        shiny::showNotification(paste(nrow(inputJSON),
                                                      tr('msgKeyImport')))
                        removeModal()
                        session$userData$openDialog <- ''
                        # re-trigger currApp (necessary for create/update/delete)
                        rv$v <- rv$v + 1
                } else {
                        showModal(importKeyDialog(failed = TRUE, errorMsg))
                }
        })

        observeEvent(input$writeInconsistencyBtn, {
                app <- session$userData$tmpDialog_app
                url <- session$userData$tmpDialog_url
                item <- session$userData$tmpDialog_item
                shiny::removeModal()
                retVal <- writeOydItem(app, url, item)
                if(session$userData$tmpDialog_notify){
                        notify$writeItemsNotification(retVal)
                }
                session$userData$tmpDialog_app <- NA
                session$userData$tmpDialog_url <- NA
                session$userData$tmpDialog_item <- NA
                session$userData$tmpDialog_notify <- NA
        })

        observeEvent(input$writeInconsistencyCancelBtn, {
                session$userData$tmpDialog_app <- NA
                session$userData$tmpDialog_url <- NA
                session$userData$tmpDialog_item <- NA
                session$userData$tmpDialog_notify <- NA
                shiny::removeModal()
        })

        # page structure ==================================
        hintjs(session, options = list("hintButtonLabel"="Hope this hint was helpful"),
               events = list("onhintclose"=I('alert("Wasn\'t that hint helpful")')))
        observeEvent(input$help,
                     introjs(session, options = list("nextLabel"="Weiter",
                                                     "prevLabel"="Zurück",
                                                     "skipLabel"="Schließen",
                                                     "doneLabel"="Fertig")))
        observeEvent(input$buttonAnalysis, {
                session$sendCustomMessage(type='setDisplayButton',
                                          'buttonAnalysis')
                output$displayAnalysis <- renderText('.')
                output$displaySource <- renderText('')
                output$displayReport <- renderText('')
        })

        observeEvent(input$buttonSource, {
                session$sendCustomMessage(type='setDisplayButton',
                                          'buttonSource')
                output$displayAnalysis <- renderText('')
                output$displaySource <- renderText('.')
                output$displayReport <- renderText('')
        })

        observeEvent(input$buttonReport, {
                session$sendCustomMessage(type='setDisplayButton',
                                          'buttonReport')
                output$displayAnalysis <- renderText('')
                output$displaySource <- renderText('')
                output$displayReport <- renderText('.')
        })

        output$displayAnalysis <- reactive({
                output$displayAnalysis <- renderText('.')
                output$displaySource <- renderText('')
                output$displayReport <- renderText('')
        })

        # Version page ====================================
        output$versionHistory <- renderText({
                do.call(paste, as.list(apply(verHistory,1,function(x){
                        paste0('<p><strong>Version ', x[1], '</strong></p>',
                               '<p>', x[2], '</p>') })))
        })

        observeEvent(input$backToApp, {
                shiny::updateNavbarPage(session, 'mainPage', selected = appName)
        })

}
