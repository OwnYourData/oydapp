# helper functions for OYD apps
# last update: 2017-09-18

rv <- shiny::reactiveValues(v = 0, u = 0)

# UI Helpers ==============================================
withBusyIndicatorUI <- function(button) {
        id <- button[['attribs']][['id']]
        div(
                `data-for-btn` = id,
                style = 'display: inline;',
                button,
                span(
                        class = "btn-loading-container",
                        hidden(
                                img(src = "ajax-loader-bar.gif",
                                    class = "btn-loading-indicator"),
                                icon("check", class = "btn-done-indicator")
                        )
                ),
                hidden(
                        div(class = "btn-err",
                            div(icon("exclamation-circle"),
                                tags$b("Error: "),
                                span(class = "btn-err-msg"))
                        )
                )
        )
}

withBusyIndicatorServer <- function(buttonId, expr) {
        # UX stuff: show the "busy" message, hide the other messages, disable the button
        loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
        doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
        errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
        shinyjs::disable(buttonId)
        shinyjs::show(selector = loadingEl)
        shinyjs::hide(selector = doneEl)
        shinyjs::hide(selector = errEl)
        on.exit({
                shinyjs::enable(buttonId)
                shinyjs::hide(selector = loadingEl)
        })

        # Try to run the code when the button is clicked and show an error message if
        # an error occurs or a success message if it completes
        tryCatch({
                value <- expr
                shinyjs::show(selector = doneEl)
                shinyjs::delay(2000, shinyjs::hide(selector = doneEl,
                                                   anim = TRUE,
                                                   animType = "fade",
                                                   time = 0.5))
                value
        }, error = function(err) { errorFunc(err, buttonId) })
}

# When an error happens after a button click, show the error
errorFunc <- function(err, buttonId) {
        errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
        errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
        errMessage <- err$message #gsub("^ddpcr: (.*)", "\\1", err$message)
        shinyjs::html(html = errMessage, selector = errElMsg)
        shinyjs::show(selector = errEl, anim = TRUE, animType = "fade")
}

# Key handling ============================================
raw2str <- function(key){
        paste(as.character(key), collapse='')
}

str2raw <- function(str){
        if(grepl("^[0-9a-f]+$", str, perl = TRUE)){
                as.raw(strtoi(sapply(
                        seq(1, nchar(str), by=2),
                        function(x) substr(str, x, x+1)), 16L))
        } else {
                raw(0)
        }
}

getKey <- function(crypt, repo){
        key <- ''
        read <- NA
        if(!is.null(crypt)) {
                if(class(crypt) == 'data.frame'){
                        if(nrow(crypt) > 0){
                                crypt$n <- unlist(lapply(crypt$repo,
                                                         nchar))
                                crypt <- crypt[with(crypt,
                                                    order(-n, repo)), ]
                                for(i in 1:nrow(crypt)){
                                        if(grepl(paste0('^',
                                                        crypt[i,'repo']),
                                                 repo)){
                                                key <- crypt[i, 'key']
                                                read <- crypt[i, 'read']
                                                break
                                        }
                                }
                        }
                }
        }
        list(key  = key,
             read = read)
}

getWriteKey <- function(crypt, repo){
        retVal <- getKey(crypt, repo)
        if(is.na(retVal$read)){
                NA
        } else {
                if(retVal$read){
                        sodium::pubkey(str2raw(retVal$key))
                } else {
                        str2raw(retVal$key)
                }
        }
}

getReadKey <- function(crypt, repo){
        retValWrite <- getKey(crypt, repo)
        if(is.na(retValWrite$read)){
                NA
        } else {
                cryptRead <- crypt[crypt$read == 'TRUE' |
                                   crypt$read == TRUE, ]
                retValRead <- getKey(cryptRead, repo)
                if(retValWrite$key == retValRead$key){
                        str2raw(retValRead$key)
                } else {
                        NA
                }
        }
}

checkItemEncryption <- function(data, checkRow = 1){
        if('version' %in% colnames(data)){
                if(is.na(data[checkRow, 'version'])){
                        FALSE
                } else {
                        if(data[checkRow, 'version'] == oydDataVersion){
                                if('nonce' %in% colnames(data)){
                                        if(nzchar(data[checkRow, 'nonce'])){
                                                TRUE
                                        } else {
                                                FALSE
                                        }
                                } else {
                                        FALSE
                                }
                        } else {
                                FALSE
                        }
                }
        } else {
                FALSE
        }
}

checkPiaEncryption <- function(app, repo = 'oyd.settings'){
        if(getRepoPubKey(app, repo) == ''){
                FALSE
        } else {
                TRUE
        }
}

checkValidKey <- function(app, repo, privateKey){
        publicKey <- getRepoPubKey(app, repo)
        if(publicKey == ''){
                FALSE
        } else {
                if(all(str2raw(publicKey) == sodium::pubkey(privateKey))){
                        TRUE
                } else {
                        FALSE
                }
        }
}

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
                        if((nrow(inputJSON) == 1) &
                           (all(c('value','nonce') %in% colnames(inputJSON)))){
                                TRUE
                        } else {
                                FALSE
                        }
                }
        }
}

validKeyInfo <- function(keyInfo, app, appRepoDefault){
        inputJSON <- tryCatch(
                as.data.frame(jsonlite::fromJSON(keyInfo)),
                error = function(e) { return(data.frame()) })
        if(nrow(inputJSON) == 0){
                FALSE
        } else {
                if(all(c('title', 'repo', 'key', 'read') %in% colnames(inputJSON))){
                        privateKey <- as.character(inputJSON[1,'key'])
                        privateKeyRaw <- sodium::sha256(charToRaw(privateKey))
                        if(checkValidKey(app, appRepoDefault, privateKeyRaw)){
                                TRUE
                        } else {
                                FALSE
                        }

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

msgDecrypt <- function(input, key){
        cipherHex <- jsonlite::fromJSON(input)$value
        nonceHex <- jsonlite::fromJSON(input)$nonce
        private_key <- sodium::sha256(charToRaw(key))
        auth_private_key <- sodium::sha256(charToRaw('auth'))
        auth_key <- sodium::pubkey(auth_private_key)
        nonce <- as.raw(strtoi(sapply(
                seq(1, nchar(nonceHex), by=2),
                function(x) substr(nonceHex, x, x+1)), 16L))
        cipher <- as.raw(strtoi(sapply(
                seq(1, nchar(cipherHex), by=2),
                function(x) substr(cipherHex, x, x+1)), 16L))
        tryCatch(rawToChar(
                sodium::auth_decrypt(cipher, private_key, auth_key, nonce)),
                error = function(e) {
                        return('') })
}

# Time handling functions =================================
getTsNow <- function(){
        DateTime2iso8601(Sys.time())
}

DateTime2iso8601 <- function(now){
        strftime(as.POSIXlt(now,
                            'UTC',
                            '%Y-%m-%dT%H:%M:%S'),
                 oydTimeFormat)
}

iso86012DateTime <- function(ts){
        as.POSIXct(ts, oydTimeFormat,
                   tz = 'UTC')
}

iso86012LocalTime <- function(ts){
        retVal <- as.POSIXct(ts, oydTimeFormat,
                             tz = 'UTC')
        attr(retVal, 'tzone') <- Sys.timezone()
        retVal
}

# Misc Helpers ============================================
# create md5 digest from specified fields in data frame
createDigest <- function(data, fields){
        if(length(data) > 0){
                if (nrow(data) > 0) {
                        data <- tidyr::unite_(data, 'merged',
                                       fields,
                                       remove=FALSE)
                        data$digest <- sapply(data$merged, digest::digest)
                        data[, c(fields,  'digest')]
                } else {
                        data.frame()
                }
        } else {
                data.frame()
        }
}

# check if a string is a valid email
validEmail <- function(email){
        emailPtrn <- "^[\\w\\.-]+@([\\w\\-]+\\.)+[A-Za-z]{2,4}$"
        if (any(grep(emailPtrn, email, perl = TRUE))) {
                TRUE
        } else {
                FALSE
        }
}

# merge 2 data frames by date
combineData <- function(dat1, dat2){
        data <- data.frame()
        if(nrow(dat1) == 0) {
                data <- dat2
        } else {
                if(nrow(dat2) == 0){
                        data <- dat1
                } else {
                        data <- merge(dat1[, !names(dat1) %in% c('id')],
                                      dat2[, !names(dat2) %in% c('id')],
                                      by='date', all=TRUE)
                }
        }
        data
}
