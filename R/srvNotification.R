# Module with all server-side functionality for OYD Apps
# last update: 2017-06-13

srvNotification <- function(input, output, session, tr) {
        readItemsNotification <- function(items) {
                readError <- attr(items, 'error')
                if(!is.null(readError)){
                        errMsg <- paste0(tr('piaReadError'), ' (',
                                         readError, ')')
                        switch(readError,
                               "error.accessDenied"={
                                       errMsg <- paste0(
                                               tr('piaReadError'), ' (',
                                               tr('piaPermissionReadError'), ')')
                               },
                               "Bad Request"={
                                       errMsg <- paste0(
                                               tr('piaReadError'), ' (',
                                               tr('piaUnknownRepoReadError'), ')')
                               },
                               {
                                       if(substr(readError, 1, 3) == 'msg'){
                                               errMsg <- paste0(
                                                       tr('piaReadError'), ' (',
                                                       tr(readError), ')')
                                       }
                               })
                        shiny::showNotification(errMsg,
                                                type = 'error')
                } else {
                        shiny::showNotification(paste0(nrow(items), ' ',
                                                       tr('msgRecordsRead')))
                }
                readWarning <- attr(items, 'warning')
                if(!is.null(readWarning)){
                        errMsg <- paste0(tr('piaReadWarning'), ' (',
                                         readWarning, ')')
                        switch(readWarning,
                               "msgUnencryptedDataWithKey"={
                                       errMsg <- tr('msgUnencryptedDataWithKey')
                               },
                               "Bad Request"={
                                       errMsg <- paste0(tr('piaReadError'), ' (',
                                                        tr('piaUnknownRepoReadError'), ')')
                               })
                        shiny::showNotification(errMsg,
                                                type = 'warning')
                }
        }

        writeItemsNotification <- function(items) {
                writeError <- attr(items, 'error')
                if(!is.null(writeError)){
                        errMsg <- paste0(tr('piaWriteError'), ' (',
                                         writeError, ')')
                        switch(writeError,
                               "Forbidden"={
                                       errMsg <- paste0(tr('piaWriteError'), ' (',
                                                        tr('piaPermissionWriteError'), ')')
                               })
                        shiny::showNotification(errMsg,
                                                type = 'error')
                } else {
                        recs <- as.data.frame(jsonlite::fromJSON(items))
                        if(nrow(recs) == 1){
                                shiny::showNotification(paste(nrow(recs),
                                                              tr('msg1RecordWrite')))
                        } else {
                                shiny::showNotification(paste(nrow(recs),
                                                              tr('msgRecordsWrite')))
                        }
                }
        }

        deleteItemNotification <- function(response) {
                switch(as.character(response),
                       "200"={
                               shiny::showNotification(tr('msgRecordDelete'))
                       },
                       "500"={
                               shiny::showNotification(tr('msgDeleteRecordMissing'),
                                                       type = 'error')
                       },
                       {
                               shiny::showNotification(paste0(
                                       tr('msgDeleteRecordError'), ' (', response, ')'))
                       })
        }

        return(list(
                readItemsNotification = readItemsNotification,
                writeItemsNotification = writeItemsNotification,
                deleteItemNotification = deleteItemNotification
        ))
}
