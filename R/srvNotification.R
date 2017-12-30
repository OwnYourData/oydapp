# Module with all server-side functionality for OYD Apps
# last update: 2017-06-13

srvNotification <- function(input, output, session, tr) {
        readItemsNotification <- function(items) {
                readError <- as.character(attr(items, 'error'))
                if(identical(readError, character(0))){
                        shiny::showNotification(paste0(nrow(items), ' ',
                                                       tr('msgRecordsRead')))
                } else {
                        errMsg <- paste0(tr('piaReadError'), ' (',
                                         readError, ')')
                        switch(readError,
                               "error.accessDenied"=,
                               "403"={
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
                writeError <- as.character(attr(items, 'error'))
                if(identical(writeError, character(0))){
                        shiny::showNotification(paste("1",
                                                      tr('msg1RecordWrite')))
                } else {
                        errMsg <- paste0(tr('piaWriteError'), ' (',
                                         writeError, ')')
                        switch(writeError,
                               "Forbidden"=,
                               "403"={
                                       errMsg <- paste0(tr('piaWriteError'), ' (',
                                                        tr('piaPermissionWriteError'), ')')
                               })
                        shiny::showNotification(errMsg,
                                                type = 'error')
                }
        }

        updateItemsNotification <- function(items) {
                updateError <- as.character(attr(items, 'error'))
                if(identical(updateError, character(0))){
                        shiny::showNotification(tr('msgRecordUpdate'))
                } else {
                        errMsg <- paste0(tr('piaUpdateError'), ' (',
                                         updateError, ')')
                        switch(updateError,
                               "Forbidden"=,
                               "403"={
                                       errMsg <- paste0(tr('piaUpdateError'), ' (',
                                                        tr('piaPermissionWriteError'), ')')
                               })
                        shiny::showNotification(errMsg,
                                                type = 'error')
                }
        }

        deleteItemNotification <- function(response) {
                switch(as.character(response),
                       "200"={
                               shiny::showNotification(tr('msgRecordDelete'))
                       },
                       "403"={
                               shiny::showNotification(tr('piaPermissionDeleteError'))
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
                updateItemsNotification = updateItemsNotification,
                deleteItemNotification = deleteItemNotification
        ))
}
