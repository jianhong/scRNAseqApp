# sqlite operation
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#'  dbGetQuery dbSendQuery dbListTables dbClearResult
#'  sqlInterpolate
#' @importFrom RSQLite SQLite
getDBconn <- function(){
    dbConnect(SQLite(),
              dbname = file.path(.globals$app_path,
                                 .globals$credential_path))
}
connectDB <- function(FUN, ...){
    con <- getDBconn()
    on.exit(dbDisconnect(con))
    FUN(conn = con, ...)
}
sendNoreplyQueryToDB <- function(...){
    conn <- getDBconn()
    on.exit(dbDisconnect(conn))
    res <- dbSendQuery(conn = conn, ...)
    dbClearResult(res)
}
#' @importFrom shinymanager read_db_decrypt
isEncrypted <- function(){
    db <- connectDB(read_db_decrypt, name = .globals$credentialTableName)
    identical(names(db), c("value", "iv"))
}
getCredential <- function(){
    res <- connectDB(read_db_decrypt, 
                     .globals$credentialTableName,
                     .globals$passphrase)
}
tableExists <- function(tableName){
    tableName %in% connectDB(dbListTables)
}
getConfigTable <- function(){
    res <- connectDB(read_db_decrypt, .globals$configTableName)
}
replaceNULL <- function(x, by=NA){
    ifelse(is.null(x), by, x)
}
createConfigTable <- function(appconf){
    appData <- lapply(appconf, FUN = function(.ele){
        if(!is(.ele, 'APPconf')){
            stop('Please check the folder privilege')
        }
        c(title=.ele$title,
          id=.ele$id,
          species=.ele$species,
          type=.ele$type,
          markers=paste(markers(.ele), collapse = .globals$configTableSep),
          keywords=paste(.ele$keywords, collapse = .globals$configTableSep),
          groupCol=paste(.ele$groupCol, collapse = .globals$configTableSep),
          ref_bib=trimBib(replaceNULL(.ele$ref$bib)),
          ref_doi=replaceNULL(.ele$ref$doi),
          ref_pmid=replaceNULL(.ele$ref$pmid),
          ref_title=replaceNULL(.ele$ref$entry$title),
          ref_author=paste(.ele$ref$entry$author,
                           collapse = .globals$configTableSep),
          ref_year=replaceNULL(.ele$ref$entry$year),
          ref_journal=replaceNULL(.ele$ref$entry$journal),
          ref_abstract=replaceNULL(.ele$ref$entry$abstract))
    })
    appData <- do.call(rbind, appData)
    colnames(appData) <- c('title', 'id', 'species', 'type', 'markers',
                           'keywords', 'groupCol', 'ref_bib', 'ref_doi',
                           'ref_pmid', 'ref_title', 'ref_author',
                           'ref_year', 'ref_journal', 'ref_abstract')
    appData <- as.data.frame(appData)
    appData$locker <- vapply(appData$id, FUN = checkLocker,
                             FUN.VALUE = logical(1L))
    connectDB(dbWriteTable, name = .globals$configTableName,
              value = appData, overwrite = TRUE)
}

updateConfigTable <- function(appconf){
    if(missing(appconf)){
        appconf <- getAppConfObj(privilege = 'all')
        createConfigTable(appconf)
    }else{
        if(is(appconf, 'APPconf')){
            appconf <- list(ele=appconf)
        }
        lapply(appconf, function(.ele){
            if(!is(.ele, 'APPconf')){
                stop('Please check the folder privilege')
            }
            query <- paste0('UPDATE ', 
                           .globals$configTableName,
                           ' SET `title`="', .ele$title, '",',
                           ' `species`="', .ele$species, '",',
                           ' `type`="', .ele$type, '",',
                           ' `markers`="',
                           paste(markers(.ele),
                                 collapse = .globals$configTableSep), '",',
                           ' `keywords`="', 
                           paste(.ele$keywords,
                                 collapse = .globals$configTableSep), '",',
                           ' `groupCol`="',
                           paste(.ele$groupCol,
                                 collapse = .globals$configTableSep), '",',
                           ' `ref_bib`="',
                           magicQuote(trimBib(replaceNULL(.ele$ref$bib))), '",',
                           ' `ref_doi`="',
                           replaceNULL(.ele$ref$doi), '",',
                           ' `ref_pmid`="',
                           replaceNULL(.ele$ref$pmid), '",',
                           ' `ref_title`="',
                           replaceNULL(.ele$ref$entry$title), '",',
                           ' `ref_author`="',
                           paste(.ele$ref$entry$author, 
                                 collapse = .globals$configTableSep), '",',
                           ' `ref_year`="',
                           replaceNULL(.ele$ref$entry$year), '",',
                           ' `ref_journal`="',
                           replaceNULL(.ele$ref$entry$journal), '",',
                           ' `ref_abstract`="',
                           replaceNULL(.ele$ref$entry$abstract), '"',
                           ' WHERE id="', .ele$id, '"')
            sendNoreplyQueryToDB(statement = query)
        })
    }
}

updateConfigTblKey <- function(key, feild, value){
    if(missing(key) || missing(feild)){
        stop('Not proper query statement.')
    }
    query <- paste0('UPDATE ', 
                   .globals$configTableName,
                   ' SET ', feild, '="', value, '"',
                   ' WHERE id="', key, '"')
    sendNoreplyQueryToDB(statement = query)
}
updateConfigTblLocker <- function(key, value){
    updateConfigTblKey(key = key, feild = 'locker', value = as.numeric(value))
}

checkKeyFromConfig <- function(key, feild, unique=TRUE){
    if(missing(key) || length(key)==0){
        # list full references
        query <- paste('SELECT', feild, 'FROM', .globals$configTableName)
    }else{
        # list references by key
        if(length(key)==1){
            query <- paste0('SELECT ', feild, ' FROM ',
                            .globals$configTableName,
                            ' WHERE id="', key, '"')
        }else{
            query <- paste0('SELECT ', feild, ' FROM ',
                            .globals$configTableName,
                            ' WHERE id in ("',
                            paste(key, collapse = '","'), '")')
        }
    }
    res <- unlist(connectDB(dbGetQuery, statement = query))
    if(unique){
        unique(sort(res))
    }else{
        res
    }
}
# list one or full references
listReferences <- function(key){
    ref <- checkKeyFromConfig(key, feild='ref_bib')
    ref <- ref[ref!=""]
    ref
}
makeSortedUnique <- function(x){
    if(length(names(x))==length(x)){
        x <- x[order(names(x))]
    }else{
        x <- sort(x)
    }
    x <- x[!duplicated(x)]
    x
}
# list all datasets
listDatasets <- function(key, privilege='', named=FALSE){
    if(is.null(privilege)){
        privilege <- ''
    }
    ds <- checkKeyFromConfig(key, feild='id', unique=FALSE)
    ds <- unname(ds)
    if(named){
        dsn <- checkKeyFromConfig(key, feild = 'title', unique = FALSE)
        names(ds) <- dsn
    }
    if(privilege=='all'){
        return(makeSortedUnique(ds))
    }
    locker <- checkKeyFromConfig(key, feild = 'locker', unique=FALSE)
    if(any(locker)){
        keep <- mapply(function(.ds, .locker){
            !.locker || grepl(.ds, privilege, fixed = TRUE)
        }, ds, locker)
        return(makeSortedUnique(ds[keep]))
    }else{
        return(makeSortedUnique(ds))
    }
}
# list all species
listSpecies <- function(key){
    checkKeyFromConfig(key, feild='species')
}

intToBin <- function(x) {
    y <- as.integer(x)
    class(y) <- "binmode"
    y <- as.character(y)
    dim(y) <- dim(x)
    y
}

ip2bin <- function(x){
    x <- strsplit(x, "\\.")
    x <- do.call(rbind, x)
    mode(x) <- "integer"
    x <- intToBin(x)
    mode(x) <- "integer"
    d <- dim(x)
    x <- sprintf("%08d", x)
    x <- matrix(x, nrow = d[1], ncol = d[2], byrow = FALSE)
    x <- apply(x, 1, paste, collapse="")
    y <- strsplit(x, "")
    y <- do.call(rbind, y)
    mode(y) <- "integer"
    apply(y, 1, function(.y) sum(.y * 2^rev((seq_along(.y)-1))))
}

touchIPtable <- function(){
    if(!is.null(.globals$IPlocationFilename)){
        if(file.exists(file.path(.globals$app_path,
                                 .globals$IPlocationFilename))){
            iptable <- readRDS(file.path(.globals$app_path,
                                         .globals$IPlocationFilename))
            connectDB(dbWriteTable, name = .globals$IPlocationTablename,
                      value = iptable, overwrite = TRUE)
            query <- paste0('CREATE UNIQUE INDEX `from` ON ',
                            .globals$IPlocationTablename, '(`from`)')
            sendNoreplyQueryToDB(statement = query)
        }
    }
}
getIPtable <- function(ips){
    if(!tableExists(.globals$IPlocationTablename)){
        return(data.frame(ip=unique(ips)))
    }
    ips <- unique(ips)
    ips <- ips[!is.na(ips)]
    ips <- ips[ips!='']
    if(length(ips)==0){
        return(data.frame(ip=ips))
    }
    ipb <- ip2bin(ips)
    res <- lapply(ipb, function(.ip){
        query <- paste0('SELECT * FROM ',
                        .globals$IPlocationTablename,
                        ' INNER JOIN (SELECT MAX(`from`) AS `start` FROM ',
                        .globals$IPlocationTablename,
                        ' WHERE `from` <= ', .ip,
                        ') AS f ON ', .globals$IPlocationTablename,
                        '.`from` = f.`start`',
                        ' WHERE `to` >= ', .ip)
        connectDB(dbGetQuery, statement = query)
    })
    names(res) <- as.character(ips)
    res <- do.call(rbind, res)
    res$from <- rownames(res)
    res$to <- NULL
    res$start <- NULL ## remove the start column introduced by the inner join
    colnames(res)[colnames(res)=='from'] <- 'ip'
    res
}
touchVisitorTable <- function(){
    counter <- NULL
    if(!tableExists(.globals$counterTableName)){
        counter <- read.delim(.globals$counterFilename, header = TRUE)
    }else{
        query <- paste('SELECT * FROM',
                       .globals$counterTableName)
        counter <- 
            connectDB(dbGetQuery,
                      statement = query)
    }
    if(!tableExists(.globals$IPlocationTablename)){
        touchIPtable()
    }
    if(length(counter)){
        if(!all(c('latitude', 'longitude', 'region', 'city') %in%
                colnames(counter))){
            counterIP <- getIPtable(counter$ip)
            counter <- merge(counter, counterIP, by='ip', all.x = TRUE)
            counter <- counter[order(counter$date), ]
            connectDB(dbWriteTable, name = .globals$counterTableName,
                      value = counter, overwrite = TRUE)
        }
    }
}
## visitor table
updateVisitorTable <- function(input, output, session){
    touchVisitorTable()
    ## update visitor stats
    update_visitor <- function(){
        req(input$remote_addr)
        current <- Sys.time()
        ip <- isolate(input$remote_addr)
        agent <- isolate(input$remote_agent)
        ## check ip and time not within 10 min
        query <- paste0('SELECT `date` FROM ',
                        .globals$counterTableName,
                        ' WHERE `date` BETWEEN "',
                        current, '" AND "', current+600,
                        '" AND `ip`="', ip, '"')
        res <- connectDB(dbGetQuery, statement = query)
        if(nrow(res)==0){
            ip2loc <- getIPtable(ip)
            ip2loc$date <- as.character(current)
            ip2loc$agent <- agent
            th <- paste(colnames(ip2loc), collapse = "`,`")
            bd <- paste(ip2loc[1, ], collapse = '","')
            query <- paste0('INSERT INTO ', .globals$counterTableName,
                            ' (`', th, '`) VALUES ("', bd, '")')
            sendNoreplyQueryToDB(statement = query)
        }
    }
    observeEvent(input$remote_addr, update_visitor())
    output$total_visitor <- renderPlot({
        counter <- listVisitors(summary=TRUE)
        ggplot(counter, aes(x=.data[["time"]], y=.data[["total"]])) +
            geom_bar(stat = "identity", fill="darkorchid4") +
            theme_minimal() + xlab("") + ylab("visitor counts") +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    })
}
listVisitors <- function(summary=FALSE, ipCounter=FALSE){
    if(summary){
        current <- Sys.time()
        # select ~730 day data 63072000 = 730*60*60*24
        query <- paste0('SELECT strftime("%Y-%m", `date`) AS `time`,',
                        ' count(`ip`) AS total,',
                        ' count(DISTINCT `ip`) AS uniqueIP FROM ',
                        .globals$counterTableName,
                        ' WHERE `date` BETWEEN "',
                        current - 63072000, '" AND "', current,
                        '" GROUP BY `time`')
    }else{
        if(ipCounter){
            query <- paste0('SELECT count(`ip`) AS total, ',
                            ' count(DISTINCT `ip`) AS uniqueIP FROM ',
                            .globals$counterTableName)
        }else{
            query <- paste0('SELECT * FROM ',
                            .globals$counterTableName)
        }
    }
    counter <- connectDB(dbGetQuery, query)
}

## comments table
touchCommentTable <- function(){
    if(!tableExists(.globals$commentsTableName)){
        sql <- paste('CREATE TABLE IF NOT EXISTS', .globals$commentsTableName, 
                     '(id INTEGER PRIMARY KEY,',
                     'uid TEXT NOT NULL,',
                     'email TEXT NOT NULL,',
                     'title TEXT NOT NULL,',
                     'comment TEXT NOT NULL,',
                     'dataset TEXT,',
                     'open INTEGER NOT NULL DEFAULT 1,',
                     "created_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%d %H:%M:%S', 'now', 'localtime')),",
                     "updated_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%d %H:%M:%S', 'now', 'localtime')))")
        sendNoreplyQueryToDB(statement=sql)
    }
}
countComments <- function(){
    sql <- paste('SELECT COUNT(*) FROM', .globals$commentsTableName)
    connectDB(dbGetQuery, sql)[[1]]
}
distinctCommentsTitles <- function(){
    touchCommentTable()
    sql <- paste("SELECT DISTINCT title FROM",
                 .globals$commentsTableName,
                 "ORDER BY updated_at DESC")
    connectDB(dbGetQuery, sql)$title
}
listComments <- function(page_size, full=FALSE, all=FALSE){
    touchCommentTable()
    col <- ifelse(full,
                  '*',
                  'uid, title, comment')
    where <- ifelse(all, 
                    "", " WHERE open=1")
    query <- paste0("SELECT ", col, " FROM ", .globals$commentsTableName,
                    where,
                    " ORDER BY updated_at DESC LIMIT ", page_size)
    connectDB(dbGetQuery, query)
}
updateComments<- function(id, coln, val){
    sql <- paste0('UPDATE ', .globals$commentsTableName,
                  " SET `", coln, "` = '", val, "',",
                  " updated_at = strftime('%Y-%m-%d %H:%M:%S', 'now', 'localtime')",
                  " WHERE id='", id, "'")
    sendNoreplyQueryToDB(statement=sql)
}
insertComments <- function(uid, email, title, comment, dataset){
    con <- getDBconn()
    on.exit(dbDisconnect(con))
    sql <- paste0('INSERT INTO ', .globals$commentsTableName,
                  ' (`uid`, `email`, `title`, `comment`, `dataset`) ',
                  'VALUES (?uid, ?email, ?title, ?comment, ?dataset)')
    query <- sqlInterpolate(
        conn = con, sql,
        uid = uid,
        email = email,
        title = title,
        comment = comment,
        dataset = dataset
    )
    res <- dbSendQuery(conn = con, statement = query)
    dbClearResult(res)
}
deleteComments <- function(id){
    sql <- paste0("DELETE FROM ", .globals$commentsTableName,
                  " WHERE id ='", id, "'")
    sendNoreplyQueryToDB(statement=sql)
}
## gene table
## gene name, expressed datasets
touchGeneTable <- function(updateDB=FALSE){
    if(updateDB || !tableExists(.globals$geneSymbolTableName)){
        datasets <- listDatasets()
        symbols <- lapply(datasets, function(.ele) {
            names(readData("sc1gene", .ele))
        })
        symbols <- data.frame(dataset = rep(datasets, lengths(symbols)),
                              symbol = unlist(symbols))
        symbols$expr <- NA
        connectDB(dbWriteTable, name = .globals$geneSymbolTableName,
                  value = symbols, overwrite = TRUE)
    }
}
listGeneSymbols <- function(genes, datasets, like = FALSE, checkExpr = FALSE){
    query <- paste0('SELECT DISTINCT `symbol` FROM ',
                    .globals$geneSymbolTableName)
    where <- NULL
    if(!missing(datasets)){
        datasets <- datasets[!is.null(datasets)]
        datasets <- datasets[!is.na(datasets)]
        if(length(datasets)==1){
            where <- c(where, paste0('`dataset` = "', datasets, '"'))
        }else{
            if(length(datasets)>1){
                where <- c(where,
                           paste0('`dataset` IN ("', 
                                  paste(datasets, collapse = '", "'), '")'))
            }
        }
    }
    if(!missing(genes)){
        genes <- genes[!is.null(genes)]
        genes <- genes[!is.na(genes)]
        genes <- tolower(genes)
        if(length(genes)==1){
            if(like){
                where <- c(where, paste0('LOWER(`symbol`) LIKE "', genes, '"'))
            }else{
                where <- c(where, paste0('LOWER(`symbol`) = "', genes, '"'))
            }
        }else{
            if(length(genes)>1){
                where <- c(where, paste0('LOWER(`symbol`) IN ("', 
                           paste(genes, collapse = '", "'), '")'))
            }
        }
    }
    if(checkExpr){
        ## default is NULL
        ## when user do search, if all expr is 0, the value will be set to 0
        ## and the updated value will not be NULL anymore.
        where <- c(where, '`expr` is NULL')
    }
    if(length(where)){
        query <- paste(query, 'WHERE', paste(where, collapse = ' AND '))
    }
    res <- connectDB(dbGetQuery, statement = query)
    res$symbol
}

setGeneExprForData <- function(symbol, dataset, expr){
    if(length(symbol)!=length(dataset)){
        return(NULL)
    }
    mapply(FUN=function(.symbol, .dataset, .expr){
        query <- paste0('UPDATE ', .globals$geneSymbolTableName,
                        ' SET `expr` = ', .expr,
                        ' WHERE `symbol` = "', .symbol, '"',
                        ' AND `dataset` = "', .dataset, '"')
        sendNoreplyQueryToDB(statement = query)
    }, symbol, dataset, expr)
}

touchGenename2Symbol <- function(){
    if(!tableExists(.globals$gn2symTableName)){
        gn2sym <- readRDS(
            system.file('extdata', 'gn2sym.rds', package = 'scRNAseqApp'))
        db <- rbind(
            data.frame(gene=gn2sym$unique, name=names(gn2sym$unique)),
            data.frame(gene=unlist(gn2sym$multiple),
                       name=rep(names(gn2sym$multiple),
                                lengths(gn2sym$multiple))))
        db <- unique(db)
        connectDB(dbWriteTable, name = .globals$gn2symTableName,
                  value = db, overwrite = TRUE)
    }
}

mapGeneSymbols <- function(genes){
    genes <- tolower(genes)
    if(length(genes)==1){
        where <- paste0('LOWER(`gene`)="', genes,
                        '" OR LOWER(`name`)="', genes, '"')
    }else{
        genes <- paste(genes, collapse = '","')
        where <- paste0('LOWER(`gene`) IN ("', genes,
                        '") OR LOWER(`name`) IN ("', genes, '")')
    }
    query <- paste0('SELECT `gene` FROM ', .globals$gn2symTableName,
                    ' WHERE ', where)
    rs <- connectDB(dbGetQuery, statement = query)
}
