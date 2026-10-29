#' @importFrom data.table setcolorder copy dcast
scMultiCoex <- function(
        inpConf,
        inpMeta,
        inp,
        inpGrp,
        grpKey,
        grpVal,
        inpGrp1c,
        inpPlt,
        dataset,
        geneIdMap,
        inpScl,
        inpRow,
        inpCol,
        inpcols,
        pointSize,
        labelsFontsize = 24,
        labelsFontFamily = 'Helvetica',
        flipXY,
        save = FALSE,
        colorBreaks,
        returnColorRange = FALSE,
        reorder=FALSE,
        orderX,
        greorder=FALSE,
        orderG,
        K=3,
        countOnly=FALSE) {
    # Identify genes that are in our dataset
    if (missing(inpGrp1c))
        inpGrp1c <- 0
    geneList <- scGenePairs(inp, geneIdMap)
    shiny::validate(need(
        nrow(geneList) <= .globals$maxNumGenePair,
        paste("More than", .globals$maxNumGenePair,
              "genes to plot! Please reduce the gene list!")
    ))
    shiny::validate(need(
        nrow(geneList) > 1,
        "Please input at least 2 genes to plot!"))
    if(nrow(geneList)>.globals$maxNumGenePair){
        showNotification(
            paste('Plotting expression data for too many genes.',
                  'It will take a while. Please be patient.'),
            duration = 5,
            type = 'message'
        )
    }
    
    # Prepare ggData
    ggData <- data.table()
    vals <- h5read(file.path(
        .globals$datafolder,
        dataset,
        .globals$filenames$sc1gexpr
    ), .globals$h5fGrp, index=list(geneIdMap[geneList$gene], NULL))
    rownames(vals) <- geneList$gene
    tmp <- inpMeta[, c(
        "sampleID",
        inpConf[inpConf$grp == TRUE]$ID),
        with = FALSE]
    if(inpGrp=='NA'||inpGrp==''){
        tmp$grpBy <- 1
        inpGrp <- FALSE
    }else{
        if(inpGrp %in% inpConf$UI){
            tmp$grpBy <- inpMeta[[inpConf[inpConf$UI == inpGrp]$ID]]
            inpGrp <- TRUE
        }else{
            tmp$grpBy <- 1
            inpGrp <- FALSE
        }
    }
    for (iGene in geneList$gene) {
        tmp$geneName <- iGene
        tmp$val <- vals[iGene, ]
        ggData <- rbindlist(list(ggData, tmp))
    }
    ggData <- subGrp(ggData, grpKey, grpVal, inpConf)
    ggData$pairId <- geneList$pairId[match(ggData$geneName, geneList$gene)]
    # group the values for each pair of genes,
    # count the N for val > 0
    ggData <- ggData[!is.na(ggData$val) & ggData$val>0]
    ggData[, 'Counts' := .N, by=c("grpBy", "pairId", "geneName")]
    ggData <- unique(ggData[, c('Counts', 'pairId', 'geneName', 'grpBy')])
    all_list <- rbindlist(
        lapply(unique(ggData$grpBy), function(g){
            copy(geneList[, c('gene', 'pairId')])[, "grpBy" := g]
        })
    )
    colnames(all_list) <- c('geneName', 'pairId', 'grpBy')
    ggData <- merge(ggData, all_list, all.y = TRUE)
    ggData$Counts[is.na(ggData$Counts)] <- 0
    ggData[, c("REI", "genePair") :=list(
        # how to decide the order?
        # switch Index = sum(A, B)==0? 0:max(A,B)/sum(A, B)), no one will be sum==0
        2*((max(.SD$Counts, na.rm=TRUE)+K)/
               (sum(.SD$Counts, na.rm=TRUE)+2*K)-
               0.5),
        paste(sort(.SD$geneName), collapse = ', ')
    ), by=c('pairId', 'grpBy')]
    if(isTRUE(countOnly)){
        ggData[, 'idx':= seq_len(.N), by=c('pairId', 'grpBy')]
        wide_dt <- dcast(
            ggData,
            as.formula("grpBy + pairId + REI ~ idx"),
            value.var = c("geneName", "Counts"),
            sep=""
        )
        setcolorder(wide_dt, c("pairId", "grpBy", "geneName1", "geneName2",
                               "Counts1", "Counts2", "REI"))
        setnames(wide_dt, c("geneName1", "geneName2", "grpBy"),
                 c("gene1", "gene2", "group"))
        if(!inpGrp){
            wide_dt$group <- NULL
        }
        return(wide_dt)
    }
    ggData <- unique(ggData[, c('genePair', 'grpBy', 'REI')])
    if(!reorder){
        ## order DEC
        orderX <- unique(ggData$genePair[order(ggData$REI, decreasing = TRUE)])
    }
    if(greorder){
        ggData$grpBy <- factor(ggData$grpBy, levels=orderG)
    }
    ggData$genePair <- factor(ggData$genePair, levels=orderX)
    if(inpGrp){
        ggOut <- ggplot(ggData, aes(x=.data$genePair, y=.data$REI, fill=.data$grpBy))
    }else{
        ggOut <- ggplot(ggData, aes(x=.data$genePair, y=.data$REI))
    }
    ggOut <- ggOut +
        geom_bar(stat = "identity", position="dodge") + 
        xlab('gene pair') + ylab('Reciprocal Expression Index') + 
        sctheme(
            Xang = 90,
            XjusH = 1,
            base_size = labelsFontsize,
            family = labelsFontFamily)
    return(ggOut)
}
