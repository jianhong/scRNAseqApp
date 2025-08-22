#' @importFrom DT DTOutput
#' @importFrom magrittr %>%
issueUI <- function(id) {
    tabPanel(
        value = id,
        HTML("Issues"),
        tabsubTitleUI(
            id,
            'Issues',
            description = paste(
                "There is a solution to every problem:",
                "simple, quick, and wrong. ",
                "--Henry Louis Mencken"
            )
        ),
        h5('New issue'),
        fluidRow(
            ## new comments here
            fluidRow(
                column(4, textInput(NS(id, 'uid'),
                                    label = 'name')), # id
                column(4, textInput(NS(id, 'email'),
                                    label = 'email')), # contact info, email
                column(4)
            ),
            fluidRow(
                textInput(NS(id, 'title'),
                          label = 'title',
                          width = '95vw')
            ),# title
            fluidRow(
                textAreaInput(NS(id, 'comment'),
                              label = 'comment',
                              width = '95vw',
                              rows = 6)
            ), # comment
            fluidRow(
                column(10, div(
                    style = "visibility:hidden;",
                    textInput(NS(id, 'token'),
                              label = NULL,
                              value = as.numeric(Sys.time()))
                )),
                column(2, div(
                    class='',
                    actionButton(NS(id, 'submit'),
                                       label = 'submit',
                                       class = "align-action-button")))
            ),
           fluidRow(
               div(class='shiny-input-textarea form-group shiny-input-container',
                   style="width:95vw",
                   h5("comment preview"),
                   div(
                       style='min-height:24px;',
                       class="form-control shiny-bound-input",
                       uiOutput(NS(id, 'previewbox')))
               )
           )
        ),
        hr(),
        
        h5('History issues'),
        fluidRow(
            ## list all the comments by pages
            ## DT ui
            DTOutput(NS(id, "issues"))
        )
    )
}
#' @importFrom DT renderDT
#' @importFrom magrittr %>%
#' @importFrom shiny markdown
issueServer <- function(id, dataSource, optCrt) {
    moduleServer(id, function(input, output, session) {
        observeEvent(input$comment, {
            output$previewbox <- renderUI(markdown(input$comment))
        })
        createCommentHTML <- function(comment, parent_comment){
            comment <- unlist(comment)
            actionBtn <- paste0(
                '    <button id="upvote_button_', comment["id"], '" ',
                'class="action-btn upvote" ',
                'onclick="Shiny.onInputChange(\'', NS(id, "upvote_clicked"),
                '\', this.id)"',
                '><i class="fas fa-thumbs-up"></i> Upvote</button>',
                '    <button id="edit_button_', comment["id"], '" ',
                'class="action-btn" ',
                'onclick="Shiny.onInputChange(\'', NS(id, "edit_clicked"),
                '\', this.id)"',
                '><i class="fas fa-edit"></i> Edit</button>',
                '<button id="reply_button_', comment["id"], '" ',
                'class="action-btn" ',
                'onclick="Shiny.onInputChange(\'', NS(id, "reply_clicked"),
                '\', this.id)"',
                '><i class="fas fa-reply"></i> Reply</button>'
            )
            if(missing(parent_comment)){
                # Main comment layout
                paste0(
                    '<div class="comment-main">',
                    '  <div class="comment-header">',
                    '    <div style="display: flex; align-items: center;">',
                    '      <div class="comment-title">', comment["title"], '</div>',
                    '      <div class="comment-author">by ', comment["uid"], '</div>',
                    '    </div>',
                    '    <div class="comment-meta">',
                    '      <span class="timestamp">', comment["updated_at"], '</span>',
                    '      <span class="vote"><i class="fas fa-thumbs-up"></i> ', comment["vote"], '</span>',
                    '    </div>',
                    '  </div>',
                    '  <div class="comment-content">',
                    '    ', markdown(comment["comment"]),
                    '  </div>',
                    '  <div class="comment-actions">', actionBtn , '  </div>',
                    '</div>'
                )
            }else{
                # Check if this is a nested reply (reply to a reply)
                parent_comment <- unlist(parent_comment)
                is_nested <- parent_comment['pid'] != parent_comment['id']
                
                css_class <- if (is_nested) "comment-nested" else "comment-reply"
                header_class <- if (is_nested) "nested-header" else "reply-header"
                author_class <- if (is_nested) "nested-author" else "reply-author"
                
                # Reply comment layout
                paste0(
                    '<div class="', css_class, '">',
                    '  <div class="reply-header ', header_class, '">',
                    '    <div class="', author_class, '">by ', comment["uid"], '</div>',
                    '    <div class="comment-meta">',
                    '      <span class="timestamp">', comment["updated_at"], '</span>',
                    '      <span class="vote"><i class="fas fa-thumbs-up"></i> ', comment["vote"], '</span>',
                    '    </div>',
                    '  </div>',
                    '  <div class="reply-content">',
                    '    ', markdown(comment["comment"]),
                    '  </div>',
                    '  <div class="reply-actions">', actionBtn , '  </div>',
                    '</div>'
                )
            }
        }
        createThreadedComments <- function(comments_df, main_comments) {
            if(nrow(comments_df)==0){
                return(c())
            }
            threaded_html <- c()
            if(missing(main_comments)){
                m <- comments_df$pid == comments_df$id
                main_comments <- comments_df[m, , drop=FALSE]
                addMain <- TRUE
            }else{
                addMain <- FALSE
            }
            if(nrow(main_comments)<1){
                return(c())
            }
            for (i in seq.int(nrow(main_comments))) {
                main_comment <- main_comments[i, , drop=TRUE]
                names(main_comment) <- colnames(comments_df)
                # Add main comment
                if(addMain){
                    threaded_html <- 
                        c(threaded_html, createCommentHTML(main_comment))
                }
                # Add replies to this main comment
                comment_replies <- 
                    comments_df[comments_df$pid == main_comment$id, , drop=FALSE]
                comment_replies <- 
                    comment_replies[comment_replies$pid != comment_replies$id, 
                                    , drop=FALSE] ## remove self
                if (nrow(comment_replies) > 0) {
                    for (j in seq.int(nrow(comment_replies))) {
                        reply <- comment_replies[j, , drop=TRUE]
                        names(reply) <- colnames(comments_df)
                        threaded_html <-
                            c(threaded_html,
                              createCommentHTML(reply, main_comment))
                        reply_reply <-
                            comments_df[comments_df$pid == reply$id, ,
                                        drop=FALSE]
                        if(nrow(reply_reply)>0){
                            threaded_html <-
                                c(threaded_html,
                                  createThreadedComments(
                                      comments_df,
                                      comment_replies[j, , drop=FALSE]))
                        }
                    }
                }
            }
            return(threaded_html)
        }
        updateCommentList <- function(){
            output$issues <- renderDT({
                data_to_display <- 
                    listComments(page_size=1000)
                if(nrow(data_to_display)<1){
                    data.frame('Title'=character(0L),
                               'Name'=character(0L),
                               'Comment'=character(0L),
                               'Actions'=character(0L))
                }else{
                    # # Sort by timestamp and organize by parent-child relationships
                    data_to_display <- split(data_to_display, data_to_display$pid)
                    data_to_display_updated_at <- unlist(lapply(data_to_display, function(.ele){
                        time <- as.POSIXct(.ele$updated_at, format = "%Y-%m-%d %H:%M:%S")
                        max(time)
                    }))
                    data_to_display <- 
                        lapply(data_to_display[order(data_to_display_updated_at,
                                              decreasing = TRUE)],
                               function(.ele){
                                   time <- as.POSIXct(.ele$created_at,
                                                      format = "%Y-%m-%d %H:%M:%S")
                                   .ele <-.ele[order(time), ,
                                               drop=FALSE]
                                   .ele
                                   })
                    data_to_display <- do.call(rbind, data_to_display)
                    data.frame(
                        CommentThread = createThreadedComments(data_to_display),
                        stringsAsFactors = FALSE
                    )
                }
                },# IMPORTANT: Allow HTML rendering for buttons
                escape = FALSE, # IMPORTANT: Allow HTML rendering for buttons
                options = list(
                    ordering = FALSE
                ),
                rownames = FALSE,
                colnames = ""
            )
        }
        # Render the paginated comments
        updateCommentList()
        checkLen <- function(n, l, m){
            if(!length(n)) return(TRUE)
            if(nchar(n)<l) return(TRUE)
            if(nchar(n)>m) return(TRUE)
            return(FALSE)
        }
        checkForm <- function(prefix=''){
            res <- TRUE
            if(!grepl('^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$',
                      input[[paste0(prefix, "email")]])){
                showNotification("Please input correct email address!",
                                 type = 'error')
                res <- FALSE
            }
            if(checkLen(input[[paste0(prefix, "uid")]], 3, 50)){
                showNotification("Please input your name ([3, 50] letters).",
                                 type = 'error')
                res <- FALSE
            }
            if(checkLen(input[[paste0(prefix, "title")]],2, 200)){
                showNotification("Please input the issue title ([2, 200] letters).",
                                 type = 'error')
                res <- FALSE
            }
            if(checkLen(input[[paste0(prefix, "comment")]],10, 1000)){
                showNotification("Please input the comment ([10, 1000] letters).",
                                 type = 'error')
                res <- FALSE
            }
            return(res)
        }
        # insert new
        observeEvent(input$submit, {
            new <- checkForm()
            if(new){
                showModal(
                    modalDialog(
                        title = "Confirm Submission",
                        p("Are you sure you want to submit this data?"),
                        p("username: ", input$uid), p('email: ', input$email),
                        h5(input$title),
                        markdown(input$comment),
                        footer = tagList(
                            actionButton(NS(id, "confirmSubmitBtn"), "Confirm"), # This is the "Yes" button
                            modalButton("Cancel") # This is the "No" button
                        )
                    )
                ) 
            }
        })
        observeEvent(input$confirmSubmitBtn, {
            removeModal()
            ## new issue
            new <- checkForm()
            ## check the intervals
            if(as.numeric(Sys.time()) - as.numeric(input$token) < 10){
                showNotification('System busy! Please try to submit issue again.',
                                 type = 'error')
                new <- FALSE
            }
            ## check the total record in database
            if(countComments()>.globals$totalComments){
                showNotification('Out of storage! Please notify the admin about this issue.',
                                 type = 'error')
                new <- FALSE
            }
            if(new){
                insertComments(uid=input$uid,
                               email=input$email,
                               title=input$title,
                               comment=input$comment,
                               dataset=dataSource()$dataset)
                updateCommentList()
            }
            updateTextInput(session, 'token', label=NULL,
                            value=as.numeric(Sys.time()))
        })
        ## upvote comments
        observeEvent(input$upvote_clicked, {
            id_to_upvote <- as.numeric(gsub("upvote_button_", "", input$upvote_clicked))
            if(is.numeric(id_to_upvote)){
                updateCommentsVote(id_to_upvote)
                updateCommentList()
            }
        })
        ## edit comments
        observeEvent(input$edit_clicked, {
            id_to_edit <- as.numeric(gsub("edit_button_", "", input$edit_clicked))
            if(is.numeric(id_to_edit)){
                info <- getCommentsById(id_to_edit)
                showModal(
                    modalDialog(
                        title = paste("Edit issue: ", info$id),
                        div(style='visibility:hidden;height:0px;width:0px;', 
                            numericInput(NS(id, 'edit_id'), label = NULL, value = info$id)),
                        textInput(NS(id, 'edit_uid'), "name", value = info$uid),
                        textInput(NS(id, 'edit_email'), "email", value = NULL),
                        textInput(NS(id, 'edit_title'), "title", value = info$title),
                        textAreaInput(NS(id, 'edit_comment'), 'comment', value = info$comment),
                        footer = tagList(
                            actionButton(NS(id, "submitEditBtn"), "Submit"), # This is the "Yes" button
                            modalButton("Cancel") # This is the "No" button
                        )
                    )
                )
            }
        })
        observeEvent(input$submitEditBtn, {
            id_to_edit <- as.numeric(input$edit_id)
            if(is.numeric(id_to_edit)){
                edit <- checkForm(prefix = 'edit_')
                if(edit){
                    info <- getCommentsById(id_to_edit)
                    if(info$email==input$edit_email){
                        removeModal()
                        null <- lapply(c('uid', 'title', 'comment'), function(coln){
                            val <- input[[paste0('edit_', coln)]]
                            if(val!=info[[coln]]){
                                updateComments(info$id, coln, val)
                            }
                        })
                        # refresh table
                        updateCommentList()
                    }else{
                        showNotification("The email address must match exactly with the record.",
                                         type = 'error')
                    }
                }
            }
        })
        ## reply comments
        observeEvent(input$reply_clicked, {
            id_to_reply <- as.numeric(gsub("reply_button_", "", input$reply_clicked))
            if(is.numeric(id_to_reply)){
                info <- getCommentsById(id_to_reply)
                if(nrow(info)){
                    showModal(
                        modalDialog(
                            title = paste("Reply issue: ", info$title),
                            div(style='visibility:hidden;height:0px;width:0px;', 
                                numericInput(NS(id, 'reply_id'), label = NULL, value = info$id),
                                textInput(NS(id, 'reply_title'), label = NULL, value = info$title),
                                textInput(NS(id, 'reply_dataset'), label=NULL, value = info$dataset)),
                            textInput(NS(id, 'reply_uid'), "name", value = NULL),
                            textInput(NS(id, 'reply_email'), "email", value = NULL),
                            textAreaInput(NS(id, 'reply_comment'), 'comment', value = NULL),
                            footer = tagList(
                                actionButton(NS(id, "submitReplyBtn"), "Submit"), # This is the "Yes" button
                                modalButton("Cancel") # This is the "No" button
                            )
                        )
                    )
                }
            }
        })
        observeEvent(input$submitReplyBtn, {
            id_to_reply <- as.numeric(input$reply_id)
            if(is.numeric(id_to_reply)){
                reply <- checkForm(prefix = 'reply_')
                if(reply){
                    removeModal()
                    insertComments(uid=input$reply_uid,
                                   email=input$reply_email,
                                   title=input$reply_title,
                                   comment=input$reply_comment,
                                   dataset=input$reply_dataset,
                                   pid=input$reply_id)
                    # refresh table
                    updateCommentList() 
                }
            }
        })
    })
}
