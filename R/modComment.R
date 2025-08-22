#' @importFrom DT DTOutput
#' @importFrom magrittr %>%
issueUI <- function(id) {
    ns <- NS(id)
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
        fluidRow(
            ## new comments here
            div(class = "newcomment-form-container",
                # Header
                div(class = "newcomment-form-header",
                    h3(class = "newcomment-form-title",
                       tags$i(class = "fas fa-comment-alt",
                              style = "color: #5f6368;"),
                       "Add New Comment"
                    )
                ),
                
                # User info row
                div(class = "newcomment-form-row",
                    div(class = "newcomment-form-group-modern has-icon",
                        tags$i(class = "newcomment-input-icon fas fa-user"),
                        tags$label("for" = ns('uid'),
                                   class = "required-field", "Your Name"),
                        div(class = "newcomment-form-control-wrapper",
                            textInput(ns('uid'), 
                                      label = NULL,
                                      placeholder = "Enter your name")
                        ),
                        div(class = "newcomment-form-info",
                            tags$i(class = "fas fa-info-circle"),
                            "We'll email you only if clarification is needed.")
                    ),
                    div(class = "newcomment-form-group-modern has-icon",
                        tags$i(class = "newcomment-input-icon fas fa-envelope"),
                        tags$label("for" = ns('email'),
                                   class = "required-field", 
                                   "Email Address"),
                        div(class = "newcomment-form-control-wrapper",
                            textInput(ns('email'), 
                                      label = NULL,
                                      placeholder = "your.email@example.com")
                        ),
                        div(class = "newcomment-form-info",
                            tags$i(class = "fas fa-info-circle"),
                            "Email acts as edit password.")
                    )
                ),
                
                # Title input
                div(class = "newcomment-title-input-container",
                    div(class = "newcomment-form-group-modern",
                        tags$label("for" = ns('title'),
                                   class = "required-field",
                                   "Comment Title"),
                        div(class = "newcomment-form-control-wrapper",
                            textInput(ns('title'), 
                                      label = NULL,
                                      placeholder = "What's this comment about?",
                                      width = "100%")
                        )
                    )
                ),
                
                # Comment textarea
                div(class = "newcomment-input-container",
                    tags$label("for" = ns('comment'),
                               class = "required-field",
                               "Your Comment"),
                    div(class = "form-control-wrapper",
                        textAreaInput(ns('comment'),
                                      label = NULL,
                                      placeholder = "Share your thoughts, ask questions, or provide feedback...",
                                      rows = 6,
                                      width = "100%")
                    )
                ),
                
                # Hidden token field
                div(style = "display: none;",
                    textInput(ns('token'), 
                              label = NULL,
                              value = as.numeric(Sys.time()))
                ),
                
                # Form actions
                div(class = "newcomment-form-actions",
                    div(class = "newcomment-form-info",
                        tags$i(class = "fas fa-info-circle"),
                        "Markdown is supported in comments.",
                        span(style = "margin-left: 16px;",
                             tags$i(class = "fas fa-lock"),
                             "  We'll never share your email."
                        )
                    ),
                    actionButton(ns('submit'),
                                 "Post Comment",
                                 class = "newcomment-submit-btn",
                                 icon = icon("paper-plane"))
                ),
                
                # Preview section
                div(class = "newcomment-preview-container",
                    div(class = "newcomment-preview-header",
                        tags$i(class = "fas fa-eye"),
                        "Markdown Comment Preview"
                    ),
                    div(class = "newcomment-preview-content",
                        uiOutput(ns('previewbox'))
                    )
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
        observeEvent(input$reply_comment, {
            output$reply_previewbox <- renderUI(markdown(input$reply_comment))
        })
        observeEvent(input$edit_comment, {
            output$edit_previewbox <- renderUI(markdown(input$edit_comment))
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
                '\', Math.random()+\'_\'+this.id)"',
                '><i class="fas fa-edit"></i> Edit</button>',
                '<button id="reply_button_', comment["id"], '" ',
                'class="action-btn" ',
                'onclick="Shiny.onInputChange(\'', NS(id, "reply_clicked"),
                '\', Math.random()+\'_\'+this.id)"',
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
                    this_html <- createCommentHTML(main_comment)
                }else{
                    this_html <- c()
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
                        this_html <-
                            c(this_html,
                              createCommentHTML(reply, main_comment))
                        reply_reply <-
                            comments_df[comments_df$pid == reply$id, ,
                                        drop=FALSE]
                        if(nrow(reply_reply)>0){
                            this_html <-
                                c(this_html,
                                  createThreadedComments(
                                      comments_df,
                                      comment_replies[j, , drop=FALSE]))
                        }
                    }
                }
                threaded_html <- c(threaded_html,
                                   paste(this_html, collapse = ' '))
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
                    ordering = FALSE,
                    lengthMenu = c(5, 10, 25, 50, 100),
                    pageLength = 10 
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
                        size = 'l',
                        # Original comment preview
                        div(class = "original-comment-preview",
                            div(class = "original-comment-header",
                                div(class = "original-comment-title",
                                    input$title),
                                div(class="comment-author",
                                    paste("by", input$uid)),
                                div('with email'),
                                div(class='comment-email',
                                    input$email)
                            ),
                            tags$i(
                                class = "fas fa-quote-left",
                                style = "color: #667eea; font-size: 14px;"),
                            div(class = "original-comment-content",
                                markdown(input$comment)
                            )
                        ),
                        p("Are you sure you want to submit this data?"),
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
            id_to_edit <- as.numeric(gsub("^.*?edit_button_", "", input$edit_clicked))
            if(is.numeric(id_to_edit)){
                info <- getCommentsById(id_to_edit)
                ns <- NS(id)
                showModal(
                    modalDialog(
                        title = paste("Edit issue: ", info$id),
                        size = 'l',
                        div(
                            # Edit form section
                            div(class = "newcomment-form-container",
                                # Hidden fields
                                div(style='display: none;', 
                                    numericInput(ns('edit_id'),
                                                 label = NULL,
                                                 value = info$id)),
                                # User info row
                                div(class = "newcomment-form-row",
                                    div(class = "newcomment-form-group-modern has-icon",
                                        tags$i(class = "newcomment-input-icon fas fa-user"),
                                        tags$label("for" = ns('edit_uid'),
                                                   class = "required-field", "Your Name"),
                                        div(class = "newcomment-form-control-wrapper",
                                            textInput(ns('edit_uid'), 
                                                      label = NULL,
                                                      value = info$uid)
                                        )
                                    ),
                                    div(class = "newcomment-form-group-modern has-icon",
                                        tags$i(class = "newcomment-input-icon fas fa-envelope"),
                                        tags$label("for" = ns('edit_email'),
                                                   class = "required-field", 
                                                   "Email Address"),
                                        div(class = "newcomment-form-control-wrapper",
                                            textInput(ns('edit_email'), 
                                                      label = NULL,
                                                      placeholder = "used as password")
                                        )
                                    )
                                ),
                                # Comment title
                                div(class = "newcomment-title-input-container",
                                    div(class = "newcomment-form-group-modern",
                                        tags$label("for" = ns('edit_title'),
                                                   class = "required-field",
                                                   "Comment Title"),
                                        div(class = "newcomment-form-control-wrapper",
                                            textInput(ns('edit_title'), 
                                                      label = NULL,
                                                      value = info$title,
                                                      width = "100%")
                                        )
                                    )
                                ),
                                # Comment field
                                div(class = "newcomment-input-container",
                                    tags$label("for" = ns('edit_comment'),
                                               class = "required-field",
                                               "Your Reply"),
                                    div(class = "form-control-wrapper",
                                        textAreaInput(ns('edit_comment'),
                                                      label = NULL,
                                                      value = info$comment,
                                                      rows = 5,
                                                      width = "100%")
                                    )
                                )
                            )
                        ),
                        
                        # Preview section
                        div(class = "newcomment-preview-container",
                            div(class = "newcomment-preview-header",
                                tags$i(class = "fas fa-eye"),
                                "Markdown Comment Preview"
                            ),
                            div(class = "newcomment-preview-content",
                                uiOutput(ns('edit_previewbox'))
                            )
                        ),
                        
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
            ns <- NS(id)
            id_to_reply <- as.numeric(gsub("^.*?reply_button_", "", input$reply_clicked))
            if(is.numeric(id_to_reply)){
                info <- getCommentsById(id_to_reply)
                if(nrow(info)){
                    showModal(
                        modalDialog(
                            title = NULL,  # We'll create a custom header
                            size = "xl",
                            
                            # Custom modal content
                            div(
                                # Modal header
                                div(class = "reply-header-content",
                                    div(class = "reply-title",
                                        tags$i(class = "fas fa-reply",
                                               style = "font-size: 20px;"),
                                        "Reply to Comment"
                                    ),
                                    div(class = "reply-subtitle",
                                        paste("Responding to:",
                                              substr(info$uid, 1, 60),
                                              if(nchar(info$uid) > 60){
                                                  "..." } else {""})
                                    )
                                )
                            ),
                            
                            # Modal body
                            div(
                                # Original comment preview
                                div(class = "original-comment-preview",
                                    div(class = "original-comment-header",
                                        tags$i(
                                            class = "fas fa-quote-left",
                                            style = "color: #667eea; font-size: 14px;"),
                                        div(class = "original-comment-title",
                                            info$title)
                                    ),
                                    div(class = "original-comment-content",
                                        markdown(info$comment)
                                    )
                                ),
                                
                                # Reply form section
                                div(class = "newcomment-form-container",
                                    div(class = "newcomment-form-header",
                                        tags$i(class = "fas fa-pen",
                                               style = "color: #667eea;"),
                                        h4(class = "form-section-title",
                                           "Your Reply")
                                    ),
                                    
                                    # Hidden fields
                                    div(style = "display: none;",
                                        numericInput(ns('reply_id'),
                                                     label = NULL,
                                                     value = info$id),
                                        textInput(ns('reply_title'),
                                                  label = NULL,
                                                  value = info$title),
                                        textInput(ns('reply_dataset'),
                                                  label = NULL, 
                                                  value = info$dataset)
                                    ),
                                    
                                    # User info row
                                    div(class = "newcomment-form-row",
                                        div(class = "newcomment-form-group-modern has-icon",
                                            tags$i(class = "newcomment-input-icon fas fa-user"),
                                            tags$label("for" = ns('reply_uid'),
                                                       class = "required-field", "Your Name"),
                                            div(class = "newcomment-form-control-wrapper",
                                                textInput(ns('reply_uid'), 
                                                          label = NULL,
                                                          placeholder = "Enter your name")
                                            ),
                                            div(class = "newcomment-form-info",
                                                tags$i(class = "fas fa-info-circle"),
                                                "We'll email you only if clarification is needed.")
                                        ),
                                        div(class = "newcomment-form-group-modern has-icon",
                                            tags$i(class = "newcomment-input-icon fas fa-envelope"),
                                            tags$label("for" = ns('reply_email'),
                                                       class = "required-field", 
                                                       "Email Address"),
                                            div(class = "newcomment-form-control-wrapper",
                                                textInput(ns('reply_email'), 
                                                          label = NULL,
                                                          placeholder = "your.email@example.com")
                                            ),
                                            div(class = "newcomment-form-info",
                                                tags$i(class = "fas fa-info-circle"),
                                                "Email acts as edit password.")
                                        )
                                    ),
                                    
                                    # Comment field
                                    div(class = "newcomment-input-container",
                                        tags$label("for" = ns('reply_comment'),
                                                   class = "required-field",
                                                   "Your Reply"),
                                        div(class = "form-control-wrapper",
                                            textAreaInput(ns('reply_comment'),
                                                          label = NULL,
                                                          placeholder = "Share your thoughts, provide additional information, or ask follow-up questions...",
                                                          rows = 5,
                                                          width = "100%")
                                        )
                                    )
                                )
                            ),
                            
                            # Preview section
                            div(class = "newcomment-preview-container",
                                div(class = "newcomment-preview-header",
                                    tags$i(class = "fas fa-eye"),
                                    "Markdown Comment Preview"
                                ),
                                div(class = "newcomment-preview-content",
                                    uiOutput(ns('reply_previewbox'))
                                )
                            ),
                            
                            # Custom footer
                            footer = div(class = "reply-actions",
                                         div(class = "newcomment-form-info",
                                             tags$i(class = "fas fa-info-circle"),
                                             "Your reply will be posted publicly"
                                         ),
                                         div(style = "display: flex; gap: 12px;",
                                             actionButton(
                                                 ns('cancelReplyBtn'),
                                                 class = "btn-reply-cancel",
                                                 "Cancel"
                                             ),
                                             actionButton(ns("submitReplyBtn"), 
                                                          tagList(
                                                              tags$i(class = "fas fa-paper-plane"),
                                                              "Post Reply"
                                                          ),
                                                          class = "btn-reply-submit")
                                         )
                            ),
                            
                            # Add modal class for styling
                            div(class = "reply-modal", style = "display: none;")
                            
                        )
                    )
                }
            }
        })
        observeEvent(input$cancelReplyBtn, {
            removeModal()
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
