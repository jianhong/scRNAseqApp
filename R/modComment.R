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
                column(4, tagList(# recaptcha
                    # htmlDependencies('recaptcha',
                    #                  src=c(href='https://www.google.com/recaptcha'),
                    #                  script='api.js?render=reCAPTCHA_site_key'),
      #               tags$script(
      #                   " function onClick(e) {
      #   e.preventDefault();
      #   grecaptcha.ready(function() {
      #     grecaptcha.execute('reCAPTCHA_site_key', {action: 'submit'}).then(function(token) {
      #         // Add your logic to submit to your backend server here.
      #         Shiny.setInputValue(id+'-recaptcha_response', token);
      #     });
      #   });
      # }"
      #               )
                ))
            ),
            fluidRow(
                textInput(NS(id, 'title'),
                          label = 'title',
                          width = '95vh')
            ),# title
            fluidRow(
                textAreaInput(NS(id, 'comment'),
                              label = 'comment',
                              width = '95vh',
                              rows = 6)
            ), # comment
            fluidRow(
                column(8, div(
                    style = "visibility:hidden;",
                    textInput(NS(id, 'token'),
                              label = NULL,
                              value = as.numeric(Sys.time()))
                )),
                column(4, actionButton(NS(id, 'submit'),
                                       label = 'submit',
                                       class = "align-action-button"))
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
issueServer <- function(id, dataSource, optCrt) {
    moduleServer(id, function(input, output, session) {
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
                    # Add a column for delete buttons
                    # The inputId of each button will be "delete_button_ROWID"
                    edit_buttons <- paste0(
                        '<button id="edit_button_', data_to_display$id, '" ',
                        'type="button" class="btn btn-primary btn-sm edit-btn" ',
                        'onclick="Shiny.onInputChange(\'', NS(id, "edit_clicked"),
                        '\', this.id)"',
                        '>Edit</button>&nbsp;',
                        '<button id="reply_button_', data_to_display$id, '" ',
                        'type="button" class="btn btn-info btn-sm reply-btn" ',
                        'onclick="Shiny.onInputChange(\'', NS(id, "reply_clicked"),
                        '\', this.id)"',
                        '>Reply</button>'
                    )
                    # Add the delete buttons as a new column to the data
                    data_to_display <- cbind(data_to_display[, -1], Actions = edit_buttons)
                    # reorder the data (data.frame)
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
                                   if(nrow(.ele)>1){
                                       .ele$title[-1] <- 
                                           paste('|_ ', .ele$title[-1])
                                   }
                                   .ele
                                   })
                    data_to_display <- do.call(rbind, data_to_display)
                    data_to_display <- 
                        data_to_display[, 
                                        c('title', 'uid',
                                          'comment', 'Actions'),
                                        drop=FALSE]
                    colnames(data_to_display) <- 
                        c('Title', 'Name', 'Comment', 'Actions')
                    rownames(data_to_display) <- NULL
                    data_to_display
                }
                },
                escape = FALSE # IMPORTANT: Allow HTML rendering for buttons
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
                        p(input$comment),
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
                    if(info$pid==0){
                        info$pid <- info$id
                    }
                    showModal(
                        modalDialog(
                            title = paste("Reply issue: ", info$title),
                            div(style='visibility:hidden;height:0px;width:0px;', 
                                numericInput(NS(id, 'reply_id'), label = NULL, value = info$pid),
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
