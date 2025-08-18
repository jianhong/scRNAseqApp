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
                              label = 'issue',
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
            DTOutput(NS(id, "issues.dt"))
        )
    )
}
#' @importFrom DT renderDT
#' @importFrom magrittr %>%
issueServer <- function(id, dataSource, optCrt) {
    moduleServer(id, function(input, output, session) {
        updateCommentList <- function(){
            output$issues.dt <- renderDT({
                listComments(page_size=1000)
            })
        }
        # Render the paginated comments
        updateCommentList()
        checkLen <- function(n, l, m){
            if(!length(n)) return(TRUE)
            if(nchar(n)<l) return(TRUE)
            if(nchar(n)>m) return(TRUE)
            return(FALSE)
        }
        checkForm <- function(){
            new <- TRUE
            if(!grepl('^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$',
                      input$email)){
                showNotification("Please input correct email address!",
                                 type = 'error')
                new <- FALSE
            }
            if(checkLen(input$uid, 3, 50)){
                showNotification("Please input your name ([3, 50] letters).",
                                 type = 'error')
                new <- FALSE
            }
            if(checkLen(input$title,2, 200)){
                showNotification("Please input the issue title ([2, 200] letters).",
                                 type = 'error')
                new <- FALSE
            }
            if(checkLen(input$comment,10, 1000)){
                showNotification("Please input the comment ([10, 1000] letters).",
                                 type = 'error')
                new <- FALSE
            }
            return(new)
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
    })
}
