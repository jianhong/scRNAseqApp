#' @importFrom DT DTOutput
#' @importFrom htmltools tags singleton tagList
#' @importFrom shiny NS fluidRow column actionButton icon
privilegeUI <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            column(
                width = 10, offset = 1,
                tags$br(), tags$br(), tags$br(),
                DTOutput(outputId = ns("table_users"))
            )
        )
    )
}

#' @importFrom DT renderDT dataTableProxy replaceData
privilegeServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        # read users table from database
        users <- reactiveVal(NULL)
        
        observe({
            db <- try({
                x <- getCredential()
            }, silent = TRUE)
            
            if (inherits(db, "try-error")) {
                showModal(modalDialog("An error occurs when connecting or reading the database."))
                users(NULL)
            } else {
                users(db)
            }
        })
        
        ALL_OPTIONS <- listDatasets(privilege='locked')
        
        # displaying users table
        output$table_users <- renderDT({
            req(users())
            users <- users()
            users <- users[, c('user', 'privilege'),
                           drop = FALSE]
            users$privilege <- gsub(';\\d*', '; ', users$privilege)
            datatable(users, selection = 'none')})
        
        # Observe cell clicks specifically for the "privilege" column (index 2)
        observeEvent(input$table_users_cell_clicked, {
            info <- input$table_users_cell_clicked
            # Ensure a cell was actually clicked and it's the "privilege" column (index 2)
            # Note: Column indices in 'cell_clicked' are 0-based
            if (is.null(info$value) || info$col != 2) return()
            
            req(users())
            users <- users()
            
            # Get current values to pre-select checkboxes
            current_text <- users$privilege[info$row]
            selected_values <- trimws(unlist(strsplit(current_text, ";")))
            
            showModal(modalDialog(
                title = paste("Edit datasets for", users[info$row, "user"]),
                checkboxGroupInput(NS(id, "edit_dataset_input"), "Select dataset:", 
                                   choices = ALL_OPTIONS, 
                                   selected = selected_values),
                footer = tagList(
                    modalButton("Cancel"),
                    actionButton(NS(id, "save_dataset"), "Save Changes",
                                 class = "btn-primary")
                )
            ))
        })
        
        # Handle Saving from the Modal
        observeEvent(input$save_dataset, {
            info <- input$table_users_cell_clicked
            # Combine selected checkboxes back into a semicolon string
            new_value <- paste(input$edit_dataset_input, collapse = "; ")
            
            # Update the reactive data
            req(users())
            users <- users()
            users[info$row, "privilege"] <- new_value
            
            # insert to database
            updatCredential(users)
            
            # Update the table display without a full reload
            proxy <- dataTableProxy("table_users")
            users$privilege <- gsub(';\\d*', '; ', users$privilege)
            replaceData(proxy, users[, c('user', 'privilege'), drop=FALSE],
                        resetPaging = FALSE)
            removeModal()
        })
    })
}