#' @importFrom shinymanager auth_ui secure_server use_language check_credentials
loginUI <- function(loginNavbarTitle, defaultDataset) {
    lan <- use_language("en")
    tabPanel(
        HTML(loginNavbarTitle),
        auth_ui(
            id = "auth",
            tags_bottom = tags$div(tags$a(
                href = paste0('?data=', defaultDataset),
                "Go to application",
                icon = icon("share")
            )),
            # set language ?
            lan = lan
        ),
        shinymanager_where("authentication"),
        shinymanager_language(lan$get_language())
    )
}
loginServer <- function(input, output, session) {
    auth <-
        secure_server(check_credentials = check_credentials(
            file.path(.globals$app_path, .globals$credential_path)))
}

#' @importFrom utils getFromNamespace
#' @importFrom shinymanager use_language secure_app fab_button pwd_ui
secureUI <- function(ui0, timeout = 30) {
    function(request) {
        query <- parseQueryString(request$QUERY_STRING)
        token <- gsub('\"', "", query$token)
        .tok <- getFromNamespace(".tok", "shinymanager")
        if (.tok$is_valid(token)) {
            enable_admin <- TRUE
            head_auth <- NULL
            fab_position <- "bottom-right"
            admin <- query$admin
            language <- query$language
            if (!is.null(language)) {
                lan <- use_language(gsub('\"', "", language))
            }
            is_forced_chg_pwd <-
                getFromNamespace(
                    "is_force_chg_pwd",
                    "shinymanager")(token = token)
            if (is_forced_chg_pwd) {
                args <- list()
                args$id <- "password"
                args$lan <- lan
                pwd_ui <-
                    fluidPage(
                        theme = .globals$theme,
                        tags$head(head_auth),
                        do.call(pwd_ui, args),
                        shinymanager_where("password"),
                        shinymanager_language(lan$get_language())
                    )
                return(pwd_ui)
            }
            .tok$set_timeout(timeout)
            if (isTRUE(enable_admin) && .tok$is_admin(token) &
                identical(admin, "true") &
                !is.null(.tok$get_sqlite_path())) {
                navbarPage(
                    title = "Admin",
                    theme = theme,
                    header =
                        tagList(
                            tags$style(
                                ".navbar-header{margin-left:16.66% !important;}"
                            ),
                            fab_button(
                                position = fab_position,
                                actionButton(
                                    inputId = ".shinymanager_logout",
                                    label = lan$get("Logout"),
                                    icon = icon("sign-out")
                                ),
                                actionButton(
                                    inputId = ".shinymanager_app",
                                    label = lan$get("Go to application"),
                                    icon = icon("share")
                                )
                            ),
                            shinymanager_where("admin")
                        ),
                    tabPanel(
                        title = tagList(icon("home"), lan$get("Home")),
                        value = "home",
                        getFromNamespace(
                            "admin_ui",
                            "shinymanager")("admin", lan),
                        shinymanager_language(lan$get_language())
                    ),
                    tabPanel(
                        title = lan$get("Logs"),
                        getFromNamespace(
                            "logs_ui",
                            "shinymanager")("logs", lan),
                        shinymanager_language(lan$get_language())
                    ),
                    tabPanel(
                        title = 'SiteInfo',
                        webstatsUI('webstats')),
                    tabPanel(
                        title = 'UploadData',
                        uploadUI("upload")),
                    tabPanel(
                        title = 'EditData',
                        editUI("editdata"))
                )
            } else {
                if (isTRUE(enable_admin) && .tok$is_admin(token) &&
                    !is.null(.tok$get_sqlite_path())) {
                    menu <-
                        fab_button(
                            position = fab_position,
                            actionButton(
                                inputId = ".shinymanager_logout",
                                label = lan$get("Logout"),
                                icon = icon("sign-out")
                            ),
                            actionButton(
                                inputId = ".shinymanager_admin",
                                label = lan$get("Administrator mode"),
                                icon = icon("cogs")
                            )
                        )
                }
                else {
                    if (isTRUE(enable_admin) && .tok$is_admin(token) &&
                        is.null(.tok$get_sqlite_path())) {
                        warning(
                            "Admin mode is only available",
                            "when using a SQLite database!",
                            call. = FALSE
                        )
                    }
                    menu <- fab_button(
                        position = fab_position,
                        actionButton(
                            inputId = ".shinymanager_logout",
                            label = lan$get("Logout"),
                            icon = icon("sign-out")
                        )
                    )
                }
                getFromNamespace("save_logs", "shinymanager")(token)
                if (is.function(ui0)) {
                    ui <- ui0(request)
                }
                tagList(
                    ui,
                    menu,
                    shinymanager_where("application"),
                    shinymanager_language(lan$get_language()),
                    singleton(tags$head(
                        tags$script(src = "shinymanager/timeout.js")
                    ))
                )
            }
        } else{
            ui0(request)
        }
    }
}
