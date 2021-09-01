custom_jumbotron <- function(header , content, button = TRUE,  ...){
  
  button_label = c(...)
  
  if (button){
    div(class = "jumbotron",
        h1(header, style = "color: #FFF;"), content, p(a(class = "btn btn-primary btn-lg button", id='tabBut', button_label)))
    
  } else {
    div(class = "jumbotron", h1(header, style = "color: #FFF;"), content)# remove the p() tag
    
  }
  
}

dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

setBoxClass <- function(status, solidHeader, collapsible, collapsed,
                        gradient, background, sidebar) {
  
  boxClass <- "box"
  if (solidHeader) {
    boxClass <- paste(boxClass, "box-solid")
  }
  
  if (!is.null(status)) {
    boxClass <- paste0(boxClass, " box-", status)
  }
  
  if (collapsible && collapsed) {
    boxClass <- paste(boxClass, "collapsed-box")
  }
  
  if (!is.null(background)) {
    boxClass <- paste0(boxClass, " bg-", background, if (gradient) "-gradient")
  }
  
  if (!is.null(sidebar)) {
    sidebarToggle <- sidebar[[1]]
    startOpen <- sidebarToggle$attribs$`data-start-open`
    if (startOpen == "true") {
      boxClass <- paste0(boxClass, " direct-chat direct-chat-contacts-open")
    } else {
      boxClass <- paste0(boxClass, " direct-chat")
    }
  }
  
  boxClass
}


setBoxStyle <- function(height, sidebar) {
  style <- NULL
  if (!is.null(height)) {
    style <- paste0("height: ", shiny::validateCssUnit(height))
  }
  # add padding if box sidebar
  if (!is.null(sidebar)) {
    style <- paste(style, "padding: 10px;")
  }
}

# create box icons and return a list of icons
createBoxTools <- function(collapsible, collapsed, closable, 
                           sidebar, dropdownMenu, boxToolSize, status, 
                           background, solidHeader) {
  
  btnClass <- paste0(
    "btn btn-box-tool", 
    if (!is.null(boxToolSize)) paste0(" btn-", boxToolSize)
  )
  
  if (is.null(status) && !is.null(background)) {
    btnClass <- paste0(
      btnClass,
      if (background %in% validStatusesPlus) {
        paste0(" bg-", background)
      }
    )
  }
  
  # status has always priority compared to background
  if (!is.null(status) && solidHeader) {
    btnClass <- paste0(
      btnClass,
      if (status %in% validStatuses) {
        paste0(" btn-", status)
      }
    )
  }
  
  collapseTag <- NULL
  if (collapsible) {
    collapseIcon <- if (collapsed) 
      "plus"
    else "minus"
    collapseTag <- shiny::tags$button(
      class = btnClass, 
      type = "button",
      `data-widget` = "collapse", 
      shiny::icon(collapseIcon)
    )
  }
  
  closableTag <- NULL
  if (closable) {
    closableTag <- shiny::tags$button(
      class = btnClass, 
      `data-widget` = "remove", 
      type = "button",
      shiny::icon("times")
    )
  } 
  
  sidebarToolTag <- NULL
  if (!is.null(sidebar)) {
    sidebar[[1]]$attribs$class <- btnClass
    sidebarToolTag <- sidebar[[1]]
  }
  
  dropdownMenuToolTag <- NULL
  if (!is.null(dropdownMenu)) {
    dropdownMenu$children[[1]]$attribs$class <- paste0(btnClass, " dropdown-toggle")
    dropdownMenuToolTag <- dropdownMenu
  }
  
  dropNulls(list(dropdownMenuToolTag, collapseTag, closableTag, sidebarToolTag))
}

custom_box <- function(..., title = NULL, footer = NULL, status = NULL, solidHeader = FALSE, 
                background = NULL, width = 6, height = NULL, collapsible = FALSE, 
                collapsed = FALSE, closable = FALSE, icon = NULL, gradient = FALSE, boxToolSize = "sm", 
                headerBorder = TRUE, label = NULL, dropdownMenu = NULL,
                sidebar = NULL, id = NULL, accessibility_statement = FALSE) {
  
  if (is.null(status)) solidHeader <- TRUE
  
  # multiple validation
  # validateBoxProps(
  #   title = title,
  #   label = label,
  #   sidebar = sidebar,
  #   dropdownMenu = dropdownMenu,
  #   status = status,
  #   gradient = gradient,
  #   collapsible = collapsible,
  #   collapsed = collapsed,
  #   solidHeader = solidHeader,
  #   background = background,
  #   width = width
  # )
  
  props <- dropNulls(
    list(
      title = if (!is.null(title)) {
        if (inherits(title, "list")) {
          unlist(
            dropNulls(
              lapply(title, function(e) {
                if (inherits(e, "shiny.tag.list") ||
                    inherits(e, "shiny.tag")) {
                  as.character(e)
                }
              })
            )
          )
        } else {
          as.character(title)
        }
      } else {
        title
      },
      status = status,
      solidHeader = solidHeader,
      background = background,
      width = width,
      height = height,
      collapsible = collapsible,
      closable = closable,
      gradient = gradient
    )
  )
  
  # set box class
  boxClass <- setBoxClass(
    status, 
    solidHeader, 
    collapsible, 
    collapsed,
    gradient, 
    background, 
    sidebar
  )
  
  # set style
  style <- setBoxStyle(height, sidebar)
  
  # box tools
  boxToolTag <- NULL
  # create card tools whenever necessary
  if (collapsible || closable || 
      !is.null(dropdownMenu) || !is.null(sidebar) || !is.null(label)) {
    boxToolTag <- shiny::tags$div(class = "box-tools pull-right")
  }
  
  # update box tools
  boxToolTag <- shiny::tagAppendChildren(
    boxToolTag,
    label,
    createBoxTools(
      collapsible, 
      collapsed, 
      closable,
      sidebar, 
      dropdownMenu,
      boxToolSize,
      status,
      background,
      solidHeader
    )
  )
  
  # header
  if (is.null(title) && 
      (closable || collapsible || 
       !is.null(dropdownMenu) || !is.null(sidebar) || !is.null(label)
      )) title <- "\u200C"
  
  if(!accessibility_statement){
    headerTag <- shiny::tags$div(
      class = paste0("box-header", if (headerBorder) " with-border"),
      # header icon
      icon, 
      shiny::tags$h2(class = "box-title", style = "color: #000000; font-size: 36px;", title)##00bfa5
    )
  } else {
    headerTag <- shiny::tags$div(
      class = paste0("box-header", if (headerBorder) " with-border"),
      # header icon
      icon, 
      shiny::tags$h1(class = "box-title", style = "color: #000000; font-size: 40px; font-weight: bold;", title)
    )
  }
  
  headerTag <- shiny::tagAppendChild(headerTag, boxToolTag)
  
  
  # body
  bodyTag <- shiny::tags$div(
    class = "box-body",
    style = style,
    ...,
    sidebar[[2]]
  )
  
  # footer 
  footerTag <- if (!is.null(footer)) {
    shiny::tags$div(
      class = "box-footer", 
      footer
    )
  }
  
  boxTag <- shiny::tags$div(class = boxClass, id = id)
  boxTag <- shiny::tagAppendChildren(boxTag, headerTag, bodyTag, footerTag)
  
  # wrapper
  shiny::tags$div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    boxTag,
    # config script to be used by card input binding
    shiny::tags$script(
      type = "application/json",
      `data-for` = id,
      jsonlite::toJSON(
        x = props,
        auto_unbox = TRUE,
        json_verbatim = TRUE
      )
    )
  )
  
}

