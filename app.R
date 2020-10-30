library(reactlog)
library(boastUtils)
library(shinyjs)
library(shinyalert)
library(shinyBS)
library(shinyWidgets)

APP_TITLE <<- "Halloween Bingo"
GRID_SIZE <<- 5
TILE_COUNT <<- GRID_SIZE^2
TILES <<- c(
  "bat", "bone", "broom", "cat", "cauldron",
  "frankenstein", "mummy", "poison", "rat",
  "skull", "spider", "vampire", "werewolf", "witch"
)
BINGO <<- c("B", "I", "N", "G", "O")
TILE_POOL <<- paste(sapply(TILES, function(tile) { paste0(BINGO, "-", tile) }))
POOL_SIZE <<- reactiveVal()
CENTER <<- GRID_SIZE %/% 2 + 1
CENTER_TILE <<- paste0("grid-", CENTER, "-", CENTER)
callHistory <<- reactiveVal(c())
callHistoryUI <<- reactiveVal(list())
declaredBingo <<- reactiveVal(list())
gameOver <<- reactiveVal(FALSE)
signalReset <<- reactiveVal(FALSE)

ui <- dashboardPage(
  skin = "yellow",
  # Header
  dashboardHeader(
    title = APP_TITLE,
    titleWidth = 300,
    tags$li(
      class = "dropdown",
      tags$a(href = "https://shinyapps.science.psu.edu/", icon("home"))
    )
  ),
  # Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "pages",
      menuItem(
        text = "Game",
        tabName = "game",
        icon = icon("gamepad")
      ),
      menuItem(
        text = "References",
        tabName = "references",
        icon = icon("leanpub")
      )
    ),
    tags$div(class = "sidebar-logo", boastUtils::psu_eberly_logo("reversed"))
  ),
  # Pages
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css2?family=Underdog&display=swap"),
      shinyjs::useShinyjs(),
      shinyalert::useShinyalert()
    ),
    tabItems(
      # Game Page
      tabItem(
        tabName = "game",
        fluidRow(
          div(
            class = "col-sm-6",
            hidden(
              div(
                id = "hostPanel",
                h3("Host Controls"),
                bsButton("call", label = "Call"),
                bsButton("sessionReset", label = "Session Reset", style = "danger"),
                p(),
                hidden(
                  div(
                    id = "remaining",
                    h3("Remaining"),
                    textOutput("remainingCalls")
                  )
                ),
                br(),
                hidden(
                  div(
                    id = "declared",
                    h3("Declared"),
                    uiOutput("declaredBingo", class = "declaredBingo")
                  )
                )
              )
            ),
            div(
              id = "playRegion",
              class = "full-block",
              div(
                class = "bingo-header",
                div(class = "B"), div(class = "I"), div(class = "N"), div(class = "G"), div(class = "O")
              ),
              uiOutput("gameBoard", class = "game-board")
            )
          ),
          div(
            class = "col-sm-6",
            div(
              class = "flex-panel sm-reverse",
              div(
                id = "playerPanel",
                h3("How to Play:"),
                tags$ul(
                  tags$li("If the", tags$strong("Host"), "calls an item on your card, click the matching tile to place down a marker.",
                          "Markers can be cleared by using the 'Clear Card' button; the game will not be reset."),
                  tags$li("You may choose a new card using the 'New Card' button", tags$strong("prior"), "to placing down your first marker."),
                  tags$li("To win, mark", tags$strong(GRID_SIZE), "tiles in a column or row,", 
                          "diagonals (corner to corner), or the four corners. The", tags$strong("free"), "tile can be included in a matching set."),
                  tags$li("Click", tags$strong("BINGO"), "when a win condition is met."),
                  tags$li("Wait for the", tags$strong("Host"), "to review your card.")
                ),
                bsButton(inputId = "clearCard", label = "Clear Card", icon = icon("refresh")),
                bsButton(inputId = "newCard", label = "New Card", icon = icon("gift")),
                bsButton(inputId = "bingo", label = "BINGO", icon = icon("hand-stop-o"), disabled = TRUE),
                br(),
                br()
              ),
              hidden(
                div(
                  id = "history",
                  h3("Call History"),
                  uiOutput("callHistory", class = "callHistory"),
                  br()
                )
              )
            )
          )
        )
      ),
      ## References ----
      tabItem(
        tabName = "references",
        h2("References"),
        p(
          class = "hangingindent",
          "Attali, D. and Edwards, T. (2018). shinyalert: Easily create pretty popup messages (modals) in 'Shiny'. (v1.0). [R package]. Available from https://CRAN.R-project.org/package=shinyalert"
        ),
        p(
          class = "hangingindent",
          "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny. (v0.61). [R package]. Available from https://CRAN.R-project.org/package=shinyBS"
        ),
        p(
          class = "hangingindent",
          "Carey, R. (2020). boastUtils: BOAST Utilities. [R Package]. Available from https://github.com/EducationShinyAppTeam/boastUtils"
        ),
        p(
          class = "hangingindent",
          "Chang, W. and Borges Ribeio, B. (2018). shinydashboard: Create dashboards with 'Shiny'. (v0.7.1) [R Package]. Available from https://CRAN.R-project.org/package=shinydashboard"
        ),
        p(
          class = "hangingindent",
          "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. (2019). shiny: Web application framework for R. (v1.4.0) [R Package]. Available from https://CRAN.R-project.org/package=shiny"
        ),
        p(
          class = "hangingindent",
          "Perrier, V., Meyer, F., Granjon, D. (2019). shinyWidgets: Custom inputs widgets for shiny. (v0.5.0) [R Package]. Available from https://CRAN.R-project.org/package=shinyWidgets"
        ),
        p(
          class = "hangingindent",
          "Today's Parent (2014). Halloween bingo game printable [Web]. https://www.todaysparent.com/kids/halloween-bingo-game-printable/"
        )
      )
    )
  )
)

# Server ----
server <- function(input, output, session) {
  # Variables
  tileset <- reactiveVal()
  gameProgress <- reactiveVal(FALSE)
  winState <- reactiveVal(FALSE)
  isHost <- reactiveVal(FALSE)
  
  .init <- function() {
    
    .checkHost()
    
    tileset(.generateTileset())
    
    sapply(1:GRID_SIZE, function(row) {
      sapply(1:GRID_SIZE, function(column) {
        id <- paste0("grid-", row, "-", column)
        observeEvent(session$input[[id]], {
          .boardBtn(id)
          if(!gameProgress()) {
            gameProgress(TRUE)
          }
        }, ignoreInit = TRUE)   
      })
    })
  }
  
  .checkHost <- function() {
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$hostKey) && query$hostKey == "2spooky" || isLocal()) {
      isHost(TRUE)
      show("hostPanel")
      show("remaining")
      hide("playRegion")
      hide("playerPanel")
      output$remainingCalls <- renderText(POOL_SIZE())
    }
  }

  # Helper Functions
  .tileCoordinates <- function(tile = NULL, index = NULL) {
    row <- -1
    col <- -1

    # if: button tile is given, derive from id
    # else: derive from index
    if (!is.null(tile)) {
      # grid-[row]-[col]
      tile <- strsplit(tile, "-")[[1]]
      tile <- tile[-1] # remove oxo

      row <- strtoi(tile[1])
      col <- strtoi(tile[2])
    } else {
      row <- (index - 1) %/% GRID_SIZE + 1
      col <- index - (GRID_SIZE * (row - 1))
    }

    coordinates <- list("row" = row, "col" = col)

    return(coordinates)
  }

  .tileIndex <- function(tile) {
    coords <- .tileCoordinates(tile)

    index <- GRID_SIZE * (coords$row - 1) + coords$col

    return(index)
  }

  .btnReset <- function(index) {
    coords <- .tileCoordinates(index = index)
    id <- paste0("grid-", coords$row, "-", coords$col)
    shinyjs::removeCssClass(id = id, "selected")
  }
  
  .checkCols <- function() {
    
    for(i in 1:GRID_SIZE) {
      tile <- paste0(BINGO[i], "-", tileset()[,i])
      matches <- match(callHistory(), tile)
      count <- length(na.omit(matches))
      
      if(count == GRID_SIZE) {
        return(TRUE)
      }
    }
    
    return(FALSE)
  }
  
  .checkRows <- function() {
    for(i in 1:GRID_SIZE) {
      tile <- paste0(BINGO[i], "-", tileset()[i,])
      matches <- match(callHistory(), tile)
      count <- length(na.omit(matches))
      
      if(count == GRID_SIZE) {
        return(TRUE)
      }
    }
    
    return(FALSE)
  }
  
  .checkDiagonals <- function() {
    match <- FALSE
    
    # Top Left => Bottom Right
    diagLeft <- diag(tileset())
    diagLeft <- paste0(BINGO, "-", diagLeft)
    match(callHistory(), diagLeft)
    
    # Top Right => Bottom Left
    diagRight <- diag(apply(t(tileset()), 2, rev))
    
    return(match) 
  }
  
  # Check the four corners
  .checkCorners <- function() {

    corners <- c(
      paste0(BINGO[1], "-", tileset()[1, 1]),                        # TL
      paste0(BINGO[GRID_SIZE], "-", tileset()[1, GRID_SIZE]),        # TR
      paste0(BINGO[1], "-", tileset()[GRID_SIZE, 1]),                # BL
      paste0(BINGO[GRID_SIZE], "-", tileset()[GRID_SIZE, GRID_SIZE]) # BR
    )
    
    matches <- match(callHistory(), corners)
    
    ifelse(length(na.omit(matches)) == 4, TRUE, FALSE)
  }
  
  .checkState <- function() {
    col <- .checkCols()
    row <- .checkRows()
    diag <- .checkDiagonals()
    corners <- .checkCorners()
    
    ifelse(col || row || diag || corners, TRUE, FALSE)
  }
  
  .sessionReset <- function() {
    TILE_POOL <<- paste(sapply(TILES, function(tile) { paste0(BINGO, "-", tile) }))
    POOL_SIZE(length(TILE_POOL))
    callHistory(c())
    callHistoryUI(list())
    declaredBingo(list())
    signalReset(FALSE) # Force update
    signalReset(TRUE)
  }

  .boardBtn <- function(tile) {
    # Toggle selected
    shinyjs::toggleCssClass(id = tile, class = "selected")
  }

  .cardReset <- function() {
    lapply(1:TILE_COUNT, .btnReset)  
  }
  
  .gameReset <- function() {
    .cardReset()
    gameProgress(FALSE)
    winState(FALSE)
    updateButton(session, inputId = "newCard", disabled = FALSE)
    updateButton(session, inputId = "bingo", disabled = TRUE)
  }

  .generateTileset <- function() {
    set <- replicate(GRID_SIZE, sample(x = TILES, size = GRID_SIZE, replace = FALSE))
    set[CENTER, CENTER] <- "free"
    colnames(set) <- BINGO
    return(set)
  }

  .newCard <- function() {
    lapply(1:TILE_COUNT, .btnReset)
    tileset(.generateTileset())
  }
  
  .printCard <- function() {
    # TODO: CLEAN ME
    HTML("<pre>", 
       paste(match(paste0(BINGO, "-", tileset()[1,]), callHistory())), "\n",
       paste(match(paste0(BINGO, "-", tileset()[2,]), callHistory())), "\n",
       paste(match(paste0(BINGO, "-", tileset()[3,]), callHistory())), "\n",
       paste(match(paste0(BINGO, "-", tileset()[4,]), callHistory())), "\n",
       paste(match(paste0(BINGO, "-", tileset()[5,]), callHistory())),
      "</pre>"
    )
  }
  
  # Event Observers ----
  observe({
    .init()
  })

  # Program the Reset Button
  observeEvent(input$clearCard, {
    .cardReset()
  }, ignoreInit = TRUE)

  # Program the New Card Button
  observeEvent(input$newCard, {
    .newCard()
  }, ignoreInit = TRUE)

  observeEvent(gameProgress(), {
    if (gameProgress()) {
      updateButton(session, inputId = "newCard", disabled = TRUE)
    }
  }, ignoreInit = TRUE)

  # Render Game Board / Attach Observers
  output$gameBoard <- renderUI({
    board <- list()
    index <- 1

    sapply(1:GRID_SIZE, function(row) {
      sapply(1:GRID_SIZE, function(column) {
        id <- paste0("grid-", row, "-", column)
        tile <- tileset()[row, column]

        classes <- "grid-fill"

        classes <- paste(classes, tile)

        board[[index]] <<- tags$li(
          actionButton(
            inputId = paste0("grid-", row, "-", column),
            label = "",
            color = "primary",
            style = "bordered",
            class = classes
          ),
          class = "grid-tile"
        )
        
        index <<- index + 1
      })
    })

    tags$ol(board, class = paste(
      "grid-board",
      "grid-fill",
      paste0("grid-", GRID_SIZE, "x", GRID_SIZE)
    ))
  })

  observeEvent(input$call, {
    # INSTEAD OF SAMPLING LISTS EACH TIME
    # SAMPLE POOL OF AVAILABLE OPTIONS
    # THIS PREVENTS REPEATS
    cleanPool <- na.omit(TILE_POOL)
    
    if(length(callHistory()) == 0) {
      callHistory("N-free")  
    }
    
    if(length(cleanPool) > 0) {
      currentCall <- sample(na.omit(cleanPool), 1)
      index <- match(currentCall, TILE_POOL)
      TILE_POOL[index] <<- NA
      callSplit <- str_split(currentCall, "-")[[1]]
      col <- callSplit[1] 
      tile <- callSplit[2]
      
      ui <- list(span(class = paste(col, "icon", tile)))
      
      # Insert the latest item to the beginning of the list
      callHistory(append(callHistory(), paste0(col, "-", tile), after = 0))
      callHistoryUI(append(callHistoryUI(), ui, after = 0))
      POOL_SIZE(length(cleanPool) - 1)
    } else {
      gameOver(TRUE)
    }
  }, ignoreInit = TRUE)
  
  observe({
    calls <- length(callHistory())
    if(calls > 0) {
      show("history")
      output$callHistory <- renderUI({
        callHistoryUI()
      })
      if(calls >= GRID_SIZE) {
        # Earliest possible win condition (includes free space)
        shinyBS::updateButton(session, inputId = "bingo", disabled = FALSE)
      }
    }
  })
  
  observe({
    if(signalReset()) {
      signalReset(FALSE)
      .gameReset()
      # .newCard() # Should players be given a new card on reset?
      shinyalert(
        inputId = "gameOver",
        title = "New Game",
        text = "The Host has reset the game.",
        type = "warning"
      )
    }
  })
  
  observe({
    if(length(declaredBingo()) > 0) {
      show("declared")
      output$declaredBingo <- renderUI({
        declaredBingo()
      }) 
    } else {
      hide("declared")
    }
  })
  
  observe({
    if(gameOver()) {
      shinyalert(
        inputId = "gameOver",
        title = "Game Over!",
        text = "All tiles have been called.",
        type = "error"
      )  
    }
  })

  observeEvent(input$pages, {
    if (input$pages == "game") {
      if (!gameProgress() && !isHost()) {
        shinyalert(
          inputId = "player",
          title = "Welcome!",
          text = "Please enter your name:",
          type = "input",
          className = "spooky",
          showConfirmButton = TRUE,
          confirmButtonText = "Play",
          showCancelButton = FALSE,
          closeOnClickOutside = FALSE,
          closeOnEsc = FALSE
        )
      }
    }
  }, ignoreInit = FALSE)
  
  observeEvent(input$bingo, {
    shinyalert(
      inputId = "bingoAlert",
      title = "Declare BINGO?",
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "Yes",
      cancelButtonText = "No"
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$bingoAlert, {
    if(input$bingoAlert) {
      winState(.checkState())
      declaredBingo(
        append(
          declaredBingo(), 
          list(
            "player" = input$player, 
            "win" = winState(), 
            "board" = (.printCard())
          )
        )
      )
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$sessionReset, {
    .sessionReset()
  }, ignoreInit = TRUE)
}

boastApp(ui = ui, server = server, config = list(log = FALSE))
