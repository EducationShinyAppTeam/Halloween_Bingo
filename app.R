library(boastUtils)
library(shinyjs)
library(shinyalert)
library(shinyBS)
library(shinyWidgets)

GRID_SIZE <<- 5
TILE_COUNT <<- GRID_SIZE^2
TILES <<- c(
  "bat", "bone", "broom", "cat", "cauldron",
  "frankenstein", "mummy", "poison", "rat",
  "skull", "spider", "vampire", "werewolf", "witch"
)
BINGO <<- c("B", "I", "N", "G", "O")
TILE_POOL <<- paste(sapply(TILES, function(tile) { paste0(BINGO, "-", tile) }))
CENTER_TILE <<- paste0("grid-", GRID_SIZE %/% 2 + 1, "-", GRID_SIZE %/% 2 + 1)
APP_TITLE <<- "Halloween Bingo"
callHistory <<- reactiveVal(c())
callHistoryUI <<- reactiveVal(list())
declaredBingo <<- reactiveVal(list())
gameOver <<- reactiveVal(FALSE)

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
                br()
              ),
              hidden(
                div(
                  id = "declared",
                  h3("Declared"),
                  uiOutput("declaredBingo", class = "declaredBingo")
                )
              )
            ),
            div(
              id = "playRegion",
              class = "full-block",
              div(
                class = "bingo-header",
                div("B"), div("I"), div("N"), div("G"), div("O")
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
                h3("Rules"),
                tags$ul(
                  tags$li("You may select a new card until a marker is placed."),
                  tags$li("Match", GRID_SIZE, "in a row/column."),
                  tags$li("Diagonals (corner to corner) and four corners also count."),
                  tags$li("Click", tags$strong("BINGO"), "when a win condition is met.")
                ),
                bsButton(inputId = "reset", label = "Reset", icon = icon("refresh")),
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
  activeBtn <- NA
  tileset <- reactiveVal()
  gameProgress <- reactiveVal(FALSE)
  winState <- reactiveVal(FALSE)
  isHost <- FALSE

  observe({
    query <- parseQueryString(session$clientData$url_search)

    if (!is.null(query$hostKey) && query$hostKey == "spooky" || isLocal()) {
      isHost <<- TRUE
      show("hostPanel")
      #hide("playRegion")
      #hide("playerPanel")
    }
  })


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
    shinyjs::removeClass(id = id, "selected")
  }
  
  .checkCols <- function() {
    match <- FALSE
    count <- 0
    
    for(i in 1:GRID_SIZE) {
      tile <- paste0(BINGO[i], "-", tileset()[,GRID_SIZE][i])
      if(match(callHistory(), tile)) {
        count <- count + 1
      }
    }
    
    match <- count == GRID_SIZE
    
    return(match)
  }
  
  .checkRows <- function() {
    # check row
    # for tileset[GRID_SIZE[n],]
    match <- FALSE
    
    return(match)
  }
  
  .checkDiagonals <- function() {
    match <- FALSE
    
    return(match) 
  }
  
  .checkCorners <- function() {
    match <- FALSE
    
    return(match)
  }
  
  .checkState <- function() {
    col <- .checkCols()
    row <- .checkRows()
    diag <- .checkDiagonals()
    corners <- .checkCorners()
    
    if(col || row || diag || corners) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  .gameCheck <- function(mat) {
    rows <- rowSums(mat)
    cols <- colSums(mat)

    if (GRID_SIZE > 1) {
      mainD <- sum(diag(mat))
      rotated <- apply(t(mat), 2, rev)
      offD <- sum(diag(rotated))

      if (GRID_SIZE %in% rows ||
        GRID_SIZE %in% cols ||
        mainD == GRID_SIZE || offD == GRID_SIZE) {
        return("win")
      } else if (-GRID_SIZE %in% rows ||
        -GRID_SIZE %in% cols == 1 ||
        mainD == -GRID_SIZE || offD == -GRID_SIZE) {
        return("lose")
      } else if (any(mat == 0)) {
        return("continue")
      } else {
        return("draw")
      }
    } else {
      ifelse(rows == 1 && rows != 0, return("win"), return("lose"))
    }
  }

  .boardBtn <- function(tile) {
    index <- .tileIndex(tile)

    if (!gameProgress()) {
      gameProgress(TRUE)
    }

    # Toggle selected
    shinyjs::toggleClass(id = tile, class = "selected")
  }

  .gameReset <- function() {
    lapply(1:TILE_COUNT, .btnReset)
    gameProgress(FALSE)
    activeBtn <- NA
    winState(NA)
  }

  .generateTileset <- function() {
    set <- replicate(GRID_SIZE, sample(x = TILES, size = GRID_SIZE, replace = FALSE))
    colnames(set) <- BINGO
    return(set)
  }

  .newCard <- function() {
    tileset(.generateTileset())
    sapply(1:GRID_SIZE, function(row) {
      sapply(1:GRID_SIZE, function(column) {
        id <- paste0("grid-", row, "-", column)
        if (id != CENTER_TILE) {
          tile <- tileset()[row, column]
          shinyjs::removeClass(id = id, class = paste(TILES))
          shinyjs::addClass(id = id, class = tile)
        }
      })
    })
  }

  # Program the Reset Button
  observeEvent(input$reset, {
    .gameReset()
  })

  # Program the New Card Button
  observeEvent(input$newCard, {
    .newCard()
  })

  observeEvent(gameProgress(), {
    if (gameProgress()) {
      updateButton(session, inputId = "newCard", disabled = TRUE)
    }
  })

  # Render Game Board / Attach Observers
  output$gameBoard <- renderUI({
    board <- list()
    index <- 1
    tileset(.generateTileset())
    sapply(1:GRID_SIZE, function(row) {
      sapply(1:GRID_SIZE, function(column) {
        id <- paste0("grid-", row, "-", column)
        tile <- tileset()[row, column]

        classes <- "grid-fill"

        if (id != CENTER_TILE) {
          classes <- paste(classes, tile)
        } else {
          classes <- paste(classes, "free")
        }

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

        observeEvent(session$input[[id]], {
          activeBtn <<- id
          .boardBtn(id)
        })

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
      
      # Earliest possible win condition
      if(length(callHistory()) >= (GRID_SIZE - 1)) {
        shinyBS::updateButton(session, inputId = "bingo", disabled = FALSE)
      }
    } else {
      gameOver(TRUE)
    }
  })
  
  observe({
    if(length(callHistory()) > 0) {
      show("history")
      output$callHistory <- renderUI({
        callHistoryUI()
      })
      #print(callHistory())
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
    # TODO: IF GAME OVER ALL REACTIVE VALUES SHOULD RESET
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
      # TODO: PUT THIS BACK -> && !isHost
      if (!gameProgress()) {
        shinyalert(
          inputId = "player",
          title = "Player",
          text = "Please enter your name:",
          type = "input",
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
  })
  
  observeEvent(input$bingoAlert, {
    if(input$bingoAlert) {
      print(tileset())
      winState(.checkState())
      print(winState())
      
      declaredBingo(append(declaredBingo(), list("player" = input$player, "win" = winState())))
    }
  })

  observeEvent(input$endGame, {
    .gameReset()
  })
}

boastApp(ui = ui, server = server)
