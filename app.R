library(boastUtils)
library(shinyjs)
library(shinyalert)
library(shinyBS)
library(shinyWidgets)

GRID_SIZE <- 5
TILE_COUNT <- GRID_SIZE ^ 2
TILES <- c(
  "bat", "bone", "broom", "cat", "cauldron",
  "frankenstein", "mummy", "poison", "rat",
  "skull", "spider", "vampire", "werewolf", "witch"
)
CENTER_TILE <- paste0("grid-", GRID_SIZE %/% 2 + 1, "-", GRID_SIZE %/% 2 + 1)
APP_TITLE <<- "Halloween Bingo"

callHistory <- reactiveVal(list())

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
      tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css2?family=Underdog&display=swap")
    ),
    tabItems(
    # Game Page
    tabItem(
      tabName = "game",
      h3(uiOutput("player")),
      fluidRow(
        div(
          class = "col-sm-6",
          div(
            class = "bingo-header",
            div("B"), div("I"), div("N"), div("G"), div("O")
          ),
          uiOutput("gameBoard", class = "game-board"),
          shinyjs::useShinyjs()
        ),
        div(
          class = "col-sm-6",
          div(
           h3("Rules"),
           tags$ul(
             tags$li("You may select a new card until a marker is placed."),
             tags$li("Match", GRID_SIZE, "in a row/column."),
             tags$li("Diagonals (corner to corner) and four corners also count."),
             tags$li("Click", tags$strong("BINGO"), "when a win condition is met.")
           ),
           bsButton(inputId = "reset", label = "Reset", icon = icon("refresh")),
           bsButton(inputId = "newCard", label = "New Card", icon = icon("gift")),
           bsButton(inputId = "bingo", label = "BINGO", icon = icon("hand-stop-o")),
           hidden(
             div(id = "history",
              br(),
              h3("Call History"),
              uiOutput("callHistory", class = "callHistory")
             )
           )
          )
        )
      ),
      hidden(
        div(id = "hostPanel",
            bsButton("call", label = "Call")    
        )
      )
    ),
    tabItem(
      tabName = "references",
      withMathJax(),
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

server <- function(input, output, session) {
  # Variables
  activeBtn <- NA
  player <- NA
  opponent <- NA
  scoreMatrix <-
    matrix(
      data = rep.int(0, times = TILE_COUNT),
      nrow = GRID_SIZE,
      ncol = GRID_SIZE
    )
  gameProgress <- reactiveVal(FALSE)
  isHost <- FALSE
  
  observe({
    query <- parseQueryString(session$clientData$url_search)  
    
    if (!is.null(query$hostKey) && query$hostKey == "spooky" || isLocal()) {
      isHost <- TRUE
      show("hostPanel")
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
  
  .score <- function(score, tile, value) {
    i <- .tileCoordinates(tile)
    
    score[i$row, i$col] <- value
    
    return(score)
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
    
    if(!gameProgress()) {
      gameProgress(TRUE)
    }
    
    # Toggle selected
    shinyjs::toggleClass(id = tile, class = "selected")
  }
  
  .gameReset <- function() {
    lapply(1:TILE_COUNT, .btnReset)
    
    scoreMatrix <<-
      matrix(
        data = rep.int(0, times = TILE_COUNT),
        nrow = GRID_SIZE,
        ncol = GRID_SIZE
      )
    gameProgress <- FALSE
    activeBtn <- NA
  }
  
  .generateTileset <- function() {
    replicate(GRID_SIZE, sample(x = TILES, size = GRID_SIZE, replace = FALSE))
  }
  
  .newCard <- function() {
    tileset <- .generateTileset()
    sapply(1:GRID_SIZE, function(row) {
      sapply(1:GRID_SIZE, function(column) {
        id <- paste0("grid-", row, "-", column)
        if (id != CENTER_TILE) {
          tile <- tileset[row, column]
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
    if(gameProgress()) {
      updateButton(session, inputId = "newCard", disabled = TRUE)
    }
  })
  
  # Render Game Board / Attach Observers
  output$gameBoard <- renderUI({
    board <- list()
    index <- 1
    
    tileset <- .generateTileset()
    
    sapply(1:GRID_SIZE, function(row) {
      sapply(1:GRID_SIZE, function(column) {
        
        id <- paste0("grid-", row, "-", column)
        tile <- tileset[row, column]
        
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
    show("history")
    col <- sample(c("B", "I", "N", "G", "O"), 1)
    tile <- list(span(class = paste(col, "icon", sample(TILES, 1))))
    
    callHistory(append(callHistory(), tile))
  })
  
  output$callHistory <- renderUI({
    callHistory()
  })
  
  # Program Submit Button
  observeEvent(input$submit, {
    index <- .tileIndex(activeBtn)
    
    updateButton(
      session = session,
      inputId = activeBtn,
      label = player,
      disabled = TRUE
    )
    
    scoreMatrix <<- .score(scoreMatrix, activeBtn, 1)
    
    # Check for game over states
    .gameState <- .gameCheck(scoreMatrix)
    completion <- ifelse(.gameState == "continue", FALSE, TRUE)
    
    if (.gameState == "win") {
      confirmSweetAlert(
        session = session,
        inputId = "endGame",
        title = "You Win!",
        text = "You've filled either a row, a column, or a main diagonal. Start over and play a new game.",
        btn_labels = "Start Over"
      )
    } else if (.gameState == "lose") {
      confirmSweetAlert(
        session = session,
        inputId = "endGame",
        title = "You lose :(",
        text = "Take a moment to review the concepts and then try again.",
        btn_labels = "Start Over"
      )
    } else if (.gameState == "draw") {
      confirmSweetAlert(
        session = session,
        inputId = "endGame",
        title = "Draw!",
        text = "Take a moment to review the concepts and then try again.",
        btn_labels = "Start Over"
      )
    }
    updateButton(session = session,
                 inputId = "submit",
                 disabled = TRUE)
  })
  
  observeEvent(input$pages, {
    if (input$pages == "game") {
      if (!gameProgress) {
        shinyalert(
          title = "Player Select",
          text = "Select whether you want to play as O or X.",
          showConfirmButton = TRUE,
          confirmButtonText = "Play as X",
          showCancelButton = TRUE,
          cancelButtonText = "Play as O"
        )
        gameProgress <<- TRUE
      }
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$endGame, {
    .gameReset()
  })
  
  observeEvent(input$shinyalert, {
    player <<- "X"
    opponent <<- "O"
    
    output$player <- renderUI({
      return(paste0("You are playing as ", player, "."))
    })
  })
}

boastApp(ui = ui, server = server)
