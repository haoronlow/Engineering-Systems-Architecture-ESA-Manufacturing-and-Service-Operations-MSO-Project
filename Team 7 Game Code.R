if(!require(shiny)){
  install.packages("shiny")
  library(shiny)
}

if(!require(shinyjs)){
  install.packages("shinyjs")
  library(shinyjs)
}
#library(shinyWidgets)
if(!require(shinyWidgets)){
  install.packages("shinyWidgets")
  library(shinyWidgets)
}
#library(ggplot2)
if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

#library(reshape)
if(!require(reshape)){
  install.packages("reshape")
  library(reshape)
}
#library(scales)
if(!require(scales)){
  install.packages("scales")
  library(scales)
}
#library(shinydashboard)
if(!require(shinydashboard)){
  install.packages("shinydashboard")
  library(shinydashboard)
}
#library(tidyverse)
if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}
#library(DBI)
if(!require(DBI)){
  install.packages("DBI")
  library(DBI)
}
#library(jsonlite)
if(!require(jsonlite)){
  install.packages("jsonlite")
  library(jsonlite)
}
#library("stringr")
if(!require(stringr)){
  install.packages("stringr")
  library(stringr)
}
#library('shinydashboardPlus')
if(!require(shinydashboardPlus)){
  install.packages("shinydashboardPlus")
  library(shinydashboardPlus)
}
#library(fresh)
if(!require(fresh)){
  install.packages("fresh")
  library(fresh)
}

#AWS Connection
getAWSConnection <- function(){
  conn <- dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "project007",
    host = "esddbinstance-1.ceo4ehzjeeg0.ap-southeast-1.rds.amazonaws.com",
    username = "project007",
    password = "XvfyY8hx")
  conn
}

#Generate, or re-generate, HTML to create modal dialog for Password creation
RegisterModal <- function() {
  modalDialog(
    title = "Enter your name for highscore",
    textInput("username", "Enter a username"),
    footer = tagList(
      modalButton("Cancel"),
      h5(actionButton("passwordok", "OK"))
    )
  )
}



#make staff not able to prepare same food at the same time? Or if they do and it overexceeds it results in a wastage cost
#if so we might have the make the refilling be step to make more sense


#staff status change by user input. Status goes by c(lead time of that food, type of food being prepared, quantity)
staff_status_change_by_user <- function(task, inventory, capacity, staff, lead, qty) 
  {if (staff[1]<=-1){
  if (task == "Chicken") {staff<-c(lead[1], 1, qty)}
  if (task == "Steak") {staff<-c(lead[2], 2, qty)}
  if (task == "Pizza") {staff<-c(lead[3], 3, qty)}
  if (task == "Sushi") {staff<-c(lead[4], 4, qty)}
  if (task == "Salad") {staff<-c(lead[5], 5, qty)}
  if (task == "Do nothing") {staff<-c(-1, 0, 0)}}
  return (staff)
}

#generate customer demand function. First generate foodlist, then foodlist converts to demand. generate cust just
#needs to return a list of length 5 so it can be modified to however you want
generatecust <- function(satisfaction=50) {
  x<-floor(satisfaction/10)
  if(x==0){x <- 1}
  demand <- c(0,0,0,0,0)
  foodlist <- c()
  
  #generates foodlist
  n <- runif(x, min = 0, max = 15)
  for (i in 1:x){
    if (between(n[i],14,15)){foodlist<-append(foodlist,5)}
    if (between(n[i],12,14)) {foodlist<-append(foodlist,4)}
    if (between(n[i],9,12)) {foodlist<-append(foodlist,3)}
    if (between(n[i],5,9))  {foodlist<-append(foodlist,2)}
    if (n[i]<5)  {foodlist<-append(foodlist,1)}
  }
  
  #converts foodlist into demand
  for (p in 1:5){
    demand[p] <- sum(foodlist==p)
  }
  return(demand)
}

#display inventory as a graph helper function
getInventoryPlot <- function(inventory, capacity) {
  x <- c("Chicken", "Steak", "Pizza", "Sushi", "Salad")
  level_order <- c("Chicken", "Steak", "Pizza", "Sushi", "Salad")
  to_plot <- data.frame(x=x,inventory,capacity)
  invplot<-melt(to_plot, id="x")
  
  ggplot(invplot,aes(x=factor(x,level=level_order),y=value,fill=variable)) + 
    geom_bar(stat="identity",position = "identity", alpha=.3) + 
    scale_y_continuous(breaks= pretty_breaks()) +
    xlab("Food Type")
}




#Perform database operation and save the score, if the player clicks the "publishscore"

publishScore <- function(username,score1,score2){
  conn <- getAWSConnection()
  querytemplate <- "INSERT INTO Playerscore (username,Cash,Satisfaction) VALUES (?id1,?id2,?id3)"
  query <- sqlInterpolate(conn, querytemplate,id1=username,id2=score1,id3=score2)
  #print(query) #for debug
  success <- FALSE
  tryCatch(
    {  # This is not a SELECT query so we use dbExecute
      result <- dbExecute(conn,query)
      print("Score published")
      success <- TRUE
    }, error=function(cond){print("publishScore: ERROR")
      print(cond)
    }, 
    warning=function(cond){print("publishScore: WARNING")
      print(cond)},
    finally = {}
  )
  dbDisconnect(conn)
}

# GetLeaderBoard 
getLeaderBoard <- function(){
  conn <- getAWSConnection()
  query <- "SELECT Playerscore.username,Playerscore.Cash,Playerscore.Satisfaction FROM Playerscore "
  
  #print(query)
  
  #low score win 
  query <- paste0(query, "ORDER BY Playerscore.Cash DESC, Playerscore.Satisfaction DESC LIMIT 10")
  
  result <- dbGetQuery(conn,query)
  dbDisconnect(conn)
  result
  #print(result)
}

#UI theme
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#92C4AB", ##green colour
    orange = "#C4B492",
    blue = "#6699CC",
    green = "#6699CC",
    red = "#C4B492"
  ),
  adminlte_sidebar(
    width = "200px",
    dark_bg = "#8A7C5F", ##brown colour
    dark_hover_bg = "#C4B492", ##light brown colour
    dark_color = "#KM4708" ## black colour
  ),
  adminlte_global(
    content_bg = "#FFF", ##white colour
    box_bg = "#FFF7EF",  ##light color
    info_box_bg = "#FFF7EF"
  )
)
header_img <- tags$img(
  src='ASIAN MART.png',
  style = 'height: 50px; width: 50px; position: absolute; left: 50%; transform: translateX(-50%);'
)
header <-  htmltools::tagQuery(shinydashboardPlus::dashboardHeader(title = "Asian Mart"))
header <- header$
  addAttrs(style = "position: relative")$ # add some styles to the header 
  find(".navbar.navbar-static-top")$ # find the header right side
  append(header_img)$ # inject our img
  allTags()


ui <- dashboardPage(#tags$li(class="dropdown", tags$a(target="_blank",tags$img(height="25px", alt="SNAP Logo", src="ASIAN MART.png", align = "left"))),
  header,  
  dashboardSidebar(
    h5(sidebarMenu(
      tags$style(".left-side, .main-sidebar {padding-top: 60px}"),
      menuItem("Instruction", tabName = "welcome", icon = icon("scroll")),
      menuItem("Pre-Game", tabName = "setting", icon = icon("gear")),
      menuItem("Game", tabName = "game", icon = icon("gamepad")),
      menuItem("Leaderboard", tabName = "Leaderboard", icon = icon("table")),
      menuItem("Lessons Learnt", tabName = "educate", icon = icon("book-open")
               ))
    )
  ),
  dashboardBody( 
    #tags$audio(src = "Speaker Joy.mp3", type = "audio/mp3", autoplay = NA),
    #use the theme
    use_theme(mytheme),
    tags$style(
      '
        @media (min-width: 768px){
          .sidebar-mini.sidebar-collapse .main-header .logo {
              width: 200px; 
          }
          .sidebar-mini.sidebar-collapse .main-header .navbar {
              margin-left: 200px;
          }
        }
        '
    ),
    tags$head(
      # Note the wrapping of the string in HTML()
      tags$style(HTML(" 
      @import url('https://fonts.googleapis.com/css2?family=Grandstander&display=swap');
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      .main-header .logo {
        font-family: 'Grandstander', sans-serif;
        font-weight: bold;
        font-size: 24px;
        text-shadow: 1px 1px 1px, color: 'black';
      }
      label {
        font-family: 'Grandstander', sans-serif;
      }
      title {
        font-family: 'Grandstander', sans-serif;
      }
      body {
        background-color: black;
      }
      h1 {
        font-family: 'Grandstander', sans-serif; color: #58828E;font-size: 36px;
      }
      h2 { 
        font-family: 'Grandstander', sans-serif; 
      }
      h4 { 
        font-family: 'Grandstander', sans-serif; color: #92C4AB;font-size: 20px;
      }
      h3 { 
        font-family: 'Grandstander', sans-serif; font-size: 12px;
      }
      h5 { 
        font-family: 'Grandstander', sans-serif;font-size: 18px;
      }
      h6 { 
        font-family: 'Grandstander', sans-serif; color: #6699CC;font-size: 13px;
      }
      .shiny-input-container {
        color:#6699CC;
      }"))
    ),
    
    
    #tab content
    tabItems(
      # First tab content
      tabItem(tabName = "welcome",
              fluidPage(
                sidebarLayout(
                  sidebarPanel=" ",
                  mainPanel(width="400px",
                            tabsetPanel(
                              tabPanel(#h6("Register"), 
                                       h1(tags$b("Welcome to the Asian Mart"), align = "center", width="400px"),
                                       h4("Below are the lead times for the food (i.e. the amount of time you need to prep)", align = "center"),
                                       h4(htmlOutput("lead_time", align = "center")),
                                       h4(htmlOutput("username", align = "center")),
                                       #uiOutput("register", align = "center"), 
                                       HTML('<center><audio src="Speaker Joy.mp3" type="audio/mp3" autoplay controls></audio></center>'),),
                              tabPanel(h6("Tutorial"), width="400px", 
                                       fluidPage(
                                         h1(tags$b("Asian Mart Simulation Tutorial"), align = "center", width="400px"),
                                         sidebarLayout(
                                           sidebarPanel=" ",
                                           mainPanel(
                                             width = 12,
                                             tabsetPanel(type="tab",
                                                         tabPanel(h6("pdf"),tags$iframe(style= "height:500px;width:100%;scrolling=yes",src="Asian Mart Simulation Tutorial.pdf"))
                                             )
                                           )
                                         )
                                       )
                              )
                            )
                  )
                )
              )
      ),
      
      tabItem(tabName = "setting",
              h1(tags$b("Here you can set the capacity of each food"), align = "center", width="400px"),
              fluidRow(
                box(
                  title = "Chicken Capacity", style = "height:100px", width = 2, solidHeader = TRUE, status = "success",
                  uiOutput("chickencap")
                ),
                box(
                  title = "Steak Capacity", style = "height:100px", width = 2, solidHeader = TRUE, status = "success",
                  uiOutput("steakcap")
                ),
                box(
                  title = "Pizza Capacity", style = "height:100px", width = 2, solidHeader = TRUE, status = "success",
                  uiOutput("pizzacap")
                ),
                box(
                  title = "Sushi Capacity", style = "height:100px", width = 2, solidHeader = TRUE, status = "success",
                  uiOutput("sushicap")
                ),
                box(
                  title = "Salad Capacity", style = "height:100px", width = 2, solidHeader = TRUE, status = "success",
                  uiOutput("saladcap")
                ),
                h5(actionButton(inputId = "Setcap", label = "Confirm", style="color: #FFF; background-color: #367D86; border-color: #0A0A0A"))
              ),
              valueBoxOutput("starting_cash"),
              htmlOutput("Invalid"),
              
              fluidRow(
                box(
                  title = "Game Mode", style = "height:100px", width = 2, solidHeader = TRUE, status = "success",
                  uiOutput("gamemode")
                )
              )
              
      ),
      
      
      # Second tab content
      tabItem(tabName = "game",
              
              
              #status display
              fluidRow(
                column(6,h1(tags$b("Inventory")),
                       plotOutput("inventoryplot", height = "80%")),
                column(6, h1(tags$b("Graphic")), #tags$img(src="AdobeStock.png",height="200px", width="600px", alt="sth went wrong", deletefile=FALSE),
                       htmlOutput("graphic"))
              ),
              tags$head(tags$style(HTML(".small-box {height: 122px}"))),
              
              
              fluidRow(
                box(
                  title = "Staff 1", style = "height:180px", width = 3, solidHeader = TRUE, status = "success", 
                  uiOutput("task1")
                ),
                box(
                  title = "Staff 2", style = "height:180px", width = 3, solidHeader = TRUE, status = "success",  
                  uiOutput("task2")
                ),
                box(
                  title = "Staff 3", style = "height:180px", width = 3, solidHeader = TRUE, status = "success", 
                  uiOutput("task3")
                ),
                useShinyjs(),
                column(3,
                       uiOutput("confirm"),
                       htmlOutput("turncost"))
                
              ),
              valueBoxOutput("turncount"),
              valueBoxOutput("cash"),
              valueBoxOutput("satisfaction"),
              box(
                width=4,
                title = "Customer orders", status = "success", solidHeader = TRUE,
                collapsible = FALSE,
                h6(tags$b(htmlOutput("current_customer"))),
                h6(tags$b(htmlOutput("next_customer")))
              ),
              
              valueBoxOutput("shortage"),
              valueBoxOutput("wastage")
              
              
      ),
      # Third tab content
      tabItem(tabName = "Leaderboard",
              h1(tags$b("See where you stand on the leaderboard"), align = "center"),
              #actionButton("publishScore", "Publish Score"),
              h4(tags$b("Your Score is"), align = "center"),
              h4(htmlOutput("showscore"), align = "center"),
              h4(uiOutput("publishscore"), align = "center"),
              div(h6(tableOutput("leaderboard"), style= 'align-self: centers;'), style="display:flex; justify-content: center")
      ),
      # "model","World Food Shortage", "CRM"
      #Fourth Tab content
      tabItem(tabName = "educate",
              fluidPage(h1(tags$b("We hope you understand..."), align = "center"),
                        tabPanel(h6("Tutorial"), width="400px", 
                                 fluidPage(
                                   sidebarLayout(
                                     sidebarPanel=" ",
                                     mainPanel(
                                       width = 12,
                                       tabsetPanel(type="tab",
                                                   tabPanel(h6("pdf"),tags$iframe(style= "height:500px;width:100%;scrolling=yes",src="Educational Message.pdf"))
                                       )
                                     )
                                   )
                                 )
                        ))
              #h4("- Difficulties of managing an establishment", align = "center"),
              #h4("- Leftover costs will also raise a major issue of food conservation in a world that is currently experiencing major food shortage in less developed countries", align = "center"),
              #h4("- Theory of (Q,r) model", align = "center"),
              #column(10, HTML('<center><img src="Q,R Model.png" height = "250"></center>')),
              #column(10, HTML('<center><img src="Q,R Model 2.png" height = "250"></center>')),
              #column(10, h4("- Customer relationship management (CRM)", align = "center")))
              
      )
      
    )
  )
)




server <- function(input, output, session) {
  v <- reactiveValues(staff1 = c(-1,0,0), staff2 = c(-1,0,0), staff3 = c(-1,0,0),
                      inventory = c(5,5,5,5,5), inventorycap = c(5,5,5,5,5),
                      lead_time = c(3,3,4,2,2),
                      current_cust = c(0,0,0,0,0), next_cust = c(2,1,1,1,0),
                      turn = 0, lastturn = 50, shortage = 0, wastage = 0,
                      satisfaction = 50, playerid = 1, username = NULL,
                      cash = 2000, food_price = 3, sale_price = 10,img = "graphic.jpg",
                      flag = FALSE,
                      starting_cash = 2000, total_cap =0,
                      cashscore = 0, total_satisfaction = 0, satisfactionscore = 0, published = 0)
  
  #turn based reactive values (only 1 turn and set back to default)
  temp <- reactiveValues(turnshort = 0, show = 1, cost = 0, order1 = 0, order2 = 0, order3 = 0)
  
  #change game mode
  mode <- reactiveValues(dynamic = 0)
  

  #Main code
  #produces the small pic unavail image when first opened
  observe({
    input$btn
    if (isolate(v$flag)) {
      v$flag <- FALSE
      invalidateLater(4000)
    } else {
      v$img <- sample(c("graphic.jpg","graphic.jpg"), 1)
    }
  })
  
  #show confirm button using temp$show
  output$confirm <- renderUI({
    if(temp$show == 1){
      h5(actionButton(inputId = "confirm", label = "End Turn", style="color: #FFF; background-color: #367D86; border-color: #0A0A0A"))}
  })
  

  observeEvent(input$confirm,{
    
    temp$order1 <- 0
    temp$order2 <- 0
    temp$order3 <- 0
    temp$cost <- 0
    temp$show <- 0
    v$flag <- TRUE
    if (v$flag)v$img <- "graphic.gif"
    
    #deduct cash on hand depending on qty of food ordered
    #player input results in staff status change
    if ((v$staff1[1] < 0) && (input$task1 != "Do nothing")){
      if (v$cash >= v$food_price*as.numeric(input$quantity1)){
        v$staff1 <- staff_status_change_by_user(input$task1, v$inventory, v$inventorycap,v$staff1, v$lead_time, as.numeric(input$quantity1))
        v$cash <- v$cash - v$food_price*as.numeric(input$quantity1)
      }
    }
    
    if ((v$staff2[1] < 0) && (input$task2 != "Do nothing")){
      if (v$cash >= v$food_price*as.numeric(input$quantity2)){
        v$staff2 <- staff_status_change_by_user(input$task2, v$inventory, v$inventorycap,v$staff2, v$lead_time, as.numeric(input$quantity2))
        v$cash <- v$cash - v$food_price*as.numeric(input$quantity2)
      }
    }
    
    if ((v$staff3[1] < 0) && (input$task3 != "Do nothing")){
      if (v$cash >= v$food_price*as.numeric(input$quantity3)){
        v$staff3 <- staff_status_change_by_user(input$task3, v$inventory, v$inventorycap,v$staff3, v$lead_time, as.numeric(input$quantity3))
        v$cash <- v$cash - v$food_price*as.numeric(input$quantity3)
      }
    }
    
    
    
    #customer demand deduction from inventory and shortage
    for (i in 1:length(v$inventory)){
      v$inventory[i] <- v$inventory[i] - v$current_cust[i]
      if (v$inventory[i] < 0) {v$shortage <- v$shortage - v$inventory[i]}
      if (v$inventory[i] < 0) {temp$turnshort <- temp$turnshort - v$inventory[i]}
      if (v$inventory[i] < 0) {v$inventory[i] <- 0}
    }
    
    print(temp$turnshort)
    
    #get money for each good sold
    v$cash <- v$cash + (sum(v$current_cust)-temp$turnshort)*v$sale_price
    
    #customer satisfaction decreases when their demand doesnt get satisfied, but increases by 5 when their demand gets satisfied
    if ((temp$turnshort == 0) && (v$turn > 0)) {v$satisfaction <- v$satisfaction + 5}
    v$satisfaction <- v$satisfaction - temp$turnshort
    if (v$satisfaction > 100) {v$satisfaction <- 100}
    if (v$satisfaction < 0) {v$satisfaction <- 0}
    
    temp$turnshort <- 0
    
    #add satisfaction to total satisfaction
    v$total_satisfaction <- v$total_satisfaction + v$satisfaction
    
    #turn count increase: end of turn
    v$turn <- v$turn+1
    
    #setscores when turn count over a certain number
    if(v$turn==v$lastturn){
      v$cashscore <- v$cash
      v$satisfactionscore <- v$total_satisfaction/v$lastturn
      print(v$cashscore)
      print(v$satisfactionscore)
    }
    
    #next customer becomes the current customer
    v$current_cust <- v$next_cust
    
    #generate next_cust list
    v$next_cust <- generatecust()
    
    #trigger if game mode is dynamic
    if(mode$dynamic == 1){
      v$next_cust <- generatecust(v$satisfaction)}
    
    #staff counter -1
    v$staff1[1] <- v$staff1[1]-1
    v$staff2[1] <- v$staff2[1]-1
    v$staff3[1] <- v$staff3[1]-1
    
    #if staff counter = 0, they will refill the food with their quantity and set their status to c(0,0,0)
    if (v$staff1[1] == 0) {
      v$inventory[v$staff1[2]] <- v$inventory[v$staff1[2]] + v$staff1[3]
      v$staff1 <- c(0,0,0)
    }
    
    if (v$staff2[1] == 0) {
      v$inventory[v$staff2[2]] <- v$inventory[v$staff2[2]] + v$staff2[3]
      v$staff2 <- c(0,0,0)
    }
    
    if (v$staff3[1] == 0) {
      v$inventory[v$staff3[2]] <- v$inventory[v$staff3[2]] + v$staff3[3]
      v$staff3 <- c(0,0,0)
    }
    
    #check for inventory overflow. If overflow, set to capacity
    for (i in 1:length(v$inventory)){
      if (v$inventory[i] > v$inventorycap[i]) {
        v$wastage <- v$wastage + (v$inventory[i]-v$inventorycap[i])
      }
    }
    
    for (i in 1:length(v$inventory)){
      if (v$inventory[i] > v$inventorycap[i]) {
        v$inventory[i] <- v$inventorycap[i]
      }
    }
    
    if (input$task1 != "Do nothing"){
      temp$show <- 0
      v$flag <- TRUE
      if (v$flag)v$img <- "graphic 2.gif"
    }
    
    if (input$task2 != "Do nothing"){
      temp$show <- 0
      v$flag <- TRUE
      if (v$flag)v$img <- "graphic 2.gif"
    }
    
    if (input$task3 != "Do nothing"){
      temp$show <- 0
      v$flag <- TRUE
      if (v$flag)v$img <- "graphic 2.gif"
    }
    
    
    
    delay(1950, temp$show <- 1)
    delay(1950, v$flag <- FALSE)
    delay(1950, v$img <- sample(c("graphic.jpg","graphic.jpg"), 1))
    
    if(v$turn==v$lastturn){
      showModal(modalDialog(
        h3("You have reached the last turn. Your score can now be published. However, you can still play on!")
      ))
    }
    
    
  })
  
  
  
  #ouput game mode UI
  output$gamemode <- renderUI({
    if(v$turn == 0){
      tagList(
        radioButtons("gamemode", label = "", choices = c("Normal", "Dynamic"), selected = "Normal")
      )
    } else {h2(input$gamemode)}
  })
  
  #change game mode
  observeEvent(input$gamemode, {
    if(input$gamemode == "Normal") {mode$dynamic = 0} else {mode$dynamic = 1}
  })

  
  #output capacity UIs
  output$chickencap <- renderUI({
    if(v$turn == 0){
      tagList(
        numericInput("chickencap",label="", 5, min = 5, step = 1)
      )}
    else{
      h2(v$inventorycap[1])
    }
  })
  output$steakcap <- renderUI({
    if(v$turn == 0){
      tagList(
        numericInput("steakcap",label="", 5, min = 5, step = 1)
      )}
    else{
      h2(v$inventorycap[2])
    }
  })
  output$pizzacap <- renderUI({
    if(v$turn == 0){
      tagList(
        numericInput("pizzacap",label="", 5, min = 5, step = 1)
      )}
    else{
      h2(v$inventorycap[3])
    }
  })
  output$sushicap <- renderUI({
    if(v$turn == 0){
      tagList(
        numericInput("sushicap",label="", 5, min = 5, step = 1)
      )}
    else{
      h2(v$inventorycap[4])
    }
  })
  output$saladcap <- renderUI({
    if(v$turn == 0){
      tagList(
        numericInput("saladcap",label="", 5, min = 5, step = 1)
      )}
    else{
      h2(v$inventorycap[5])
    }
  })
  
  observeEvent(input$quantity1,{
    temp$order1<-0
    if(input$task1 != "Do nothing"){
      temp$order1 <- input$quantity1
    }
    temp$cost<-(temp$order1+temp$order2+temp$order3)*3
  })
  observeEvent(input$quantity2,{
    temp$order2<-0
    if(input$task2 != "Do nothing"){
      temp$order2 <- input$quantity2
    }
    temp$cost<-(temp$order1+temp$order2+temp$order3)*3
  })
  observeEvent(input$quantity3,{
    temp$order3<-0
    if(input$task3 != "Do nothing"){
      temp$order3 <- input$quantity3
    }
    temp$cost<-(temp$order1+temp$order2+temp$order3)*3
  })
  
  observeEvent(input$task1,{
    temp$order1<-0
    if(input$task1 != "Do nothing"){
      temp$order1 <- input$quantity1
    }
    temp$cost<-(temp$order1+temp$order2+temp$order3)*3
  })
  observeEvent(input$task2,{
    temp$order2<-0
    if(input$task2 != "Do nothing"){
      temp$order2 <- input$quantity2
    }
    temp$cost<-(temp$order1+temp$order2+temp$order3)*3
  })
  observeEvent(input$task3,{
    temp$order3<-0
    if(input$task3 != "Do nothing"){
      temp$order3 <- input$quantity3
    }
    temp$cost<-(temp$order1+temp$order2+temp$order3)*3
  })
  
  
  #update starting cash
  observeEvent(input$chickencap,{
    v$total_cap <- input$chickencap + input$steakcap + input$pizzacap + input$sushicap + input$saladcap
    v$starting_cash <- 2750-30*v$total_cap
  })
  observeEvent(input$steakcap,{
    v$total_cap <- input$chickencap + input$steakcap + input$pizzacap + input$sushicap + input$saladcap
    v$starting_cash <- 2750-30*v$total_cap
  })
  observeEvent(input$pizzacap,{
    v$total_cap <- input$chickencap + input$steakcap + input$pizzacap + input$sushicap + input$saladcap
    v$starting_cash <- 2750-30*v$total_cap
  })
  observeEvent(input$sushicap,{
    v$total_cap <- input$chickencap + input$steakcap + input$pizzacap + input$sushicap + input$saladcap
    v$starting_cash <- 2750-30*v$total_cap
  })
  observeEvent(input$saladcap,{
    v$total_cap <- input$chickencap + input$steakcap + input$pizzacap + input$sushicap + input$saladcap
    v$starting_cash <- 2750-30*v$total_cap
  })
  
  output$starting_cash <- renderValueBox({ 
    valueBox(v$starting_cash,h5("Cash"),icon=icon("wallet"),
             color = "light-blue")
  })
  
  observeEvent(input$Setcap, {
    if((v$starting_cash >= 0)&&(v$turn == 0)){
      v$inventorycap[1] <- input$chickencap
      v$inventorycap[2] <- input$steakcap
      v$inventorycap[3] <- input$pizzacap
      v$inventorycap[4] <- input$sushicap
      v$inventorycap[5] <- input$saladcap
      v$cash <- v$starting_cash
    }
    
  })
  
  output$Invalid <- renderText({
    if(v$starting_cash < 0){
      paste("You cannot have less than 0 dollars")
    }
  })
  
  
  
  #Output for main game
  
  #output the graphics
  output$graphic <- renderUI({
    tags$div(
      tags$img(id = "anim", src = v$img, width = 400)
    )
  })
  
  output$turncost <- renderText({
    paste(h5("Cost this turn:"), h5(temp$cost))
  })
  
  #statuses on turncount and current inventory, shortage, wastage so far
  output$turncount <- renderValueBox({ 
    valueBox(v$turn,h5("Turns"),icon=icon("list"),
            color = "light-blue")
  })
  
  output$inventory <- renderText({ 
    paste("Chicken:", v$inventory[1],"/",v$inventorycap[1], 
          "Steak:", v$inventory[2],"/",v$inventorycap[2],
          "Pizza:", v$inventory[3],"/",v$inventorycap[3],
          "Sushi:", v$inventory[4],"/",v$inventorycap[4],
          "Salad:", v$inventory[5],"/",v$inventorycap[5])
  })
  
  output$shortage <- renderValueBox({ 
    valueBox(v$shortage,h5("Shortage"),icon=icon("list"),
             color = "orange")
  })
  
  output$wastage <- renderValueBox({ 
    valueBox(v$wastage,h5("Wastage"),icon=icon("list"),
             color = "red")
  })
  
  #shows product lead times
  output$lead_time <- renderText({ 
    paste("Chicken:", v$lead_time[1], "Steak:", v$lead_time[2],"Pizza:", v$lead_time[3],"Sushi:", v$lead_time[4],"Salad:", v$lead_time[5])
  })

  #Show registered username
  output$username <- renderText({
    paste("username:", v$username)
  })
  
  #UI output when staff are available
  output$task1 <- renderUI({
    if(v$staff1[1] < 0){
      tagList(
        selectInput("task1",label="Type of food",choices=c("Do nothing","Chicken","Steak", "Pizza", "Sushi", "Salad")),
        sliderInput("quantity1",label="Quantity", min=5,max=25,value=5,step=5)
        )
    } else {
      if(v$staff1[1]==0){
        h4("Cooling down... please wait...")
      } else {
        tagList(
          h6(tags$b("Turns left before food is done")),
          h6(htmlOutput("staff_1_availability")),
          h6(tags$b("Type of food")),
          h6(htmlOutput("staff_1_food")),
          h6(tags$b("Quantity")),
          h6(htmlOutput("staff_1_qty"))
      )}
    }
  })
  
  output$task2 <- renderUI({
    if(v$staff2[1] < 0){
      tagList(
        selectInput("task2",label="Type of food",choices=c("Do nothing","Chicken","Steak", "Pizza", "Sushi", "Salad")),
        sliderInput("quantity2",label="Quantity", min=5,max=25,value=5,step=5)
      )
    } else {
      if(v$staff2[1]==0){
        h4("Cooling down... please wait...")
      } else {
        tagList(
          h6(tags$b("Turns left before food is done")),
          h6(htmlOutput("staff_2_availability")),
          h6(tags$b("Type of food")),
          h6(htmlOutput("staff_2_food")),
          h6(tags$b("Quantity")),
          h6(htmlOutput("staff_2_qty"))
        )}
    }
  })
  
  output$task3 <- renderUI({
    if(v$staff3[1] < 0){
      tagList(
        selectInput("task3",label="Type of food",choices=c("Do nothing","Chicken","Steak", "Pizza", "Sushi", "Salad")),
        sliderInput("quantity3",label="Quantity", min=5,max=25,value=5,step=5)
      )
    } else {
      if(v$staff3[1]==0){
        h4("Cooling down... please wait...")
      } else {
        tagList(
          h6(tags$b("Turns left before food is done")),
          h6(htmlOutput("staff_3_availability")),
          h6(tags$b("Type of food")),
          h6(htmlOutput("staff_3_food")),
          h6(tags$b("Quantity")),
          h6(htmlOutput("staff_3_qty"))
        )}
    }
  })
  
  #shows what staff(s) are doing
  #staff1
  output$staff_1_availability <- renderText({ 
    paste(v$staff1[1])
  })
  output$staff_1_food <- renderText({
    if(v$staff1[2] == 1) {paste("Chicken")} else {
      if(v$staff1[2] == 2) {paste("Steak")} else {
        if(v$staff1[2] == 3) {paste("Pizza")} else {
          if(v$staff1[2] == 4) {paste("Sushi")} else {
            if(v$staff1[2] == 5) {paste("Salad")}
          }
        }
      }
    }
  })
  output$staff_1_qty <- renderText({
    paste(v$staff1[3])
  })
  
  #staff2
  output$staff_2_availability <- renderText({ 
    paste(v$staff2[1])
  })
  output$staff_2_food <- renderText({
    if(v$staff2[2] == 1) {paste("Chicken")} else {
      if(v$staff2[2] == 2) {paste("Steak")} else {
        if(v$staff2[2] == 3) {paste("Pizza")} else {
          if(v$staff2[2] == 4) {paste("Sushi")} else {
            if(v$staff2[2] == 5) {paste("Salad")}
          }
        }
      }
    }
  })
  output$staff_2_qty <- renderText({
    paste(v$staff2[3])
  })
  
  #staff3
  output$staff_3_availability <- renderText({ 
    paste(v$staff3[1])
  })
  output$staff_3_food <- renderText({
    if(v$staff3[2] == 1) {paste("Chicken")} else {
      if(v$staff3[2] == 2) {paste("Steak")} else {
        if(v$staff3[2] == 3) {paste("Pizza")} else {
          if(v$staff3[2] == 4) {paste("Sushi")} else {
            if(v$staff3[2] == 5) {paste("Salad")}
          }
        }
      }
    }
  })
  output$staff_3_qty <- renderText({
    paste(v$staff3[3])
  })
  
  #shows cash on hand
  output$cash <- renderValueBox({ 
    valueBox(v$cash,h5("Cash"),icon=icon("wallet"),
             color = "light-blue")
  })
  
  #shows customer satisfaction
  output$satisfaction <- renderValueBox({ 
    valueBox(v$satisfaction,h5("Customer Satisfaction"),icon=icon("award"),
             color = "blue")
  })
  
  #shows current customer and next customer
  output$current_customer <- renderText({ 
    paste("Customer order:", v$current_cust[1], v$current_cust[2], v$current_cust[3], v$current_cust[4], v$current_cust[5])
  })
  
  output$next_customer <- renderText({ 
    paste("Next Customer order:", v$next_cust[1], v$next_cust[2],v$next_cust[3],v$next_cust[4],v$next_cust[5])
  })
  
  #render plot for inventory
  output$inventoryplot <- renderPlot({
    getInventoryPlot(v$inventory, v$inventorycap)
  }, height = 300, width = 400)

  
  #Show publishscore button once end of game (round 50)
  output$publishscore <- renderUI({
    if((v$turn >= v$lastturn) && (v$published == 0))
      tagList(
        actionButton("register", "Publish Your Score")
      )
  })
  
  
  
  
  ######leaderboard and registering
  output$register <- renderUI({
    if(length(v$username)==0)
      tagList(
        h5(actionButton("register", "Register", style="color: #FFF; background-color: #367D86; border-color: #0A0A0A"))
      )
  })
  
  output$showscore <- renderText({
    if(v$turn >= v$lastturn){
      paste("Cash:", v$cashscore, "Satisfaction:", v$satisfactionscore)
    }
  })
  
  
  
  output$leaderboard <- renderTable({
    numclicks <- input$passwordok
    leaderboard <- getLeaderBoard()
    leaderboard
    }, 
    align = 'c')
  
  #Fire some code if the user clicks the Register button
  observeEvent(input$register, {
    showModal(RegisterModal())
  })
  
  # Fire some code if the user clicks the passwordok button
  observeEvent(input$passwordok, {
    v$username <- input$username
    publishScore(v$username,v$cashscore,v$satisfactionscore)
    v$published <- 1
    removeModal()
    
  })
  
  
  
  
  
}


shinyApp(ui, server)
