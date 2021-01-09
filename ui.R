dashboardPage(
  dashboardHeader(
    title = tags$span(tags$img(src='img/Warner_Bros_Television_2019.png', height="90%"), "TV Show Production"),
    titleWidth = 300
  ),
  dashboardSidebar(
    sidebarMenu(id = "tab",
                menuItem("Amorts", tabName = "amorts"),
                menuItem("Budget Breakdown", tabName = "budget_breakdown"),
                menuItem("Cashflow Forecast", tabName = "cashflow_forecast")
    ),
    conditionalPanel(condition = "input.tab=='amorts'",
      selectInput("tvshow", "TV show:", choices =c(unique(showproject[order(TITLE)]$TITLE))),
      uiOutput("season"),
      numericInput("episode","Number of Episode:",10)
    ),
    conditionalPanel(condition = "input.tab=='budget_breakdown'",
      selectInput("account","Bank Account:",c("RBC","BOA","SAP")),
      selectInput("currency","Currency:",c("USD","CAN")),
      actionButton("add","Add")
    ),
    conditionalPanel(condition = "input.tab=='budget_breakdown'",
                     dateInput("start","Start Date:"),
                     dateInput("end","End Date:")
    )
  ),
  dashboardBody(
    tags$head(
      tags$script(src="js/dataTables.cellEdit.js")
    ),
    tabItems(
      tabItem(tabName = "amorts",
        fluidRow(
          tabBox(id = "cost_summary_tabset1",
            width = 12,            
            tabPanel("Amorts",
              column(1,offset=10,actionButton("nextstep_1","Next Step")),
              DT::dataTableOutput("amort_table")
            )
          )
        )
      ),
      tabItem(tabName = "budget_breakdown",
        fluidRow(
          tabBox(id = "dataset",
            width = 12,
            tabPanel("Budget Breakdown",
              column(1,offset=0,actionButton("laststep_2","Last Step")),
              column(1,offset=9,actionButton("nextstep_2","Next Step")),
              DT::dataTableOutput("breakdown_table")
            )
          )
        )
      ),
      tabItem(tabName = "cashflow_forecast",
              div(
                tabsetPanel(id = "mainTabset_sum", 
                            tabPanel("Filming Schedule",
                                     column(1,offset=0,actionButton("laststep_3","Last Step")),
                                     DTOutput("schedule")                          
                                     )
                            ),class = "span7"
                )
              )
      )
    )
  )

