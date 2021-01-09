library(shinydashboard)
library(shinyjs)
library(datasets)
library(ggplot2)
library(data.table)
library(DT)
library(viridis)
library(tidyr)
library(tidyverse)
library(grid)
library(reshape2)
library(cluster)
library(scales)
library(plotly)
library(rlist)
library(waiter)
library(rhandsontable)
library(odbc)
library(DBI)
library(shinycssloaders)


v_showseasonproject<-fread("v_showseasonproject.csv")

### BEGIN Uncomment to run locally on RStudio Server
# load("~/.smithee.RData")
# con <- pool::dbPool(drv = odbc::odbc(),
#                                dsn = "snowflake",
#                                uid = "SVC_DS_EZBUDGET",
#                                pwd = easy_service)
# rm("easy_service")
### END Uncomment to run locally on RStudio Server

### BEGIN Uncomment to publish app to RStudio Connect
con <- pool::dbPool(drv = odbc::odbc(),
                               dsn = "snowflake",
                               uid = Sys.getenv("easy_service_uid"),
                               pwd = Sys.getenv("easy_service_pwd"))
### END Uncomment to publish app to RStudio Connect


showproject<-dbGetQuery(con,"Select * From cidw_ezbudget_publish.ezbudget_views.v_TVShowProjectTree Where IsFakeGL = 0",na.strings = "NULL")
setDT(showproject)
getAmort<-function(title,season){
  Amort<-dbGetQuery(con, 
           paste0(
             "Select c.*,g.*
              From
               (
               Select a.BudgetId, a.Amount,b.AccountCode,b.Description as AccountName,b.GroupDescription as GroupName,b.SummaryName as RoleName
               From
               (
                 Select AccountId, sum(Amount) as Amount,BudgetId
                 From cidw_ezbudget_publish.ezbudget_views.v_BudgetDetail
                 GROUP BY AccountId,BudgetId
               ) a
               JOIN
               cidw_ezbudget_publish.ezbudget_views.v_rpt_coasummary b
               On a.AccountId = b.AccountId
             ) c
             Join
             (
               Select e.budgetid, e.budgettypedomainid, e.title as AmortType, f.SeasonDescription,f.Title
               From cidw_ezbudget_publish.ezbudget_views.v_Budget e
               Join cidw_ezbudget_publish.ezbudget_views.v_TVShowProjectTree f
               On e.ProjectId=f.ProjectId
               WHERE e.budgetnumber like '%B000%'
             ) g
             On c.BudgetId = g.BudgetId
             WHERE g.budgettypedomainid = 162",
              "And g.Title like '",  title, "'",
              "And g.seasonDescription like '", season, "'"
             )
           )
  setDT(Amort)
  return(Amort)
}
getSeries<-function(title,season){
  Series<-dbGetQuery(con, 
                    paste0(
                      "Select c.*,g.*
                       From
                       (
                       Select a.BudgetId, a.Amount,b.AccountCode,b.Description as AccountName,b.GroupDescription as GroupName,b.SummaryName as RoleName
                       From
                       (
                         Select AccountId, sum(Amount) as Amount,BudgetId
                         From cidw_ezbudget_publish.ezbudget_views.v_BudgetDetail
                         GROUP BY AccountId,BudgetId
                       ) a
                       JOIN
                       cidw_ezbudget_publish.ezbudget_views.v_rpt_coasummary b
                       On a.AccountId = b.AccountId
                     ) c
                     Join
                     (
                       Select e.budgetid, e.budgettypedomainid, f.SeasonDescription,f.Title
                       From cidw_ezbudget_publish.ezbudget_views.v_Budget e
                       Join cidw_ezbudget_publish.ezbudget_views.v_TVShowProjectTree f
                       On e.ProjectId=f.ProjectId
                       WHERE e.budgetnumber like '%B000%'
                     ) g
                     On c.BudgetId = g.BudgetId
                     WHERE g.budgettypedomainid = 196",
                              "And g.Title like '",  title, "'",
                              "And g.seasonDescription like '", season, "'"
                            )
  )
  setDT(Series)
  return(Series)
}

onStop(function() {
  cat("Doing application cleanup\n")
  pool::poolClose(jdbcConnection)
})