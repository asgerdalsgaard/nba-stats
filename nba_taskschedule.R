#Taskscheduler
library(taskscheduleR)

taskscheduler_create(
  taskname = "nba_taskschedule",
  rscript = "./nba_webscraping.R",
  schedule = "MONTHLY", #minute, daily, once, weekly, monthly
  starttime = format(Sys.time() + 60, "%H:%M"), #formater Sys.time og Sys.Date så de læses som POSIXct 
  startdate = format(Sys.Date(),"%Y-%m-%d")
)

taskscheduler_delete(taskname = "nba_taskschedule")
