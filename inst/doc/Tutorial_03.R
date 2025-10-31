## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
library(ActiGlobe)

## ----Load Data, message=FALSE, warning=FALSE, include = FALSE-----------------
data("FlyEast")


## ----BriefSum, message=FALSE, warning=FALSE, include = FALSE------------------
BdfList = 
BriefSum(df = FlyEast,
         SR = 1/60,
         Start = "2017-10-24 13:45:00")



## ----message=FALSE, warning=FALSE, include = FALSE----------------------------
Bdf <- BdfList$Bdf

## ----View df, message=FALSE, warning=FALSE, include = FALSE-------------------
df <- BdfList$df

## ----View TLog, message=FALSE, warning=FALSE, include=FALSE-------------------
data(TLog)


## ----TAdjust, message=FALSE, warning=FALSE, include = FALSE-------------------
Bdf.adj = TAdjust(Bdf, TLog)

## ----include=FALSE------------------------------------------------------------
dfList = Act2Daily(df = df,
          Bdf = Bdf.adj,
          VAct = "Activity",
          VTm = "Time",
          Incomplete = FALSE,
          Travel = TRUE)

## ----example graph, echo=FALSE, message=FALSE, warning=FALSE, fig.height=3.5, fig.width=7----
df2 <- do.call(rbind, dfList$Daily_df)

ggActiGlobe(df = df2, 
               Bdf = Bdf.adj,
               VAct = "Activity",
               VDT = "DateTime")

## ----Act2Daily, echo = TRUE, eval=FALSE---------------------------------------
# dfList = Act2Daily(df = df,
#           Bdf = Bdf.adj,
#           VAct = "Activity",
#           VTm = "Time",
#           Incomplete = TRUE,
#           Travel = TRUE)

## ----Automated Split Data, message=FALSE, warning=FALSE, eval=FALSE, fig.height=5, fig.width=7----
# ### Here, we selectively generate graphs at random for day 11 and day 12.
# for(i in names(dfList$Daily_df)[11:12]) {
# 
#   plot(dfList$Daily_df[[i]]$Activity, main = i)
# 
# }

## ----write.act, echo = TRUE, eval=FALSE---------------------------------------
# dfList = write.act(Dir = "The_PATH_in_the_computer_WHERE_the_daily_recordings_will_be_stored",
#                    ID = "JD",
#                    df = df,
#                    Bdf = Bdf.adj,
#                    VAct = "Activity",
#                    VTm = "Time")

## ----write.cosinor, echo = TRUE, eval=FALSE-----------------------------------
# dfList = write.cosinor(Dir = "The_PATH_in_the_computer_WHERE_the_daily_recordings_will_be_stored",
#                        ID = "JD",
#                        DailyAct = dfList$Daily_df,
#                        Bdf = Bdf.adj,
#                        VAct = "Activity",
#                        VTm = "Time")

