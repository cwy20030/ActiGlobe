## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ActiGlobe)

### Optional Library
library(zeallot)

## ----Load Data, message=FALSE, warning=FALSE----------------------------------
data("FlyEast")

head(FlyEast)   ### Only the first few lines

## ----BriefSum, message=FALSE, warning=FALSE-----------------------------------
BdfList = 
BriefSum(df = FlyEast,
         SR = 1/60,
         Start = "2017-10-24 13:45:00")


str(BdfList,max.level = 1) ### An overview of the output structure from BriefSum()

## ----eval=FALSE---------------------------------------------------------------
# c(Bdf, df) %<-%
# BriefSum(df = FlyEast,
#          SR = 1/60,
#          Start = "2017-10-24 13:45:00")

## ----message=FALSE, warning=FALSE---------------------------------------------
Bdf <- BdfList$Bdf
head(Bdf)

## ----View df, message=FALSE, warning=FALSE------------------------------------
df <- BdfList$df

head(df) ### This should give us the same first few lines of FlyEast dataset with a few new columns created by [BrifSum()].

## ----View TLog, message=FALSE, warning=FALSE----------------------------------
data(TLog)

head(TLog)

## ----IANA header, echo=FALSE, message=FALSE, warning=FALSE, paged.print=TRUE----
head(IANA)

## ----TAdjust------------------------------------------------------------------
Bdf.adj = TAdjust(Bdf, TLog)

head(Bdf.adj)

## ----include=FALSE, eval=FALSE------------------------------------------------
# dk <- Bdf.adj[c("Date","Epoch","UTC","TZ_code","Daylight_Saving","Recording_Start","Recording_End","GL_Offset","nDataPoints","Cumulative_Start_Second","Cumulative_End_Second")]
# dk$Cumulative_End_Second2 <- Bdf$Cumulative_End_Second
# dk$Cumulative_Start_Second2 <- Bdf$Cumulative_Start_Second
# 

## ----message=FALSE, warning=FALSE, paged.print=TRUE, out.width="30%"----------
knitr::kable(Bdf[10:15,]) ### Only display 6 days 

knitr::kable(Bdf.adj[10:15,]) ### Only display 6 days 

## ----Original_Data, message=FALSE, warning=FALSE, fig.height=3, fig.width=8----
ggActiGlobe(df = df, 
            Bdf = Bdf,
            VAct = "Activity",
            VDT = "DateTime")

## ----Pre-processed_Data, message=FALSE, warning=FALSE, fig.height=3, fig.width=8----
### Reconstruct the longitudinal recording with proper segmentation
dfList = Act2Daily(df = df,
          Bdf = Bdf.adj,
          VAct = "Activity",
          VTm = "Time",
          Incomplete = TRUE,
          Travel = TRUE)

df2 <- do.call(rbind, dfList$Daily_df)

ggActiGlobe(df = df2, 
               Bdf = Bdf.adj,
               VAct = "Activity",
               VDT = "DateTime")

## ----Original Daily, eval=FALSE, fig.height=5, fig.width=7--------------------
# 
# for(i in 1:length(x)) {
# x <- Bdf$Cumulative_Start_Second
# y <- Bdf$Cumulative_End_Second
# GX <- df$Activity[(x[i]:y[i])/60]
# print(plot(GX,main = i, font.lab = 2,ylab = "Activity (counts)"))
# 
# }
# 

## ----Adjusted Daily, eval=FALSE, fig.height=5, fig.width=7--------------------
# 
# for(i in 1:length(x)) {
# x <- Bdf.adj$Cumulative_Start_Second
# y <- Bdf.adj$Cumulative_End_Second
# GX <- df$Activity[(x[i]:y[i])/60]
# print(plot(GX,main = i, font.lab = 2,ylab = "Activity (counts)"))
# 
# }

## ----Automated Split Data, eval=FALSE, fig.height=5, fig.width=7, message=FALSE, warning=FALSE----
# for(i in names(dfList$Daily_df)) {
# 
#   plot(dfList$Daily_df[[i]]$Activity, main = i, font.lab = 2,ylab = "Activity (counts)")
# 
# }

