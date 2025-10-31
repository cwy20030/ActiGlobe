## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
library(ActiGlobe)

## ----Import 20171027, message=FALSE, warning=FALSE, results='asis'------------
# Import data
data("FlyEast")

 BdfList =
 BriefSum(df = FlyEast,
          SR = 1/60,
         Start = "2017-10-24 13:45:00")


# Let's extract actigraphy data from a single day
 df <- BdfList$df
 df <- subset(df, df$Date == "2017-10-27")

## ----Manual kdf,message=FALSE, warning=FALSE, eval=FALSE, include=FALSE, results='asis'----
# library(circular)
# library(ggplot2)
# 
# # data: df with columns time_hour (0-24) and activity (nonnegative)
# # convert hours to radians
# df$theta <- C2T(df$Time) / 24 * 2 * pi
# 
# # simple weighted kernel density on a linear grid with wrapping
# wrap_kde <- function(theta, w, bw = 0.3, grid = 200) {
#   grid_theta <- seq(0, 2*pi, length.out = grid)
#   dens <- sapply(grid_theta, function(tg) {
#     # wrapped gaussian kernel sum with weights
#     sum(w * dnorm(circular::conversion.circular(tg - theta), mean = 0, sd = bw))
#   })
#   data.frame(theta = grid_theta, density = dens)
# }
# 
# kdf <- wrap_kde(df$theta, df$Activity, bw = 0.4)
# kdf$hour <- kdf$theta * 24 / (2 * pi)
# 
# ggplot(kdf, aes(x = hour, y = density)) +
#   geom_line(size = 1) +
#   scale_x_continuous(breaks = seq(0,24,by=3), limits = c(0,24)) +
#   labs(x = "Clock hour", y = "Weighted density (activity × frequency)") +
#   theme_minimal()
# 

## ----Manual plot kdf, message=FALSE, warning=FALSE, eval=FALSE, include=FALSE, results='asis'----
# # assume kdf has columns theta in [0,2*pi] and density
# # numerical integrals via trapezoid rule on an ordered grid
# theta <- kdf$theta
# f <- kdf$density
# n <- length(theta)
# 
# # ensure theta sorted and grid spacing consistent
# ord <- order(theta)
# theta <- theta[ord]
# f <- f[ord]
# # wrap endpoint so integration over [0,2pi]
# theta_ext <- c(theta, theta[1] + 2*pi)
# f_ext <- c(f, f[1])
# 
# # trapezoid integration helper
# trap_int <- function(x, y) sum( (y[-1] + y[-length(y)]) * diff(x) ) / 2
# 
# I0 <- trap_int(theta_ext, f_ext)               # integral of f over 0..2pi
# Icos <- trap_int(theta_ext, f_ext * cos(theta_ext))
# Isin <- trap_int(theta_ext, f_ext * sin(theta_ext))
# 
# MESOR <- I0 / (2 * pi)
# a1 <- Icos / pi
# b1 <- Isin / pi
# Amplitude <- sqrt(a1^2 + b1^2)
# Acrophase <- (atan2(-b1, a1) %% (2*pi))
# Acrophase_hour <- Acrophase * 24 / (2*pi)
# 
# list(MESOR = MESOR,
#      a1 = a1,
#      b1 = b1,
#      Amplitude = Amplitude,
#      Acrophase_rad = Acrophase,
#      Acrophase_hour = Acrophase_hour)
# 

## ----Model 1 OLS, message=FALSE, warning=FALSE, fig.height=5, fig.width=7-----
fit.ols <- 
CosinorM(time = df$Time,
activity = df$Activity,
tau = 24,
method = "OLS")

### Look at the parameters
fit.ols$coef.cosinor

### Plot 
ggCosinorM(fit.ols)

## ----eval=FALSE, include=FALSE------------------------------------------------
# ### A Single Plot
# plot(fit.ols$fitted.values, ylim = c(0,2000), main = "Single-phase OLS Cosinor Model",ylab = "Activity (counts)", cex = 0.6, font.lab = 2, col = "#1b9e77")
# abline(v = 60 * (24 - (fit.ols$coef.cosinor[3] %% (2 * pi))*24 / (2*pi) ), lwd = 4) ### Plot the vertical line to identify the peak of the activity.
# abline(v = which.max(fit.ols$fitted.values), col = "orange", lty = "dotted", lwd = 4) ### Plot the vertical line to identify the TRUE peak of the activity.
# points(df$Activity, pch = 19, cex = 0.6, col = "#7570b3")

## ----Model 1 FGLS, message=FALSE, warning=FALSE, fig.height=5, fig.width=7----
fit.fgls <- 
CosinorM(time = df$Time,
activity = df$Activity,
tau = 24,
method = "FGLS")

### Look at the parameters
fit.fgls$coef.cosinor

### Plot 
ggCosinorM(fit.fgls)

## ----Model 2 OLS, message=FALSE, warning=FALSE, fig.height=5, fig.width=7-----
fit.ols2 <- 
CosinorM(time = df$Time,
activity = df$Activity,
tau = c(12,24),
method = "OLS")




Coefs <- fit.ols2$coef.cosinor
Coefs[grepl("Acrophase|12",names(Coefs))]


### Plot 
ggCosinorM(fit.ols2)


## ----eval=FALSE, include=FALSE------------------------------------------------
# 
# plot(fit.ols2$fitted.values, ylim = c(0,2000), main = "Dual-phase OLS Cosinor Model",ylab = "Activity (counts)", cex = 0.6, font.lab = 2, col = "#1b9e77")
# abline(v =   60 * (24 - (fit.ols2$coef.cosinor[4] %% (2 * pi))*24 / (2*pi) ), lwd = 4, col = "black") ### Plot the vertical line to identify the peak of the activity.
# abline(v =   60 * (12 - (fit.ols2$coef.cosinor[5] %% (2 * pi))*12 / (2*pi) ), lwd = 4, col = "brown") ### Plot the vertical line to identify the peak of the activity.
# abline(v = which.max(fit.ols2$fitted.values), col = "orange", lty = "dotted", lwd = 4) ### Plot the vertical line to identify the TRUE peak of the activity.
# points(df$Activity, pch = 19, cex = 0.6, col = "#7570b3")

## ----Model 2 FGLS, message=FALSE, warning=FALSE, fig.height=5, fig.width=7----

fit.fgls2 <- 
CosinorM(time = df$Time,
activity = df$Activity,
tau = c(12,24),
method = "FGLS")


fit.fgls2$coef.cosinor


### Plot 
ggCosinorM(fit.fgls2)

## ----Model 1 KDE, message=FALSE, warning=FALSE--------------------------------
fit.KDE <- 
  CosinorM.KDE(time = df$Time,
             activity = df$Activity)

### Look at the parameters
fit.KDE$coef.cosinor


## ----import ggplot------------------------------------------------------------
### For external graphic plot. 
library(ggplot2)

## ----Model 1 OLS vs KDE, message=FALSE, warning=FALSE, fig.height=5, fig.width=7----

# Prepare data
n <- length(fit.ols$fitted.values)
plot_df <- data.frame(
  hour = seq(0, 24, length.out = n),
  fitted = as.numeric(fit.ols$fitted.values),
  activity = df$Activity
)

# Cosinor and true-peak hours
phi <- fit.ols$coef.cosinor[3]
cosinor_peak_hour <- (24 - (phi %% (2 * pi)) * 24 / (2 * pi)) %% 24
true_peak_hour <- plot_df$hour[which.max(df$Activity)]

# Kernel density (assumes kdf$hour in hours)
kdf_plot <- data.frame(hour = fit.KDE$kdf$hour, density = fit.KDE$kdf$density)
y_primary_max <- 3000
kdf_plot$density_scaled <- kdf_plot$density / max(kdf_plot$density) * y_primary_max

# KDE vline location
kde_vline_hour <- fit.KDE$coef.cosinor["Acrophase.hr"]

# Named colours for mapping
my_cols <- c(
  "Raw Activity"     = "#7570b3",
  "Cosinor (24Hr)"   = "#1b9e77",
  "KDE"   = "#999999",
  "Cosinor Peak"     = "black",
  "Maximum Activity" = "#e7298a",
  "KDE Peak"         = "#e41a1c"
)

ggplot() +
  # Lines/points with color mapped to a fixed name so they appear in legend
  geom_line(data = plot_df, aes(x = hour, y = fitted, color = "Cosinor (24Hr)"), size = 0.9) +
  geom_point(data = plot_df, aes(x = hour, y = activity, color = "Raw Activity"), size = 1.0, alpha = 0.6) +
  geom_line(data = kdf_plot, aes(x = hour, y = density_scaled, color = "KDE"), size = 1) +
  # vertical lines mapped to colour names so they appear in the legend
  geom_vline(aes(xintercept = cosinor_peak_hour, color = "Cosinor Peak"), linetype = "solid", size = 1) +
  geom_vline(aes(xintercept = true_peak_hour, color = "Maximum Activity"), linetype = "dotted", size = 1) +
  geom_vline(aes(xintercept = kde_vline_hour, color = "KDE Peak"), linetype = "longdash", size = 1) +
  # scales and labels
  scale_x_continuous(
    name = "Hour",
    limits = c(0, 24),
    breaks = c(0, 4, 8, 12, 16, 20, 24),
    labels = c(0, 4, 8, 12, 16, 20, 24)
  ) +
  scale_y_continuous(
    name = "Activity",
    limits = c(0, y_primary_max),
    sec.axis = sec_axis(~ . / y_primary_max * max(kdf_plot$density), name = "Circularized Gaussian Kernel Density")
  ) +
  scale_color_manual(name = NULL, values = my_cols) +
 ggtitle("Overlay of Fitting of Cosinor \n and Circularized Gaussian Kernel Density Estimation") +

  theme_minimal(base_size = 12) +
  theme(
    text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    axis.ticks = element_line(size = 0.4)
  )




## ----Model 1 FGLS vs KDE, message=FALSE, warning=FALSE, eval=FALSE, include=FALSE, fig.height=7, fig.width=7----
# ##### FGLS-Cosinor vs. Kernel Density Estimation
# plot(fit.fgls$fitted.values,
#      type = "l",
#      lwd = 3,
#      ylim = c(0, 1000),
#      xlim = c(1, length(fit.fgls$fitted.values)),
#      xlab = "Time (minutes)",
#      ylab = "Activity (Halfed Amplitude)",
#      main = "FGSL-based Cosinor Model vs. Kernel Density Estimation")
# 
# # Cosinor peak line
# abline(v = 60 * (24 - (fit.fgls$coef.cosinor[3] %% (2 * pi)) * 24 / (2 * pi)),
#        lwd = 4) #### trough_time <- (acrophase - pi) %% (2 * pi) * 60
# 
# # True peak line
# abline(v = which.max(fit.fgls$fitted.values),
#        col = "orange",
#        lty = "dotted",
#        lwd = 4)
# 
# # Raw activity points
# points(df$Activity/2,
#        cex = 0.5)
# 
# # Overlay kernel density
# par(new = TRUE)
# plot(fit.KDE$kdf$hour * 60,
#      fit.KDE$kdf$density / 100,
#      type = "l",
#      lwd = 3,
#      col = "grey",
#      axes = FALSE,
#      xlab = "",
#      ylab = "",
#      xlim = c(1, length(fit.fgls$fitted.values)),
#      ylim = c(0, 2000))
# 

## ----message=FALSE, warning=FALSE, eval=FALSE, include=FALSE------------------
# # Base plot: OLS fitted values
# plot(fit.ols$fitted.values,
#      type = "l",
#      ylim = c(0, 2000),
#      xlab = "Time (minutes)",
#      ylab = "Activity",
#      main = "Traditional Cosinor Model vs. Kernel Density Estimation")
# 
# # Cosinor peak line
# abline(v = 60 * (24 - (fit.ols$coef.cosinor[3] %% (2 * pi)) * 24 / (2 * pi)),
#        lwd = 4) #### trough_time <- (acrophase - pi) %% (2 * pi) * 60
# 
# # True peak line
# abline(v = which.max(fit.ols$fitted.values),
#        col = "orange",
#        lty = "dotted",
#        lwd = 4)
# 
# # Raw activity points (smaller)
# points(df$Activity,
#        cex = 0.5)
# 
# # Overlay kernel density
# par(new = TRUE)
# plot(fit.KDE$kdf$hour * 60,
#      fit.KDE$kdf$density / 100,
#      type = "l",
#      lwd = 3,
#      col = "grey",
#      axes = FALSE,
#      xlab = "",
#      ylab = "",
#      xlim = c(1, length(fit.ols$fitted.values)),
#      ylim = c(0, 2000))
# 

