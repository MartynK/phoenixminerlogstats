# https://www.isixsigma.com/tools-templates/control-charts/a-guide-to-control-charts/

library(dplyr)
library(lubridate)
library(ggplot2)
library(EnvStats)
library(gridExtra)

get_time <- function(line_of_file) {
  lin <- strsplit(line_of_file," ")
  tim <- lin[[1]][1] %>% as.character()
  tim <- substr(tim, 1, nchar(tim)-1) %>% as_datetime()
  return(tim)
}

get_wait_times <- function( tim) {
  dtim <- c()
  for ( i in 2:length(tim)) {
    dtim <- c( dtim, tim[i] - tim[i-1])
  }
  return(dtim)
}

dat <- readLines(con = "data/log2.txt")


tim <- c()
for(i in 1:length(dat)) {

  lin <- strsplit( dat[i], " ")[[1]]
  lin <- lin[ (length(lin)-2):length(lin)]
  
  if ( i == 1) {
    starttime <- get_time( dat[i])
  }
  
  if ( identical( lin, c("ETH", "share", "found!")) == TRUE) {
    if ( get_time( dat[i]) > starttime + 300) { # For warming up & whatnot
      tim <- c(tim, get_time( dat[i]))
    }
  }
}

dtim <- get_wait_times(tim)

hist(dtim, breaks = 100)

dtim %>% ecdf() %>% plot()

estim <- eexp( dtim, ci = TRUE)

bot <- c()
for (i in 1:200) {
 bot <- c( bot, sample(dtim, replace = TRUE) %>% mean(.))
}
hist(bot, breaks = 50)
1/quantile( bot, probs = c(.05, .95))


plot( tim[-1], dtim)

expquant <- function( lamb, p = .95) {
  (-log(1-p))/lamb  
}

expquant(.02,.99)

trf_exp_norm <- function( dat, lambd) {
  pexp(dat, lambd) %>% qnorm() %>% return()
}

trf_norm_exp <- function( dat, lambd) {
  pnorm(dat) %>% qexp(.,rate = lambd) %>% return()
}

ntim <- trf_exp_norm(dtim, estim$parameters)

hist(ntim)
plot(ntim)

smooth.spline(ntim) %>% plot()

#####

baseline <- dtim[1:30]
process  <- dtim[31:length(dtim)]

base_lamb <- eexp( baseline)$parameters

base_norm <- trf_exp_norm( baseline, base_lamb)

process_norm <- trf_exp_norm( process, base_lamb)
proc_2sig <- trf_norm_exp( 2, base_lamb)
proc_3sig <- trf_norm_exp( 3, base_lamb)
proc_5sig <- trf_norm_exp( 4.75343, base_lamb)

sigs <- expand.grid( x = as_datetime(tim[2]),
                     label = c("mean + 2 sigma ~ 97.7%", 
                               "mean + 3 sigma ~ 99.8%", 
                               "mean + ~4.75 sigma < 1/1M "))
sigs$y <- c( proc_2sig, proc_3sig, proc_5sig) 


data.frame( time = as_datetime(tim[-1]), 
            dt   = dtim) %>%
  ggplot( aes( x = time,
               y = dt)) +
    theme_bw() +
    geom_point() +
    geom_hline( yintercept = proc_2sig, color = "blue") +
    geom_hline( yintercept = proc_3sig, color = "salmon4") +
    geom_hline( yintercept = proc_5sig, color = "red", lwd = 2) +
    scale_x_datetime() +
    labs( y = "Time between blocks (sec)",
          x = "Time") +
    geom_label( data = sigs, aes( x = x, y = y, label = label, 
                                  vjust = 1, hjust = 0))


data.frame( dt   = dtim) %>%
  ggplot( aes( x = dt)) +
  theme_bw() +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.7, 
                 fill = "lightblue", color = "black", 
                 bins = ceiling(sqrt(length(dtim)))) +
  geom_density(alpha=0.5, fill = "grey90", color = "darkblue", 
               size = 2, color = "darkblue", kernel = "epanechnikov")+
  geom_vline(aes(xintercept=mean(dt)),
             linetype="dashed") +
  stat_function(fun = dexp, args = list(rate = 1/mean(dtim)), color = "darkgreen", size = 2) +
  labs( y = "",
        x = "Time between blocks (seconds)",
        caption = "Blue: Density; Green: Theoretical; note: should diverge for small values",
        title = "Distribution of time between blocks")

  


data.frame( time = as_datetime(tim[32:length(tim)]), 
            dt_norm = process_norm) %>%
  ggplot( aes( x = time,
               y = dt_norm)) +
  theme_bw() +
  geom_point()  +
  geom_hline( yintercept = 0, color = "red") +
  geom_hline( yintercept = c(-3,3), color = "blue") +
  geom_hline( yintercept = mean(process_norm), color = "salmon4", linetype = "dashed") +
  labs( y = "Time between blocks (z-score)",
        x = "Time",
        caption = "dashed line: drift \n 99.7% should be between +-3") +
  scale_y_continuous( breaks = -3:3)


group <- 5

mn  <- c()
ran <- c()
for (i in 1:floor(length(process_norm)/5)) {
  mn  <- c(mn,  mean( process_norm[(i*5 - 4):(i*5)]))
  ran <- c(ran, max( process_norm[(i*5 - 4):(i*5)]) - min( process_norm[(i*5 - 4):(i*5)]))
  
}
rbar <- mean(ran)
D4 <-  2.114 #For group = 5

pmean <- data.frame(  x = as_datetime(tim[ seq( 32+5, length(tim), by = 5)]),
                      mn = mn,
                      ran = ran) %>%
  ggplot( aes(x = x, y = mn)) +
    theme_bw() +
    geom_point() +
    geom_line( linetype = "dotted") +
    geom_hline( yintercept = c(-3/sqrt(group),3/sqrt(group)), color = "blue") +
    geom_hline( yintercept = mean(mn), color = "grey20", linetype = "dashed") +
    labs( y = "Mean z-score for time between blocks",
          x = "Time",
          caption = "99.7% should be between blue limits\ndashed: drift") 

pran  <- data.frame(  x = as_datetime(tim[ seq( 32+5, length(tim), by = 5)]),
                      mn = mn,
                      ran = ran) %>%
  ggplot( aes(x = x, y = ran)) +
  theme_bw() +
  geom_point() +
  geom_line( linetype = "dotted") +
  geom_hline( yintercept = rbar * D4, color = "blue") +
  labs( y = "Range (per 5 shares)",
        x = "Time",
        caption = "99.7% should be below blue limit") 

grid.arrange( pmean,pran)


