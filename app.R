
library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(EnvStats)
library(gridExtra)

options( shiny.maxRequestSize  = 100*1024^2)

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

data_to_times <- function(fil) {
    
    dat <- readLines(con = fil)

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
    
    return(tim)
    
}


expquant <- function( lamb, p = .95) {
    (-log(1-p))/lamb  
}

trf_exp_norm <- function( dat, lambd) {
    pexp(dat, lambd) %>% qnorm() %>% return()
}

trf_norm_exp <- function( dat, lambd) {
    pnorm(dat) %>% qexp(.,rate = lambd) %>% return()
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Shares found by Worker according to the (Phoenix Miner's) log file"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            fileInput("log1", "Upload log file",
                      multiple = FALSE
                      ),
            
            actionButton('convert_button','Convert'),
            #textOutput(renderText("May take up to 2 minutes for ~100Mb")),
            checkboxInput("dtims","Delay between blocks", value = TRUE),
            checkboxInput("mnrange","Mean/range cards"),
            checkboxInput("histplot","Histogram"),
            textOutput("grovel")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("dtimsPlot"),
           plotOutput("mnrangePlot"),
           plotOutput("histPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    data <- reactiveVal()

    observeEvent(input$convert_button, {
        req(input$log1)
        input$log1$datapath  %>% data_to_times() %>% data() 
    })
    
    output$mnrangePlot <- renderPlot({
        
        req(input$convert_button == TRUE &
            input$mnrange == TRUE)
        tim <- data()

        dtim <- get_wait_times(data())

        estim <- eexp( dtim, ci = TRUE)

        ntim <- trf_exp_norm(dtim, estim$parameters)


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
            labs( y = "Mean z-score for\n time between blocks",
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
    })
    
    output$dtimsPlot <- renderPlot({
        req(input$convert_button == TRUE  &
                input$dtims == TRUE)
        tim <- data()
        dtim <- get_wait_times(tim)
        
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
                                          vjust = 0, hjust = 0))



    })
    
    output$histPlot <- renderPlot({
        req(input$convert_button == TRUE  &
                input$histplot == TRUE)
        tim <- data()
        dtim <- get_wait_times(tim)
        
        
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
        
    })
    
    output$grovel <- renderText( "If you found this useful, please consider donating hash power or ETH to: \n 
                                 0x253b92ccb486b0755b348d4b83b1c59e4819d034" )
}

# Run the application 
shinyApp(ui = ui, server = server)
