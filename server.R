
library(shiny)

shinyServer(function(input, output, session) {
    
    data <- reactive({
        inFile <- input$file1 
        if (is.null(inFile)){return(NULL)} 
        read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                 quote=input$quote)
    })
    
    
    # Updata value user could select
    observe({
        updateSelectInput(
            session,
            "var1",
            choices=names(data()))
        
    })
    # Updata value user could select
    observe({
        updateSelectInput(
            session,
            "var2",
            choices=names(data()))
        
    })
    # Updata value user could select
    observe({
        updateSelectInput(
            session,
            "var3",
            choices=names(data()))
        
    })
    # Updata value user could select
    observe({
        updateSelectInput(
            session,
            "x2",
            choices=names(data()))
        
    })
    # Updata value user could select
    observe({
        updateSelectInput(
            session,
            "y",
            choices=names(data()))
        
    })
    
    # Output a t distribution plot
    output$tplot <- renderPlot({
        # Display the Student's t distributions with various
        # degrees of freedom and compare to the normal distribution
        
        x <- seq(input$range[1], input$range[2], length=100)
        hx <- dnorm(x)  
        labels <- c('Distribusi t', 'Distribusi normal')
        
        plot(x, hx, type="l", lty=2, xlab="Nilai x", col = "black",
             ylab="Density", main="Distribusi t")
        
        
        lines(x, dt(x,input$df), lwd=2, col="red")
        
        
        legend("topright", inset=.05, title="Distribusi",
               labels, lwd=2, lty=c(1, 2), col=c("red", "black"))
    })
    
    # Output a data table for the upload tab page
    output$contents <- renderTable({
        inFile <- input$file1 
        if (is.null(inFile))
            return(NULL)
        read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
        
    })
    
    # Output a histogram for the variables user chose
    output$graph <- renderPlot({
        var1 <- data()[,input$var1]
        var2 <- data()[,input$var2]
        if (is.null(var1)){return(NULL)}
        if (is.null(var2)){return(NULL)}
        graph2 <- ifelse(input$sample == 'oneSamp', FALSE, TRUE)
        p1 <- hist(var1, breaks = input$bins)
        p2 <- hist(var2, breaks = input$bins)
        plot(p1, col=rgb(0,0,1,1/4))
        if(input$sample == 'twoSamp')
            plot(p2, col=rgb(1,0,0,1/4),add = graph2)
    })
    
    # Output of discriptive summary of this variable
    output$disc <-  renderPrint({
        Data <- data()
        if (is.null(Data)){return(NULL)}
        summary(Data)
    })
    
    # Output of the data structure
    output$str <- renderPrint({
        Data <- data()
        if (is.null(Data)){return(NULL)}
        str(Data)
    })
    
    # Create a one sample and two sample t-test reactive function
    ttestout <- reactive({
        var1 <- data()[,input$var1]
        conf <- input$conf
        if (is.null(var1)){return(NULL)}
        t1 <- t.test(var1, alternative = input$tail, mu = input$test, conf.level = conf)
        var2 <- data()[,input$var2]
        if (is.null(var2)){return(NULL)}
        ve <- ifelse(input$varequal == 'y', TRUE, FALSE)
        t2 <- t.test(var1, var2, alternative = input$tail, var.equal = ve, conf.level = conf)
        if(input$sample == "oneSamp") {return(t1)}
        if(input$sample == "twoSamp") {return(t2)}
        
    })
    
    # Output of one sample t value of t-test
    output$tvalue <- renderPrint({
        vals <- ttestout()
        if (is.null(vals)){return(NULL)}
        vals$statistic
    })
    
    # Output of p value
    output$pvalue <- renderPrint({
        vals <- ttestout()
        if (is.null(vals)){return(NULL)}
        vals$p.value 
    })
    
    # Create a prop test function
    proptestout <- reactive({
        conf <- input$conf
        p1 <- prop.test(x = input$x, n = input$n, p = input$p, alternative = input$tail, 
                        conf.level = input$conf, correct = TRUE)
        p2 <- prop.test(x = input$x, n = input$n, p = input$p, alternative = input$tail, 
                        conf.level = input$conf, correct = FALSE)
        if(input$correct == 'y') {return(p1)}
        if(input$correct == 'n') {return(p2)}
        
        
    })
    
    # Output of one sample pearson chi-squared value of prop test
    output$propvalue <- renderPrint({
        vals2 <- proptestout()
        if (is.null(vals2)){return(NULL)}
        vals2$statistic
    })
    
    # Output of p value
    output$pvalue2 <- renderPrint({
        vals2 <- proptestout()
        if (is.null(vals2)){return(NULL)}
        vals2$p.value 
    })
    
    # Create a variance test function
    vartestout <- reactive({
        x2 <- data()[,input$x2]
        y <- data()[,input$y]
        conf <- input$conf
        if (is.null(x2)){return(NULL)}
        if (is.null(y)){return(NULL)}
        v1 <- var.test(x2, y, alternative = input$tail, 
                       conf.level = input$conf)
        
    })
    
    # Output of one sample F value of var test
    output$fvalue <- renderPrint({
        vals3 <- vartestout()
        if (is.null(vals3)){return(NULL)}
        vals3$statistic
    })
    
    # Output of p value
    output$pvalue3 <- renderPrint({
        vals3 <- vartestout()
        if (is.null(vals3)){return(NULL)}
        vals3$p.value 
    })
    
    
    
})