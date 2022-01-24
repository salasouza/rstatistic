
### Analysis of Variance separated by groups

setwd("D:/anova")

sink("anova.txt")

input <- read.table(file="dataset.txt", sep="\t", header = TRUE)

model <- by(input,input$order, function(x){
    
    f1 = levels(factor(x$f1))
    f2 = levels(factor(x$f2))
    f3 = levels(factor(x$f3))
    order = levels(factor(x$order))
    
    for( i in (1: length(f1))){
        for(j in (1: length(order))){
            
            di <- x[x$f1 == f1[i] & x$order == order[j] ,]
            
            write(paste('\nf1:',"f1", '\n'), stderr())
            
            # Build model 
            
            anova.1 <- aov(values ~ f2 * f3, di)
            
            jpeg(file = paste(f1[i],order[j], '.jpeg', sep = ''))
            op <-  par(mfrow = c(2, 2))
            plot(anova.1, main = paste(f1[i], order[j]))
            par(op)
            dev.off()
            
            print(order[j])
            
            jpeg(file = paste("Boxplot",f1[i], order[j], '.jpeg', sep = ''))
            boxplot(values~f3, data = x[x$f1 == f1[i] & x$order == order[j] ,],col= c("white","grey", "blue", "red", "grey50"), 
                    main=paste(f1[i], order[j]), 
                    xlab ="f3", ylab="Value", ylim=c(0,600))
            dev.off()
            
            print(f1[i])
            
            write(paste('\nf1:', "f1", f1[i],order[j],'\n'), stderr())
            
            print(summary(anova.1))
            
            write("Done!", stderr())
            
            cat('\n')
            cat("=================================================================")
            cat('\n')
        }
    } 
    write("Analyse Finished! \n", stderr())  
    
    cat('Good coffee!!!')  
})

sink()
