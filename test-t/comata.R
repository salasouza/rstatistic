# ============================================================
# Test T Welch Two Sample
#
#   + Report with analysis descriptive     
#
# ===========================================================

setwd("comata")

sink("Report.txt")

mydata <- read.table(file="dataset.txt", sep="\t", header = TRUE)

options(warn = -1)

model <- by(mydata,mydata$dose, function(x){
    
    compound = levels(factor(x$compound))
    dose = levels(factor(x$dose))
    
    for( i in (1: length(dose))){
        for(j in (1: length(compound))){
            
            di <- x[x$compound == compound[j] & x$dose == dose[i] ,]
            
            cat(paste('\n<<< COMPOUND:',compound[j],"|", "DOSE:" ,
                      dose[i],' >>>','\n'))
            
            cat('\n')
            
            # Build model 
            
            result <- t.test(di$resp1,
                            di$resp2,  
                            var.equal = FALSE, conf.level = 0.95)
            
            write(paste('\nDescription', compound[j], "|", dose[i], '\n'), stderr())
            
            t <- data.frame(Compound = di$compound, 
                            Dose = di$dose, 
                            t = result$statistic, 
                            pValue = result$p.value)
            print(t)
            
            #print(compound[j])
            #print(dose[i])
            
            cat(paste('\nCompound:',compound[j],"|", "Dose:" ,dose[i],'\n'))
            
            print(result)
            
            write("Done!", stderr())
            
            cat('\n')
            cat("========================================================")
            cat('\n')
        }
    } 
    write("Analyse Finished! \n", stderr())  

    cat('Good coffee!!!')    
})

sink()
