#!/usr/bin/env Rscript

# ===========================================================================
# Default Settings 
# ===========================================================================

options(stringsAsFactors=FALSE)

suppressPackageStartupMessages(library("tools"))
suppressPackageStartupMessages(library("optparse"))
suppressPackageStartupMessages(library("ggfortify"))
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("broom"))
suppressPackageStartupMessages(library("reshape2"))

# ===========================================================================
# Option setting - Option Parser
# ===========================================================================

# Make option

option_list<- list(
    make_option(c('-d', '--dataset'), type =  'character', 
                default = 'dataset.txt',
                help = 'input [default %default]',
                dest = 'dataSet'),
    
    make_option(c('-m','--model'), default = 'model.txt', action = 'store',
                help = 'the txt model [default %default]',
                dest = 'model'),
    
    make_option(c('-a', '--analysis'), type = 'character', default = 'anova', 
                help = 'Analysis of variance [default %default]', 
                dest = 'analysis'),
    
    make_option(c('-r', '--resp_var'), type =  'numeric', 
                default = 3,
                help = 'Column number of the first response var [default %default]',
                dest = 'respVar'),
    make_option(c("-o", "--output"), default="result.txt",
                help="Output file name. [default=%default]")
)


# Parser

parser <- OptionParser(
    usage = "%prog [options] file", 
    option_list=option_list,
    description = paste('Applied Statistical Analysis',
                        'Author: Souza, S.S., Cedraz, H.', 
                        'Version: 0.1.0', 
                        'E-mail: sal.souzasa@gmail.com', 
                        sep = "\n", collapse = '\n'))
# Arguments

arguments <- parse_args(parser, positional_arguments = TRUE)

# Opt

opt <- arguments$options


# ===========================================================================
# Begin
# ===========================================================================

file = opt$dataSet


if (!file.exists(file) ) {
    write(paste("Dataset file",file,"does not exist\n"), stderr())
    stop()
}


# Input data

dados <- read.csv(file="dataset.txt", sep="\t")

folder <- 'models'

if(!file.exists(file.path(folder))) 
    dir.create(file.path(folder), recursive = TRUE, showWarnings = FALSE)

for(i in paste(opt$respVar):ncol(dados)){
    print(paste("Variable Response", names(dados[i])))
    system(paste(paste('echo'), paste0('`', 'echo ', names(dados[i]),'\ ', '``','cat ', opt$model,'`', ' > ', folder, '/', paste(names(dados[i])),'_model.txt'))) 
}

map <- list()

files <- dir(path = file.path(folder), pattern = "_model.txt$", full.names = TRUE)

map <- lapply(names(dados[paste(opt$respVar):ncol(dados)]), grep,x=files,value=TRUE)

names(map) <- paste(names(dados[paste(opt$respVar):ncol(dados)]))


# Apply statistical analysis 


if(casefold(opt$analysis, upper = FALSE) == 'anova'){
    model <- list()
    for(i in map){
        print(i)
        if(file.exists(i)){
            con = file(i, open = "r")
            line = noquote(readLines(con, warn = FALSE))
            modelo <- model.frame(line, data = dados)
            fit <- aov(modelo, data = dados)
            res <- anova(aov(modelo, data = dados))
            
            layout(matrix(c(1,2,3,4),2,2,byrow=T))
            plot(fit, which = c(1:3, 5), main = paste("Model:", line))
            
            cat('\n')
            print(res)
            cat('\n')
        }
    }
    
}else if(casefold(opt$analysis, upper = FALSE) == 'lm'){
    model <- list()
    for(i in map){
        print(i)
        if(file.exists(i)){
            con = file(i, open = "r")
            line = noquote(readLines(con, warn = FALSE))
            modelo <- model.frame(line, data = dados)
            res <- anova(lm(modelo, data = dados))
            
            print(summary(lm(modelo, data = dados)))
        }
    }
    
}else if(casefold(opt$analysis, upper = FALSE) == 'lme'){
    model <- list()
    for(i in map){
        print(i)
        if(file.exists(i)){
            con = file(i, open = "r")
            line = noquote(readLines(con, warn = FALSE))
            modelo <- model.frame(line, data = dados)
            res <- anova(aov(modelo, data = dados))
            print(summary(manova(modelo, data = dados)))
        }
    }
}


# ===========================================================================
# Output
# ===========================================================================

output = ifelse(opt$output == "result", "", opt$output)

write.table(res, output, col.names=TRUE, row.names=TRUE, quote=FALSE, sep="\t")

paste("Analyse Finished! See ", opt$output, "Folder.", "Good coffee!!!")

cat('\n')

# ===========================================================================
