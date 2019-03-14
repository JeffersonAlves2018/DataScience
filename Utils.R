#
#---------------------------------------------------------------
# Funções Customizadas para aplicações diversas
#---------------------------------------------------------------
#
# Autor.............: Jefferson A R Alves
#
# Última atualização: 23/02/2019 
#
# Versão............: 01.01
#
#---------------------------------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#     nome                      descrição
#---------------------------------------------------------------
# view_corr_lattice       recebe um dataframe e plota 
#                         os gráficos de correlaçao nos
#                         métodos "pearson" e "spearman"
#---------------------------------------------------------------
# extract_date            recebe um conjunto de datas e
#                         converte para ooutro formato 
#---------------------------------------------------------------
# normalize               aplica a normalização a um  
#                         dataset via método minmax
#---------------------------------------------------------------
# view_lm_corr_plot       gera uma regressão linear a 
#                         2 elementos 
#---------------------------------------------------------------
# get_dataset_train_test  realiza a divisão dos dados em
#                         2 subconjuntos (treino  e teste)
#                         conforme parâmetros
#---------------------------------------------------------------
# generate_sample_target  gera randomicamente uma distribuição 
#                         de valores target conf. parâmetros
#---------------------------------------------------------------
#
#------> Funções DSA
#
# set.asPOSIXct
# char.toPOSIXct
# set.asPOSIXct2
# fact.conv
# get.date
# POSIX.date
# var.log
# month.count
# serList
# unserList
#
# scale.features          aplica a técnica de scale as var´s
# to.factors              conversão de variáveis para fator
# run.feature.selection   seleção de var´s via RandomForest
#
#----------------------------------------------------
# Função de Geração de Gráfico de Correlação
#----------------------------------------------------
view_corr_lattice <- function(df, col_list){
  
  # definição dos métodos de análise
  methods <- c("pearson", "spearman")
  
  # aplicação da correlação aos dados
  cors <- lapply(methods, 
                 function(method)(
                                  cor(df[, ..col_list], method = method)
                                  )
                 )
  
  # função de plotagem dos dados conforme métodos 
  require(lattice)
  plot.cors <- function(x, labs){
    diag(x) <- 0.0 
    plot( levelplot(x, 
                    main = paste("Correlation Plot by Method", labs),
                    scales = list(x = list(rot = 90), cex = 1.0)) )
  }
  
  # plotagem dos dados
  Map(plot.cors, cors, methods)
}



#----------------------------------------------------
# Função de Decomposição de Data
#----------------------------------------------------
extract_date <- function(date, type_res) {
  if (type_res == 'string'){
      day <- format(date, format="%d")
      month <- format(date, format="%m")
      year <- format(date, format="%Y")
  }
    
  if (type_res == 'integer'){
      day <- as.integer(format(date, format="%d"))
      month <- as.integer(format(date, format="%m"))
      year <- as.integer(format(date, format="%Y"))    
  }
    
  if(type_res == 'numeric'){
      day <- as.numeric(format(date, format="%d"))
      month <- as.numeric(format(date, format="%m"))
      year <- as.numeric(format(date, format="%Y"))    
  }
  
  cbind(day, month, year) # concatena as variáveis e retorna
}
#mutate(df_norm, date2 = ymd(df_norm$date), day = day(date2), month = month(date2), year = year(date2))



#----------------------------------------------------
# Função de normalização
#----------------------------------------------------
normalize <- function(x) {
  norm <- ((x - min(x))/(max(x) - min(x)))
  return (norm)
}


#----------------------------------------------------
# Função de Análise de Correlação e Plotagem
#----------------------------------------------------
view_lm_corr_plot <- function(x,y){
library(plyr)
    # gera o modelo e plota o resultado de correlação
  fit <- lm(y~x)
  plot(x,y)
  abline(fit, col="red")
}


 

#----------------------------------------------------
# Função de Análise de Correlação e Plotagem
#----------------------------------------------------
get_dataset_train_test <- function(df, test_pct = 20, target_idx = 2, sequence_idx = -1) {
  # define o índice de amostragem
  train_pct = 100 - test_pct
  set.seed(test_pct)
  test_idx <- sample(nrow(df), nrow(df) * test_pct/100, replace=FALSE)
  
  # define os datasets de teste
  test.all <- df[test_idx,]    
  test.data <- test.all[, -c(target_idx, sequence_idx)]
  test.target <- test.all[,target_idx]
  
  # define os datasets de treino
  train.all <- df[-test_idx,]
  train.data <- train.all[,-c(target_idx, sequence_idx)]
  train.target <- train.all[,target_idx]
  
  # caso tenha sido indicado o índice, mantém
  if(sequence_idx > -1){ 
    test.all.sequence <- df[test_idx, sequence_idx]
    train.all.sequence <- df[-test_idx, sequence_idx]
  }else{
    test.all.sequence <- NULL
    train.all.sequence <- NULL
  }
  
  
  # retorna uma lista com os datasets
  rtrn_list <- list( testdata = test.data, 
                     testdir = test.target,
                     testseq = test.all.sequence,
                     traindata = train.data, 
                     traindir = train.target,
                     trainseq = train.all.sequence
  )
  
  return(rtrn_list)    
}



# https://stackoverflow.com/questions/31750173/how-can-i-save-everything-from-console
#------
#savehistory("dataout.txt")
#sink("dataout.txt")
#source('script.R', echo = TRUE)





#----------------------------------------------------
# Função de Geração de Valores Target (Classificação)
#----------------------------------------------------
generate_sample_target <- function(n = 10, s = c('Y', 'N')) {
  target <- do.call(paste0, replicate(1, sample(s, n, TRUE), FALSE))
  return(target)
}




#----------------------------------------------------
# Função de Plotagem Múltipla
#----------------------------------------------------
# view_plot_mulitple <- function(data.frame, t_lines, t_cols){
#   library(ggplot2)
#   library(shiny)
#   
#   par(mfrow=c(t_lines, t_cols)) # define a grade
#   dev.off()
#   
#   df <- data.frame
#   
#   for(i in 1:length(names(df))){
#     mname <- substitute(df[,i])
#     
#     if(is.factor(df[,i])){
#       plot(df[,i],main=names(df)[i])
#     }else{
#       hist(df[,i],main=names(df)[i])
#     }
#   }
# }
# 
# dfplot <- function(data.frame)
# {
#   df <- data.frame
#   ln <- length(names(data.frame))
#   for(i in 1:ln){
#     if(is.factor(df[,i])){
#       plot(df[,i],main=names(df)[i])}
#     else{hist(df[,i],main=names(df)[i])}
#   }
# }



#----------------------------------------------
# função de geração de combinação de parâmetros
#----------------------------------------------
# gera.combinacao <- function(x, o){
#   # remove um item caso tenha sido definido
#   x <- x[which(!x %in% o)]
#   
#   # cria uma lista multidimensional conformme elementos
#   res <- as.list(NULL)
#   for(i in 1:length(x)){ res[[i]] <- unlist( combinations(n=length(x), r=i, v=x) ) } 
#   
#   return(res)
# }
# 
# # teste de chamada
# gera.combinacao(lista, '-')
# df <- as.vector( gera.combinacao(lista, '-') )
# df





#----------------------------------------------------
# Funções do arquivo Tools - DSA
#----------------------------------------------------


#----------------------------------------------------
# Função de normalização
#----------------------------------------------------
scale.features <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- scale(df[[variable]], center=T, scale=T)
  }
}



#----------------------------------------------------
# Função de conversão de variáveis para fator
#----------------------------------------------------
to.factors <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.factor(df[[variable]])
  }
  return(df)
}




#----------------------------------------------------
# Função para seleção de variáveis
#----------------------------------------------------
run.feature.selection <- function(num.iters=20, feature.vars, class.var){
  set.seed(10)
  variable.sizes <- 1:10
  control <- rfeControl(functions = rfFuncs, method = "cv",
                        verbose = FALSE, returnResamp = "all",
                        number = num.iters)
  results.rfe <- rfe(x = feature.vars, y = class.var,
                     sizes = variable.sizes,
                     rfeControl = control)
  return(results.rfe)
}



set.asPOSIXct <- function(inFrame) { 
  dteday <- as.POSIXct(
    as.integer(inFrame$dteday), 
    origin = "1970-01-01")
  
  as.POSIXct(strptime(
    paste(as.character(dteday), 
          " ", 
          as.character(inFrame$hr),
          ":00:00", 
          sep = ""), 
    "%Y-%m-%d %H:%M:%S"))
}

char.toPOSIXct <-   function(inFrame) {
  as.POSIXct(strptime(
    paste(inFrame$dteday, " ", 
          as.character(inFrame$hr),
          ":00:00", 
          sep = ""), 
    "%Y-%m-%d %H:%M:%S")) }


set.asPOSIXct2 <- function(inFrame) { 
  dteday <- as.POSIXct(
    as.integer(inFrame$dteday), 
    origin = "1970-01-01")
}


fact.conv <- function(inVec){
  outVec <- as.factor(inVec)
  levels(outVec) <- c("Segunda", "Terca", "Quarta", 
                      "Quinta", "Sexta", "Sabado", 
                      "Domingo")
  outVec
}

get.date <- function(Date){
  temp <- strftime(Date, format = "%Y-%m-%d %H:%M:%S")
  substr(unlist(temp), 1, 10)
}


POSIX.date <- function(Date,Hour){
  as.POSIXct(strptime(paste(Date, " ", as.character(Hour), 
                            ":00:00", sep = ""), 
                      "%Y-%m-%d %H:%M:%S"))
}

var.log <- function(inFrame, col){
  outVec <- ifelse(inFrame[, col] < 0.1, 1, inFrame[, col])
  log(outVec)
}

month.count <- function(inFrame){
  Dteday <- strftime(inFrame$dteday, format = "%Y-%m-%dT%H:%M:%S")
  yearCount <- as.numeric(unlist(lapply(strsplit(
    Dteday, "-"), 
    function(x){x[1]}))) - 2011 
  inFrame$monthCount <- 12 * yearCount + inFrame$mnth
  inFrame
}

serList <- function(serlist){
  
  messages  <- c("O input nao eh uma lista ou tem comprimento maior que 0",
                 "Elementos nulos",
                 "A serializacao falhou")
  
  if(!is.list(serlist) | is.null(serlist) | 
     length(serlist) < 1) {
    warning(messages[2])
    return(data.frame(as.integer(serialize(
      list(numElements = 0, payload = NA), 
      connection = NULL))))}
  
  nObj  <-  length(serlist)
  
  tryCatch(outframe <- data.frame(payload = as.integer(
    serialize(list(numElements = nObj, 
                   payload = serlist), 
              connection=NULL))),
    error = function(e){warning(messages[3])
      outframe <- data.frame(
        payload = as.integer(serialize(list(
          numElements = 0, payload = NA), 
          connection=NULL)))}
  )
  outframe
}


unserList <- function(inlist){
  
  messages <- c("A coluna payload esta missing ou com tipo incorreto de dado",
                "Erro ao executar esta funcao",
                "A funcao gerou uma lista vazia")
  
  if(!is.integer(inlist$payload) | dim(inlist)[1] < 2 | 
     is.null(inlist$payload)){
    warning(messages[1]) 
    return(NA)
  }
  
  tryCatch(outList <- unserialize(as.raw(inlist$payload)),
           error = function(e){warning(messages[2]); return(NA)})
  
  if(outList$numElements < 1 ) {warning(messages[3]); 
    return(NA)}
  
  outList$payload
}







