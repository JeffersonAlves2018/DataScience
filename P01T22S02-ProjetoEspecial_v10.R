# Overview: Planning a celebration is a balancing act of preparing just enough food to go around without 
# being stuck eating the same leftovers for the next week. The key is anticipating how many guests will come. 
# Grupo Bimbo must weigh similar considerations as it strives to meet daily consumer demand for fresh bakery 
# products on the shelves of over 1 million stores along its 45,000 routes across Mexico.
# Currently, daily inventory calculations are performed by direct delivery sales employees who must 
# single-handedly predict the forces of supply, demand, and hunger based on their personal experiences with 
# each store. With some breads carrying a one week shelf life, the acceptable margin for error is small.
# In this competition, Grupo Bimbo invites Kagglers to develop a model to accurately forecast inventory 
# demand based on historical sales data. Doing so will make sure consumers of its over 100 bakery products 
# aren’t staring at empty shelves, while also reducing the amount spent on refunds to store owners with 
# surplus product unfit for sale.
# 
# Data: In this competition, you will forecast the demand of a product for a given week, at a particular store. The dataset you are given consists of 9 weeks of sales transactions in Mexico. Every week, there are delivery trucks that deliver products to the vendors. Each transaction consists of sales and returns. Returns are the products that are unsold and expired. The demand for a product in a certain week is defined as the sales this week subtracted by the return next week.
# 
# The train and test dataset are split based on time, as well as the public and private leaderboard dataset split.
# 
# Things to note:
# •	There may be products in the test set that don't exist in the train set. This is the expected behavior of inventory data, since there are new products being sold all the time. Your model should be able to accommodate this.
# •	There are duplicate Cliente_ID's in cliente_tabla, which means one Cliente_ID may have multiple NombreCliente that are very similar. This is due to the NombreCliente being noisy and not standardized in the raw data, so it is up to you to decide how to clean up and use this information. 
# •	The adjusted demand (Demanda_uni_equil) is always >= 0 since demand should be either 0 or a positive value. The reason that Venta_uni_hoy - Dev_uni_proxima sometimes has negative values is that the returns records sometimes carry over a few weeks.
# 
# File Descriptions
# •	train.csv — the training set
# •	test.csv — the test set
# •	sample_submission.csv — a sample submission file in the correct format
# •	cliente_tabla.csv — client names (can be joined with train/test on Cliente_ID)
# •	producto_tabla.csv — product names (can be joined with train/test on Producto_ID)
# •	town_state.csv — town and state (can be joined with train/test on Agencia_ID)
# 
# Data Fields
# •	Semana — Week number (From Thursday to Wednesday)
# •	Agencia_ID — Sales Depot ID
# •	Canal_ID — Sales Channel ID
# •	Ruta_SAK — Route ID (Several routes = Sales Depot)
# •	Cliente_ID — Client ID
# •	NombreCliente — Client name
# •	Producto_ID — Product ID
# •	NombreProducto — Product Name
# •	Venta_uni_hoy — Sales unit this week (integer)
# •	Venta_hoy — Sales this week (unit: pesos)
# •	Dev_uni_proxima — Returns unit next week (integer)
# •	Dev_proxima — Returns next week (unit: pesos)
# •	Demanda_uni_equil — Adjusted Demand (integer) (This is the target you will predict)

# Considerações sobre o projeto:
#   
# 1. Como o estudo visa analisar a questão do atendimento a demanda dos produtos (variáveis Agencia_ID, 
# Canal_ID e Producto_ID) e não outros aspectos tais como logístico (variável Ruta_SAK) e perfil consumidor 
# (variável Cliente_ID), foram selecionadas apenas as seguintes variáveis para a criação do modelo:
#   
# • Semana — Week number (From Thursday to Wednesday)
# • Agencia_ID — Sales Depot ID
# • Canal_ID — Sales Channel ID
# • Producto_ID — Product ID
# • Venta_uni_hoy — Sales unit this week (integer)
# • Dev_uni_proxima — Returns unit next week (integer)
# • Demanda_uni_equil — Adjusted Demand (integer) (target will be predict)
# 
# 2. Como não existe demanda negativa para um produto, haverá um ajuste para 0 quando o cálculo da demanda 
# (Venta_uni_hoy - Dev_uni_proxima) for negativo;
# 
# 3. Os produtos contidos no dataset de teste e ausentes no dataset de treino deverão ser inseridos neste 
# útlimo com demanda zero, observando as particularidades (Agencia_ID, Canal_ID, Cliente_ID e Ruta_SAK), 
# assegurando ao modelo o conhecimento da existência dos mesmos;

setwd("D:/DSA/Cursos/FCD/01_R_Azure/Top_22/Prj02/Workspace")

#----------------------------------------------------
# 0. Bibliotecas Necessárias
#----------------------------------------------------
library(data.table)
library(sqldf)
library(tibble)
library(psych) 

library(tidyr)
library(dplyr)
library(plyr)

library(reshape2)
library(plotly)
library(class)

library(ggpubr)
library(lattice)
library(e1071)
library(jtools)
library(ggplot2)
library(gridExtra)
library(kableExtra)
library(rhandsontable)

library(DAAG)


source('Utils.r')

#----------------------------------------------------
# 1. Coleta de Dados
#----------------------------------------------------
#system.time( df_client <- fread('cliente_tabla.csv') )
#system.time( df_product <- fread('producto_tabla.csv') )
#system.time( df_town <- fread('town_state.csv') )

system.time( df_train <- fread('train.csv', nrows = 200000) )
system.time( df_test <- fread('test.csv', nrows = 50000) )

# visualizando a estrutura e os dados
#as.tibble(df_client)
#as.tibble(df_product)
#as.tibble(df_town)

glimpse(df_train)
glimpse(df_test)


#----------------------------------------------------
# 2. Exploração de Dados
#----------------------------------------------------

# verificando "missing values"
#any(is.na(df_client))
#any(is.na(df_product))
#any(is.na(df_town))
any(is.na(df_train))
any(is.na(df_test))

par(mfrow=c(1, 2))

# df_train -> analisando a estrutura e correlação das variáveis
col_names <- c('Semana', 'Agencia_ID', 'Canal_ID', 'Ruta_SAK', 'Cliente_ID', 'Producto_ID')
view_corr_lattice(df_train, names(df_train)[1:6])


# visualizando graficamente a distribuição
ggarrange(histogram(df_train$Semana), 
          histogram(df_train$Agencia_ID), 
          histogram(df_train$Canal_ID), 
          histogram(df_train$Ruta_SAK), 
          histogram(df_train$Cliente_ID), 
          histogram(df_train$Producto_ID), 
          nrow = 3, ncol = 2
)

# visualizando:
#  1. A relação entre a variável Canal_ID e Demanda_uni_equil
#  2. A densidade da variável Demanda_uni_equil e a necessidade de tratamento futuro
scatter.smooth(x=df_train$Canal_ID, y=df_train$Demanda_uni_equil)
plot(density(df_train$Demanda_uni_equil), main="Density Plot: Demand", ylab="Frequency")
polygon(density(df_train$Demanda_uni_equil), col="red")


# visualizando graficamente os valores únicos por variáveis
df_uniq <- as.vector(sapply(df_train[,c('Semana', 'Agencia_ID', 'Canal_ID', 'Ruta_SAK', 'Cliente_ID', 'Producto_ID')], function(x) as.numeric(length(unique(x)))))
df_bar <- barplot(df_uniq, main = "Unique Values", xlab = "", ylab = "n", names.arg = c('Semana', 'Agencia_ID', 'Canal_ID', 'Ruta_SAK', 'Cliente_ID', 'Producto_ID'), col = c("grey", "blue", "red", "purple", "green", "yellow"), border = 1, axis.lty = 1, log = "y")
text(df_bar, 0.5, paste("n = ", df_uniq,sep=""), cex=1, pos = 3, adj = 4) 

  
# considerando o objetivo principal que é prever a demanda dos produtos por depósito e a análise de 
# correlação entre as variáveis, optou-se por gerar um dataset contendo apenas algumas variáveis.
col_names <- c('Agencia_ID', 'Canal_ID', 'Producto_ID', 'Venta_uni_hoy', 'Dev_uni_proxima ')

# visualizando a demanda de produto por Produto_ID
dg01 <- sqldf("SELECT 
                Producto_ID, Agencia_ID, Canal_ID, sum(Demanda_uni_equil) as Demanda_uni_equil
            FROM 
                df_train
            WHERE
              Producto_ID BETWEEN 100 AND 700
            GROUP BY 
                Producto_ID, Agencia_ID, Canal_ID
            ORDER BY
                Producto_ID, Agencia_ID, Canal_ID, Demanda_uni_equil desc")

head(dg01)
ggplot(dg01, aes(Producto_ID, Demanda_uni_equil, group=1)) + geom_point() + 
  facet_grid(. ~Canal_ID) 


#----------------------------------------------------
# 3. Tratamento dos dados
#----------------------------------------------------


# reestruturando os datasets de forma a ficarem semelhantes conforme aspectos principais

# criando uma  variavel ID no dataset df_train
df_train <- mutate(df_train, id = row_number())

# criando a variável Demanda_uni_equil no dataset test
df_test$Demanda_uni_equil <- 0

# realiza o cálculo da demanda com base nas vendas desta semana e o retorno de produto da próxima
df_train$Demanda_uni_equil <- (df_train$Venta_uni_hoy - df_train$Dev_uni_proxima) 
df_train$Demanda_uni_equil <- ifelse(df_train$Demanda_uni_equil < 0, 0, df_train$Demanda_uni_equil)
#head(df_train[df_train$Demanda_uni_equil < 0,])

# remove as variaveis utilizadas no cálculo
df_train$Venta_uni_hoy <- NULL
df_train$Dev_uni_proxima <- NULL

# armazena a variável ID dos dataset.s e remove as originais 
df_train_seq <- df_train$id
df_train$id <- NULL

df_test_seq <- df_test$id
df_test$id <- NULL

# conforme considerações iniciais, serão todas as variáveis que não fazem parte do objetivo principal
df_train$Venta_hoy <-NULL
df_train$Dev_proxima <-NULL


# identifica os produtos do dataset de teste ausentes no dataset de treino 
df_prod <- anti_join(df_test, df_train, by = 'Producto_ID')
glimpse(df_prod)

# visualizando graficamente os valores únicos por variáveis
df_uniq <- as.vector(sapply(df_prod[,c('Semana', 'Agencia_ID', 'Canal_ID', 'Ruta_SAK', 'Cliente_ID', 'Producto_ID')], function(x) as.numeric(length(unique(x)))))
df_bar <- barplot(df_uniq, main = "Unique Values", xlab = "", ylab = "n", names.arg = c('Semana', 'Agencia_ID', 'Canal_ID', 'Ruta_SAK', 'Cliente_ID', 'Producto_ID'), col = c("grey", "blue", "red", "purple", "green", "yellow"), border = 1, axis.lty = 1, log = "y")
text(df_bar, 0.5, paste("n = ", df_uniq,sep=""), cex=1, pos = 3, adj = 4) 
sprintf("Total de Produtos Fora dos Dados de Treino: %d", as.vector(df_uniq)[6], 2)


# insere os produtos ausentes no dataset de treino 
df_train <- rbind.data.frame(df_train, df_prod)


# removendo os dataset´s
rm(df_prod)
rm(df_uniq)
rm(df_bar)


# definindo o conjunto de variáveis
df_cols <- c('Semana', 'Agencia_ID', 'Canal_ID', 'Ruta_SAK', 'Cliente_ID', 'Producto_ID')


# Normalizando os dados de treino
df_train_norm <- df_train
summary(df_train_norm[,c('Semana', 'Agencia_ID', 'Canal_ID')])
df_train_norm <- as.data.frame(lapply(df_train[, df_cols], normalize))
summary(df_train_norm[,c('Semana', 'Agencia_ID', 'Canal_ID')])
df_train_norm$Demanda_uni_equil <- df_train$Demanda_uni_equil


# Normalizando os dados de teste
df_test_norm <- df_test
summary(df_test_norm[,c('Semana', 'Agencia_ID', 'Canal_ID')])
df_test_norm <- as.data.frame(lapply(df_test[, ..df_cols], normalize))
summary(df_test_norm[,c('Semana', 'Agencia_ID', 'Canal_ID')])
df_test_norm$Demanda_uni_equil <- df_test$Demanda_uni_equil



#----------------------------------------------------
# 4. Treinamento do Modelo
#----------------------------------------------------

# Criando o modelo de regressão - v01
formula.init.v01 <- "Demanda_uni_equil ~ ."
formula.init.v01 <- as.formula(formula.init.v01)
lr.model_v01 <- lm(formula = formula.init.v01, data = df_train_norm)

# Visualizando os detalhes do modelo
summ(lr.model_v01, n.sd = 2, robust = "HC1")


#----------------------------------------------------
# 5. Otimização do Modelo e Avaliação dos Resultados
#----------------------------------------------------

# Criando o modelo de regressão - v02
formula.init.v02 <- "Demanda_uni_equil ~ Semana + Cliente_ID + Canal_ID + Ruta_SAK + Producto_ID"
formula.init.v02 <- as.formula(formula.init.v02)
lr.model_v02 <- lm(formula = formula.init.v02, data = df_train_norm)


# Criando o modelo de regressão - v03
formula.init.v03 <- "Demanda_uni_equil ~ Cliente_ID + Ruta_SAK + Producto_ID"
formula.init.v03 <- as.formula(formula.init.v03)
lr.model_v03 <- lm(formula = formula.init.v03, data = df_train_norm)


# Criando o modelo de regressão - v04
formula.init.v04 <- "Demanda_uni_equil ~ Canal_ID + Ruta_SAK + Producto_ID"
formula.init.v04 <- as.formula(formula.init.v04)
lr.model_v04 <- lm(formula = formula.init.v04, data = df_train_norm)


# comparando os modelos
plot_summs(lr.model_v01, lr.model_v02, lr.model_v03, lr.model_v04, scale = TRUE, plot.distributions = TRUE)
export_summs(lr.model_v01, lr.model_v02, lr.model_v03, lr.model_v04, scale = TRUE,   error_format = "({statistic})")
# (S.E. = {std.error})


# You can force knitr to give the console style of output by setting the chunk option render = 'normal_print'.
#effect_plot(lr.model_v01, pred = lr.model_v01.pred, interval = TRUE, plot.points = TRUE)



#----------------------------------------------------
# 6. Execução do Modelo
#----------------------------------------------------

# escolhendo o modelo e aplicando aos dados de teste
lr.model.pred <- predict.lm(lr.model_v02, df_test_norm, interval = "predict") #prediction
as.tibble(lr.model.pred)
summary(lr.model.pred)



# verificando o erro
df_erro <-as.data.frame(cbind(lr.model.pred))
df_erro$demanda <- 0

# verifica a demanda e ajusta o indicador
df_erro$demanda <- ifelse(df_erro$fit < 0, -1, df_erro$demanda)
df_erro$demanda <- ifelse(df_erro$fit > 0, 1, df_erro$demanda)
df_erro$demanda <- ifelse(df_erro$fit == 0, 0, df_erro$demanda)

# Verificando a proporção
df_erro$demanda <- factor(df_erro$demanda, levels = c(1, 0, -1), labels = c("Positiva (> 0)", "Nula (=0)", "Negativa (<0)"))
round(prop.table(table(df_erro$demanda)) * 100, digits = 1)
head(df_erro)

# exibe a taxa de acerto
df_taxa_acerto <- round(prop.table(table(df_erro$demanda)) * 100, digits = 1)
sprintf("Taxa de Acerto do Modelo: %.2f%%", round(as.vector(df_taxa_acerto)[1], digits = 2), 2)


# exibe o resultado formatado
df_taxa_acerto <- as.data.frame(df_taxa_acerto)
str(df_taxa_acerto)
colnames(df_taxa_acerto)
colnames(df_taxa_acerto)[1] <- 'Demanda Apurada'
colnames(df_taxa_acerto)[2] <- '%'

rhandsontable(df_taxa_acerto, rowHeaders = NULL)
#gridExtra::grid.table(df_taxa_acerto)
