# ARS com twitter


if (! "rtweet" %in% installed.packages()) install.packages("rtweet")
if (! "dplyr" %in% installed.packages()) install.packages("dplyr")
if (! "tidyr" %in% installed.packages()) install.packages("tidyr")
if (! "ggplot2" %in% installed.packages()) install.packages("ggplot2")
if (! "igraph" %in% installed.packages()) install.packages("igraph")
if (! "stringr" %in% installed.packages()) install.packages("stringr")
if (! "sna" %in% installed.packages()) install.packages("sna")
if (! "blockmodels" %in% installed.packages()) install.packages("blockmodels")
if (! "tm" %in% installed.packages()) install.packages("tm")
if (! "wordcloud" %in% installed.packages()) install.packages("wordcloud")
# Carrega os pacotes necessários

library(rtweet)   # conexão com o twitter API
library(dplyr)    
library(tidyr)    
library(ggplot2)  
library(igraph)   # ARS - análises e visualização
library(stringr)  
library(tm)       # Mineração de texto
library(wordcloud) # nuvens de palavras
library(sna)
library(blockmodels)


## Buscando dados no Twitter ####
source("chaves.R")
my_token = create_token(app,
                        consumer_key,
                        consumer_secret,
                        access_token,
                        access_secret)

# Verifica os limites de pesquisa para este token
my_limits = rate_limits(my_token)
my_limits

# Vamos fazer uma busca de 10000 tweets dos ultimos 5 ou 6 dias, contendo as hashtags 
# #elenao, #elesim, #bolsonaro17 e #haddad13
rt = search_tweets(q = "#elenao OR #elesim OR #bolsonaro17 OR #haddad13", 
                   n = 10000,
                   token = my_token)

# uma busca de celso portiolli
rt2 <- search_tweets(
  q = '"Celso Portiolli" OR Portiolli',
  n = 1000,
  token = my_token
)


# Análises preliminares ####
class(rt)
rt %>% glimpse

# Quais informações estão disponíveis pra nós nessa coleta?
names(rt)
names(rt2)

# Vamos verificar o texto de alguns tweets coletados
head(rt$text)

# Com um comando simples, vamos verificar a distribuição desses tweets no tempo
ts_plot(rt)

# Agora vamos guardar os tweets em um banco de dados para poder analisá-lo a qualquer momento
# Para não corrermos o risco de perder dados salvando um arquivo antigo por cima,
# vamos definir o nome do arquivo como tweets_coleta e acrescentar data e hora no final.
#write_as_csv(rt, file_name = paste0("tweets_coleta_", gsub("[\\-]|[:]|[ ]","_", Sys.time()), ".csv"))

























