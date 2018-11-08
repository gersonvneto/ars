
# Se já tivermos os tweets salvos num arquivo csv, podemos importá-los para o R com o comando
#rt = read_twitter_csv("tweets_coleta_2018_11_06_14_11_09.csv")
#rt2 = read_twitter_csv("tweets_coleta_2018_11_08_16_17_25.csv")

# O processo de construção de uma rede
# Objeto de classe "igraph".
# Umas das formas mais eficientes é montar uma matriz de duas colunas sendo a primeira o
# sender e a segunda o receiver.

# Rede de citações em tweets
grep("@\\w+", rt$text, v=T) %>% length
receivers = str_extract_all(rt$text, "@\\w+")
receivers2 = str_extract_all(rt2$text, "@\\w+")
receivers[10:20]


# construindo a matriz
topcit = max(sapply(receivers, length)) # Verifica a maior quantidade de citações num tweet
topcit2 = max(sapply(receivers2, length))
# Cria uma matriz vazia para guardar os citados
citados = matrix(nrow = nrow(rt), ncol = topcit) 
citados2 = matrix(nrow = nrow(rt2), ncol = topcit2) 

# Preenche com os citados
for (i in 1:nrow(rt)) {
  for (j in 1:length(receivers[[i]])) {
    citados[i,j] = receivers[[i]][j]
  }
}

citados

# Preenche com os citados2
for (i in 1:nrow(rt2)) {
  for (j in 1:length(receivers2[[i]])) {
    citados2[i,j] = receivers2[[i]][j]
  }
}

citados2



# Cola os senders na primeira coluna
rede_citacoes = cbind(rt$screen_name, citados)
rede_citacoes2 = cbind(rt2$screen_name, citados2)
head(rede_citacoes2)

# transforma para que fique com apenas duas colunas
rede_citacoes = rede_citacoes %>% 
  as_tibble %>% 
  gather("col", "name", -V1) %>% 
  mutate(sender = V1,
         receiver = str_remove_all(name, "@")) %>% 
  select(sender, receiver) %>% 
  na.omit %>% 
  as.matrix

rede_citacoes2 = rede_citacoes2 %>% 
  as_tibble %>% 
  gather("col", "name", -V1) %>% # e meio que um transpor
  mutate(sender = V1,
         receiver = str_remove_all(name, "@")) %>% 
  select(sender, receiver) %>% 
  na.omit %>% 
  as.matrix

# Cria o objeto igraph
g_citacoes = graph_from_edgelist(rede_citacoes, directed = T) # parametro directed para dizer se e orientado
g_citacoes

g_citacoes2 = graph_from_edgelist(rede_citacoes2, directed = T)
g_citacoes2

# Plota
layoutgg = layout_with_kk(g_citacoes)
plot(g_citacoes, vertex.size = 5, vertex.label = NA,
     edge.arrow.size = .2, edge.color = adjustcolor("grey",.8),
     layout = layoutgg)

layoutgg2 = layout_with_kk(g_citacoes2)
plot(g_citacoes2, vertex.size = 5, vertex.label = NA,
     edge.arrow.size = .2, edge.color = adjustcolor("grey",.8),
     layout = layoutgg2)


# Plotando com o tamanho nos nós
plot(g_citacoes, vertex.size = igraph::degree(g_citacoes, normalized = T)*300,
     vertex.label = NA, vertex.color = adjustcolor("orange", .7),
     edge.arrow.size = .2, edge.color = adjustcolor("grey",.8),
     layout = layoutgg)

plot(g_citacoes2, vertex.size = igraph::degree(g_citacoes2, normalized = T)*300,
     vertex.label = NA, vertex.color = adjustcolor("orange", .7),
     edge.arrow.size = .1, edge.color = adjustcolor("grey",.8),
     layout = layoutgg2)


# Quem são os usuários mais citados?
indeg = igraph::degree(g_citacoes, mode = "in")
sort(indeg, decreasing = T)
sort(indeg, decreasing = T)[1:10]

indeg2 = igraph::degree(g_citacoes2, mode = "in")
sort(indeg2, decreasing = T)
sort(indeg2, decreasing = T)[1:10]

# Verificando a distribuição do grau
ggplot(NULL, aes(indeg)) +
  geom_histogram() +
  labs(title = "Distribuição do Grau", x = "", y = "", 
       subtitle = "Dados coletados do Twitter em 06/11/2018")

ggplot(NULL, aes(indeg2)) +
  geom_histogram() +
  labs(title = "Distribuição do Grau", x = "", y = "", 
       subtitle = "Dados coletados do Twitter em 06/11/2018")


# Plotando com grau de entrada no tamanho dos vértices e labels
plot(g_citacoes, vertex.size = igraph::degree(g_citacoes, mode = "in", normalized = T)*200,
     vertex.label.cex = igraph::degree(g_citacoes, mode = "in") / 300, 
     vertex.label = NA,
     vertex.color = adjustcolor("orange", .7),
     edge.arrow.size = .3, edge.color = adjustcolor("grey",.8),
     layout = layoutgg)


plot(g_citacoes2, vertex.size = igraph::degree(g_citacoes2, mode = "in", normalized = T)*200,
     vertex.label.cex = (igraph::degree(g_citacoes2, mode = "in")+0.01) / 50, 
     #vertex.label = NA,
     vertex.color = adjustcolor("orange", .7),
     edge.arrow.size = .3, edge.color = adjustcolor("grey",.8),
     layout = layoutgg)


## Identificando comunidades
# O igraph possui alguns algoritmos para detecção de comunidades em grafos.
# Vamos experimentar os mais comuns
grupos_walktrap = cluster_walktrap(g_citacoes)  # Rápido
grupos_infomap = cluster_infomap(g_citacoes)    # Lento!!

grupos_walktrap2 = cluster_walktrap(g_citacoes2)  # Rápido
grupos_infomap2 = cluster_infomap(g_citacoes2)    # Lento!!

membership(grupos_walktrap)
membership(grupos_infomap)

membership(grupos_walktrap2)
membership(grupos_infomap2)

# Plota com os grupos
plot(g_citacoes, vertex.size = igraph::degree(g_citacoes, mode = "in", normalized = T)*200,
     #vertex.label.cex = degree(g_citacoes, mode = "in") / 300, 
     vertex.label = NA,
     vertex.color = membership(grupos_walktrap),
     edge.arrow.size = .3, edge.color = adjustcolor("grey",.8),
     layout = layoutgg,
     mark.groups = communities(grupos_walktrap))


plot(g_citacoes2, vertex.size = igraph::degree(g_citacoes2, mode = "in", normalized = T)*200,
     #vertex.label.cex = degree(g_citacoes, mode = "in") / 300, 
     vertex.label = NA,
     vertex.color = membership(grupos_walktrap2),
     edge.arrow.size = .3, edge.color = adjustcolor("grey",.8),
     layout = layoutgg2,
     mark.groups = communities(grupos_walktrap2))

# Não é bom para redes muito grandes. Vamos tentar uma redução por equivalência estrutural usando
# blockmodeling

# O método usando as funções do pacote sna não é eficiente para redes muito grandes. Caso
# queira usá-lo basta "descomentar" o bloco de código abaixo usando CTRL + SHIFT + C

# library(sna)
# #ec = equiv.clust(as.matrix(get.adjacency(g_citacoes)))
# blocks = blockmodel(as.matrix(get.adjacency(g_citacoes)), 
#                     ec = equiv.clust(as.matrix(get.adjacency(g_citacoes))),
#                     k = 5)
# 
# # Plota agora com os cluster gerados por blockmodel
# plot(g_citacoes, vertex.size = igraph::degree(g_citacoes, mode = "in", normalized = T)*200,
#      #vertex.label.cex = degree(g_citacoes, mode = "in") / 300, 
#      vertex.label = NA,
#      vertex.color = comunidades,
#      edge.arrow.size = .3, edge.color = adjustcolor("grey",.8),
#      layout = layoutgg)
#      #mark.groups = communities(grupos_walktrap))



# 2 Alternativa: usando o pacote blockmodels
# Mais eficiente para redes muito grandes
library(blockmodels)
modelo = BM_bernoulli("SBM", as.matrix(get.adjacency(g_citacoes)))
modelo$estimate()
melhor = which.max(modelo$ICL)

modelo %>% glimpse
modelo$memberships[[melhor]]$Z

comunidades = vector("integer", nrow(rt))
for (i in 1:nrow(modelo$memberships[[melhor]]$Z)) {
  comunidades[i] = which.max(modelo$memberships[[melhor]]$Z[i,])
}

comunidades
group_marks = lapply(1:4, function(x) which(comunidades == x))

 # modelo 2
modelo2 = BM_bernoulli("SBM", as.matrix(get.adjacency(g_citacoes2)))
class(modelo2)
modelo2$estimate()
melhor2 = which.max(modelo2$ICL)

modelo2 %>% glimpse
modelo2$memberships[[melhor2]]$Z

comunidades2 = vector("integer", nrow(rt2))
for (i in 1:nrow(modelo2$memberships[[melhor2]]$Z)) {
  comunidades2[i] = which.max(modelo2$memberships[[melhor2]]$Z[i,])
}

comunidades2
group_marks2 = lapply(1:4, function(x) which(comunidades2 == x))


# Plota agora com os cluster gerados por blockmodel
plot(g_citacoes, vertex.size = igraph::degree(g_citacoes, mode = "in", normalized = T)*200,
     #vertex.label.cex = igraph::degree(g_citacoes, mode = "in") / 300,
     vertex.label = NA,
     vertex.color = comunidades + 1,
     edge.arrow.size = .1, edge.color = adjustcolor("grey",.8),
     layout = layoutgg)

plot(g_citacoes2, vertex.size = igraph::degree(g_citacoes2, mode = "in", normalized = T)*200,
     vertex.label.cex = igraph::degree(g_citacoes2, mode = "in") / 300,
     vertex.label = NA,
     vertex.color = comunidades2 + 1,
     edge.arrow.size = .1, edge.color = adjustcolor("grey",.8),
     layout = layoutgg2)
 


