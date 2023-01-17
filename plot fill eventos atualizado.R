## Fazendo uma base alternativa anual para captar apenas
#o movimmento dos eventos extremos ao longo do tempo, já que
#essa base não possui a variável "ocorrências" como sua antiga versão,
#mas manteremos a base principal para não perder a data exata
#em que ocorreu o evento

library(tidyverse) # manipulação da base
library(lubridate) # útil para trabalhar com datas
library(brazilmaps) # Importando a base de mapas do IBGE
library(tmap)
library(sf)


# Base desastres no brasil
desastres <- read.csv("C:/Users/anadu/Desktop/ic/ic2/antigos/new2/DesastresBr.csv",
                      encoding = "latin1",sep=";")
desastres <- desastres %>%
  select(data, ibge, descricao_tipologia)

## Criando uma coluna para identificar o ano em que o choque ocorreu

ano <- year(dmy(desastres$data))
desastres <- cbind(desastres, ano)
desastres <- filter(desastres, ano > "1999")

## Agrupando a base por ano, visto que o objetivo  é fazer a análise anual do total
# de ocorrências 

# antes criamos uma coluna de ocorrencias associadas a 1, assumindo que existe apenas 
# uma ocorrencia dados os valores que são usados para agregar 
#c(ano, ibge, descricao_tipologia)


desastres["ocorrencias"] <- length(51058) 

# Somando a coluna de ocorrencias

group_desastres <- desastres %>%
  group_by(ano, ibge, descricao_tipologia) %>%
  summarise(ocorrencias = sum(ocorrencias),
            .groups = "keep")


## Juntando as coordenadas de cada municipio par fazer o plot do mapa

# Primeiro, importaremos as coordenadas de Estado para marcar as divisões de Estado que
# aparecerá no plot

estadosbr <- get_brmap(geo="State", class = "sf")

# Agora, as coordenadas de municipio para agrupar a nossa base de eventos extremos
# nao aprecera no plot

cidadebr = get_brmap(geo = "City",
                     class = "sf")

## Juntando a base e as coordenadas

merge <- merge(group_desastres, cidadebr, by.x = c("ibge"),
               by.y = c("City"), all.x = T)

merge <- merge %>%
  select(ano, ibge, descricao_tipologia, ocorrencias, geometry)


## Separando bases para cada tipologia, no caso, são de interesse os desastres hidrologicos
alagamentos <- filter(merge, descricao_tipologia == "Alagamentos")
enxurradas <- filter(merge, descricao_tipologia == "Enxurradas")
inundacoes <- filter(merge, descricao_tipologia == "Inundações")
chuvas_intensas <- filter(merge, descricao_tipologia == "Chuvas Intensas")


## Separar por quintil

qa <- quantile(1:6, probs = seq(0,1,1/5)) # Para a distribuição de alagamentos

qe <- quantile(1:11, probs = seq(0,1,1/5)) # Para enxurradas

qi <- quantile(1:5, probs = seq(0,1,1/5)) # Para inundacoes

qc <- quantile(1:28, probs = seq(0,1,1/5)) # PAra chuvas intensas


## plotaar 
# Primeiro transformamos o data frame em sf para gerarmos o plot

alagamentos <- st_sf(alagamentos)
enxurradas <- st_sf(enxurradas)
inundacoes <- st_sf(inundacoes)
chuvas_intensas <- st_sf(chuvas_intensas)

tm_shape(alagamentos)+
  tm_fill(col = "ocorrencias", palette = "Blues", n=5, contrast = c(0.45,1),legend.hist = T,style = "fixed", breaks = qa)+
  tm_facets(by="ano", free.coords = F, scale.factor = 15)+
  tm_layout(title = "Alagamentos", bg.color = "grey80")+
  tm_shape(estadosbr)+
  tm_borders(col = "grey30", lwd = 0.5, lty= "solid", alpha =1)


tm_shape(enxurradas)+
  tm_fill(col = "ocorrencias", palette = "Blues", n=5, contrast = c(0.45,1), legend.hist = T,style = "fixed", breaks = qe)+
  tm_facets(by="ano", free.coords = F,  scale.factor = 15)+
  tm_layout(title = "Enxurradas", bg.color = "grey80")+
  tm_shape(estadosbr)+
  tm_borders(col = "grey30", lwd = 0.5, lty= "solid", alpha =1)

tm_shape(inundacoes)+
  tm_fill(col = "ocorrencias", palette = "Blues", n=5, contrast = c(0.45,1), legend.hist = T,style = "fixed", breaks = qi)+
  tm_facets(by="ano", free.coords = F,  scale.factor = 15)+
  tm_layout(title = "Inundações", bg.color = "grey80")+
  tm_shape(estadosbr)+
  tm_borders(col = "grey30", lwd = 0.5, lty= "solid", alpha =1)

tm_shape(chuvas_intensas)+
  tm_fill(col = "ocorrencias", palette = "Blues", n=5, contrast = c(0.45,1), legend.hist = T,style = "fixed", breaks = qc)+
  tm_facets(by="ano", free.coords = F, scale.factor = 15)+
  tm_layout(title = "Chuvas Intensas", bg.color = "grey80")+
  tm_shape(estadosbr)+
  tm_borders(col = "grey30", lwd = 0.5, lty= "solid", alpha =1)


