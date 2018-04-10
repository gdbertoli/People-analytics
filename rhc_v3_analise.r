###########################################################
#Script de exploração
###########################################################

rm(list=ls())

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readr)
library(scales)

######################################################
#Importing
######################################################

setwd("C:/Users/gbertoli/Documents/4. People")

rhc <- read_csv("rhc_limpa_v2.csv")

######################################################
#Idades:
######################################################

#TODOS ATIVOS
(idade_media_ativo <- mean(rhc$idade[rhc$status == "ativo"], na.rm = T))
(idade_mediana_ativo <- median(rhc$idade[rhc$status == "ativo"], na.rm = T))

rhc %>%
	filter(status == "ativo") %>%
	ggplot(aes(x = idade)) +
	geom_histogram(breaks = seq(15, 60, by = 1),
		fill = "#4040a1", col = "white") +
	labs(title = "Idade dos colaboradores ativos", 
		x = "", y = "") +
	theme(plot.title = element_text(size = 18, face = "bold",
		hjust = 0.5, color = "#563f46")) +
	geom_vline(xintercept = idade_media_ativo,
		lty = 2, col = "#563f46", size = 1)+
	annotate("text", x = 40, y = 18, label = "Total = 472 pessoas\nMédia = 31,5 anos",
		size = 5, col = "#563f46", hjust = 0)

#Producao apenas
(idade_media_producao <- mean(rhc$idade[rhc$status == "ativo" & rhc$producao == T], na.rm = T))
(idade_mediana_producao <- median(rhc$idade[rhc$status == "ativo" & rhc$producao == T], na.rm = T))

rhc %>%
	filter(status == "ativo") %>%
	filter(producao == T) %>%
	ggplot(aes(x = idade)) +
	geom_histogram(breaks = seq(15, 60, by = 1),
		fill = "#4040a1", col = "white") +
	labs(title = "Idade dos colaboradores ativos da produção", 
		x = "", y = "", caption = "Todos os colaboradores MOD") +
	theme(plot.title = element_text(size = 18, face = "bold",
		hjust = 0.5, color = "#563f46"),
		plot.caption = element_text(face = "italic", color = "#563f46")) +
	geom_vline(xintercept = idade_media_producao,
		lty = 2, col = "#563f46", size = 1)+
	annotate("text", x = 40, y = 12, label = "Total = 261 pessoas\nMédia = 31 anos",
		size = 5, col = "#563f46", hjust = 0)

#Não-produção
(idade_media_adm <- mean(rhc$idade[rhc$status == "ativo" & rhc$producao == F], na.rm = T))
(idade_mediana_adm <- median(rhc$idade[rhc$status == "ativo" & rhc$producao == F], na.rm = T))

rhc %>%
	filter(status == "ativo") %>%
	filter(producao == F) %>%
	ggplot(aes(x = idade)) +
	geom_histogram(breaks = seq(15, 60, by = 1),
		fill = "#4040a1", col = "white") +
	labs(title = "Idade dos colaboradores ativos não-produção", 
		x = "", y = "", caption = "Todos os colaboradores MOI, CIAL e ADM") +
	theme(plot.title = element_text(size = 18, face = "bold",
		hjust = 0.5, color = "#563f46"),
		plot.caption = element_text(face = "italic", color = "#563f46")) +
	geom_vline(xintercept = idade_media_adm,
		lty = 2, col = "#563f46", size = 1)+
	annotate("text", x = 40, y = 10, label = "Total = 211 pessoas\nMédia = 32,2 anos",
		size = 5, col = "#563f46", hjust = 0)

#Ambos (densidade e hist)

rhc %>%
	filter(status == "ativo") %>%
	ggplot(aes(x = idade, fill = producao)) +
	geom_histogram(breaks = seq(15, 60, by = 1),
		col = "white", alpha = 0.9) +
	labs(title = "Idade dos colaboradores ativos", 
		x = "", y = "") +
	theme(plot.title = element_text(size = 18, face = "bold",
		hjust = 0.5, color = "#563f46")) +
	scale_fill_manual("Setor", values = c("#4040a1", "#618685"),
		labels = c("Adm", "Produção"))

rhc %>%
	filter(status == "ativo") %>%
	ggplot(aes(x = idade, fill = producao)) +
	geom_density(alpha = 0.6) +
	labs(title = "Idade dos colaboradores ativos", 
		x = "", y = "") +
	theme(plot.title = element_text(size = 18, face = "bold",
		hjust = 0.5, color = "#563f46")) +
	scale_fill_manual("Setor", values = c("#4040a1", "#fefbd8"),
		labels = c("Adm", "Produção"))


##################################################################
#2) Idade de admissao
##################################################################

(admissao_media_ativo <- mean(rhc$idade_admissao[rhc$status == "ativo"], na.rm = T))
(admissao_mediana_ativo <- median(rhc$idade_admissao[rhc$status == "ativo"], na.rm = T))

rhc %>%
	filter(status == "ativo") %>%
	ggplot(aes(x = idade_admissao)) +
	geom_histogram(breaks = seq(15, 60, by = 1),
		fill = "#4040a1", col = "white") +
	labs(title = "Idade de admissão dos colaboradores ativos ", 
		x = "", y = "") +
	theme(plot.title = element_text(size = 18, face = "bold",
		hjust = 0.5, color = "#563f46")) +
	geom_vline(xintercept = admissao_media_ativo,
		lty = 2, col = "#563f46", size = 1)+
	annotate("text", x = 40, y = 20, label = "Total = 472 pessoas\nMédia = 28,8 anos",
		size = 5, col = "#563f46", hjust = 0)

#Prod

(admissao_media_producao <- mean(rhc$idade_admissao[rhc$status == "ativo" & rhc$producao == T], na.rm = T))
(admissao_mediana_producao <- median(rhc$idade_admissao[rhc$status == "ativo" & rhc$producao == T], na.rm = T))

rhc %>%
	filter(status == "ativo") %>%
	filter(producao == T) %>%
	ggplot(aes(x = idade_admissao)) +
	geom_histogram(breaks = seq(15, 50, by = 1),
		fill = "#4040a1", col = "white") +
	labs(title = "Idade de admissão dos colaboradores ativos da produção", 
		x = "", y = "") +
	theme(plot.title = element_text(size = 18, face = "bold",
		hjust = 0.5, color = "#563f46")) +
	geom_vline(xintercept = admissao_media_producao,
		lty = 2, col = "#563f46", size = 1)+
	annotate("text", x = 35, y = 15, label = "Total = 261 pessoas\nMédia = 28,6 anos",
		size = 5, col = "#563f46", hjust = 0)

#Não-Prod

(admissao_media_adm <- mean(rhc$idade_admissao[rhc$status == "ativo" & rhc$producao == F], na.rm = T))
(admissao_mediana_adm <- median(rhc$idade_admissao[rhc$status == "ativo" & rhc$producao == F], na.rm = T))

rhc %>%
	filter(status == "ativo") %>%
	filter(producao == F) %>%
	ggplot(aes(x = idade_admissao)) +
	geom_histogram(breaks = seq(15, 60, by = 1),
		fill = "#4040a1", col = "white") +
	labs(title = "Idade de admissão dos colaboradores ativos do administrativo", 
		x = "", y = "") +
	theme(plot.title = element_text(size = 18, face = "bold",
		hjust = 0.5, color = "#563f46")) +
	geom_vline(xintercept = admissao_media_adm,
		lty = 2, col = "#563f46", size = 1)+
	annotate("text", x = 35, y = 10, label = "Total = 211 pessoas\nMédia = 29,1 anos",
		size = 5, col = "#563f46", hjust = 0)

#Ambos - Densidade

rhc %>%
	filter(status == "ativo") %>%
	ggplot(aes(x = idade_admissao, fill = producao)) +
	geom_density(alpha = 0.6) +
	labs(title = "Idade de admissão dos colaboradores ativos", 
		x = "", y = "") +
	theme(plot.title = element_text(size = 18, face = "bold",
		hjust = 0.5, color = "#563f46")) +
	 scale_fill_manual("Setor", values = c("#4040a1", "#fefbd8"),
		labels = c("Adm", "Produção")) +
	scale_x_continuous(limits = c(10, 60))

################################################
#3) Tempo de casa
################################################

(tenure_media_ativo <- mean(rhc$tenure[rhc$status == "ativo"], na.rm = T))
(tenure_mediana_ativo <- median(rhc$tenure[rhc$status == "ativo"], na.rm = T))

rhc %>%
	filter(status == "ativo") %>%
	ggplot(aes(x = tenure)) +
	geom_histogram(breaks = seq(0, 15, by = 1),
		fill = "#4040a1", col = "white") +
	labs(title = "Tempo de casa dos colaboradores ativos ", 
		x = "", y = "") +
	theme(plot.title = element_text(size = 18, face = "bold",
		hjust = 0.5, color = "#563f46")) +
	scale_x_continuous(breaks = c(seq(0,15,1)),
		labels = c(seq(0,15,1))) + 
	geom_vline(xintercept = tenure_mediana_ativo,
		lty = 2, col = "#563f46", size = 1)+
	annotate("text", x = 2.5, y = 130, label = "Metade dos colaboradores têm\nmenos de 1 ano e 3 meses de casa",
		size = 4.5, col = "#563f46", hjust = 0)

#Prod


(tenure_media_producao <- mean(rhc$tenure[rhc$status == "ativo" & rhc$producao == T], na.rm = T))
(tenure_mediana_producao <- median(rhc$tenure[rhc$status == "ativo" & rhc$producao == T], na.rm = T))

rhc %>%
	filter(status == "ativo") %>%
	filter(producao == T) %>%
	ggplot(aes(x = tenure)) +
	geom_histogram(breaks = seq(0, 15, by = 1),
		fill = "#4040a1", col = "white") +
	labs(title = "Tempo de casa dos colaboradores ativos da produção", 
		x = "", y = "") +
	theme(plot.title = element_text(size = 18, face = "bold",
		hjust = 0.5, color = "#563f46")) +
	scale_x_continuous(breaks = c(seq(0,15,1)),
		labels = c(seq(0,15,1))) + 
	geom_vline(xintercept = tenure_mediana_producao,
		lty = 2, col = "#563f46", size = 1)+
	annotate("text", x = 2.5, y = 80, label = "Metade dos colaboradores têm\nmenos de 1 ano e 3 meses de casa",
		size = 4.5, col = "#563f46", hjust = 0)

#Não-Prod


(tenure_media_adm <- mean(rhc$tenure[rhc$status == "ativo" & rhc$producao == F], na.rm = T))
(tenure_mediana_adm <- median(rhc$tenure[rhc$status == "ativo" & rhc$producao == F], na.rm = T))

rhc %>%
	filter(status == "ativo") %>%
	filter(producao == F) %>%
	ggplot(aes(x = tenure)) +
	geom_histogram(breaks = seq(0, 15, by = 1),
		fill = "#4040a1", col = "white") +
	labs(title = "Tempo de casa dos colaboradores ativos do administrativo", 
		x = "", y = "") +
	theme(plot.title = element_text(size = 18, face = "bold",
		hjust = 0.5, color = "#563f46")) +
	scale_x_continuous(breaks = c(seq(0,15,1)),
		labels = c(seq(0,15,1))) + 
	geom_vline(xintercept = tenure_mediana_adm,
		lty = 2, col = "#563f46", size = 1)+
	annotate("text", x = 2.5, y = 60, label = "Metade dos colaboradores têm\nmenos de 1 ano e 3 meses de casa",
		size = 4.5, col = "#563f46", hjust = 0)


#Quantiles

round(quantile(rhc$tenure[rhc$status == "ativo"],
	probs = seq(0, 1, 0.25), na.rm = T), digits = 2)

round(quantile(rhc$tenure[rhc$status == "ativo" & rhc$producao == T],
	probs = seq(0, 1, 0.25), na.rm = T), digits = 2)

round(quantile(rhc$tenure[rhc$status == "ativo" & rhc$producao == F],
	probs = seq(0, 1, 0.25), na.rm = T), digits = 2)


##Idade dos trabalhadores com mais tempo de casa

(idade_media_antigos <- mean(rhc$idade[rhc$status == "ativo" & rhc$tenure > 4], na.rm = T))
(idade_mediana_antigos <- median(rhc$idade[rhc$status == "ativo" & rhc$tenure > 4], na.rm = T))


rhc %>%
	filter(status == "ativo") %>%
	filter(tenure > 4) %>%
	ggplot(aes(x = idade, fill = producao)) +
	geom_histogram(breaks = seq(20, 60, by = 1), col = "white") +
	labs(title = "Idade dos colaboradores ativos com mais de 4 anos de casa", 
		x = "", y = "") +
	theme(plot.title = element_text(size = 18, face = "bold",
		hjust = 0.5, color = "#563f46")) + 
	geom_vline(xintercept = idade_media_antigos,
		lty = 2, col = "#563f46", size = 1)+
	annotate("text", x = 42, y = 7, label = "Total: 124\nMédia: 35,7 anos",
		size = 4.5, col = "#563f46", hjust = 0) +
	scale_fill_manual("Setor", values = c("#4040a1", "#618685"),
		labels = c("Adm", "Produção")) +
	theme(legend.position = "bottom") +
	scale_x_continuous(breaks = seq(20, 60, 5), labels = seq(20, 60, 5)) +
	scale_y_continuous(breaks = seq(0, 10, 2), labels = seq(0, 10, 2))


##Idade de admissão dos trabalhadores com mais tempo de casa

(admissao_media_antigos <- mean(rhc$idade_admissao[rhc$status == "ativo" & rhc$tenure > 4], na.rm = T))
(admissao_mediana_antigos <- median(rhc$idade_admissao[rhc$status == "ativo" & rhc$tenure > 4], na.rm = T))


rhc %>%
	filter(status == "ativo") %>%
	filter(tenure > 4) %>%
	ggplot(aes(x = idade_admissao, fill = producao)) +
	geom_histogram(breaks = seq(20, 50, by = 1), col = "white") +
	labs(title = "Idade de admissão dos colaboradores ativos com mais de 4 anos de casa", 
		x = "", y = "") +
	theme(plot.title = element_text(size = 18, face = "bold",
		hjust = 0.5, color = "#563f46")) + 
	geom_vline(xintercept = admissao_media_antigos,
		lty = 2, col = "#563f46", size = 1)+
	annotate("text", x = 37, y = 7, label = "Total: 124\nMédia: 28,3 anos",
		size = 4.5, col = "#563f46", hjust = 0) +
	scale_fill_manual("Setor", values = c("#4040a1", "#618685"),
		labels = c("Adm", "Produção")) +
	theme(legend.position = "bottom") +
	scale_x_continuous(breaks = seq(20, 50, 5), labels = seq(20, 50, 5)) +
	scale_y_continuous(breaks = seq(0, 10, 2), labels = seq(0, 10, 2))

#########################################################
# 4) Idade do público por sexo
#########################################################

#Masculino:

rhc %>% 
	filter(status == "ativo") %>% 
	group_by(sexo, producao) %>% 
	summarise(media = mean(idade,na.rm=T), sd = sd(idade,na.rm=T))

(idade_media_masc <- mean(rhc$idade[rhc$status == "ativo" & rhc$sexo == "masculino"], na.rm = T))
(idade_media_fem <- mean(rhc$idade[rhc$status == "ativo" & rhc$sexo == "feminino"], na.rm = T))

rhc %>%
	filter(status == "ativo") %>%
ggplot(aes(x = idade, fill = sexo)) +
	geom_histogram(breaks = seq(15, 60, by = 1), col = "white") +
	labs(title = "Idade dos colaboradores ativos por sexo", 
		x = "", y = "") +
	theme(plot.title = element_text(size = 18, face = "bold",
		hjust = 0.5, color = "#563f46")) +
	scale_x_continuous(breaks = c(seq(20,60,5)),
		labels = c(seq(20,60,5))) +
	scale_fill_manual("Setor", values = c("#4040a1", "#618685"),
		labels = c("Feminino", "Masculino")) +
	theme(legend.position = "bottom") +
	geom_vline(xintercept = idade_media_masc, lty = 2,
		col = "#618685", size = 1) +
	geom_vline(xintercept = idade_media_fem, lty = 2,
		col = "#4040a1", size = 1)


rhc %>%
	filter(status == "ativo") %>%
	group_by(sexo, producao) %>%
	summarise(n())
	summarise(media = mean(tenure, na.rm=T), sd = sd(idade,na.rm=T))

rhc %>%
	filter(status == "ativo") %>%
ggplot(aes(x = tenure, fill = sexo)) +
	geom_histogram(breaks = seq(0, 15, by = 1), col = "white") +
	labs(title = "Tempo de casa dos colaboradores ativos por sexo", 
		x = "", y = "") +
	theme(plot.title = element_text(size = 18, face = "bold",
		hjust = 0.5, color = "#563f46")) +
	scale_x_continuous(breaks = c(seq(0,15,1)),
		labels = c(seq(0,15,1))) +
	scale_fill_manual("Setor", values = c("#4040a1", "#618685"),
		labels = c("Feminino", "Masculino")) +
	theme(legend.position = "bottom")

#########################################################
# 5) Idade do público por temporário
#########################################################

rhc %>% 
	filter(status == "ativo") %>% 
	group_by(sexo, temporario) %>% 
	summarise(media = mean(idade,na.rm=T), sd = sd(idade,na.rm=T), n())

rhc %>% 
	filter(status == "ativo") %>% 
	group_by(producao, temporario) %>% 
	summarise(media = mean(idade,na.rm=T), sd = sd(idade,na.rm=T), n())

geom_histogram(breaks = seq(15, 60, by = 1), col = "white")

rhc %>%
	filter(status == "ativo") %>%
ggplot(aes(x = idade, fill = temporario)) +
	geom_density(alpha = 0.5) +
	labs(title = "Idade por temporário/efetivo", 
		x = "", y = "") +
	theme(plot.title = element_text(size = 18, face = "bold",
		hjust = 0.5, color = "#563f46")) +
	scale_x_continuous(breaks = c(seq(15,60,5)),
		labels = c(seq(15,60,5))) +
	scale_fill_manual("Setor", values = c("#4040a1", "#655611"),
		labels = c("Efetivos", "Temporário")) +
	theme(legend.position = "bottom")

rhc %>% 
	filter(status == "ativo") %>% 
	filter(producao == T) %>%
	group_by(temporario) %>% 
	summarise(media = mean(idade_admissao,na.rm=T),
		sd = sd(idade_admissao,na.rm=T), n())

#########################################################
# 6) Idade dos desligados
#########################################################

rhc %>%
	filter(status != "ativo") %>%
	filter(producao == T) %>%
	group_by(temporario) %>%
	summarise(med = mean(idade_desligamento,na.rm=T),
		sd = sd(idade_desligamento,na.rm=T), n())

rhc %>%
	filter(status != "ativo") %>%
	filter(producao == T) %>%
	group_by(temporario) %>%
	summarise(med = mean(tenure,na.rm=T),
		sd = sd(tenure,na.rm=T), n())

#########################################################
# ESE - Desligamento de temporários
#########################################################

#2017

rhc %>%
	filter(temporario == T) %>%
	filter(producao == T) %>%
	filter(data_de_desligamento >= as.Date("2017-01-01") &
		data_de_desligamento <= as.Date("2017-12-31")) %>%
	nrow()

rhc %>%
	filter(temporario == T) %>%
	filter(producao == T) %>%
	filter(data_de_desligamento >= as.Date("2018-01-01"))%>%
	nrow()

#########################################################
# ESE - Admitidos de temporários
#########################################################

rhc %>%
	filter(temporario == T) %>%
	filter(producao == T) %>%
	filter(admissão >= as.Date("2017-01-01") &
		admissão <= as.Date("2017-12-31")) %>%
	nrow()

rhc %>%
	filter(temporario == T) %>%
	filter(producao == T) %>%
	filter(admissão >= as.Date("2018-01-01"))%>%
	nrow()

#########################################################
# ESE - Admitidos e desligados de temporários
#########################################################

rhc %>%
	filter(temporario == T) %>%
	filter(producao == T) %>%
	filter(admissão >= as.Date("2017-01-01")) %>%
	filter(status == "desligado") %>%
	nrow()

#########################################################
# ESE - Admitidos e desligados de efetivos
#########################################################

rhc %>%
	filter(temporario == F) %>%
	filter(producao == T) %>%
	filter(admissão >= as.Date("2017-01-01")) %>%
	filter(status == "desligado") %>%
	nrow()

rhc %>%
	filter(temporario == F) %>%
	filter(producao == T) %>%
	filter(admissão >= as.Date("2017-01-01")) %>%
	filter(admissão <= as.Date("2017-12-31")) %>%
	nrow()

rhc %>%
	filter(temporario == F) %>%
	filter(producao == T) %>%
	filter(admissão >= as.Date("2018-01-01")) %>%
	nrow()

#########################################################
# ESE - Desligados por sexo e temporário
#########################################################
#Todos:

prop.table(table(rhc$temporario[rhc$data_de_desligamento >= as.Date("2017-01-01")],
	rhc$sexo[rhc$data_de_desligamento >= as.Date("2017-01-01")]))

#Prod:

prop.table(table(rhc$temporario[rhc$data_de_desligamento >= as.Date("2017-01-01") &
	rhc$producao == T],
	rhc$sexo[rhc$data_de_desligamento >= as.Date("2017-01-01")&
	rhc$producao == T]))

#########################################################
# ESE - Temporários contratados
#########################################################

duplicatas1 <- rhc[duplicated(rhc$nome),] 
duplicatas2 <- rhc[duplicated(rhc$nome, fromLast = T),]

duplicatas <- rbind(duplicatas1, duplicatas2)

contratados_17 <- filter(duplicatas, admissão >= as.Date("2017-01-01"),
                         status == "ativo")
demitidos_17 <- filter(duplicatas, data_de_desligamento >= as.Date("2017-01-01"),
                      temporario == T)

demitidos_17$nome %in% contratados_17$nome

rm(duplicatas1, duplicatas2, duplicatas, contratados_17, demitidos_17)

##########################################################################
# Turn-over 2017 (saídas em 2017/mean(ativos no começo + ativos no final))
##########################################################################

#ativos no começo de 2017:

rhc %>%
  filter(admissão <= as.Date("2017-01-01")) %>%
  filter(data_de_desligamento > as.Date("2017-01-01") |
           is.na(data_de_desligamento)) %>%
  nrow()

#ativos no final de 2017:

rhc %>%
  filter(admissão <= as.Date("2017-12-31")) %>%
  filter(data_de_desligamento > as.Date("2017-12-31") |
           is.na(data_de_desligamento)) %>%
  nrow()

#média de ativos anual 2017

ativos_2017 <- (348+421)/2

#demitidos em 2017:

demitidos_2017 <- rhc %>%
  filter(data_de_desligamento >= as.Date("2017-01-01"),
         data_de_desligamento <= as.Date("2017-12-31")) %>%
  nrow()

#Turn-over

100*demitidos_2017/ativos_2017 #60.34% :O

############################################
# First-year Turnover 2017
############################################

rhc %>%
  filter(data_de_desligamento >= as.Date("2017-01-01"),
         data_de_desligamento <= as.Date("2017-12-31")) %>%
  filter(tenure < 1) %>%
  nrow()

#1-y Turnover 2017:

173*100/demitidos_2017 #74.57%

############################################
# Turnover de temporários e efetivos
############################################

rhc %>%
  filter(data_de_desligamento >= as.Date("2017-01-01"),
         data_de_desligamento <= as.Date("2017-12-31")) %>%
  filter(temporario == T) %>%
  nrow()

#temporarios:
152*100/demitidos_2017 #65.52%

#efetivos:
80*100/demitidos_2017 #34.48%



