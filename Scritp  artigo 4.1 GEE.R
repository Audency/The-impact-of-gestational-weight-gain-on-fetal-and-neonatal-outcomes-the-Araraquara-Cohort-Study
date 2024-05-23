
####################################################################################################################################################################################
######################################################################### Banco de dados Araraquara - Tese#########################################################################
###################################################################################################################################################################################

########################################################################### defenir o directorio de trabalho#######################################################################

setwd("/Users/audenciovictor1/Desktop/AUDENCIO/PhD Files/Tese/Tese Artigos/Artigo 4/Base de dados")

###################################################################### Chamar os pacotes #######################################################################################

library(psych)    # O pacote "psych" é voltado para a área de psicometria e fornece uma variedade de funções para análise e visualização de dados psicológicos. Ele oferece recursos como análise fatorial, análise de confiabilidade, análise de componentes principais, gráficos de dispersão, cálculo de correlações, entre outros. 
library(pipeR) # Permite encadear operações em uma sequência mais legível e eficiente. Ele usa o operador %>>% para encadear as operações, facilitando a leitura e a compreensão do código.
require(dplyr) # O pacote "dplyr" é uma parte do pacote "tidyverse" e fornece um conjunto consistente de funções para manipulação de dados em R. Ele inclui funções para filtrar linhas, selecionar colunas, criar variáveis calculadas, agrupar dados, ordenar dados e resumir dados.
library(tidyverse) # Ë uma coleção de pacotes R projetados para análise de dados e manipulação de dados. Ele inclui o pacote "dplyr" mencionado acima, juntamente com outros pacotes como "ggplot2" (para visualização de dados), "tidyr" (para arrumação de dados), "readr" (para leitura de dados retangulares) e outros.
library(foreign) # O pacote "foreign" fornece funções para importar dados de outros formatos de arquivo, como SPSS, SAS e Stata, para o ambiente R. Ele permite que você leia arquivos com extensões como .sav, .por, .sas7bdat, .dta, entre outros
library(haven) # um pacote complementar ao "foreign" e fornece funções para importar e exportar dados em formatos de arquivo específicos, como arquivos do SPSS, SAS e Stata. Ele é útil quando você precisa trabalhar com dados em um desses formatos.
library(readxl) # O pacote "readxl" fornece funções para importar dados de arquivos do Excel (.xls e .xlsx) para o ambiente R. Ele permite que você leia planilhas do Excel, convertendo-as em data frames R para análise e manipulação.
library(geepack) # O pacote do modelo modelo de GEE

install.packages("pacman")
##################################################################### Pacotes para descitiva  #################################################################################
pacman::p_load(
  rio,          # importa arquivos
  here,         # localiza arquivos
  skimr,        # gera visualização dos dados
  tidyverse,    # gestão dos dados + gráficos no ggplot2 
  gtsummary,    # resumo estatísticos e testes
  rstatix,      # resumo e testes estatísticos
  janitor,      # adiciona números absolutos e porcentagens às tabelas
  scales,       # facilmente converte proporções para porcentagens
  flextable )    # converte tabelas para o formato de imagens)


## Importando o dataset com do excel incluindo todas Sheet
q1 <- read_excel("dados tese1.xls", sheet = "Questionário 1")
q2 <- read_excel("dados tese1.xls", sheet = "Questionário 2")
q3 <- read_excel("dados tese1.xls", sheet = "Questionário 3")
q8 <- read_excel("dados tese1.xls", sheet = "Questionário 8")
q9 <- read_excel("dados tese1.xls", sheet = "Questionário 9")
q11 <- read_excel("dados tese1.xls", sheet = "Questionário 11")
q12 <- read_excel("dados tese1.xls", sheet = "Questionário 12")
q13 <- read_excel("dados tese1.xls", sheet = "Questionário 13")
q14 <- read_excel("dados tese1.xls", sheet = "Questionário 14")
q15 <- read_excel("dados tese1.xls", sheet = "Questionário 15")
q16 <- read_excel("dados tese1.xls", sheet = "Questionário 16")
q17 <- read_excel("dados tese1.xls", sheet = "Questionário 17")
q18 <- read_excel("dados tese1.xls", sheet = "Questionário 18")
q19 <- read_excel("dados tese1.xls", sheet = "Questionário 19")
q20 <- read_excel("dados tese1.xls", sheet = "Questionário 20")
q21 <- read_excel("dados tese1.xls", sheet = "Questionário 21")

dados1 <- bind_cols(q1,q2,q3,q8,q9,q11,q12,q13,q14,q15,q16,q17,q18,q19,q20,q21)
rm(q1,q2,q3,q8,q9,q11,q12,q13,q14,q15,q16,q17,q18,q19,q20,q21)

### lidando agora com os valores Omissisos
dados1[dados1 == 999] <- NA
dados1[dados1 == 9999] <- NA
dados1[dados1 == 999999] <- NA
dados1[dados1 == 99999] <- NA
dados1[dados1 == 	 99999999] <- NA
dados1[dados1 == 	 333884.3 ] <- NA
dados1[dados1 == 777] <- NA
dados1[dados1 == 7777] <- NA
dados1[dados1 == 77777] <- NA
dados1[dados1 == 777777] <- NA
dados1[dados1 == 5555] <- NA
dados1[dados1 == 55555] <- NA
dados1[dados1 == 555] <- NA
dados1[dados1 == 88888] <- NA
dados1[dados1 == 8888] <- NA
dados1[dados1 == 888] <- NA

#################################################### remover as linhas com NA na Varival Peso pregestacional e Posparto #########################################################
dados1 <- dados1[complete.cases(dados1$d_PesoParto),]
dados1 <- dados1[complete.cases(dados1$a_pesopre),]
dados1 <- dados1[complete.cases(dados1$a_estat1),]
####################################################################################################################################################################################
########################################## Base de dados com as variveis novas ###################################################################################################
####################################################################################################################################################################################

### Criar um subset para predicacao com as features

variables <- c( "a_idade", "a_imcga","b_imcga","c_imcga","a_imcpg","a_fmp","b_fmp", "c_fmp", "a_igusg","b_igusg","c_igusg","a_escola",
                "a_npari","a_npcomo", "a_rendpcr", "a_cor","a_civil","a_vigorh", "b_vigorh","c_vigorh", "a_moderh","b_moderh", "c_moderh","a_fumog",
                "b_fumog", "c_fumog","a_alcool", "b_alcool","c_alcool","a_agdm","b_agdm","c_agdm", "a_aghas","b_aghas","c_aghas","a_agui","b_agui",
                "c_agui", "a_agceva", "b_agceva","c_agceva","a_pcr","b_pcr","c_pcr","a_hb","b_hb", "c_hb","a_hba1c","b_hba1c","c_hba1c", "a_insul",
                "b_insul", "c_insul", "a_ct", "b_ct", "c_ct", "a_hdl","b_hdl","c_hdl", "a_ldl","b_ldl","c_ldl","a_tg","b_tg", "c_tg","a_tipoparto",
                "b_tsausg","b_tsausg","c_tsausg", "b_ttsbusg","c_ttsbusg","b_ttscusg", "c_ttscusg","a_pesousg", "b_pesousg","c_pesousg","f_FMP", 
                "f_FFMp", "d_Apgar5","d_CompNasc","d_PesoNasc", "d_intergr...1507", "d_SexoRN", "a_estat1", "a_pesopre", "d_PesoParto","d_InPond",
                "d_perintg", "d_IntercRN", "d_IGUSGrn")


# Removendo as duplicatas
unique_variables <- unique(variables)

# Banco de dados 
dados <- subset(dados1, select = unique_variables)

names(dados)[names(dados) == "d_intergr...1507"] <- "d_intergr"
dados$id <- cbind(id = seq(1, nrow(dados)))
###################################################################################################################################################################################
############################################# As variaveis para o artigo de Influencia GWG e desfechos materno e e do concepto################################################################################
###################################################################################################################################################################################

 a_idade # Idade materna (0)  ≤19/ (1) 20-35/ (2) ≥35)
 a_cor # raca,((1) branca / (2) preta / (3) amarela / (4) indígena / (5) parda)  
 a_civil # estado civil --- Situação conjugal da gestante (1) casada / (2) solteira (com companheiro) / (3) solteira (sem companheiro) / (4) separada/viúva
 a_rendpcr    # Renda per capita
 a_escola # Escolaridade  da gestante (em anos de estudos) : (0) ≤4/ (1) 5-11 /  (2) ≥35)
 a_grauesc  # Grau de escolaridade da gestante: (0) Sem escolaridade, (1) Ensino Fundamental (1ª a 9ª série) Incompleto, (2) Ensino Fundamental (1ª a 9ª série) Completo, (3) Ensino Médio Incompleto, (4) Ensino Médio Completo, (5) Ensino Superior Incompleto, (6) Ensino Superior Completo, (7) Pós-graduação Incompleta, (8) Pós-graduação Completa
 a_moradia # tipo de moradia: (0)Alugada/ (1)Própria quitada / (2) Própria não quitada/ (3) Posse/(4)Emprestada /(5)Outra
 a_materia	#Material de construção da residência: (0) Madeira, (1) Alvenaria, (2) “pau a pique”, (3) Outro
 a_npcomo	 #Número de pessoas por cômodo
 a_fumog # Fumar
 a_alcool # Uso de álcool na gestação
 a_ngesta  # N° de gestações anteriores
 a_npari	# Quantas vezes a pessoa já pariu
 a_estat1 # altura, 
 a_circbracm #Circunferência do braço
 a_imcpg # IMC-pregest,  
 a_imcga # IMC gestacional atual (Kg/m²)
 a_pesopre # peso pregestational, 
 d_PesoParto # Peso pos parto
 a_fmp # Percentual de gordura corporal (%)
 a_vigord #actividade fisica (Em quantos dias de uma semana normal, você realiza atividades VIGOROSAS por pelo menos 10 minutos contínuos, como por exemplo, correr, fazer ginástica, aeróbica, jogar futebol, pedalar rápido na bicicleta, jogar basquete, fazer serviços domésticos pesados em casa, no quintal ou no jardim, carregar pesos elevados ou qualquer atividade que faça você suar BASTANTE ou aumentem MUITO sua respiração ou batimentos do coração?),
 d_Igcapur 	#Idade Gestacional (Método Capurro) em semanas e dias
 d_IGUSGrn	#Idade Gestacional ao nascimento (USG coorte) em semanas e dias
 d_IGDUMrn	#Idade Gestacional ao nascimento (DUM) em semanas e dias
 d_SexoRN	  #Sexo do recém-nascido: (0)Feminino, (1) Masculino, (2)  Não determinado
 a_insul	  #Insulina de jejum (uUI/mL)
 a_homa	   #HOMA (uUI/mL)
 a_hba1c # hemoglobina glicada, 
 o_tg #	Triglicerídeos (mg/dL)
 a_ct # colestreol
 o_hdl# HDL
 o_ldl# LDL
 gpg # ganho de peso gestacional 
 a_agdm ## Diabestes gestacional 
 a_aghas ## Hipertensao arterial
 a_agui    ## Infeccao urinaria 
 a_agceva  ###. Vaginitis e cervucit
 a_pcr     ## Pcr
 
############################################################### Desfechos fetais  #############################################################################################
 c_tsausg   # Espessura do tecido subcutâneo do abdome (TSA) em mm  ( 1 ou 2 trimestre ?????????)
 c_ttsbusg  #Área de tecido subcutâneo do braço (TSB)
 c_ttscusg  # Área de tecido subcutâneo da coxa (TSC) 
 rciu       # RCIU- Restricao de crescimento intrauterino
 c_pesousg	#Peso fetal estimado em gramas
 
 ########################################################### Desfechos  neonatais  ###########################################################################################
f_FMP        #% Massa gorda (%MG)
f_FFMp       #% Massa livre de gordura (%MLG)
d_CompNasc   #Tamanho neonatal (comprimento ao nascer) medio ????
d_Apgar5     #Apgar no 5° minuto
d_PesoNasc   #Peso ao nascer
d_InPond     #Índice ponderal
d_intergr    #Adequação do peso ao nascer (Intergrowth)

##################################################### Transformar as varariaveis em numerica #######################################################################################
dados <- dados %>%  mutate_all(as.numeric)

############################################# Peso pos parto  ##########################################################
sort(dados$d_PesoParto, decreasing = T)
summary(dados$d_PesoParto)
dados$d_PesoParto[dados$d_PesoParto == 989.50] <- 98.9

####################################################################################################################################################################################
########################################## Criando a varivel GPG de acordo com IOM###################################################################################################
####################################################################################################################################################################################

# Leitura dos dados de IMC pré-gestacional e ganho de peso gestacional

# Cálculo do IMC pré-gestacional
dados$imc <- dados$a_pesopre / (dados$a_estat1/100)^2

# Calcular o ganho de peso total
dados$gpg <- dados$d_PesoParto - dados$a_pesopre

## O ganho de peso gestacional quantitativo 
dados$categoria1<- dados$gpg 

# Categorização do ganho de peso gestacional
dados$gpg_cat <-  for (i in 1:nrow(dados)) {
  if (dados$imc[i] < 18.5) {
    if (dados$gpg[i] < 12.5) {
      dados$categoria[i] <- "Abaixo"
    } else if (dados$gpg[i] >= 12.5 && dados$gpg[i] <= 18) {
      dados$categoria[i] <- "Dentro"
    } else {
      dados$categoria[i] <- "Acima"
    }
  } else if (dados$imc[i] >= 18.5 && dados$imc[i] < 25) {
    if (dados$gpg[i] < 11.5) {
      dados$categoria[i] <- "Abaixo"
    } else if (dados$gpg[i] >= 11.5 && dados$gpg[i] <= 16) {
      dados$categoria[i] <- "Dentro"
    } else {
      dados$categoria[i] <- "Acima"
    }
  } else if (dados$imc[i] >= 25 && dados$imc[i] < 30) {
    if (dados$gpg[i] < 7) {
      dados$categoria[i] <- "Abaixo"
    } else if (dados$gpg[i] >= 7 && dados$gpg[i] <= 11.5) {
      dados$categoria[i] <- "Dentro"
    } else {
      dados$categoria[i] <- "Acima"
    }
  } else {
    if (dados$gpg[i] < 5) {
      dados$categoria[i] <- "Abaixo"
    } else if (dados$gpg[i] >= 5 && dados$gpg[i] <= 9) {
      dados$categoria[i] <- "Dentro"
    } else {
      dados$categoria[i] <- "Acima"
    }
  }
}

# Exibição dos resultados
table(dados$categoria)
prop.table(table(dados$categoria))*100

# Reordenar a ordem das categorias  das varivaies 
dados$categoria <- factor(dados$categoria, levels = c("Dentro", "Abaixo", "Acima"))

dados %>% tabyl(categoria) %>% adorn_pct_formatting()

write.csv(dados, "DadosFabiano.csv", row.names = FALSE)
##################################################################################################################################################################################
########################################################### Criar categorias de IMC pregestacional ###############################################################################
##################################################################################################################################################################################
##################################################################################################################################################################################
dados$cat_imc <- cut(dados$imc, 
                     breaks = c(0, 18.5, 25, 30, Inf), 
                     labels = c("Baixo peso", "Peso normal", "Sobrepeso", "Obesidade"))

dados %>% tabyl(cat_imc) %>% adorn_pct_formatting()
##################################################################################################################################################################################
##################################################################################################################################################################################
########################################## Construcao de tabelas descritivas  das variveis preditoras e o Desfecho - GPG #########################################################
##################################################################################################################################################################################

############################################# Idade ##########################################################
# Transformando valores negativos em positivos
dados$a_idade <- abs(dados$a_idade)
# Removendo valores iguais a 0
dados <- dados[dados$a_idade != 0,]
kruskal.test(a_idade ~ categoria, data = dados)
########################################## calcular a média e o desvio padrão do Peso Pre-gestational###########################################################
summary(dados$a_pesopre)
kruskal.test(a_pesopre ~ categoria, data = dados)
###########################################  calcular a médiana e o desvio padrão do IMC-Pre-gestacional ###################################################### 
summary(dados$a_imcga)
summary(dados$b_imcga)
summary(dados$c_imcga)
kruskal.test(a_imcga ~ categoria, data = dados)
kruskal.test(b_imcga ~ categoria, data = dados)
kruskal.test(c_imcga ~ categoria, data = dados)
###########################################  calcular a médiana  da Percentual de gordura corporal (%) ######################################################### 
summary(dados$a_fmp)
kruskal.test(a_fmp ~ categoria, data = dados)
kruskal.test(b_fmp ~ categoria, data = dados)
kruskal.test(c_fmp ~ categoria, data = dados)
############################################# Idade gestacional em semana #######################################################################################
sort(dados$a_igusg , decreasing = T )
summary(dados$a_igusg)
summary(dados$b_igusg)
summary(dados$c_igusg)

kruskal.test(a_igusg ~ categoria, data = dados)
kruskal.test(b_igusg ~ categoria, data = dados)
kruskal.test(b_igusg ~ categoria, data = dados)
########################## Tabela de contigencia da Escolaridade  da gestante (em anos de estudos) ############################################################# 
sort(dados$a_escola, decreasing = T )
summary(dados$a_escola)
dados$a_escola[dados$a_escola == 90.09] <- NA

kruskal.test(a_escola ~ categoria, data = dados)
########################## Numero de partos anteriores  ############################################################# 
summary(dados$a_npari)

kruskal.test(a_npari~ categoria, data = dados)
######################### calcular a médiana e o IQR da Renda per capita em Reais ############################################################################### 
dados$a_rendpcr <- round(dados$a_rendpcr, 3)
summary(dados$a_rendpcr)

kruskal.test(a_rendpcr~ categoria, data = dados)
##################################### Tabela de contingencia da raca ########################################################################################### 
dados$a_cor <- ifelse(dados$a_cor == 1, "branco", "Nbranco")
dados$a_cor <- ifelse(dados$a_cor == "branco", 1, 0)

dados %>% tabyl(a_cor) %>% adorn_pct_formatting()
chisq.test(dados$a_cor, dados$categoria)
##################################### Tabela de contingencia Estado civil  ##################################################################################### 
####recodificando a variável a_civil em duas categorias
dados$a_civil <- ifelse(dados$a_civil %in% c(1 , 2), 1, 2) #### recodificar 2  categorias: 1-Casada ou em união estável, 2-  Solteira, separada ou viúva 

dados$a_civil <- factor(dados$a_civil, levels = c(1, 2), labels = c("Casada,UE", "Solteira/separada/viúva"))
dados$a_civil <- ifelse(dados$a_civil == "Casada,UE",0, ifelse(dados$a_civil== "Solteira/separada/viúva", 1, NA ))
dados %>% tabyl(a_civil) %>% adorn_pct_formatting()

chisq.test(dados$a_civil, dados$categoria)
############################################ # calcular a média e o desvio padrão da Actividade fisica  por GPG#################################################
dados$a_moderh <- ifelse(dados$a_moderh< 150, "inadequado", "adequado")
dados$a_moderh <- ifelse(dados$a_moderh == "inadequado", 1, 0)

dados$b_moderh <- ifelse(dados$b_moderh< 150, "inadequado", "adequado")
dados$b_moderh <- ifelse(dados$b_moderh == "inadequado", 1, 0)

dados$c_moderh <- ifelse(dados$c_moderh< 150, "inadequado", "adequado")
dados$c_moderh <- ifelse(dados$c_moderh == "inadequado", 1, 0)

dados %>% tabyl(a_moderh) %>% adorn_pct_formatting()
dados %>% tabyl(b_moderh) %>% adorn_pct_formatting()
dados %>% tabyl(c_moderh) %>% adorn_pct_formatting()

chisq.test(dados$a_moderh, dados$categoria)
chisq.test(dados$b_moderh, dados$categoria)
chisq.test(dados$c_moderh, dados$categoria)
############################################# Tabela de contingencia  do Habitio de Fumar por GPG ############################################################## 
dados$a_fumog <- factor(dados$a_fumog, levels = c(0, 1), labels = c("Não", "Sim"))
dados$b_fumog <- factor(dados$b_fumog, levels = c(0, 1), labels = c("Não", "Sim"))
dados$c_fumog <- factor(dados$c_fumog, levels = c(0, 1), labels = c("Não", "Sim"))

dados %>% tabyl(a_fumog) %>% adorn_pct_formatting()
dados %>% tabyl(b_fumog) %>% adorn_pct_formatting()
dados %>% tabyl(c_fumog) %>% adorn_pct_formatting()

chisq.test(dados$a_fumog, dados$categoria)
chisq.test(dados$b_fumog, dados$categoria)
chisq.test(dados$c_fumog, dados$categoria)
############################################# Tabela de contingencia  do Acolool  por GPG ###################################################################### 
dados$a_alcool <- factor(dados$a_alcool, levels = c(0, 1), labels = c("Não", "Sim"))
dados$b_alcool <- factor(dados$b_alcool, levels = c(0, 1), labels = c("Não", "Sim"))
dados$c_alcool <- factor(dados$c_alcool, levels = c(0, 1), labels = c("Não", "Sim"))

dados %>% tabyl(a_alcool) %>% adorn_pct_formatting()
dados %>% tabyl(b_alcool) %>% adorn_pct_formatting()
dados %>% tabyl(c_alcool) %>% adorn_pct_formatting()

chisq.test(dados$a_alcool, dados$categoria)
chisq.test(dados$b_alcool, dados$categoria)
chisq.test(dados$c_alcool, dados$categoria)
############################################# Tabela de contingencia  do Diabetes por GPG########################################################################
dados$a_agdm <- factor(dados$a_agdm, levels = c(0, 1), labels = c("Não", "Sim"))
dados$b_agdm <- factor(dados$b_agdm, levels = c(0, 1), labels = c("Não", "Sim"))
dados$c_agdm <- factor(dados$c_agdm, levels = c(0, 1), labels = c("Não", "Sim"))

dados %>% tabyl(a_agdm) %>% adorn_pct_formatting()
dados %>% tabyl(b_agdm) %>% adorn_pct_formatting()
dados %>% tabyl(c_agdm) %>% adorn_pct_formatting()

chisq.test(dados$a_agdm, dados$categoria)
chisq.test(dados$b_agdm, dados$categoria)
chisq.test(dados$c_agdm, dados$categoria)
################################################## Tabela de contingencia  do Hipertensao por GPG ############################################################## 
dados$a_aghas <- factor(dados$a_aghas, levels = c(0, 1), labels = c("Não", "Sim"))
dados$b_aghas <- factor(dados$b_aghas, levels = c(0, 1), labels = c("Não", "Sim"))
dados$c_aghas <- factor(dados$c_aghas, levels = c(0, 1), labels = c("Não", "Sim"))

dados %>% tabyl(a_aghas) %>% adorn_pct_formatting()
dados %>% tabyl(b_aghas) %>% adorn_pct_formatting()
dados %>% tabyl(c_aghas) %>% adorn_pct_formatting()

chisq.test(dados$a_aghas, dados$categoria)
chisq.test(dados$b_aghas, dados$categoria)
chisq.test(dados$c_aghas, dados$categoria)
############################################# Tabela de contingencia  Infeccao Urinaria por GPG ################################################################ 
dados$a_agui <- factor(dados$a_agui, levels = c(0, 1), labels = c("Não", "Sim"))
dados$b_agui <- factor(dados$b_agui, levels = c(0, 1), labels = c("Não", "Sim"))
dados$c_agui <- factor(dados$c_agui, levels = c(0, 1), labels = c("Não", "Sim"))

dados %>% tabyl(a_agui) %>% adorn_pct_formatting()
dados %>% tabyl(b_agui) %>% adorn_pct_formatting()
dados %>% tabyl(c_agui) %>% adorn_pct_formatting()

chisq.test(dados$a_agui, dados$categoria)
chisq.test(dados$b_agui, dados$categoria)
chisq.test(dados$c_agui, dados$categoria)

############################################# Tabela de contingencia  Cervicit/Vaginite por GPG################################################################ 
dados$a_agceva <- factor(dados$a_agceva, levels = c(0, 1), labels = c("Não", "Sim"))
dados$b_agceva <- factor(dados$b_agceva, levels = c(0, 1), labels = c("Não", "Sim"))
dados$c_agceva <- factor(dados$c_agceva, levels = c(0, 1), labels = c("Não", "Sim"))

dados %>% tabyl(a_agceva) %>% adorn_pct_formatting()
dados %>% tabyl(b_agceva) %>% adorn_pct_formatting()
dados %>% tabyl(c_agceva) %>% adorn_pct_formatting()

chisq.test(dados$a_agceva, dados$categoria)
chisq.test(dados$b_agceva, dados$categoria)
chisq.test(dados$c_agceva, dados$categoria)
################################################# Tabela de contingencia do PCR por GPG ########################################################################
summary(dados$a_pcr)
summary(dados$b_pcr)
summary(dados$c_pcr)

kruskal.test(a_pcr~ categoria, data = dados)
kruskal.test(b_pcr~ categoria, data = dados)
kruskal.test(c_pcr~ categoria, data = dados)
#################################################Tabela de Contingencia  da Hemoglobina por GPG ###############################################################
summary(dados$a_hb)
summary(dados$b_hb)
summary(dados$b_hb)

kruskal.test(a_hb~ categoria, data = dados)
kruskal.test(b_hb~ categoria, data = dados)
kruskal.test(c_hb~ categoria, data = dados)
#################################################Tabela de Contingencia  da Hemoglobina glicada por GPG #######################################################
############################################# heoglobina glicada ##############################################################################################
sort(dados$a_hba1c, decreasing = T)
dados$a_hba1c [dados$a_hba1c == 55.5] <- NA
summary(dados$a_hba1c)
summary(dados$b_hba1c)
summary(dados$c_hba1c)

kruskal.test(a_hba1c~ categoria, data = dados)
kruskal.test(b_hba1c~ categoria, data = dados)
kruskal.test(c_hba1c~ categoria, data = dados)
################################################ Tabela de contingencia da INSULINA por GPG #################################################################
summary(dados$a_insul)
summary(dados$b_insul)
summary(dados$c_insul)

kruskal.test(a_insul~ categoria, data = dados)
kruskal.test(b_insul~ categoria, data = dados)
kruskal.test(c_insul~ categoria, data = dados)
################################################# calcular a média e o desvio padrão da  Colesterol por GPG################################################
summary(dados$a_ct)
summary(dados$b_ct)
summary(dados$c_ct)

kruskal.test(a_ct~ categoria, data = dados)
kruskal.test(b_ct~ categoria, data = dados)
kruskal.test(c_ct~ categoria, data = dados)
################################################# calcular a média e o desvio padrão da HDL por GPG##########################################################
summary(dados$a_hdl)
summary(dados$b_hdl)
summary(dados$c_hdl)

kruskal.test(a_hdl~ categoria, data = dados)
kruskal.test(b_hdl~ categoria, data = dados)
kruskal.test(c_hdl~ categoria, data = dados)
########################################################### calcular a média e o desvio padrão da LDL por GPG################################################
summary(dados$a_ldl)
summary(dados$b_ldl)
summary(dados$c_ldl)

kruskal.test(a_ldl~ categoria, data = dados)
kruskal.test(b_ldl~ categoria, data = dados)
kruskal.test(c_ldl~ categoria, data = dados)
################################################# calcular a médiana e o IQR de Triaglicerideos  por GPG. ###################################################
summary(dados$a_tg)
summary(dados$b_tg)
summary(dados$c_tg)

kruskal.test(a_tg~ categoria, data = dados)
kruskal.test(b_tg~ categoria, data = dados)
kruskal.test(c_tg~ categoria, data = dados)

############################################################################################################################################################### 
############################################################################################################################################################### 
######################################################## Desfechos featis e neonatais  ########################################################################
############################################################################################################################################################### 

###################################################################Fetais #####################################################################################

################################################## Espessura do tecido subcutâneo do abdômen (TSA)#############################################################
summary(dados$b_tsausg)
summary(dados$c_tsausg)

kruskal.test(b_tsausg~ categoria, data = dados)
kruskal.test(c_tsausg~ categoria, data = dados)

################################################## Espessura do tecido subcutâneo do Braco (TSA)#############################################################
summary(dados$b_ttsbusg)
summary(dados$c_ttsbusg)

kruskal.test(b_ttsbusg~ categoria, data = dados)
kruskal.test(c_ttsbusg~ categoria, data = dados)
##################################################Área de tecido subcutâneo da coxa (TSC) ######################################################################
summary(dados$b_ttscusg)
summary(dados$c_ttscusg)

kruskal.test(b_ttscusg~ categoria, data = dados)
kruskal.test(c_ttscusg~ categoria, data = dados)
##################################################Peso fetal ######################################################################
summary(dados$a_pesousg)
summary(dados$b_pesousg)
summary(dados$c_pesousg)

kruskal.test(a_pesousg~ categoria, data = dados)
kruskal.test(b_pesousg~ categoria, data = dados)
kruskal.test(c_pesousg~ categoria, data = dados)
########################################################### RCIU ##################################################################################
# Função para calcular a restrição do crescimento intrauterino 2 trimestre 
clasif <- function(b_pesousg, b_igusg) {
  # Dados de referência do percentil 10
  dados_referencia <- data.frame(
    idade_gestacional = c(22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40),
    percentil_10 = c(481, 538, 602, 674, 757, 849, 951, 1065, 1190, 1326, 1473, 1630, 1795, 1967, 2144, 2321, 2495, 2663, 2818)
  )
  # Encontrar a linha correspondente à idade gestacional mais próxima
  linha <- dados_referencia[which.min(abs(dados_referencia$idade_gestacional - b_igusg)), ]
  
  # Verificar se as medidas estão abaixo do percentil 10
  classificacao <- ifelse(is.na(b_pesousg) | is.na(b_igusg)| b_igusg< 22, "Ausente",
                          ifelse(b_pesousg < linha$percentil_10, "Restrito", "Normal"))
  return(classificacao)
  }

# Criar uma coluna "clasif" usando a função clasif
dados$clasif <- mapply(clasif, dados$b_pesousg, dados$b_igusg)

# Substituir as categorias existentes por novas categorias
dados$ b_rciu <- ifelse(dados$clasif == "Normal", "normal",
                     ifelse(dados$clasif == "Restrito", "RCIU", NA))

dados$b_rciu <- ifelse(dados$b_rciu == "RCIU", 1, 0)
################################################################################################################################################
################### Função para calcular a restrição do crescimento intrauterino 3 trimestre ########################################################
clasif <- function(c_pesousg, c_igusg) {
  # Dados de referência do percentil 10
  dados_referencia <- data.frame(
    idade_gestacional = c(22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40),
    percentil_10 = c(481, 538, 602, 674, 757, 849, 951, 1065, 1190, 1326, 1473, 1630, 1795, 1967, 2144, 2321, 2495, 2663, 2818)
  )
  # Encontrar a linha correspondente à idade gestacional mais próxima
  linha <- dados_referencia[which.min(abs(dados_referencia$idade_gestacional - c_igusg)), ]
  
  # Verificar se as medidas estão abaixo do percentil 10
  classificacao <- ifelse(is.na(c_pesousg) | is.na(c_igusg)| c_igusg< 22, "Ausente",
                          ifelse(c_pesousg < linha$percentil_10, "Restrito", "Normal"))
  return(classificacao)
}

# Criar uma coluna "clasif" usando a função clasif
dados$clasif <- mapply(clasif, dados$c_pesousg, dados$c_igusg)

# Substituir as categorias existentes por novas categorias
dados$ c_rciu <- ifelse(dados$clasif == "Normal", "normal",
                        ifelse(dados$clasif == "Restrito", "RCIU", NA))

dados$c_rciu <- ifelse(dados$c_rciu == "RCIU", 1, 0)


dados %>% tabyl(b_rciu) %>% adorn_pct_formatting()
dados %>% tabyl(c_rciu) %>% adorn_pct_formatting()


chisq.test(dados$b_rciu, dados$categoria)
chisq.test(dados$c_rciu, dados$categoria)
################################################################### Neonatais ######################################################################
####################################################################################################################################################

##################################################% Massa gorda (%MG)##############################################################################
summary(dados$f_FMP)

kruskal.test(f_FMP~ categoria, data = dados)
##################################################% Massa livre de gordura (%MLG)##################################################################
summary(dados$f_FFMp)

kruskal.test(f_FFMp~ categoria, data = dados)
################################################## Prematutidade ###############################################################################
sort(dados$d_IGUSGrn , decreasing = T )
dados$d_IGUSGrn <- dados$d_IGUSGrn/ 7

dados$premat <- ifelse(dados$d_IGUSGrn < 37, "Prematuros", "Termos")
dados$premat <- ifelse(dados$premat == "Prematuros", 1, 0)

dados %>% tabyl(premat) %>% adorn_pct_formatting()

chisq.test(dados$premat, dados$categoria)
################################################## Apgar no 5° minuto #########################################################################
summary(dados$d_Apgar5)
dados$d_Apgar5 <- ifelse(dados$d_Apgar5 >= 7, "sim", "não")
dados$d_Apgar5 <- ifelse(dados$d_Apgar5 == "sim", 1, 0)

dados %>% tabyl(d_Apgar5) %>% adorn_pct_formatting()

chisq.test(dados$d_Apgar5, dados$categoria)
##################################################Comprimento ao nascer ########################################################################
summary(dados$d_CompNasc)

kruskal.test(d_CompNasc~ categoria, data = dados)
##################################################Peso ao nascer ###############################################################################
summary(dados$d_PesoNasc)

kruskal.test(d_PesoNasc~ categoria, data = dados)
##################################################Adequação do peso ao nascer GIG e PIG ########################################################################
dados$d_intergr
dados$d_intergr <- ifelse(dados$d_intergr == 1, "PIG", 
                           ifelse(dados$d_intergr == 2, "AIG",
                                  ifelse(dados$d_intergr == 3, "GIG", NA)))

dados$d_intergr <- ifelse(dados$d_intergr == "PIG", 1, ifelse(dados$d_intergr == "AIG",  0, ifelse(dados$d_intergr == "GIG",2, NA )))
                          
dados %>% tabyl(d_intergr) %>% adorn_pct_formatting()

chisq.test(dados$d_intergr, dados$categoria)

dados$gig <- ifelse(dados$d_intergr %in% c(1, 0), 0, ifelse(dados$d_intergr == 2, 1, dados$d_intergr))
dados %>% tabyl(gig) %>% adorn_pct_formatting()

dados$pig <-  ifelse(dados$d_intergr %in% c(2, 0), 0, ifelse(dados$d_intergr == 1, 1, dados$d_intergr))
dados %>% tabyl(pig) %>% adorn_pct_formatting()

##################################################Peso ao nascer categorico  ########################################################################
summary(dados$d_PesoNasc)
dados$d_bpn <- ifelse(dados$d_PesoNasc <2500, "Bpn", "normal")
dados$d_bpn <- ifelse(dados$d_bpn == "Bpn", 1, 0)

                           
dados %>% tabyl(d_bpn) %>% adorn_pct_formatting()
chisq.test(dados$d_bpn, dados$categoria)
###############################################################  Tabela descritiva para os tres trimestre ###############################################################

###################################### Descricao de todo o banco de dados  do pacote SKimmer ####################################################################
skim(dados)

#### Sumarizacao do Skimer 
dados %>% get_summary_stats("a_idade", "a_imcga","b_imcga","c_imcga","a_imcpg","a_fmp","b_fmp", "c_fmp", "a_igusg","b_igusg","c_igusg","a_escola",
                            "a_npari","a_npcomo", "a_rendpcr", "a_cor","a_civil","a_vigorh", "b_vigorh","c_vigorh", "a_moderh","b_moderh", "c_moderh","a_fumog",
                            "b_fumog", "c_fumog","a_alcool", "b_alcool","c_alcool","a_agdm","b_agdm","c_agdm", "a_aghas","b_aghas","c_aghas","a_agui","b_agui",
                            "c_agui", "a_agceva", "b_agceva","c_agceva","a_pcr","b_pcr","c_pcr","a_hb","b_hb", "c_hb","a_hba1c","b_hba1c","c_hba1c", "a_insul",
                            "b_insul", "c_insul", "a_ct", "b_ct", "c_ct", "a_hdl","b_hdl","c_hdl", "a_ldl","b_ldl","c_ldl","a_tg","b_tg", "c_tg","a_tipoparto",
                            "b_tsausg","b_tsausg","c_tsausg", "b_ttsbusg","c_ttsbusg","b_ttscusg", "c_ttscusg","a_pesousg", "b_pesousg","c_pesousg","f_FMP", 
                            "f_FFMp", "d_Apgar5","d_CompNasc","d_PesoNasc", "d_intergr...1507", "d_SexoRN", "a_estat1", "a_pesopre", "d_PesoParto","d_InPond",
                            "d_perintg", "d_IntercRN", "d_IGUSGrn")
###############################################################################################################################################################

################################################################################################################################################################################
###############################################################   EMPILHAR DADOS #########################################################################################
  
######## criando variaveis vazias ###########################
dados[, c("a_tsausg", "a_ttsbusg", "a_ttscusg", "a_rciu", "a_FMP", "b_FMP", 
          "a_FFMp", "b_FFMp", "a_Apgar5", "b_Apgar5", "a_PesoNasc", "b_PesoNasc", 
          "a_CompNasc", "b_CompNasc", "a_intergr", "b_intergr", "a_bpn", "b_bpn",
          "a_categoria", "b_categoria")] <- NA                          

# Criando um data frame com todas as colunas empilhadas
Base_Dados<- dados
Base_Dados <- Base_Dados %>%  mutate_all(as.numeric)

Dados_Empilhados <- data.frame (
  id = rep (Base_Dados$id, 3),
  idade = rep(Base_Dados$a_idade, 3),   # Repetir a coluna Idade 3 vezes
  imcpg = rep(Base_Dados$a_imcpg, 3),
  imcga = stack(Base_Dados[, c("a_imcga", "b_imcga", "c_imcga")])$values, 
  fmp = stack(Base_Dados[, c("a_fmp", "b_fmp", "c_fmp")])$values,
  igusg = stack(Base_Dados[, c("a_igusg", "b_igusg", "c_igusg")])$values,
  escola = rep(Base_Dados$a_escola, 3),
  npari = rep(Base_Dados$a_npari, 3),
  npcomo = rep(Base_Dados$a_npcomo, 3),
  rendpcr = rep(Base_Dados$a_rendpcr, 3),
  cor = rep(Base_Dados$a_cor, 3),
  civil = rep(Base_Dados$a_civil, 3),
  moderh = stack(Base_Dados[, c("a_moderh", "b_moderh", "c_moderh")])$values,
  pig = stack(Base_Dados[, c("pig", "pig", "pig")])$values,
  gig = stack(Base_Dados[, c("gig", "gig", "gig")])$values,
  fumog = stack(Base_Dados[, c("a_fumog", "b_fumog","c_fumog")])$values,
  alcool = stack(Base_Dados[, c("a_alcool", "b_alcool", "c_alcool")])$values,
  agdm = stack(Base_Dados[, c("a_agdm", "b_agdm", "c_agdm")])$values,
  aghas = stack(Base_Dados[, c("a_aghas", "b_aghas", "c_aghas")])$values,
  agui = stack(Base_Dados[, c("a_agui", "b_agui", "c_agui")])$values,
  agceva = stack(Base_Dados[, c("a_agceva", "b_agceva", "c_agceva")])$values,
  pcr = stack(Base_Dados[, c("a_pcr", "b_pcr", "c_pcr")])$values,
  hb = stack(Base_Dados[, c("a_hb", "b_hb", "c_hb")])$values,
  hba1c = stack(Base_Dados[, c("a_hba1c", "b_hba1c", "c_hba1c")])$values,
  insul = stack(Base_Dados[, c("a_insul", "b_insul", "c_insul")])$values,
  ct = stack(Base_Dados[, c("a_ct", "b_ct", "c_ct")])$values,
  hdl = stack(Base_Dados[, c("a_hdl", "b_hdl", "c_hdl")])$values,
  tg = stack(Base_Dados[, c("a_tg", "b_tg", "c_tg")])$values,
  tsausg = stack(Base_Dados[, c("a_tsausg", "b_tsausg", "c_tsausg")])$values,
  tsbusg = stack(Base_Dados[, c("a_ttsbusg","b_ttsbusg","c_ttsbusg")])$values,  
  ttscusg = stack(Base_Dados[, c("a_ttscusg", "b_ttscusg", "c_ttscusg")])$values,   
  Insul = stack(Base_Dados[, c("a_pesousg", "b_pesousg", "c_pesousg")])$values,
  rciu = stack(Base_Dados[, c("a_rciu", "b_rciu", "c_rciu")])$values,
  premat = stack(Base_Dados[, c("premat", "premat", "premat")])$values,
  fmp = stack(Base_Dados[, c("a_FMP", "b_FMP", "f_FMP")])$values,    
  FFMp = stack(Base_Dados[, c("a_FFMp", "b_FFMp", "f_FFMp")])$values,   
  Apgar5 = stack(Base_Dados[, c("d_Apgar5", "d_Apgar5", "d_Apgar5")])$values, 
  PesoNasc = stack(Base_Dados[, c("d_PesoNasc", "d_PesoNasc", "d_PesoNasc")])$values, 
  pesofet = stack(Base_Dados[, c("a_pesousg", "b_pesousg", "c_pesousg")])$values, 
  CompNasc = stack(Base_Dados[, c("d_CompNasc", "d_CompNasc", "d_CompNasc")])$values,  
  intergr = stack(Base_Dados[, c("d_intergr", "d_intergr", "d_intergr")])$values, 
  bpn = stack(Base_Dados[, c("d_bpn", "d_bpn", "d_bpn")])$values,
  gpg = stack(Base_Dados[, c("categoria","categoria", "categoria")])$values)

# Criando a variável tempo
Dados_Empilhados$trimestre <- rep(c(1, 2, 3), each = nrow(Base_Dados))

dados2 <- dados 
dados <- Dados_Empilhados

##############################################################################################################################################################################
############################################################################# Modelagens  Por GEE ############################################################################
##############################################################################################################################################################################
install.packages("gee")
install.packages("MESS")
install.packages("geepack")
library(geepack)
library(gee)
library(MESS)

###########################################################################################################################################################################
################################################################# Modelagens Brutas #######################################################################################
################################################################ Crescimento Fetal ########################################################################################
dados$gpg <- as.factor(dados$gpg)
modelo_rciu <- geeglm (rciu ~  gpg , data = dados,  binomial(link = "logit"), corstr = "ar1", id= id)
summary (modelo_rciu)

# Cálculo dos ORs e ICs
cc <- confint.default(modelo_rciu)
ctab <- cbind(est = coef(modelo_rciu), cc)
ORs <- exp(ctab)
ORs

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
or_rciu <- data.frame(cfetal = "rciu", or.ci = or.ci)
view(or_rciu)

     
################### Prematuridade ########################################################################
modelo_premat <- geeglm(premat ~ gpg, data = dados,  binomial(link = "logit"), corstr = "ar1", id= id)
summary(modelo_premat)

# Cálculo dos ORs e ICs
cc <- confint.default(modelo_premat)
ctab <- cbind(est = coef(modelo_premat), cc)
ORs <- exp(ctab)
ORs

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
or_premat <- data.frame(cfetal = "premat", or.ci = or.ci)
view(or_premat)

################### Apgar no 5° minuto ###################################################################
modelo_Apgar5 <- geeglm(Apgar5 ~ gpg, data = dados,  binomial(link = "logit"), corstr = "ar1", id= id)
summary(modelo_Apgar5)

# Cálculo dos ORs e ICs
cc <- confint.default(modelo_Apgar5)
ctab <- cbind(est = coef(modelo_Apgar5), cc)
ORs <- exp(ctab)
ORs

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
or_Apgar5 <- data.frame(cfetal = "Apgar5", or.ci = or.ci)
view(or_Apgar5)

#####################################   PIG ##################################################################################
modelo_pig <- geeglm(pig ~ gpg, data = dados,  binomial(link = "logit"), corstr = "ar1", id= id)
summary(modelo_Apgar5)

# Cálculo dos ORs e ICs
cc <- confint.default(modelo_pig)
ctab <- cbind(est = coef(modelo_pig), cc)
ORs <- exp(ctab)
ORs

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
or_pig <- data.frame(cfetal = "pig", or.ci = or.ci)
view(or_pig)

#####################################   GIG ##################################################################################
modelo_gig <- geeglm(gig ~ gpg, data = dados,  binomial(link = "logit"), corstr = "ar1", id= id)
summary(modelo_gig)

# Cálculo dos ORs e ICs
cc <- confint.default(modelo_gig)
ctab <- cbind(est = coef(modelo_gig), cc)
ORs <- exp(ctab)
ORs

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
or_gig <- data.frame(cfetal = "gig", or.ci = or.ci)
view(or_gig)

################### Baixo Peso ao nascer #######################################################################
modelo_bpn <- geeglm(bpn ~ gpg, data = dados,  binomial(link = "logit"), corstr = "ar1", id= id)
summary(modelo_bpn)

# Cálculo dos ORs e ICs
cc <- confint.default(modelo_bpn)
ctab <- cbind(est = coef(modelo_bpn), cc)
ORs <- exp(ctab)
ORs

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
or_bpn <- data.frame(bpn = "bpn", or.ci = or.ci)
view(or_bpn)
######################################## Peso fetal ###################################################################

modelo_pesofet <- geeglm(pesofet ~ gpg, data = dados,  gaussian, corstr = "ar1", id= id)
summary(modelo_pesofet)
confint(modelo_pesofet)

# Cálculo dos ORs e ICs
cc <- confint.default(modelo_pesofet)
ctab <- cbind(est = coef(modelo_pesofet), cc)
ORs <- (ctab)
ORs

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
or_pesofet <- data.frame (pesofet = "pesofet", or.ci = or.ci)
view(or_pesofet)
########################################  Comprimento ao nascer ###################################################################
modelo_CompNasc <- geeglm(CompNasc ~ gpg, data = dados,  gaussian, corstr = "ar1", id= id)
summary(modelo_CompNasc)

# Cálculo dos ORs e ICs
cc <- confint.default(modelo_CompNasc)
ctab <- cbind(est = coef(modelo_CompNasc), cc)
ORs <- (ctab)
ORs

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
or_CompNasc <- data.frame (CompNasc = "CompNasc", or.ci = or.ci)
view(or_CompNasc)

######################################## Peso ao nascer ###################################################################
modelo_PesoNasc <- geeglm(PesoNasc ~ gpg, data = dados,  gaussian, corstr = "ar1", id= id)
summary(modelo_PesoNasc)

# Cálculo dos ORs e ICs
cc <- confint.default(modelo_PesoNasc)
ctab <- cbind(est = coef(modelo_PesoNasc), cc)
ORs <- (ctab)
ORs

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
or_PesoNasc <- data.frame (PesoNasc = "PesoNasc", or.ci = or.ci)
view(or_PesoNasc)

######################################## Espessura do tecido subcutâneo do abdômen (TSA)###################################################################

modelo_tsausg <- geeglm(tsausg ~ gpg, data = dados,  gaussian , corstr = "ar1", id= id)
summary(modelo_tsausg)

# Cálculo dos ORs e ICs
cc <- confint.default(modelo_tsausg)
ctab <- cbind(est = coef(modelo_tsausg), cc)
ORs <- (ctab)
ORs

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
or_tsausg <- data.frame (tsausg = "tsausg", or.ci = or.ci)
view(or_tsausg)

######################################## Área de tecido subcutâneo do braço (TSB) ###################################################################
modelo_tsbusg <- geeglm(tsbusg ~ gpg, data = dados, gaussian , corstr = "ar1", id= id)
summary(modelo_tsbusg)

# Cálculo dos ORs e ICs
cc <- confint.default(modelo_tsbusg)
ctab <- cbind(est = coef(modelo_tsbusg), cc)
ORs <- (ctab)
ORs

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
or_tsbusg <- data.frame (tsbusg = "tsbusg", or.ci = or.ci)
view(or_tsbusg)

######################################## Área de tecido subcutâneo da coxa (TSC)###################################################################
modelo_ttscusg <- geeglm(ttscusg ~ gpg, data = dados,  gaussian , corstr = "ar1", id= id)
summary(modelo_ttscusg)

# Cálculo dos ORs e ICs
cc <- confint.default(modelo_ttscusg)
ctab <- cbind(est = coef(modelo_ttscusg), cc)
ORs <- (ctab)
ORs

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
or_ttscusg <- data.frame (ttscusg = "ttscusg", or.ci = or.ci)
view(or_ttscusg)

######################################## Porcentagem de Massa gorda (%MG) ###################################################################
modelo_fmp <- geeglm(fmp~ gpg, data = dados,  gaussian, corstr = "ar1", id= id)
summary(modelo_fmp)

# Cálculo dos ORs e ICs
cc <- confint.default(modelo_fmp)
ctab <- cbind(est = coef(modelo_fmp), cc)
ORs <-(ctab)
ORs

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
or_fmp<- data.frame (fmp = "fmp", or.ci = or.ci)
view(or_fmp)

######################################## Porcentagem de Massa livre de gordura (%MLG) ###################################################################
modelo_FFMp<- geeglm(FFMp ~ gpg, data = dados,  gaussian , corstr = "ar1", id= id)
summary(modelo_FFMp)

# Cálculo dos ORs e ICs
cc <- confint.default(modelo_FFMp)
ctab <- cbind(est = coef(modelo_FFMp), cc)
ORs <- (ctab)
ORs

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
or_FFMp<- data.frame (FFMp = "FFMp", or.ci = or.ci)
view(or_FFMp)

###################Tabela Final com todas OR e IC#######################################################################
or_total <- rbind(or_intergra, or_d_IGUSGrn, or_d_Apgar5, or_intergrc ,or_bpn )
view(or_total)
write.table(or_total, "table_or_ci.txt",
            sep="\t", row.names=F)


or_total
or_intergrd
or_bpn

############################################################################################################################################################################
############################################################################# Modelagens Ajustada ##########################################################################
# Calculando as medianas
median_hb <- median(dados$hb, na.rm = TRUE)
median_hba1c <- median(dados$hba1c, na.rm = TRUE)
median_tg <- median(dados$tg, na.rm = TRUE)
median_hdl <- median(dados$hdl, na.rm = TRUE)
median_ct <- median(dados$ct, na.rm = TRUE)
median_rendpcr <- median(dados$rendpcr, na.rm = TRUE)
median_igusg <- median(dados$igusg, na.rm = TRUE)

# Substituindo os valores faltantes pelas medianas
dados$hb[is.na(dados$hb)] <- median_hb
dados$hba1c[is.na(dados$hba1c)] <- median_hba1c
dados$tg[is.na(dados$tg)] <- median_tg
dados$hdl[is.na(dados$hdl)] <- median_hdl
dados$ct[is.na(dados$ct)] <- median_ct
dados$rendpcr[is.na(dados$rendpcr)] <- median_rendpcr
dados$igusg[is.na(dados$igusg)] <- median_igusg


# Função para calcular a moda ignorando valores NA
calculate_mode <- function(x) {
  tbl <- table(x)
  mode_val <- names(tbl)[which.max(tbl)]
  return(mode_val)
}

# Substituir valores NA pela moda de cada variável
dados$moderh[is.na(dados$moderh)] <- calculate_mode(dados$moderh[!is.na(dados$moderh)])
dados$fumog[is.na(dados$fumog)] <- calculate_mode(dados$fumog[!is.na(dados$fumog)])
dados$alcool[is.na(dados$alcool)] <- calculate_mode(dados$alcool[!is.na(dados$alcool)])
dados$agdm[is.na(dados$agdm)] <- calculate_mode(dados$agdm[!is.na(dados$agdm)])
dados$aghas[is.na(dados$aghas)] <- calculate_mode(dados$aghas[!is.na(dados$aghas)])
dados$agui[is.na(dados$agui)] <- calculate_mode(dados$agui[!is.na(dados$agui)])
dados$agceva[is.na(dados$agceva)] <- calculate_mode(dados$agceva[!is.na(dados$agceva)])
dados$npari[is.na(dados$npari)] <- calculate_mode(dados$npari[!is.na(dados$npari)])



#######################Crescimento Fetal ########################################################################
modelo_rciu <- geeglm (rciu ~  gpg + idade+ imcpg + igusg + npcomo + npari+ cor + fumog + civil+ hb + hba1c + tg + hdl + ct 
                       + moderh +alcool+ agdm+ aghas  , data = dados,  binomial(link = "logit"), corstr = "ar1", id= id)
summary(modelo_rciu)

# Cálculo dos ORs e ICs
cc <- confint.default(modelo_rciu)
ctab <- cbind(est = coef(modelo_rciu), cc)
ORs <- exp(ctab)
ORs

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
or_rciu <- data.frame(cfetal = "rciu", or.ci = or.ci)
view(or_rciu)


################### Prematuridade ########################################################################
modelo_premat <- geeglm(premat ~ gpg + idade+ imcpg + igusg + npcomo + npari+ cor + fumog + civil+ hb +  hba1c + tg + hdl + ct 
                        + moderh +alcool+ agdm+ aghas , data = dados,  binomial(link = "logit"), corstr = "ar1", id= id)
summary(modelo_premat)

# Cálculo dos ORs e ICs
cc <- confint.default(modelo_premat)
ctab <- cbind(est = coef(modelo_premat), cc)
ORs <- exp(ctab)
ORs

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
or_premat <- data.frame(cfetal = "premat", or.ci = or.ci)
view(or_premat)

################### Apgar no 5° minuto ###################################################################
modelo_Apgar5 <- geeglm(Apgar5 ~ gpg +idade+ imcpg + igusg + npcomo + npari+ cor + fumog + civil+ hb +  hba1c + tg + hdl + ct 
                        + moderh +alcool+ agdm+ aghas  , data = dados,   binomial(link = "logit"), corstr = "ar1", id= id)
summary(modelo_Apgar5)

# Cálculo dos ORs e ICs
cc <- confint.default(modelo_Apgar5)
ctab <- cbind(est = coef(modelo_Apgar5), cc)
ORs <- exp(ctab)
ORs

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
or_Apgar5 <- data.frame(cfetal = "Apgar5", or.ci = or.ci)
view(or_Apgar5)

#############################################   PIG ##################################################################################
modelo_pig <- geeglm(pig ~ gpg +idade+ imcpg + igusg + npcomo + npari+ cor + fumog + civil+ hb +  hba1c + tg + hdl + ct 
                     + moderh +alcool+ agdm+ aghas , data = dados,  binomial(link = "logit"), corstr = "ar1", id= id)
summary(modelo_Apgar5)

# Cálculo dos ORs e ICs
cc <- confint.default(modelo_pig)
ctab <- cbind(est = coef(modelo_pig), cc)
ORs <- exp(ctab)
ORs

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
or_pig <- data.frame(cfetal = "pig", or.ci = or.ci)
view(or_pig)

################################################   GIG ##################################################################################
modelo_gig <- geeglm( gig ~ gpg +idade+ imcpg + igusg + npcomo + npari+ cor + fumog + civil+ hb +  hba1c + tg + hdl + ct 
                     + moderh +alcool+ agdm+ aghas , data = dados,  binomial(link = "logit"), corstr = "ar1", id= id)
summary(modelo_gig)

# Cálculo dos ORs e ICs
cc <- confint.default(modelo_gig)
ctab <- cbind(est = coef(modelo_gig), cc)
ORs <- exp(ctab)
ORs

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
or_gig <- data.frame(cfetal = "gig", or.ci = or.ci)
view(or_gig)

################### Baixo Peso ao nascer #######################################################################
modelo_bpn <- geeglm(bpn ~ gpg +idade+ imcpg + igusg + npcomo + npari+ cor + fumog + civil+ hb +  hba1c + tg + hdl + ct 
                     + moderh +alcool+ agdm+ aghas , data = dados,  binomial(link = "logit"), corstr = "ar1", id= id)
summary(modelo_bpn)

# Cálculo dos ORs e ICs
cc <- confint.default(modelo_bpn)
ctab <- cbind(est = coef(modelo_bpn), cc)
ORs <- exp(ctab)
ORs

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
or_bpn <- data.frame(bpn = "bpn", or.ci = or.ci)
view(or_bpn)
######################################## Peso fetal ###################################################################

modelo_pesofet <- geeglm(pesofet ~ gpg +idade+ imcpg + igusg + npcomo + npari+ cor + fumog + civil+ hb +  hba1c + tg + hdl + ct 
                         + moderh +alcool+ agdm+ aghas , data = dados,  gaussian, corstr = "ar1", id= id)
summary(modelo_pesofet)

# Cálculo dos ORs e ICs
cc <- confint.default(modelo_pesofet)
ctab <- cbind(est = coef(modelo_pesofet), cc)
ORs <- (ctab)
ORs

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
or_pesofet <- data.frame (pesofet = "pesofet", or.ci = or.ci)
view(or_pesofet)
########################################  Comprimento ao nascer ###################################################################
modelo_CompNasc <- geeglm(CompNasc ~ gpg +idade+ imcpg + igusg + npcomo + npari+ cor + fumog + civil+ hb +  hba1c + tg + hdl + ct 
                          + moderh +alcool+ agdm+ aghas , data = dados,  gaussian, corstr = "ar1", id= id)
summary(modelo_CompNasc)

# Cálculo dos ORs e ICs
cc <- confint.default(modelo_CompNasc)
ctab <- cbind(est = coef(modelo_CompNasc), cc)
ORs <- (ctab)
ORs

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
or_CompNasc <- data.frame (CompNasc = "CompNasc", or.ci = or.ci)
view(or_CompNasc)

######################################## Peso ao nascer ###################################################################
modelo_PesoNasc <- geeglm(PesoNasc ~ gpg +idade+ imcpg + igusg + npcomo + npari+ cor + fumog + civil+ hb +  hba1c + tg + hdl + ct 
                          + moderh +alcool+ agdm+ aghas , data = dados,  gaussian, corstr = "ar1", id= id)
summary(modelo_PesoNasc)

# Cálculo dos ORs e ICs
cc <- confint.default(modelo_PesoNasc)
ctab <- cbind(est = coef(modelo_PesoNasc), cc)
ORs <- (ctab)
ORs

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
or_PesoNasc <- data.frame (PesoNasc = "PesoNasc", or.ci = or.ci)
view(or_PesoNasc)

######################################## Espessura do tecido subcutâneo do abdômen (TSA)###################################################################

modelo_tsausg <- geeglm(tsausg ~ gpg +idade+ imcpg + igusg + npcomo + npari+ cor + fumog + civil+ hb +  hba1c + tg + hdl + ct 
                        + moderh +alcool+ agdm+ aghas , data = dados,  gaussian , corstr = "ar1", id= id)
summary(modelo_tsausg)

# Cálculo dos ORs e ICs
cc <- confint.default(modelo_tsausg)
ctab <- cbind(est = coef(modelo_tsausg), cc)
ORs <- (ctab)
ORs

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
or_tsausg <- data.frame (tsausg = "tsausg", or.ci = or.ci)
view(or_tsausg)

######################################## Área de tecido subcutâneo do braço (TSB) ###################################################################
modelo_tsbusg <- geeglm(tsbusg ~ gpg +idade+ imcpg + igusg + npcomo + npari+ cor + fumog + civil+ hb +  hba1c + tg + hdl + ct 
                        + moderh +alcool+ agdm+ aghas , data = dados, gaussian , corstr = "ar1", id= id)
summary(modelo_tsbusg)

# Cálculo dos ORs e ICs
cc <- confint.default(modelo_tsbusg)
ctab <- cbind(est = coef(modelo_tsbusg), cc)
ORs <- (ctab)
ORs

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
or_tsbusg <- data.frame (tsbusg = "tsbusg", or.ci = or.ci)
view(or_tsbusg)

######################################## Área de tecido subcutâneo da coxa (TSC)###################################################################
modelo_ttscusg <- geeglm(ttscusg ~ gpg+idade+ imcpg + igusg + npcomo + npari+ cor + fumog + civil+ hb +  hba1c + tg + hdl + ct 
                         + moderh +alcool+ agdm+ aghas , data = dados,  gaussian , corstr = "ar1", id= id)
summary(modelo_ttscusg)

# Cálculo dos ORs e ICs
cc <- confint.default(modelo_ttscusg)
ctab <- cbind(est = coef(modelo_ttscusg), cc)
ORs <- (ctab)
ORs

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
or_ttscusg <- data.frame (ttscusg = "ttscusg", or.ci = or.ci)
view(or_ttscusg)

######################################## Porcentagem de Massa gorda (%MG) ###################################################################
modelo_fmp <- geeglm(fmp ~ gpg +idade+ imcpg + igusg + npcomo + npari+ cor + fumog + civil+ hb +  hba1c + tg + hdl + ct 
                     + moderh +alcool+ agdm+ aghas , data = dados,  gaussian , corstr = "ar1", id= id)
summary(modelo_fmp)

# Cálculo dos ORs e ICs
cc <- confint.default(modelo_fmp)
ctab <- cbind(est = coef(modelo_fmp), cc)
ORs <- (ctab)
ORs

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
or_fmp<- data.frame (fmp = "fmp", or.ci = or.ci)
view(or_fmp)

######################################## Porcentagem de Massa livre de gordura (%MLG) ###################################################################
modelo_FFMp<- geeglm(FFMp ~ gpg +idade+ imcpg + igusg + npcomo + npari+ cor + fumog + civil+ hb +  hba1c + tg + hdl + ct 
                     + moderh +alcool+ agdm+ aghas , data = dados,  gaussian , corstr = "ar1", id= id)
summary(modelo_FFMp)

# Cálculo dos ORs e ICs
cc <- confint.default(modelo_FFMp)
ctab <- cbind(est = coef(modelo_FFMp), cc)
ORs <- (ctab)
ORs

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
or_FFMp<- data.frame (FFMp = "FFMp", or.ci = or.ci)
view(or_FFMp)

###################Tabela Final com todas OR e IC##################################################################################################
or_total <- rbind(or_intergra, or_d_IGUSGrn, or_d_Apgar5, or_intergrc ,or_bpn )
view(or_total)
write.table(or_total, "table_or_ci.txt",
            sep="\t", row.names=F)


or_total
or_intergrd
or_bpn


view(result_d_PesoNasc)