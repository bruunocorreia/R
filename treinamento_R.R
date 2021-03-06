
# Carregando biblioteca para manipula��o de dados ----------------------------

install.packages("dplyr") #instalar biblioteca
library(dplyr) #carregar biblioteca

install.packages("pacman")
library(pacman)
p_load(magrittr)

#Criar um data_frame pela biblioteca Dplyr
nisi_dev <- data.frame(nome= c("Willzao","Bet�o","Vin�oReporter","Bruno"),
                       idade= c(25,23,21,NA),
                       cid = c("z32","a15","j13","z01"),
                       cargo = c("analista","analista","analista","analista"))

#Verificar estat�stica da tabela
summary(nisi_dev)

#verificar tipos das vari�veis da tabela
str(nisi_dev)


# Diferentes formas de acessar linhas e colunas  --------------------------

nisi_dev[1:3,] #acessar da linha 1 a 3 e todas as colunas
nisi_dev[,1] #acessar todas as linhas e a coluna 1

nisi_dev[,c("nome","idade")] #todas as linhas e as colunas nome e idade
nisi_dev[1,c("nome","cid")] #linha1 e colunas nome e cid

nisi_dev$nome #acessando a coluna nome
nisi_dev$idade #acessando a coluna idade

#3 formas de acessar a primeira coluna no R
nisi_dev[,"nome"]
nisi_dev[,1]
nisi_dev$nome

View(nisi_dev) #enxergar a tabela criada

# Exerc�cio 1 - Acessar colunas e linhas do data_frame --------------------

starwars <- starwars #carregando a base starwars
str(starwars) #olhando os tipos de vari�veis de starwars
View(starwars)

#1) acessar as colunas name,mass e hair_color 
starwars[,c("name","height","mass")]
starwars[,1:3]

#2) acessar todas as colunas e as primeiras 5 linhas do data_frame
starwars[1:5,]

#3) acessar primeira linha e primeira coluna do data_frame starwars
starwars[1,1]
starwars[,"name"]


# Explorando a biblioteca Dplyr -------------------------------------------

#Fun��es base para c�lculos
mean(nisi_dev$idade,na.rm = T) #c�lculo da m�dia dos n�o nulos
median(nisi_dev$idade,na.rm = T) #c�lculo da mediana dos n�o nulos
sd(nisi_dev$idade,na.rm = T) #c�lculo do desvio padr�o dos n�o nulos
max(nisi_dev$idade,na.rm = T) #c�lculo do m�ximo dos n�o nulos
min(nisi_dev$idade,na.rm = T) #c�lculo do m�nimo padr�o dos n�o nulos
summary(nisi_dev$idade,na.rm = T) #resumo das medidas 

#Filtro (dplyr)
nisi_dev %>% filter(idade < 25) #Filtrando idade menos que 25
nisi_dev %>% filter(cargo == "analista") #filtrando cargo analista

#Filtro (which)
index <- which(nisi_dev$nome =='Willzao') #filtra o n�mero da linha que o Will est�
nisi_dev[index,]

#Filtro de valores nulos (NAs)
nisi_dev %>% filter(!is.na(idade)) #vai tirar todos que possuem NA na idade
nisi_dev %>% filter(!is.na(idade) & idade > 22) #Sem NAs e com idade maior que 22anos

#Filtro com %in%
lista <- c("Bet�o","Vin�oReporter")
nisi_dev %>% filter(nome %in% lista)

#Comando Select
select(nisi_dev,nome:cid) #selecionar da coluna nome at� cid
select(nisi_dev,2:3)      #selecionar da coluna 2 at� a 3

#Carregar biblioteca de Texto
p_load(stringr)

#Filtros com opera��es l�gicas
nisi_dev %>% filter(idade <25 | str_detect(nome,'B')) #Menor que25 anos ou nome com a letra B
nisi_dev %>% select(idade,cargo) %>% filter(idade <25 & idade >=23) #idade e cargo,idade de 23a25
nisi_dev %>% filter(cargo == "analista" | idade == 23) #Cargo analista ou idade = 23 anos
nisi_dev %>% select(-idade) #seleciona todas colunas menos a de idade
nisi_dev %>% select(-one_of(c("nome","cargo"))) #seleciona todas colunas menos nome e cargo

#filtrando linhas que possuem idade < 24 anos sem Dplyr
nisi_dev[nisi_dev["idade"]<24,] 

#Operador %<>% realiza os procedimentos e atribui o valor a vari�vel
nisi_dev2 <- nisi_dev
nisi_dev2 %<>% filter(idade <24) #retira quem tem idade <24 da base e grava
View(nisi_dev2) #Visualizando a tabela

#Criando uma nova coluna com o comando MUTATE do dplyr
nisi_dev %>% mutate(ano_nascimento = 2019-idade) #exibindo data_frame com uma coluna criada de ano de nascimento
nisi_dev %<>% mutate(ano_nascimento = 2019-idade) #adicionando nova coluna ao data_frame

#Cortar peda�os de uma vari�vel texto
substr("Caneta azul",1,6) #cortando as 6 primeiras letras da palavra
substr(nisi_dev$nome,1,3) #cortando as 3 primeiras letras da coluna nome do data_frame

#Usando Mutate com substr
nisi_dev %<>% mutate(cid_letra=substr(cid,1,1)
                    ,cid_numero=substr(cid,2,3))

#If e Else -- Comando Se
ifelse(nisi_dev["nome"]=='Bruno','achou','n�o achou') 

#Select CAse
nisi_dev["nome"] <- case_when(nisi_dev$nome == "Willzao" ~ "Wilerson",
                              nisi_dev$nome == "Bet�o" ~ "Jos� Roberto",
                              nisi_dev$nome  == "Vin�oReporter"~"Vinicius",
                              TRUE ~"Bruno Alves")

nisi_dev %>% mutate(idade = ifelse(idade == 23 
                                   ,mean(idade,na.rm = T) #se verdadeiro
                                   ,idade)) #se falso

nisi_dev %>% select(idade) %>% sum(na.rm=TRUE) #soma toas as idade n�o nulas

#ordenar (order by)
nisi_dev %>% arrange(nome) #orderna por nome usando dplyr
nisi_dev[order(nisi_dev$nome),] #ordena por nome sem usar nenhuma biblioteca

nisi_dev %>% arrange(desc(idade)) #ordernar por idade decrescente

#distintos
nisi_dev %>% distinct(nome) #distinct com dplyr
unique(nisi_dev["nome"]) #distinct sem pacote

#contagem 
nisi_dev %>% distinct(nome) %>% count() #contando nomes distintos
count(unique(nisi_dev["nome"])) #contando nomes distintos sem pacote
nisi_dev %>% filter(cargo =='analista') %>% count() #contando analistas

#Apagando coluna inteira do data frame
nisi_dev$cargo = NULL #apagando a coluna cargo do data frame

#Exemplo de funcoes utilizando a base starwars
starwars %>% filter(hair_color == "black" & skin_color == "yellow") %>% select(name)

#exemplo
starwars_humanos <- starwars %>%
                  mutate(height = as.character(height)) %>%
                  select(name,height,mass,birth_year,gender,species) %>%
                  filter(species == "Human" & gender == "female")

# Fun��es, group by, joins ------------------------------------------------

#Fun��es
imc <- function(altura,massa){return(massa/altura^2)} #fun��o para c�lculo do IMC
imc(1.76,75) #Imc Bruno
imc(starwars$height,starwars$mass) #Imc starwars

primeira_3letras <- function(nome) {return (substr(nome,1,2))} #Fun��o que retorno 3 primeiras letras
primeira_3letras("Wilerson")

Potencia_RaizQuadrada <- function(x1,x2) {return(list(x1*x1,sqrt(x1)))}
Potencia_RaizQuadrada(25) #fun��o gambiarra que retorna uma lista

#Group By/Tabela Din�mica
starwars %>% group_by(gender) %>% 
  summarise(media_atura = mean(height), media_massa = mean(mass))

starwars %>% group_by(gender,species) %>% 
  summarise(contagem = n()) %>% head(3) 

#Juntando linhas e colunas
df1 <- data.frame (id = c(1,2,3,4,5),
                   nome=c("Carlos","lucia","Marcio","Marcelo","Luciene"),
                   filhos = c(2,2,0,4,0),
                   idade = c(20,30,26,50,21),
                   salario = c(1200,5000,6000,15000,2000))
View(df1)

df2 <- data.frame( id=c(1,7,3,6,8),
                   fumante=c(1,1,0,0,1),
                   diabetes = c(T,F,F,T,F),
                   hipertenso = c(T,F,F,F,F),
                   nome=c("Carlos","lucia","Marcio","Marcelo","Luciene"))
View(df2)

#Union all sem precisar ter a mesma quantidade colunas
bind_rows(df1,df2)

#Colocar uma tabela do lado da outra
bind_cols(df1,df2)
cbind(df1,df2)

#Joins
inner_join(df1,df2,by=c("id")) #inner join com chave id
inner_join(df1,df2,by=c("nome")) #inner join com chave nome
inner_join(df1,df2,by=c("nome","id")) #inner join com duas chaves

left_join(df1,df2,by=c("id")) #left join com chave id
left_join(df1,df2,by=c("nome")) #left join com chave nome

right_join(df1,df2,by=c("id")) #left join com chave id
right_join(df1,df2,by=c("nome")) #left join com chave nome

full_join(df1,df2,by=c("id")) #full join com chave id

#renomeando uma vari�vel
nisi_dev %<>% rename(colaboradores = nome) #renomeando a coluna nome para colaboradores (com Dplyr)
names(nisi_dev)[1] <- 'colaboradores' #renomeando a primeira coluna para nome (sem Dplyr)

df2 %<>% rename(id_paciente = id) #renomeando df2 de id para id_paciente
df2

inner_join(df1,df2,by=c("id"="id_paciente")) #Join com colunas com o nome diferente

#comando For
for(i in 1:length(nisi_dev$nome))
{if (nisi_dev$nome[i] == "Willzao") print(nisi_dev$idade[i])} #se will printar idade

# Bonus -------------------------------------------------------------------

#Converter Variav�is
nisi_dev$idade <- as.numeric(nisi_dev$idade) #convertendo coluna em n�merico
nisi_dev$cid <- as.character(nisi_dev$cid) #convertendo a coluna para varchar

#Usar SQL no R
library(sqldf)
sqldf("select * from nisi_dev where nome like '%b%'") 

#Estatisticas b�sicas
table(starwars$species,starwars$gender) %>% head(3) #tabela de frequ�ncia
prop.table(table(starwars$species,starwars$gender)) %>% head(3) #tabela de propor��o

quantile(starwars$mass,c(.01,.05,.10,.90,.95,.99),na.rm = T) #percentil tirando os n�meros nulos
boxplot(starwars$mass) #plotar boxplot
hist(starwars[starwars$mass<500,"mass"])

#Detec��o de Outlier
library(outliers)
outlier(starwars$mass)  #verificar um outlier

table(starwars$mass,useNA = "always") #verificar se h� nulos na tabela

# Exerc�cios 2 --------------------------------------------------------------

#1 Imprima (print) na tela o nome de todos os humanos do sexo feminino que est�o na base.
starwars %>% filter(gender == "female") %>% select(name)

#2 Imprima na tela a esp�cie e o nome de todos os personagens que s�o origin�rios de Tatooine e possuem o ano de nascimento maior ou igual a 40.
starwars %>% filter(homeworld == "Tatooine" & birth_year >= 40) %>% select(name,species) 

#3 Utilizando dplyr, selecione todas as vari�veis que contenham a palavra "color" e exiba todas as combina��es poss�veis de cores para estas vari�veis.
starwars %>% select(contains("color")) %>% distinct()

#4 Filtre todos os personagens que tem a pele clara (fair) e possuem sua massa registrada no sistema (sem NAs), ordenando a mesma em ordem decrescente. Exiba somente as vari�veis nome e massa (mass).
starwars %>% filter(skin_color == "fair" & !is.na(mass)) %>% arrange(desc(mass)) %>% select(name,mass)  

#5 Crie um novo dataframe com referencia no dataset "starwars"  e dentro deste, crie uma vari�vel que contenha as 3 primeiras letras do nome de cada personagem.
starwars_new <- starwars %>% mutate(name_3_letters = substr(name,1,3))
starwars_new %>% select(name_3_letters)

#6 Dentro do mesmo dataframe, Crie uma nova vari�vel chamada "imc", que calcula o imc de todos os personagens do dataset (lembre-se que a altura est� em cm).
starwars_new %<>% mutate(height=height/100,imc = mass/(height^2))
starwars_new %>% select(name,imc) %>% filter(!is.na(imc)) %>% arrange(desc(imc))

#7 Com um novo dataframe, agrupe o dataset pela cor dos olhos. O dataframe deve conter uma coluna com a contagem de personagens com cada cor. Tamb�m deve conter uma coluna com a m�dia da altura desses grupos e desvio padr�o
nova_tabela <- starwars %>% group_by(eye_color) %>% summarise(media_altura = mean(height,na.rm = T),desvio = sd(height,na.rm = T),qtd = n())


