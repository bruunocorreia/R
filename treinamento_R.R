
# Carregando biblioteca para manipulação de dados ----------------------------

install.packages("dplyr") #instalar biblioteca
library(dplyr) #carregar biblioteca

install.packages("pacman")
library(pacman)
p_load(magrittr)

#Criar um data_frame pela biblioteca Dplyr
nisi_dev <- data.frame(nome= c("Willzao","Betão","VinãoReporter","Bruno"),
                       idade= c(25,23,21,NA),
                       cid = c("z32","a15","j13","z01"),
                       cargo = c("analista","analista","analista","analista"))

#Verificar estatística da tabela
summary(nisi_dev)

#verificar tipos das variáveis da tabela
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

# Exercício 1 - Acessar colunas e linhas do data_frame --------------------

starwars <- starwars #carregando a base starwars
str(starwars) #olhando os tipos de variáveis de starwars
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

#Funções base para cálculos
mean(nisi_dev$idade,na.rm = T) #cálculo da média dos não nulos
median(nisi_dev$idade,na.rm = T) #cálculo da mediana dos não nulos
sd(nisi_dev$idade,na.rm = T) #cálculo do desvio padrão dos não nulos
max(nisi_dev$idade,na.rm = T) #cálculo do máximo dos não nulos
min(nisi_dev$idade,na.rm = T) #cálculo do mínimo padrão dos não nulos
summary(nisi_dev$idade,na.rm = T) #resumo das medidas 

#Filtro (dplyr)
nisi_dev %>% filter(idade < 25) #Filtrando idade menos que 25
nisi_dev %>% filter(cargo == "analista") #filtrando cargo analista

#Filtro (which)
index <- which(nisi_dev$nome =='Willzao') #filtra o número da linha que o Will está
nisi_dev[index,]

#Filtro de valores nulos (NAs)
nisi_dev %>% filter(!is.na(idade)) #vai tirar todos que possuem NA na idade
nisi_dev %>% filter(!is.na(idade) & idade > 22) #Sem NAs e com idade maior que 22anos

#Filtro com %in%
lista <- c("Betão","VinãoReporter")
nisi_dev %>% filter(nome %in% lista)

#Comando Select
select(nisi_dev,nome:cid) #selecionar da coluna nome até cid
select(nisi_dev,2:3)      #selecionar da coluna 2 até a 3

#Carregar biblioteca de Texto
p_load(stringr)

#Filtros com operações lógicas
nisi_dev %>% filter(idade <25 | str_detect(nome,'B')) #Menor que25 anos ou nome com a letra B
nisi_dev %>% select(idade,cargo) %>% filter(idade <25 & idade >=23) #idade e cargo,idade de 23a25
nisi_dev %>% filter(cargo == "analista" | idade == 23) #Cargo analista ou idade = 23 anos
nisi_dev %>% select(-idade) #seleciona todas colunas menos a de idade
nisi_dev %>% select(-one_of(c("nome","cargo"))) #seleciona todas colunas menos nome e cargo

#filtrando linhas que possuem idade < 24 anos sem Dplyr
nisi_dev[nisi_dev["idade"]<24,] 

#Operador %<>% realiza os procedimentos e atribui o valor a variável
nisi_dev2 <- nisi_dev
nisi_dev2 %<>% filter(idade <24) #retira quem tem idade <24 da base e grava
View(nisi_dev2) #Visualizando a tabela

#Criando uma nova coluna com o comando MUTATE do dplyr
nisi_dev %>% mutate(ano_nascimento = 2019-idade) #exibindo data_frame com uma coluna criada de ano de nascimento
nisi_dev %<>% mutate(ano_nascimento = 2019-idade) #adicionando nova coluna ao data_frame

#Cortar pedaços de uma variável texto
substr("Caneta azul",1,6) #cortando as 6 primeiras letras da palavra
substr(nisi_dev$nome,1,3) #cortando as 3 primeiras letras da coluna nome do data_frame

#Usando Mutate com substr
nisi_dev %<>% mutate(cid_letra=substr(cid,1,1)
                    ,cid_numero=substr(cid,2,3))

#If e Else -- Comando Se
ifelse(nisi_dev["nome"]=='Bruno','achou','não achou') 

#Select CAse
nisi_dev["nome"] <- case_when(nisi_dev$nome == "Willzao" ~ "Wilerson",
                              nisi_dev$nome == "Betão" ~ "José Roberto",
                              nisi_dev$nome  == "VinãoReporter"~"Vinicius",
                              TRUE ~"Bruno Alves")

nisi_dev %>% mutate(idade = ifelse(idade == 23 
                                   ,mean(idade,na.rm = T) #se verdadeiro
                                   ,idade)) #se falso

nisi_dev %>% select(idade) %>% sum(na.rm=TRUE) #soma toas as idade não nulas

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

# Funções, group by, joins ------------------------------------------------

#Funções
imc <- function(altura,massa){return(massa/altura^2)} #função para cálculo do IMC
imc(1.76,75) #Imc Bruno
imc(starwars$height,starwars$mass) #Imc starwars

primeira_3letras <- function(nome) {return (substr(nome,1,2))} #Função que retorno 3 primeiras letras
primeira_3letras("Wilerson")

Potencia_RaizQuadrada <- function(x1,x2) {return(list(x1*x1,sqrt(x1)))}
Potencia_RaizQuadrada(25) #função gambiarra que retorna uma lista

#Group By/Tabela Dinâmica
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

#renomeando uma variável
nisi_dev %<>% rename(colaboradores = nome) #renomeando a coluna nome para colaboradores (com Dplyr)
names(nisi_dev)[1] <- 'colaboradores' #renomeando a primeira coluna para nome (sem Dplyr)

df2 %<>% rename(id_paciente = id) #renomeando df2 de id para id_paciente
df2

inner_join(df1,df2,by=c("id"="id_paciente")) #Join com colunas com o nome diferente

#comando For
for(i in 1:length(nisi_dev$nome))
{if (nisi_dev$nome[i] == "Willzao") print(nisi_dev$idade[i])} #se will printar idade

# Bonus -------------------------------------------------------------------

#Converter Variavéis
nisi_dev$idade <- as.numeric(nisi_dev$idade) #convertendo coluna em númerico
nisi_dev$cid <- as.character(nisi_dev$cid) #convertendo a coluna para varchar

#Usar SQL no R
library(sqldf)
sqldf("select * from nisi_dev where nome like '%b%'") 

#Estatisticas básicas
table(starwars$species,starwars$gender) %>% head(3) #tabela de frequência
prop.table(table(starwars$species,starwars$gender)) %>% head(3) #tabela de proporção

quantile(starwars$mass,c(.01,.05,.10,.90,.95,.99),na.rm = T) #percentil tirando os números nulos
boxplot(starwars$mass) #plotar boxplot
hist(starwars[starwars$mass<500,"mass"])

#Detecção de Outlier
library(outliers)
outlier(starwars$mass)  #verificar um outlier

table(starwars$mass,useNA = "always") #verificar se há nulos na tabela

# Exercícios 2 --------------------------------------------------------------

#1 Imprima (print) na tela o nome de todos os humanos do sexo feminino que estão na base.
starwars %>% filter(gender == "female") %>% select(name)

#2 Imprima na tela a espécie e o nome de todos os personagens que são originários de Tatooine e possuem o ano de nascimento maior ou igual a 40.
starwars %>% filter(homeworld == "Tatooine" & birth_year >= 40) %>% select(name,species) 

#3 Utilizando dplyr, selecione todas as variáveis que contenham a palavra "color" e exiba todas as combinações possíveis de cores para estas variáveis.
starwars %>% select(contains("color")) %>% distinct()

#4 Filtre todos os personagens que tem a pele clara (fair) e possuem sua massa registrada no sistema (sem NAs), ordenando a mesma em ordem decrescente. Exiba somente as variáveis nome e massa (mass).
starwars %>% filter(skin_color == "fair" & !is.na(mass)) %>% arrange(desc(mass)) %>% select(name,mass)  

#5 Crie um novo dataframe com referencia no dataset "starwars"  e dentro deste, crie uma variável que contenha as 3 primeiras letras do nome de cada personagem.
starwars_new <- starwars %>% mutate(name_3_letters = substr(name,1,3))
starwars_new %>% select(name_3_letters)

#6 Dentro do mesmo dataframe, Crie uma nova variável chamada "imc", que calcula o imc de todos os personagens do dataset (lembre-se que a altura está em cm).
starwars_new %<>% mutate(height=height/100,imc = mass/(height^2))
starwars_new %>% select(name,imc) %>% filter(!is.na(imc)) %>% arrange(desc(imc))

#7 Com um novo dataframe, agrupe o dataset pela cor dos olhos. O dataframe deve conter uma coluna com a contagem de personagens com cada cor. Também deve conter uma coluna com a média da altura desses grupos e desvio padrão
nova_tabela <- starwars %>% group_by(eye_color) %>% summarise(media_altura = mean(height,na.rm = T),desvio = sd(height,na.rm = T),qtd = n())


