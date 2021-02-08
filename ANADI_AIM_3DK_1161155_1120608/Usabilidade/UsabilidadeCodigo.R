######IMPORT######
library(readr)
dados <- read_csv("E:/Faculdade/ANADI/dadosquest.txt", 
                  col_types = cols(Timestamp = col_skip()))
View(dados)

######MUDAR NOMES COLUNAS######
names(dados) <- c("Genero", "Faxa_etaria","App_Usada","App_fav","G_intuitiva","G_prod","G_qual_app","G_qual_serv","G_freq","U_intuitiva","U_prod","U_qual_app","U_qual_serv","U_freq")

detach()
attach(dados)

#######GRAFICOS DE GENERO######
sex <- dados[1]

detach()
attach(sex)

remove(lbls)
remove(counts)
remove(pct)
remove(slices)

# Pie Chart
slices <- c(table(sex))
lbls <- c("Feminino", "Masculino" ,"Prefiro não dizer")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="") 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Genero") 

remove(lbls)
remove(counts)
remove(pct)
remove(slices)

# Simple Bar Plot
counts <- table(sex$Genero)
barplot(counts, main="Genero",col=rainbow(length(counts))) 

#AMBOS SAO FORMAS BOAS DE APRESENTAR OS DADOS


##########FAXA ETARIA#############

age <- dados[2]

detach()
attach(age)

remove(lbls)
remove(counts)
remove(pct)
remove(slices)

# Pie Chart
slices <- c(table(age$Faxa_etaria))
lbls <- c("13 - 18:","19 - 25:","26 - 35:","36 - 45:","46 - 60:","60+:")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Faxa Etária") 

remove(lbls)
remove(counts)
remove(pct)
remove(slices)

# Simple Bar Plot
counts <- table(age$Faxa_etaria)
barplot(counts, main="Faxa Etária",col=rainbow(length(counts))) 

# O PIE CHART Ã O MELHOR PARA APRESENTAR OS DADOS 


#########APP UTILIZADA#########

app <- dados[3]
detach()
attach(app)

remove(lbls)
remove(counts)
remove(pct)
remove(slices)

# Simple Bar Plot
counts <- table(app$App_Usada)
barplot(counts, main="App Utilizada",col=rainbow(length(counts))) 

remove(lbls)
remove(counts)
remove(pct)
remove(slices)

# Pie Chart
slices <- c(table(app$App_Usada))
lbls <- c("Não utilizo nenhuma das duas apps","Utilizo ambas as apps","Utilizo apenas a Glovo","Utilizo apenas a Uber Eats")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="App Utilizada") 


#########APP FAVORITA#########

#dentro dos que usam as duas 
appfav <- subset(dados[4], App_fav!=""  )
detach()
attach(appfav)

remove(lbls)
remove(counts)
remove(pct)
remove(slices)

# Pie Chart
slices <- c(table(appfav$App_fav))
lbls <- c("Glovo","Uber Eats")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%",sep="") 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="App Favorita")

remove(lbls)
remove(counts)
remove(pct)
remove(slices)

# Simple Bar Plot
counts <- table(appfav$App_fav)
barplot(counts, main="App Favorita",col=rainbow(length(counts))) 



######GLOVO QUESTIONS ######

#G-Diria que a aplicacao pode se dizer intuitiva?
#G-Foi facil encontrar os produtos que procura?
#G-Como classifica a qualidade da aplicacao?
#G-Como classifica a qualidade do servico?,
#G-Quao frequentemente utiliza a aplicacao?



######Diria que a aplicacao pode se dizer intuitiva?######

Glovo_intuitiva <- subset(dados[5], G_intuitiva!=""  )
detach()
attach(Glovo_intuitiva)

remove(lbls)
remove(counts)
remove(pct)
remove(slices)

# Pie Chart
slices <- c(table(Glovo_intuitiva$G_intuitiva))
lbls <- c("Não","Sim")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Glovo - Diria que a aplicacao pode se dizer intuitiva?")

remove(lbls)
remove(counts)
remove(pct)
remove(slices)

# Simple Bar Plot
counts <- table(Glovo_intuitiva$G_intuitiva)
barplot(counts, main="Glovo - Diria que a aplicacao pode se dizer intuitiva?",col=rainbow(length(counts))) 


######Foi facil encontrar os produtos que procura?######

Glovo_prod <- subset(dados[6], G_prod!=""  )
detach()
attach(Glovo_prod)

remove(lbls)
remove(counts)
remove(pct)
remove(slices)

# Pie Chart
slices <- c(table(Glovo_prod$G_prod))
lbls <- c("Não","Sim")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Glovo - Foi fácil encontrar os produtos que procura? ")

remove(lbls)
remove(counts)
remove(pct)
remove(slices)

# Simple Bar Plot
counts <- table(Glovo_prod$G_prod)
barplot(counts, main="Glovo - Foi fácil encontrar os produtos que procura?",col=rainbow(length(counts))) 

######Como classifica a qualidade da aplicacao?######

Glovo_qual_app <- subset(dados[7], G_qual_app!=""  )
detach()
attach(Glovo_qual_app)

remove(lbls)
remove(counts)
remove(pct)
remove(slices)

# Boxplot
boxplot(Glovo_qual_app$G_qual_app, main="Glovo - Como Classifica a qualidade da aplicação?",col = rainbow(1)) 

remove(lbls)
remove(counts)
remove(pct)
remove(slices)

# Simple Bar Plot
counts <- table(Glovo_qual_app$G_qual_app)
barplot(counts, main="Glovo - Como Classifica a qualidade da aplicação?",col=rainbow(length(counts))) 

######Como classifica a qualidade do servico?######

Glovo_qual_serv <- subset(dados[8], G_qual_serv!=""  )
detach()
attach(Glovo_qual_serv)

remove(lbls)
remove(counts)
remove(pct)
remove(slices)

# Boxplot
boxplot(Glovo_qual_serv$G_qual_serv, main="Glovo - Como Classifica a qualidade do serviço?",col = rainbow(1)) 

remove(lbls)
remove(counts)
remove(pct)
remove(slices)

# Simple Bar Plot
counts <- table(Glovo_qual_serv$G_qual_serv)
barplot(counts, main="Glovo - Como Classifica a qualidade do serviço?",col=rainbow(length(counts)))

######Quao frequentemente utiliza a aplicacao?######

Glovo_freq <- subset(dados[9], G_freq!=""  )
detach()
attach(Glovo_freq)

remove(lbls)
remove(counts)
remove(pct)
remove(slices)

# Pie Chart
slices <- c(table(Glovo_freq$G_freq))
lbls <- c("De longe a longe ao longo do ano","Uma ou duas vezes por mes","Uma vez por semana")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Glovo - Quão frequentemente utiliza a aplicação?")

remove(lbls)
remove(counts)
remove(pct)
remove(slices)

# Simple Bar Plot
counts <- table(Glovo_freq$G_freq)
barplot(counts, main="Glovo - Quão frequentemente utiliza a aplicação?",col=rainbow(length(counts))) 




######UBER EATS QUESTIONS ######

#U-Diria que a aplicacao pode se dizer intuitiva?
#U-Foi facil encontrar os produtos que procura?
#U-Como classifica a qualidade da aplicacao?
#U-Como classifica a qualidade do servico?,
#U-Quao frequentemente utiliza a aplicacao?




######Diria que a aplicacao pode se dizer intuitiva?######

Uber_intuitiva <- subset(dados[10], U_intuitiva!=""  )
detach()
attach(Uber_intuitiva)

remove(lbls)
remove(counts)
remove(pct)
remove(slices)

# Pie Chart
slices <- c(table(Uber_intuitiva$U_intuitiva))
lbls <- c("Não","Sim")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Uber Eats - Diria que a aplicação pode se dizer intuitiva?")

remove(lbls)
remove(counts)
remove(pct)
remove(slices)

# Simple Bar Plot
counts <- table(Uber_intuitiva$U_intuitiva)
barplot(counts, main="Uber Eats - Diria que a aplicação pode se dizer intuitiva?",col=rainbow(length(counts))) 

######Foi facil encontrar os produtos que procura?######

Uber_prod <- subset(dados[11], U_prod!=""  )
detach()
attach(Uber_prod)

remove(lbls)
remove(counts)
remove(pct)
remove(slices)

# Pie Chart
slices <- c(table(Uber_prod$U_prod))
lbls <- c("Não","Sim")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Uber Eats - Foi fácil encontrar os produtos que procura? ")

remove(lbls)
remove(counts)
remove(pct)
remove(slices)

# Simple Bar Plot
counts <- table(Uber_prod$U_prod)
barplot(counts, main="Uber Eats - Foi fácil encontrar os produtos que procura?",col=rainbow(length(counts))) 

######Como classifica a qualidade da aplicacao?######
Uber_qual_app <- subset(dados[12], U_qual_app!=""  )
detach()
attach(Uber_qual_app)

remove(lbls)
remove(counts)
remove(pct)
remove(slices)

# Boxplot
boxplot(Uber_qual_app$U_qual_app, main="Uber Eats - Como Classifica a qualidade da aplicação?",col = rainbow(1)) 

remove(lbls)
remove(counts)
remove(pct)
remove(slices)

# Simple Bar Plot
counts <- table(Uber_qual_app$U_qual_app)
barplot(counts, main="Uber Eats - Como Classifica a qualidade da aplicação?",col=rainbow(length(counts))) 

######Como classifica a qualidade do servico?######

Uber_qual_serv <- subset(dados[13], U_qual_serv!=""  )
detach()
attach(Uber_qual_serv)

remove(lbls)
remove(counts)
remove(pct)
remove(slices)

# Boxplot
boxplot(Uber_qual_serv$U_qual_serv, main="Uber Eats - Como Classifica a qualidade do servico?",col = rainbow(1)) 

remove(lbls)
remove(counts)
remove(pct)
remove(slices)

# Simple Bar Plot
counts <- table(Uber_qual_serv$U_qual_serv)
barplot(counts, main="Uber Eats - Como Classifica a qualidade do servico?",col=rainbow(length(counts)))

######Quao frequentemente utiliza a aplicacao?######

Uber_freq <- subset(dados[14], U_freq!=""  )
detach()
attach(Uber_freq)

remove(lbls)
remove(counts)
remove(pct)
remove(slices)

# Pie Chart
slices <- c(table(Uber_freq$U_freq))
lbls <- c("De longe a longe ao longo do ano","Multiplas vezes por semana","Uma ou duas vezes por mes","Uma vez por semana")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Uber Eats - Quão frequentemente utiliza a aplicação?")

remove(lbls)
remove(counts)
remove(pct)
remove(slices)

# Simple Bar Plot
counts <- table(Uber_freq$U_freq)
barplot(counts, main="Uber Eats - Quão frequentemente utiliza a aplicação?",col=rainbow(length(counts))) 
