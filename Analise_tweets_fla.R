# Análise de Tweets sobre a partida de futebol Flamengo X LDU, ocorrida em 13 de março de 2019 pela Copa Libertadores da América
# Data 02/06/2019 (updated)
# Autor: Diego Abreu | https://github.com/DiegoAbreu/


## Carregamento do CSV em um dataframe ----
tweets_fla <- read.csv("tweets_fla.csv",stringsAsFactors = FALSE)

## Visualização do Dataset ----
View(tweets_fla)

## Instalação pacote SQL para consultas ----
install.packages('sqldf')
require(sqldf)

## Tratamento dos dados ----
    # Data e Hora:
        # Conversão de fuso-horário:
        troca_fuso <- tweets_fla$created_at
        troca_fuso <- strptime(troca_fuso, "%a %b %d %H:%M:%S %z %Y", tz = "GMT")
        tweets_fla$tempo <- as.POSIXct(troca_fuso, tz = "GMT") 
        tweets_fla$tempo <- format(tweets_fla$tempo, tz = "America/Sao_Paulo", usetz = TRUE)
        
        # Divisão da coluna tempo em data e hora:
        a <- strsplit(tweets_fla$tempo, ' ') 
        b <- do.call(rbind.data.frame, a)
        colnames(b) <- c("DATA", "HORA", "FUSO")
        b <- data.frame(lapply(b, as.character), stringsAsFactors=FALSE)
        tweets_fla <- cbind(tweets_fla, b)
        tweets_fla$created_at <- NULL
        tweets_fla$tempo <- NULL
        tweets_fla$FUSO <- NULL
    
    # Renomeação Colunas:
        colnames(tweets_fla) <- c("SEGUIDORES","SEGUE","NOME","USUARIO","TWEET","VERIFICADO","DATA", "HORA" )
    
    # Transformação dos valores da coluna VERIFICADO:
        tweets_fla$VERIFICADO[tweets_fla$VERIFICADO=="False"] <- "NÃO"
        tweets_fla$VERIFICADO[tweets_fla$VERIFICADO=="True"] <- "SIM"
        
    # Tweets em Minúsculo:
        tweets_fla$TWEET <- tolower(tweets_fla$TWEET)
        
## Análises ----
    # Quantidades ----
        # Total do Dataset  ----
        dim(tweets_fla) # 183.027 Tweets; 8 Colunas
        QTD_tweets = NROW(tweets_fla)
        
        # Horários  ----
        inicio <- min(tweets_fla$HORA) # Horário do primeiro tweet coletado: 20:59:56
        fim <- max(tweets_fla$HORA) # Horário do último tweet coletado: 23:49:57
        
        inicio <- strptime(inicio, "%H:%M:%S")
        fim <- strptime(fim, "%H:%M:%S")
        tempo_de_coleta = ((as.numeric(fim - inicio))*60)*60
        
        install.packages("lubridate")
        library(lubridate)
        tempo_de_coleta <- seconds_to_period(tempo_de_coleta) # 2 horas, 50 minutos e 1 segundo de coleta
        
        # Momentos com mais tweets  ----
        Cronologia = sqldf("select HORA, count(*)HORA from tweets_fla group by HORA order by 1")
        colnames(Cronologia) <- c("HORARIO", "QTD_TWEETS")
        Top_Cronologia = sqldf("select HORA, count(*)HORA from tweets_fla group by HORA order by 2 desc")
        Top10_momentos = head(Top_Cronologia, 10)
        colnames(Top10_momentos) <- c("HORARIO", "QTD_TWEETS")
        
        # Usuários ----
        QTD_usuarios_total = length(unique(tweets_fla$USUARIO)) # 77.225 Usuários
        
        # Media de Tweets por usuário ----
        Media = QTD_tweets/QTD_usuarios_total # Média de 2.37 tweets por ussuário
        
        # Verificados  ----
        QTD_usuarios_twittando = unique(sqldf("select USUARIO, VERIFICADO from tweets_fla"))
        QTD_usuarios_verificados = table(QTD_usuarios_twittando$VERIFICADO)   # 125 Usuários verificados; 77100 Usuários não verificados.
        
        # Top 10 Twitteiros ----
        Top10_twitteiros = sqldf("select USUARIO, count(*)TWEET from tweets_fla group by USUARIO order by 2 desc limit 10")
        Top10_twitteiros$TWEET = as.numeric(as.character(Top10_twitteiros$TWEET))
        
        # Top 10 Twitteiros Verificados ----
        Top10_twitteiros_verificados = sqldf("select USUARIO, count(*)TWEET from tweets_fla Where VERIFICADO == 'SIM' group by USUARIO order by 2 desc limit 10")
        Top10_twitteiros_verificados$TWEET = as.numeric(as.character(Top10_twitteiros_verificados$TWEET))
        
        # Quantidade de retweets ----
        RTs = sqldf("select USUARIO, TWEET from tweets_fla Where TWEET like 'RT%'")
        QTD_RTs = length(RTs$TWEET) # 116.036 Retweets

        # Quantidade de tweets originais ----
        TWs = sqldf("select USUARIO, TWEET from tweets_fla Where TWEET not like 'RT%'")
        QTD_TWs = length(TWs$TWEET)  # 66.991 Tweets originais
        
        # Citacao por jogador  ----
          #Diego Alves
        QTD_Diego_Alves = sqldf("select TWEET from tweets_fla  Where TWEET like '%diego alves%'")
        QTD_Diego_Alves = length(QTD_Diego_Alves$TWEET) #8084 Tweets
          # Pará
        QTD_Para = sqldf("select TWEET from tweets_fla  Where TWEET like '%pará%'") 
        QTD_Para = length(QTD_Para$TWEET) #607 Tweets
         # Léo Duarte
        QTD_Leo_Duarte = sqldf("select TWEET from tweets_fla  Where TWEET like '%leo duarte%'") 
        QTD_Leo_Duarte = length(QTD_Leo_Duarte$TWEET) #19 Tweets
          # Rodrigo Caio
        QTD_Rodrigo_Caio = sqldf("select TWEET from tweets_fla  Where TWEET like '%rodrigo caio%'") 
        QTD_Rodrigo_Caio = length(QTD_Rodrigo_Caio$TWEET) #151 Tweets
          # Renê
        QTD_Rene = sqldf("select TWEET from tweets_fla  Where TWEET like '%rene%'") 
        QTD_Rene = length(QTD_Rene$TWEET) #113 Tweets
          # Trauco
        QTD_Trauco = sqldf("select TWEET from tweets_fla  Where TWEET like '%trauco%'") 
        QTD_Trauco = length(QTD_Trauco$TWEET) #401 Tweets
          # Cuellar
        QTD_Cuellar = sqldf("select TWEET from tweets_fla  Where TWEET like '%cuellar%'") 
        QTD_Cuellar = length(QTD_Cuellar$TWEET) #1018 Tweets
          # Arrascaeta
        QTD_Arrascaeta = sqldf("select TWEET from tweets_fla  Where TWEET like '%arrasca%'") 
        QTD_Arrascaeta = length(QTD_Arrascaeta$TWEET) #1598 Tweets
          # Willian Arão
        QTD_Arao = sqldf("select TWEET from tweets_fla Where TWEET like '%arao%' or '%arão%'") 
        QTD_Arao = length(QTD_Arao$TWEET) #183 Tweets
          # Diego
        QTD_Diego = sqldf("select TWEET from tweets_fla Where TWEET like '%diego%'") 
        QTD_Diego = length(QTD_Diego$TWEET) - QTD_Diego_Alves #5076 Tweets
          # Everton Ribeiro
        QTD_Everton = sqldf("select TWEET from tweets_fla Where TWEET like '%everton%'") 
        QTD_Everton = length(QTD_Everton$TWEET) #4520 Tweets
          # Gabriel Barbosa
        QTD_Gabigol = sqldf("select TWEET from tweets_fla Where TWEET like '%gabi%'") 
        QTD_Gabigol = length(QTD_Gabigol$TWEET) #6512 Tweets
          # Bruno Henrique
        QTD_Bruno_Henrique = sqldf("select TWEET from tweets_fla Where TWEET like '%brun%'") 
        QTD_Bruno_Henrique = length(QTD_Bruno_Henrique$TWEET) #1725 Tweets
          # Uribe
        QTD_Uribe = sqldf("select TWEET from tweets_fla Where TWEET like '%uribe%'") 
        QTD_Uribe = length(QTD_Uribe$TWEET) #2077 Tweets
          # Abel
        QTD_Abel = sqldf("select TWEET from tweets_fla Where TWEET like '%abel%'") 
        QTD_Abel = length(QTD_Abel$TWEET) #2286 Tweets
          # Cesar
        QTD_Cesar = sqldf("select TWEET from tweets_fla Where TWEET like '%cesar%'") 
        QTD_Cesar = length(QTD_Cesar$TWEET) #23 Tweets
          # Gabriel Batista
        QTD_Gabriel_Batista = sqldf("select TWEET from tweets_fla Where TWEET like '%gabriel batist%'") 
        QTD_Gabriel_Batista = length(QTD_Gabriel_Batista$TWEET) #2 Tweets
          # Rodinei
        QTD_Rodinei = sqldf("select TWEET from tweets_fla Where TWEET like '%rodinei%'") 
        QTD_Rodinei = length(QTD_Rodinei$TWEET) #532 Tweets
          # Thuler
        QTD_Thuler = sqldf("select TWEET from tweets_fla Where TWEET like '%thuler%'") 
        QTD_Thuler = length(QTD_Thuler$TWEET) #2 Tweets
          # Hugo Moura
        QTD_Hugo_Moura = sqldf("select TWEET from tweets_fla Where TWEET like '%hugo moura%'") 
        QTD_Hugo_Moura = length(QTD_Hugo_Moura$TWEET) #0 Tweets
          # Ronaldo
        QTD_Ronaldo = sqldf("select TWEET from tweets_fla Where TWEET like '%ronaldo%'") 
        QTD_Ronaldo = length(QTD_Ronaldo$TWEET) #65 Tweets
          # Piris da Motta
        QTD_Piris_da_Motta = sqldf("select TWEET from tweets_fla Where TWEET like '%piris%'") 
        QTD_Piris_da_Motta = length(QTD_Piris_da_Motta$TWEET) #35 Tweets
          # Vitor Gabriel
        QTD_Vitor_Gabriel = sqldf("select TWEET from tweets_fla Where TWEET like '%vitor gabriel%'") 
        QTD_Vitor_Gabriel = length(QTD_Vitor_Gabriel$TWEET) #0 Tweets
          # Lucas Silva
        QTD_Lucas_Silva = sqldf("select TWEET from tweets_fla Where TWEET like '%lucas silva%'") 
        QTD_Lucas_Silva = length(QTD_Lucas_Silva$TWEET) #1 Tweets
        
        # Jogadores Mais citados ----
        Jogadores = c("Diego Alves", "Para", "Leo Duarte", "Rodrigo Caio", "Rene", "Trauco", "Cuellar",
                     "Arrascaeta", "Arao", "Diego", "Everton", "Gabigol", "Bruno Henrique", "Uribe", 
                     "Abel", "Cesar", "Gabriel_Batista", "Rodinei", "Thuler", "Hugo_Moura", "Ronaldo", 
                     "Piris da Motta", "Vitor Gabriel", "Lucas Silva")
        
        Citacoes = c(QTD_Diego_Alves, QTD_Para, QTD_Leo_Duarte, QTD_Rodrigo_Caio, QTD_Rene, QTD_Trauco, QTD_Cuellar,
                     QTD_Arrascaeta, QTD_Arao,QTD_Diego, QTD_Everton, QTD_Gabigol, QTD_Bruno_Henrique, QTD_Uribe, 
                     QTD_Abel, QTD_Cesar, QTD_Gabriel_Batista, QTD_Rodinei, QTD_Thuler, QTD_Hugo_Moura, QTD_Ronaldo, 
                     QTD_Piris_da_Motta, QTD_Vitor_Gabriel, QTD_Lucas_Silva)
        
        Jogadores_citados <- cbind.data.frame(Jogadores,Citacoes)
        Top10_Jogadores_citados = sqldf("select Jogadores, Citacoes from Jogadores_citados order by 2 desc limit 10")
        str(Jogadores_citados)
        # Palavras mais usadas ----
            # Palavras  ----
            palavras <- tweets_fla$TWEET
            palavras <- gsub("\n"," ", palavras)
            palavras = gsub(":", " ", palavras)
            palavras <- strsplit(palavras," ")
            palavras.freq<-table(unlist(palavras))
            tabela_de_palavras = cbind.data.frame(names(palavras.freq),as.integer(palavras.freq))
            colnames(tabela_de_palavras) <- c("PALAVRA","FREQUENCIA")
        
            # Top5 Hashtags mais usadas ----
            Top5_hashtags = sqldf("select PALAVRA, FREQUENCIA from tabela_de_palavras Where PALAVRA Like '#%' order by 2 desc limit 5")
       
            # Top5 @ mais citadas ----
            Top5_arrobas = sqldf("select PALAVRA, FREQUENCIA from tabela_de_palavras Where PALAVRA Like '@%' order by 2 desc limit 5")
            
            # 100 termos mais usados e Nuvem de palavras ----
            install.packages("tm")
            library(tm)
            
            nuvem <- tweets_fla$TWEET
            nuvem <- gsub("\n"," ", nuvem)
            nuvem = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", nuvem)
            nuvem = gsub("http\\w+", "", nuvem)
            nuvem <- gsub("rt","", nuvem)
            nuvem <- gsub("@","", nuvem)
            nuvem = gsub(":", "", nuvem)
            nuvem = gsub("[[:punct:]]", "", nuvem)
            nuvem = gsub("'\'", "", nuvem)
            nuvem = gsub("[[:digit:]]", "", nuvem)
            nuvem = gsub("(rt|via)((?:\\b\\W*@\\w+)+)", " ", nuvem)
            nuvem <- tolower(nuvem)
            nuvem = gsub("[ \t]{2,}", " ", nuvem)
            nuvem = gsub("^\\s+|\\s+$", "", nuvem)
            nuvem <- stringi::stri_trans_general(nuvem, "latin-ascii")
            nuvem <- iconv(nuvem, from = "UTF-8", to = "ASCII")
            
            SW <- stopwords("pt")
            SW <- tolower(SW)
            lista <- c ("ta","pra","x","ne", "tao","pro", "paida")
            SW <- append(SW, lista)
            nuvemSw <- tm::removeWords(nuvem,c(SW,'rt'))
              
            nuvemSw <- strsplit(nuvemSw," ")
            nuvem.freq<-table(unlist(nuvemSw))
            tabela_nuvem = cbind.data.frame(names(nuvem.freq),as.integer(nuvem.freq))
            colnames(tabela_nuvem) <- c("PALAVRA","FREQUENCIA")
            
            tabela_nuvem$PALAVRA <- toupper(tabela_nuvem$PALAVRA)
            Top100_palavras = sqldf("select PALAVRA, FREQUENCIA from tabela_nuvem Where Length(PALAVRA) > 2 and Length(PALAVRA) < 10 order by 2 desc limit 100")
            
## Visualizações ---- 
    # Gráficos  ----
        install.packages('ggplot2')
        library(ggplot2)
        library(dplyr)
        
        Tema_fundo =  theme(panel.background = element_rect(fill = "Black",colour = "Black",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "white"))   
            
        # Momentos com mais tweets  ---- 
        ggplot(data = Cronologia, aes(x = HORARIO, y = QTD_TWEETS))+
        geom_area(aes( color = "firebrick", fill=QTD_TWEETS)) + scale_y_continuous(expand = c(0, 0))+
        scale_x_discrete(name ="Horário",  breaks=c("20:59:56","21:39:04","22:17:56","23:07:54","23:49:57"))+
        labs(y="Tweets")+ Tema_fundo + theme(legend.position = "none") + ggtitle("Tweets ao longo do período:")
        
        # Top 10 momentos com mais tweets   ---- 
        ggplot(Top10_momentos, aes(x = reorder(HORARIO, -QTD_TWEETS), y = QTD_TWEETS, label = QTD_TWEETS)) +
        geom_col(aes(fill = QTD_TWEETS))+ scale_fill_continuous(low='black', high='firebrick', guide='colorbar')+
        theme(panel.background = element_blank())+ geom_text(size = 4, color = "white", position = position_stack(vjust = 0.5))+
        labs(x=NULL, y=NULL)+ ggtitle("Top 10 momentos com mais tweets") + labs(fill = "Tweets")
        
        # Top 10 Twitteiros  ---- 
        ggplot(Top10_twitteiros, aes(x = reorder(USUARIO, TWEET), y = TWEET, label = TWEET)) +
        geom_col(aes(fill = TWEET))+ scale_fill_continuous(low='black', high='firebrick', guide='colorbar')+
        theme(panel.background = element_blank())+ geom_text(size = 4, color = "white", position = position_stack(vjust = 0.5))+
        labs(x=NULL, y=NULL)+ ggtitle("Top 10 Twitteiros") + labs(fill = "Tweets")+ coord_flip()
        
        # Top 10 Twitteiros Verificados ---- 
        ggplot(Top10_twitteiros_verificados, aes(x = reorder(USUARIO, TWEET), y = TWEET, label = TWEET)) +
        geom_col(aes(fill = TWEET))+ scale_fill_continuous(low='black', high='firebrick', guide='colorbar')+
        theme(panel.background = element_blank())+ geom_text(size = 4, color = "white", position = position_stack(vjust = 0.5))+
        labs(x=NULL, y=NULL)+ ggtitle("Top 10 Twitteiros Verificados") + labs(fill = "Tweets")+ coord_flip()
        
        # Top 10 Jogadores citados nos Tweets ---- 
        ggplot(Top10_Jogadores_citados, aes(x = reorder(Jogadores, -Citacoes), y = Citacoes, label = Citacoes)) +
        geom_col(aes(fill = Citacoes))+ scale_fill_continuous(low='black', high='firebrick', guide='colorbar')+
        theme(panel.background = element_blank())+ geom_text(size = 4, color = "white", position = position_stack(vjust = 0.5))+
        labs(x=NULL, y=NULL)+ ggtitle("Top 10 Jogadores do Flamengo citados nos Tweets") + labs(fill = "Tweets")
        
        # Top 5 Hashtags  ---- 
        ggplot(Top5_hashtags, aes(x = reorder(PALAVRA, -FREQUENCIA), y = FREQUENCIA, label = FREQUENCIA)) +
        geom_col(aes(fill = FREQUENCIA))+ scale_fill_continuous(low='black', high='firebrick', guide='colorbar')+
        theme(panel.background = element_blank())+ geom_text(size = 4, color = "white", position = position_stack(vjust = 0.5))+
        labs(x=NULL, y=NULL)+ ggtitle("Top 5 Hashtags usadas no Tweets") + labs(fill = "Tweets")
        
        # Top 5 arrobas  ---- 
        ggplot(Top5_arrobas, aes(x = reorder(PALAVRA, -FREQUENCIA), y = FREQUENCIA, label = FREQUENCIA)) +
        geom_col(aes(fill = FREQUENCIA))+ scale_fill_continuous(low='black', high='firebrick', guide='colorbar')+
        theme(panel.background = element_blank())+ geom_text(size = 4, color = "white", position = position_stack(vjust = 0.5))+
        labs(x=NULL, y=NULL)+ ggtitle("Top 5 @ citadas no Tweets") + labs(fill = "Tweets")
        
    # Nuvem de Palavras ----   
    install.packages("wordcloud") # word-cloud generator 
    install.packages("RColorBrewer") # color palettes
    library(wordcloud)
    library(RColorBrewer)
    
    set.seed(1234)
    wordcloud(words = Top100_palavras$PALAVRA, freq = Top100_palavras$FREQUENCIA, 
              min.freq = 1,max.words=100, random.order=FALSE, colors=brewer.pal(10, "RdGy"))
    
           
              
              

              
              