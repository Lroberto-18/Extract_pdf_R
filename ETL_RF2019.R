library("dplyr")
library("pdftools")

directory <- "RF2019/"
files <- list.files(directory)
d_out <- list(Titulo= c(),Nome=c(),Cargo=c(),cpf=c(),Siape=c(),D_inicio=c(),D_termino=c(),docente=c(), bolsista=c(), voluntario=c())
d_frame <- data.frame(d_out)
data_out_team <- list(docente=c(), bolsista=c(), voluntario=c())
team_dataFrame = data.frame(data_out_team)
team_names <- c()
index_team <- c()
data_text <- c()
data_out <- c()
index <- c()

keywords = c(
  ## 1. Título
  '1.TÍTULO DA AÇÃO DE EXTENSÃO:\n', 'Informe o título completo do Programa/Projeto',
  ## 2. Dados coordenador
  '2.1. Nome:',   '2.2. Cargo:',
  ## 3. Cargo do coordenador
  '2.2. Cargo:',   '2.3. CPF:',
  ## 4. CPF coordenador
  '2.3. CPF:',  '2.4. SIAPE:',
  ## 5. SIAPE
  '2.4. SIAPE:',  '3. PERÍODO',
  ## 6. Data início
  '3.1. Início:',   '3.2. Término:',
  ## 7. Data Término
  '3.2. Término:',   '4. EQUIPE',
  ## 8. Público
  '5.1. Identificação do público beneficiado.', '6. AÇÕES VINCULADAS ',
  ## 9. Municipios
  '8.1 Descrição dos locais de realização das ações.','8.2 Descrição das atividades')
keywords = matrix(keywords, ncol = 2, byrow = T)
keywords_remove <- c("MINISTÉRIO\\s+DA\\s+EDUCAÇÃO\n\\s+UNIVERSIDADE\\s+FEDERAL\\s+DO\\s+CARIRI\n\\s+PRÓ-REITORIA DE EXTENSÃO\n",
                     "Av.\\s+Tenente\\s+Raimundo\\s+Rocha,\\s+1639,\\s+Bairro\\s+Cidade\\s+Universitária.\n",
                     "CEP:\\s+63040-360,\\s+Juazeiro\\s+do\\s+Norte\\s+–\\s+CE.",
                     "[__]+\n", "E-mail:\\s+proex@ufca.edu.br.\\s+Fone:\\s+\\(88)\\s+3221-9285/9286.\n")

keywords_team <- c("DOCENTES E TÉCNICO-ADMINISTRATIVOS\n ", "BOLSISTAS\n",
                   "BOLSISTAS\n", "VOLUNTÁRIOS\n",
                   "VOLUNTÁRIOS\n", "*CH termo: Carga Horária"
)
keywords_team = matrix(keywords_team, ncol = 2, byrow = T)

keywords_func <- c("\\sC\\s", "(C);",
                   "\\sCA\\s", "(CA);",
                   "\\sO\\s",  "(O);",
                   "\\sT\\s",  "(T);",
                   "\\sTA\\s","(TA);",
                   "\\sC\n", "(C);",
                   "\\sCA\n", "(CA);",
                   "\\sO\n",  "(O);",
                   "\\sT\n",  "(T);",
                   "\\sTA\n","(TA);")
keywords_func <- matrix(keywords_func,ncol=2, byrow = T)

#para limpar dados dos voluntários
keys_team_clear3 <- c("\\s+CH\\s+","\\s+Mês\\s+","\\s+48h\\s+","\\s+48h\n", "\\s+2019\\s+","\\s+Ano:\n",
                      "Julho","","",
                      "\\s+Ano:\\s+","\\s+Nome/\\s+","\\s+12h\\s+","\\s+53h\\s+","\\s+Abril\\s+", "\\s+Maio\\s+","\\s+Junho\\s+")

#para limpar os dados dos coordenadores
keys_team_clear1 <- c("Nome/Unidade\\s+Acadêmica/SIAPE","Função\\*",
                      "\\*Função:","Coordenador\\/tutor\\s+\\(C\\);","Coordenador\\s+Adjunto\\s+\\(CA\\);",
                      "Técnico-Administrativo\\s+\\(TA\\);","Outros\\s+\\(O\\).\n","\\*")


#para limpar os dados dos bolsistas
keys_team_clear2 <- c("Nome/Curso/Matrícula", "EXTENSÃO:")

#função de remover intervalos entre espaços vazios e \n
indexNullBreak <- function(txt_team){
  index_null <- gregexpr("\\s{3,50}",txt_team)[[1]]
  index_break <-gregexpr("\n",txt_team)[[1]]
  clear <- txt_team
  for(i in 1:length(index_break)){
    for(ind in 1:length(index_null)){
      if(index_break[i]<index_null[ind]) {
        if(index_break[i]==index_null[ind-1]){
          string <-  substr(txt_team[1],index_null[ind-2],index_break[i])
          clear <-  gsub(string,"", clear)
        }else{
          string <-  substr(txt_team[1],index_null[ind-1],index_break[i])
          clear <-  gsub(string,"", clear)
        }
      }
    }
  }
  return(clear)
}

#loop principal
txt_teame_z <- c()
for(z in 1:length(files)){
  data_text <-c()
  data_out <-c()
  index <- c()
  text_pdf = c(pdf_text(paste0(directory, files[z])))
  file <- paste0(directory, files[z])
  #Remover ruído 
  for( j in 1:length(text_pdf)){
    for(i in 1:length((keywords_remove))){
      text_pdf[j] = gsub(keywords_remove[i], "", text_pdf[j])
    }
    data_text <- paste(data_text, text_pdf[j])
  }
  #Extrair dados da página 1
  for(i in 1:nrow(keywords)){
    index <- gregexpr(keywords[i,1],data_text[1])[[1]]
    index <- c(index, gregexpr(keywords[i,2],data_text[1])[[1]])
    data_out <- c(data_out,substr(data_text, index[1]+nchar(keywords[i,1]), index[2]-1))
  }
  #Formatar dados da página 1
  #for(i in 1:length(data_out)){
    #data_out[i] <- gsub("\n","", data_out[i]) #remover    \n
    #data_out[i] <- gsub("\\s{3,50}"," ", data_out[i]) #remover espaços vazios
  #}
  #Extrair dados da Equipe
  team_names <- c()
  for(i in 1:nrow(keywords_team)){
    #Extrair dados em formato de texto da equipe de extensão
    index_team <- gregexpr(keywords_team[i,1],data_text[1])[[1]]
    index_team <- c(index_team, gregexpr(keywords_team[i,2],data_text[1])[[1]])
    #team_dataFrame[1,i] <- substr(data_text, index_team[1]+nchar(keywords_team[i,1]), index_team[2]-1)
    team_names <- append(team_names, substr(data_text, index_team[1]+nchar(keywords_team[i,1]), index_team[2]-1))
  }
  for (t in 1:length(team_names)){
    formatted <- c()
    txt_team <- team_names[t]
    if(t==1){
      for(y in 1:length(keys_team_clear1)){
        txt_team <-  gsub(keys_team_clear1[y],"", txt_team)
      }
      for(x in 1:nrow(keywords_func)){
        txt_team <- gsub(keywords_func[x,1],keywords_func[x,2], txt_team)
      }
    }
    clear <- indexNullBreak(txt_team)
    if(t==3){
      #limpar dados VOLUNTARIOS
      for(y in 1:length(keys_team_clear3)){
        clear <-  gsub(keys_team_clear3[y],"", clear)
      }
    }
    if(t==2){
      #limpar dados BOLSISTAS
      for(y in 1:length(keys_team_clear2)){
        clear <-  gsub(keys_team_clear2[y]," ", clear)
      }
    }
    clear <- gsub("\\s{3,50}"," ", clear)
    formatted <- append(formatted, clear)
    ##team_dataFrame[1,t] <- formatted[t]
    data_out <- append(data_out, formatted)
  }
  #Construir DataFrame
  for(i in 1:length(data_out)){
    d_frame[z,i] <- data_out[i]
  }
}



key_city <- c("Município","Descrição\\s+do\\s+local\\/Instituição\\/comunidade\\s+onde\\s+foi",
              "realizada\\s+a\\s+ação\\s+de\\s+extensão")
#locais <- d_frame[3,9]
locais <- d_frame[3,9]
for(i in 1:length(key_city)){
  locais <- gsub(key_city[i],"", locais)
}
#posições do \n+texto
aux2 = gregexpr("\n(\\s)?\\w", locais)[[1]]
firstword <- min(aux2) #primeira ocorrencia do \n+texto
locais <- substring(locais,firstword, nchar(locais))
aux2 = gregexpr("\n(\\s)?\\w", locais)[[1]]
aux3 <- gregexpr("\\s{3,250}", locais)[[1]]

cities <-c()
for(i in 1:length(aux2)){
  for(j in 1:length(aux3)){
    if(aux2[i]<aux3[j]){
      cities <- append(cities,substring(locais,aux2[i],aux3[j]))
      break
    }
  }
}

aux2 <- append(aux2, nchar(locais))
locations <-c()
len <- length(aux2)-1
for(i in 1:len){
  locations <- append(locations,substring(locais,aux2[i]+nchar(cities[i]), aux2[i+1]))
}

############ locais beneficiados
d_t <- list(t= c(), a= c())
d_teste <- data.frame(d_t)
for(i in 1:nrow(d_frame)){
  print(i)
  locais <- d_frame[i,9]
  for(i in 1:length(key_city)){
    locais <- gsub(key_city[i],"", locais)
  }
  #posições do \n+texto
  aux2 = gregexpr("\n(\\s)?\\w", locais)[[1]]
  firstword <- min(aux2) #primeira ocorrencia do \n+texto
  locais <- substring(locais,firstword, nchar(locais))
  aux2 = gregexpr("\n(\\s)?\\w", locais)[[1]]
  aux3 <- gregexpr("\\s{3,250}", locais)[[1]]
  
  cities <-c()
  for(i in 1:length(aux2)){
    for(j in 1:length(aux3)){
      if(aux2[i]<aux3[j]){
        cities <- append(cities,substring(locais,aux2[i],aux3[j]))
        break
      }
    }
  }
  aux2 <- append(aux2, nchar(locais))
  locations <-c()
  len <- length(aux2)-1
  for(i in 1:len){
    locations <- append(locations,substring(locais,aux2[i]+nchar(cities[i]), aux2[i+1]))
  }
  outcities <- c()
  outlocs <- c()
  for(i in 1:length(locations)){
    outcities <- paste(outcities,cities[i])
    outlocs <- paste(outlocs,locations[i])
  }
  outlocs <- gsub("\n", ";",outlocs)
  outcities <- gsub("\n", ";",outcities)
  d_teste[i,1] <- outcities
  d_teste[i,2] <- outlocs
}

