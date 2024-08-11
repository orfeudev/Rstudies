# pacote necessário para ler os arquivos com UTF-8/Latin
library(readr)
# linha de codigo que puxa o arquivo, basta substituir pelo caminho correto. delim=”;” é o
# delimitador de um dado pro outro.
dados <- read_delim("C:/Users/orfeu/Desktop/trabalho-AED/Notas_capes_2017.csv", delim = ";", locale =
                      locale(encoding = "latin1"))
# Se "latin1" não funcionar, tente "UTF-8"
# dados <- read_delim("C:/Users/orfeu/Desktop/trabalho-AED/Notas_capes_2017.csv",delim = ";", locale = locale(encoding = "UTF-8"))
unicos_ifes <- unique(dados$ifes)
# Exibir os valores únicos
print(unicos_ifes)
# arquivo CSV com todas as universidades do arquivo para criação do script.
write.csv(unicos_ifes, "C:/Users/orfeu/Desktop/trabalho-AED/unique_ifes.csv", row.names = FALSE)
# Função para mapear a sigla da universidade à região
get_region <- function(uf) {
  nordeste <- c("UFC", "UFBA", "UFAL", "UFPB", "UFPE", "UFRN", "UFMA", "UFS", "UFPI",
                "UNILAB", "UNIVASF",
                "UNIFOR", "UECE", "UFRPE", "UVA-CE", "URCA", "IFCE", "UNILAB", "UNI7",
                "UFCA", "UERN",
                "UFERSA", "UNP", "IFRN", "UNICHRISTUS", "UFPB/J.P.", "UFPB/AREIA",
                "UFPB/RT", "UNIPÊ",
                "UEPB", "IFPB", "UFCG", "UFPE", "UNICAP", "FESP/UPE", "IMIP",
                "NESC/CPQAM", "IFPE",
                "FJN", "ITEP", "FBV", "CESAR", "UNIVASF", "FPS", "FACENE", "UFAL", "UNEAL",
                "CESMAC",
                "FG", "FADIC", "FUFSE", "UNIT-SE", "IFS", "UNIT/ALAGOAS", "UFBA", "UEFS",
                "UCSAL",
                "UNEB", "UESB", "UESC", "EBMSP", "IFBA", "UNIFACS", "UFRB", "CIMATEC",
                "FTC", "CPQGM",
                "FAMAM", "UFOB", "IFBAIANO")
  norte <- c("UNIR", "UFAC", "UFAM", "INPA", "IFAM", "UNINILTON", "UEA", "UFRR",
             "UERR", "CPQLMD/FIOCRUZ",
             "CESAR-AM", "UNIFAP", "UFPA", "UFRA", "UNAMA", "UEPA", "IFPA", "UFOPA",
             "IEC", "CESUPA",
             "FSCMPA", "ITV DS", "UNIFESSPA", "MPEG", "UFT", "UFMA", "UEMA",
             "IFMA/MC", "UNICEUMA",
             "FUFPI", "UESPI", "IFPI", "UNINOVAFAPI")
  centro_oeste <- c("UnB", "UFG", "UFMS", "UFMT", "UFT")
  sudeste <- c("UFMG", "UFJF", "UFRJ", "UFF", "UFRRJ", "UNIFESP", "UFES", "UFSJ",
               "UFU")
  sul <- c("UFPR", "UFRGS", "UFSC", "UFFS", "FURG")
  if (uf %in% nordeste) {
    return("Nordeste")
  } else if (uf %in% norte) {
    return("Norte")
  } else if (uf %in% centro_oeste) {
    return("Centro-Oeste")
  } else if (uf %in% sudeste) {
    return("Sudeste")
  } else if (uf %in% sul) {
    return("Sul")
  }
  return(NA)
}
# Adiciona a coluna 'regiao' ao data.frame com base na sigla da instituição
dados$regiao <- sapply(dados$ifes, get_region)
# Seleciona apenas os cursos acadêmicos
cursos_academicos <- subset(dados, Mod == "Acad")
# Seleciona todas as pós-graduações do Nordeste
pos_graduacoes_nordeste <- subset(cursos_academicos, regiao == "Nordeste")
# Seleciona somente as pós-graduações do Nordeste com Nota >= 6
pos_graduacoes_nordeste_nota <- subset(pos_graduacoes_nordeste, Nota >= 6)
# Cria um data.frame apenas com os cursos da área de "MATEMÁTICA E ESTATÍSTICA"
matematica_estatistica <- subset(cursos_academicos, area == "MATEMÁTICA E
ESTATÍSTICA")
# Seleciona apenas aqueles cursos de Matemática e Estatística com nota inferior a 4
matematica_estatistica_nota_baixa <- subset(matematica_estatistica, Nota < 4)
# Seleciona somente os cursos de Matemática e Estatística com publicações A1 >= 10
matematica_estatistica_publicacoes_a1 <- subset(matematica_estatistica_nota_baixa, A1
                                                >= 10)
# No data.frame original, cria uma coluna somando o total de publicações A1, A2, ...
dados$total_publicacoes <- rowSums(dados[, c("A1", "A2", "B1", "B2", "B3", "B4", "B5",
                                             "C")], na.rm = TRUE)
# Verificando os resultados
head(cursos_academicos)
head(pos_graduacoes_nordeste)
head(pos_graduacoes_nordeste_nota)
head(matematica_estatistica)
head(matematica_estatistica_nota_baixa)
head(matematica_estatistica_publicacoes_a1)
head(dados$total_publicacoes)