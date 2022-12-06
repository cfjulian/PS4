rm(list = ls())

# Cargamos los paquetes
library(pacman)
p_load(tidyverse, rstudioapi)

#Directorio
setwd(dirname(getActiveDocumentContext()$path))
getwd()

#Cargar datos
train <- read.csv("train.csv")
test <- read.csv("test.csv")
train$milloz <- train$id 
test$milloz <- test$id
#estudiamos la base
head(train)
head(test)
glimpse(train)
glimpse(test)

#Miramos a quien pertenece cada tweet en train
table(train$name)

####Limpieza####
#cargamos librerias
p_load("stopwords", "stringi", "tm", "rvest")

# Creamos una lista con todos los stopwords en español
stopwords_español <- stopwords::stopwords("es", source = "snowball")

# Eliminamos los acentos de los stopwords
stopwords_español <- stri_trans_general(str = stopwords_español,
                                        id = "Latin-ASCII")

####Normalizamos nuestros textos####
# Eliminamos los acentos de train y test
train$text <- stri_trans_general(str = train$text, id = "Latin-ASCII")
test$text <- stri_trans_general(str = test$text, id = "Latin-ASCII")

#Ponemos todo en minúscula
train$text <- tolower(train$text)
test$text <- tolower(test$text)

# Reemplazamos todos los caracteres no alfanumericos con un espacio
train$text <- str_replace_all(train$text, "[^[:alnum:]]", " ")
test$text <- str_replace_all(test$text, "[^[:alnum:]]", " ")

# Eliminamos los números
train$text <- gsub('[[:digit:]]+', '', train$text)
test$text <- gsub('[[:digit:]]+', '', test$text)

# Quitamos stopwords
train$text <- removeWords(train$text, stopwords_español)
test$text <- removeWords(test$text, stopwords_español)

# Eliminamos todos los espacios extras
train$text <- gsub("\\s+", " ", str_trim(train$text))
test$text <- gsub("\\s+", " ", str_trim(test$text))

####Lematización####

# Usando el comando automático de R con una aproximación más eficiente
p_load(udpipe)

# Creamos el id de cada documento
train$id <- paste0("doc", 1:nrow(train))
test$id <- paste0("doc", 1:nrow(test))


# Descargamos el modelo pre entrenado
udmodel <- udpipe_download_model(language = "spanish", model_dir = getwd(),
                                 overwrite = TRUE)
modelo <- udpipe_load_model(file = udmodel$file_model)

#corremos el modelo
x <- udpipe_annotate(modelo, x = train$text)
tidy_train <- as.data.frame(x)

y <- udpipe_annotate(modelo, x = test$text)
tidy_test <- as.data.frame(y)

#Quitamos NAs de lemma
tidy_train <- subset(tidy_train, !is.na(lemma))
table(!is.na(tidy_train$lemma))
tidy_test <- subset(tidy_test, !is.na(lemma))
table(!is.na(tidy_test$lemma))

#Ejemplo de como queda un tweet vs original
tidy_train[tidy_train$doc_id == "doc101", "lemma"]
train[train$id == "doc101", "text"]

#Contar la frecuencia de ocurrencia de cada palabra para cada documento
word_count_train <- tidy_train %>%
  group_by(doc_id) %>%
  count(lemma) %>%
  ungroup()

word_count_test <- tidy_test %>%
  group_by(doc_id) %>%
  count(lemma) %>%
  ungroup()

#Contar la frecuencia de ocurrencia de cada palabra en todos los documentos y
#filtrar palabras con ocurrencia menor a 20 (raras) y mayor a 
#(total de documentos)*0.5 (muy frecuentes)
filtro_palabras_train <- word_count_train %>% 
  count(lemma) %>% 
  mutate(filtro1 = n <= 20,
         filtro2 = n >= 5538*0.5,
         filtro = !(filtro1 |filtro2)) %>% 
  select(-n)

filtro_palabras_test <- word_count_test %>% 
  count(lemma) %>% 
  mutate(filtro1 = n <= 20,
         filtro2 = n >= 1500*0.5,
         filtro = !(filtro1 |filtro2)) %>% 
  select(-n)

#aplicar filtro
word_count_train <- word_count_train %>% 
  left_join(filtro_palabras_train) %>% 
  filter(filtro) %>% 
  select(-filtro, -filtro1, -filtro2)

word_count_test <- word_count_test %>% 
  left_join(filtro_palabras_test) %>% 
  filter(filtro) %>% 
  select(-filtro, -filtro1, -filtro2)

#Se colaron stopwords, se vuelven a eliminar
# Eliminamos los acentos de train y test
word_count_train$lemma <- stri_trans_general(str = word_count_train$lemma,
                                             id = "Latin-ASCII")
word_count_test$lemma <- stri_trans_general(str = word_count_test$lemma,
                                            id = "Latin-ASCII")
#Quitamos stopwords
filtro_train <- !(word_count_train$lemma %in% stopwords_español)
word_count_train <- word_count_train[filtro_train, ]

filtro_test <- !(word_count_test$lemma %in% stopwords_español)
word_count_test <- word_count_test[filtro_test, ]

#Se crea la matriz donde las palabras están relacionadas con los textos
p_load(tm, tidytext)
train_dtm <- cast_dtm(data = word_count_train, 
         document = doc_id, 
         term = lemma, 
         value = n)
inspect(train_dtm)

test_dtm <- cast_dtm(data = word_count_test, 
                      document = doc_id, 
                      term = lemma, 
                      value = n)
inspect(test_dtm)

#Visualicemos las palabras más relevantes
#train
p_load(wordcloud)
freqtrain <- sort(colSums(as.matrix(train_dtm)), 
              decreasing = T)
dev.new(width = 1000, height = 1000, unit = "px")
wordcloud(names(freqtrain), freqtrain, max.words = 50,
          random.order = FALSE, min.freq = 0,
          colors = brewer.pal(8, "Accent"),
          scale = c(4, 0.5), rot.per = 0)

#test
freqtest <- sort(colSums(as.matrix(test_dtm)), 
                  decreasing = T)
dev.new(width = 1000, height = 1000, unit = "px")
wordcloud(names(freqtest), freqtest, max.words = 50,
          random.order = FALSE, min.freq = 0,
          colors = brewer.pal(8, "Accent"),
          scale = c(4, 0.5), rot.per = 0)

#Matriz pero ahora con pesos 
train_dtm2 <- cast_dtm(data = word_count_train, 
                         document = doc_id, 
                         term = lemma, 
                         value = n,
                         weighting = tm::weightTfIdf)
inspect(train_dtm2)

test_dtm2 <- cast_dtm(data = word_count_test, 
                       document = doc_id, 
                       term = lemma, 
                       value = n,
                       weighting = tm::weightTfIdf)
inspect(test_dtm2)

#Visualicemos las palabras más relevantes
#train
p_load(wordcloud)
freqtrain2 <- sort(colSums(as.matrix(train_dtm2)), 
                  decreasing = T)
dev.new(width = 1000, height = 1000, unit = "px")
wordcloud(names(freqtrain), freqtrain, max.words = 50,
          random.order = FALSE, min.freq = 0,
          colors = brewer.pal(8, "Accent"),
          scale = c(4, 0.5), rot.per = 0)

#test
freqtest2 <- sort(colSums(as.matrix(test_dtm2)), 
                 decreasing = T)
dev.new(width = 1000, height = 1000, unit = "px")
wordcloud(names(freqtest), freqtest, max.words = 50,
          random.order = FALSE, min.freq = 0,
          colors = brewer.pal(8, "Accent"),
          scale = c(4, 0.5), rot.per = 0)

#Re-escalar a enteros para hacer LDA
X <- as.matrix(train_dtm2)
X_std <- (X - min(X)) / (max(X) - min(X))
X_scaled <- X_std * (1000 - 0) + 0
X_scaled <- round(X_scaled, 0)

Y <- as.matrix(test_dtm2)
Y_std <- (Y - min(Y)) / (max(Y) - min(Y))
Y_scaled <- Y_std * (1000 - 0) + 0
Y_scaled <- round(Y_scaled, 0)

# Por toda esta gestión toca volver a hacer el proceso desde cero
#train

train_dtm3 <- X_scaled %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  cast_dtm(document = rowname,
           term = name, 
           value = value)
inspect(train_dtm3)

dim(train_dtm3)

#test
test_dtm3 <- Y_scaled %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  cast_dtm(document = rowname,
           term = name, 
           value = value)
inspect(test_dtm3)

dim(test_dtm3)

#LDA####
# Ahora estamos listos para usar LDA
p_load(topicmodels)
train_lda <- LDA(train_dtm3, k = 3, 
                   control = list(seed = 666))

train_lda

#Probabilidad de que la palabra pertenezca a cada tema
train_topics <- tidy(train_lda, matrix = "beta")
train_topics

#Términos más comunes
train_top_terms <- train_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

train_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

#Documento por tema
train_documents <- tidy(train_lda, matrix = "gamma")
train_documents

#Tema elegido en test
train_documents <- tidy(train_lda, matrix = "gamma")
train_documents

#Predicción sobre train
topics_train <- topics(train_lda)

#Predicción sobre test
test_topics <- posterior(train_lda,test_dtm3)
(test_topics <- apply(test_topics$topics, 1, which.max))
test_topics

#Unimos predicciones con las bases
df <- as.data.frame(topics_train, row.names = NULL)
df <- cbind(id = rownames(df), df,
          row.names = NULL)
train_final <- left_join(train, df, by="id")

df1 <- as.data.frame(test_topics, row.names = NULL)
df1 <- cbind(id = rownames(df1), df1,
             row.names = NULL)
test_final <- left_join(test, df1, by="id")

#Miramos que tan buena es la clasificación dependiendo del tema
#analisis1: tema 1=Petro, 2=Uribe, 3=Lopez
analisis1 <- train_final
analisis1$prediccion <- ifelse(analisis1$topics_train==1, "Petro", 
                               ifelse(analisis1$topics_train==2, "Uribe",
                                      "Lopez"))
table(analisis1$name==analisis1$prediccion)
#analisis2: 1=Uribe, 2=Petro, 3=Lopez 
analisis2 <- train_final
analisis2$prediccion <- ifelse(analisis2$topics_train==2, "Petro", 
                               ifelse(analisis2$topics_train==1, "Uribe",
                                      "Lopez"))
table(analisis2$name==analisis2$prediccion)
#analisis3: 1=Petro, 2=Lopez, 3=Uribe
analisis3 <- train_final
analisis3$prediccion <- ifelse(analisis3$topics_train==1, "Petro", 
                               ifelse(analisis3$topics_train==3, "Uribe",
                                      "Lopez"))
table(analisis3$name==analisis3$prediccion)
#analisis4: 1=Uribe, 2=Lopez, 3=Petro
analisis4 <- train_final
analisis4$prediccion <- ifelse(analisis4$topics_train==3, "Petro", 
                               ifelse(analisis4$topics_train==1, "Uribe",
                                      "Lopez"))
table(analisis4$name==analisis4$prediccion)
#analisis5: 1=Lopez, 2=Uribe, 3=Petro
analisis5 <- train_final
analisis5$prediccion <- ifelse(analisis5$topics_train==3, "Petro", 
                               ifelse(analisis5$topics_train==2, "Uribe",
                                      "Lopez"))
table(analisis5$name==analisis5$prediccion)
#analisis6: 1=Lopez, 2=Petro, 3=Uribe
analisis6 <- train_final
analisis6$prediccion <- ifelse(analisis6$topics_train==2, "Petro", 
                               ifelse(analisis6$topics_train==3, "Uribe",
                                      "Lopez"))
table(analisis6$name==analisis6$prediccion)

#El mejor es el modelo 6 (1=Lopez,2=Petro,3=Uribe)

#Unimos a sample_submission
sample_submission <- read.csv("sample_submission.csv")
sample_submission <- subset(sample_submission, select = -name)

test_final$name <- ifelse(test_final$test_topics==2, "Petro", 
                          ifelse(test_final$test_topics==3,
                                 "Uribe", "Lopez"))
test_final <- subset(test_final, select = -id)
colnames(test_final)[colnames(test_final) == "milloz"] ="id"
sample_submission <- left_join(sample_submission,
                               test_final, by="id")
sample_submission <- subset(sample_submission,
                            select = -c(text, test_topics))

write.csv(sample_submission,
          file="C:/Users/Luz Myriam Fonseca/Documentos/Julian/9 Semestre/BDMLAE/Taller 4/Camargo_Daniels_Gonzales.csv",
          fileEncoding = "UTF-8",
          row.names = FALSE)
read.csv("Camargo_Daniels_Gonzales.csv")
