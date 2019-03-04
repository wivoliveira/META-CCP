# load the necessary packages
library(raster)
library(sp)
library(rgdal)
library(nleqslv)

# Observations:
  # Most part of the code was written in Portuguese.
  # The symbology used in this code is not exactly the same described in the article


#           ARQUIVO CRIADO PARA PERMITIR A CLASSIFICAÇÃO POR MÁXIMA VEROSSIMILHANÇA DE IMAGENS,
#               BEM COMO A CLASSIFICAÇÃO CONTEXTUAL POR MEIO POR MEIO DO FILTRO DE MAIORIA

# #################################################################################
#                           META-CPP: OUTPUT DATA
# #################################################################################
# DIRECTOTY USED TO SAVE ALL OUTPUT DATA
working_directory <<- "D:\\Research Data\\Results\\Output\\"
setwd(working_directory)

# #################################################################################
#                           META-CPP: INPUT DATA
# #################################################################################

  # Directory where all input data are located (variable used to facilitate the definition of all the required input data)
  base_directory <<- "D:\\Research Data\\Data\\Recorte_L8_OLI\\"

  # STAGE 1 - Construction of classification models
    # LABELLED SAMPLES (Training samples)
    
    # Definition of the number of classes (K) and their names
    K <<- 6
    name_classes <- vector("list", K)
    name_classes[[1]] <- "Secondary vegetation"
    name_classes[[2]] <- "Water"
    name_classes[[3]] <- "Bare soil"
    name_classes[[4]] <- "Forest"
    name_classes[[5]] <- "Clean pasture"
    name_classes[[6]] <- "Dirty pasture"
    name_classes <<- name_classes
    
    # IMPORTANT: remember to list the directories of the sample files considering the same order the classes were defined above
    print("Loading files of training samples...")
    #-------------------
    #---- FROM SHP -----
    #-------------------
    Samples_treino_shp <- vector("list", K)
    Samples_treino_shp[[1]] <- shapefile(paste0(base_directory, "Amostras\\VegSecundaria.shp"))
    Samples_treino_shp[[2]] <- shapefile(paste0(base_directory, "Amostras\\Agua.shp"))
    Samples_treino_shp[[3]] <- shapefile(paste0(base_directory, "Amostras\\Estrada.shp"))
    Samples_treino_shp[[4]] <- shapefile(paste0(base_directory, "Amostras\\Floresta.shp"))
    Samples_treino_shp[[5]] <- shapefile(paste0(base_directory, "Amostras\\PastoLimpo.shp"))
    Samples_treino_shp[[6]] <- shapefile(paste0(base_directory, "Amostras\\PastoSujo.shp"))
    
    # IMAGE IN WHICH THE LABELLED SAMPLES WERE COLLECTED
    raster_phase1 <- paste0(base_directory, "Imagem\\Recorte_Img_PA_B1_B7_20140612.tif")
    
    # Number of classification models to be generated (J)
    J <- 100
    #J <- as.integer(readline(prompt="How many classification models do you want to construct (J)? "))
    
    # Number of samples per class that will be randomly samples at each iteration (to construct each model)
    N <- 200
    #N <- as.integer(readline(prompt="How many samples (per-class) do you want to randomly select at each iteration (N)? "))
    
    
  # Stage 2 - Calculation of per-class variability
    # LABELLED PIXELS
    PixelSet <- vector("list", K)
    PixelSet[[1]] <- shapefile(paste0(base_directory, "Fase 2 - Pixels rotulados e imagem de referencia\\Pixels_RecorteL8_VegSecund.shp"))
    PixelSet[[2]] <- shapefile(paste0(base_directory, "Fase 2 - Pixels rotulados e imagem de referencia\\Pixels_RecorteL8_Agua.shp"))
    PixelSet[[3]] <- shapefile(paste0(base_directory, "Fase 2 - Pixels rotulados e imagem de referencia\\Pixels_RecorteL8_Estrada.shp"))
    PixelSet[[4]] <- shapefile(paste0(base_directory, "Fase 2 - Pixels rotulados e imagem de referencia\\Pixels_RecorteL8_Floresta.shp"))
    PixelSet[[5]] <- shapefile(paste0(base_directory, "Fase 2 - Pixels rotulados e imagem de referencia\\Pixels_RecorteL8_PastoLimpo.shp"))
    PixelSet[[6]] <- shapefile(paste0(base_directory, "Fase 2 - Pixels rotulados e imagem de referencia\\Pixels_RecorteL8_PastoSujo.shp"))
    
    # IMAGE IN WHICH THE LABELLED PIXELS WERE COLLECTED
    raster_phase2 <- paste0(base_directory, "Fase 2 - Pixels rotulados e imagem de referencia\\Imagem\\Recorte_Img_PA_B1_B7_20140612.tif")
  
  # Stage 3: No input data required
    
  # Stage 4 - Image classification and High Discriminability Pixels (HDP) identification
    # Image to be classified
    raster_phase4 <- paste0(base_directory, "Imagem\\Recorte_Img_PA_B1_B7_20140612.tif")
  
  
  # Test samples
    #-------------------
    #---- FROM SHP -----
    #-------------------
    Samples_teste_shp <- vector("list", K)
    Samples_teste_shp[[1]] <- shapefile(paste0(base_directory, "Fase 5 - Amostras de teste\\Samples_Teste_VegSecundaria.shp"))
    Samples_teste_shp[[2]] <- shapefile(paste0(base_directory, "Fase 5 - Amostras de teste\\Samples_Teste_Agua_RioEstreito.shp"))
    Samples_teste_shp[[3]] <- shapefile(paste0(base_directory, "Fase 5 - Amostras de teste\\Samples_Teste_Estrada.shp"))
    Samples_teste_shp[[4]] <- shapefile(paste0(base_directory, "Fase 5 - Amostras de teste\\Samples_Teste_Floresta.shp"))
    Samples_teste_shp[[5]] <- shapefile(paste0(base_directory, "Fase 5 - Amostras de teste\\Samples_Teste_PastoLimpo.shp"))
    Samples_teste_shp[[6]] <- shapefile(paste0(base_directory, "Fase 5 - Amostras de teste\\Samples_Teste_PastoSujo.shp"))
    
  # Stage 5 - Contextual classification
    
    # C-Factor interval (eg., from 1 to 150, with step of 1)
    #seq_C <- c(seq(1, 150, by = 1))
    seq_C <- c(seq(5, 20, by = 5))
    
    # Majority filter: dimension of the moving windows to be used
    #kernel_list <- c(3, 5, 7)
    kernel_list <- c(3, 7)
    
# #################################################################################

# #######################################################################################################################################
  
  
  
  
  
# #################################################################################
# FUNÇÃO PARA GERADAÇÃO DA MATRIZ DE CONFUSÃO E DE SÍNTESE DOS RESULTADOS
# #################################################################################

ConfusionMatrix <- function(stage, OutputName_ConfusionMatrix, ncell_raster_ph4, ncol_raster_ph4, ResultsMatrix, linha_matrix, map_asMatrix, n_kernel, C){
  if (stage == "MLC") { print("MLC: Calculando estatísticas e gerando matriz de confusão.")
  }else if (stage == "MF") { print("MF convencional: Calculando estatísticas e gerando matriz de confusão.")
  }else if (stage == "MF_MPAD") { print("MF MPAD: Calculando estatísticas e gerando matriz de confusão.") }
  
  I <- K
  setwd(working_directory)
  
  #----Criando a matrix de confusão para a classificação inicial----
  CM_nrow <- I + 12
  CM_ncol <- I + 5
  ConfusionMatrix <- matrix(nrow = CM_nrow, ncol = CM_ncol)
  
  if (stage == "MLC") { ConfusionMatrix[1,1] <- "Classificação Inicial - MLC"
  }else if (stage == "MF"){ConfusionMatrix[1,1] <- paste0("Classificação Contextual - MF (sem uso de máscara)")
  }else if (stage == "MF_MPAD"){
    ConfusionMatrix[1,1] <- paste0("Classificação Contextual - MF")
    ConfusionMatrix[1,3] <- paste0("Limiar MPAD: ", C, " x DP")}
  
  ConfusionMatrix[3,1] <- "Class/Refer"
  for (class in 1:I){
    ConfusionMatrix[3, class + 1] <- name_classes[[class]]
    ConfusionMatrix[class + 3, 1] <- name_classes[[class]]
  }
  ConfusionMatrix[3, I + 3] <- "Total"
  ConfusionMatrix[3, I + 4] <- "Acur. Usuário"
  ConfusionMatrix[3, I + 5] <- "Erro Comissão (Inclusão)"
  ConfusionMatrix[I + 5, 1] <- "Total"
  ConfusionMatrix[I + 6, 1] <- "Acur. Produtor"
  ConfusionMatrix[I + 7, 1] <- "Erro Omissão (Exclusão)"
  
  ConfusionMatrix[I + 9, 1] <- "Exatidão Global (observada)"
  
  ConfusionMatrix[I + 11, 1] <- "Exatidão Global (se classif. fosse aleatória)"
  ConfusionMatrix[I + 12, 1] <- "Kappa Global"
  
  # Zerando os contadores em todas as células
  ConfusionMatrix[4:(I+3), 2:(I+1)] <- 0
  
  # Indexadores
  linha <- 1
  coluna <- 1
  
  for (j in 1:ncell_raster_ph4){
    
    if(Samples_teste_global[linha, coluna] != 0){ # irá considerar apenas os pixels referentes à amostras de teste
      # Preenchendo a matriz de confusão
      ConfusionMatrix[map_asMatrix[linha, coluna] + 3, Samples_teste_global[linha, coluna] + 1] <- as.numeric(ConfusionMatrix[map_asMatrix[linha, coluna] + 3, Samples_teste_global[linha, coluna] + 1]) + 1
    }
    
    # Gambiarra para identificar o número da linha e coluna correspondente ao pixel analisado
    if (coluna == ncol_raster_ph4){
      coluna <- 1
      linha <- linha + 1
    }
    else{
      coluna <- coluna + 1
    }
  }

  for (class in 1:I){
    ConfusionMatrix[class + 3, I + 3] <- sum(as.numeric(ConfusionMatrix[class + 3, 2:(I+1)] ))
    ConfusionMatrix[I + 5, class + 1] <- sum(as.numeric(ConfusionMatrix[4:(I+3), class + 1] ))
  }
  
  # Contagem do número total de pixels analisados, avaliando se a contagem é igual se feita por linha ou por coluna
  if ( sum(as.numeric(ConfusionMatrix[I +5, 2:(I+1)] )) == sum(as.numeric(ConfusionMatrix[4:(I+3), I+3] )) ){
    ConfusionMatrix[I+5, I+3] <- sum(as.numeric(ConfusionMatrix[I +5, 2:(I+1)] ))
    
    numeric_matrix <- apply(ConfusionMatrix[4:(I+3), 2:(I+1) ], 2, as.numeric)
    
    ## EXATIDÃO GLOBAL
    ConfusionMatrix[I+9, 2] <- sum(diag(numeric_matrix)) / as.numeric(ConfusionMatrix[I+5, I+3])
    
    if (stage != "MF_MPAD"){
      ResultsMatrix <- rbind(ResultsMatrix, NA)
      linha_matrix <- linha_matrix + 1
    }
    
    if (stage == "MLC") {
      ResultsMatrix[linha_matrix, 1] <- "Classificação Pontual"
    } else if (stage == "MF") {
      ResultsMatrix[linha_matrix, 1] <- paste0("Classificação Contextual - MF ", n_kernel, "X", n_kernel)
    } 
      
    ResultsMatrix[linha_matrix, 3] <- ConfusionMatrix[I+9, 2]
    
  } else {
    ConfusionMatrix[I+5, I+3] <- "Erro na contagem"
  }
  
  ##------------------- CÁLCULO DAS ACURÁCIAS E ERROS -------------------
  for (class in 1:I){
    ConfusionMatrix[class + 3, I + 4] <- as.numeric(ConfusionMatrix[class + 3, class + 1]) / as.numeric(ConfusionMatrix[class + 3, I + 3])
    ConfusionMatrix[class + 3, I + 5] <- 1 - as.numeric(ConfusionMatrix[class + 3, I + 4])
    
    ConfusionMatrix[I + 6, class + 1] <- as.numeric(ConfusionMatrix[class + 3, class + 1]) / as.numeric(ConfusionMatrix[I + 5, class + 1])
    ConfusionMatrix[I + 7, class + 1] <- 1 - as.numeric( ConfusionMatrix[I + 6, class + 1])
  }
  
  if (stage == "MF") {
    ## DANO DE CLASSIFICAÇÃO: ÍNDICE DE PERMANÊNCIA ESTRUTURAL
    ResultsMatrix[linha_matrix, 4] <- round( ((as.numeric(ResultsMatrix[linha_matrix, 3]) / as.numeric(ResultsMatrix[4, 3])) - 1) * 100, 3)
  } else if(stage == "MF_MPAD"){
    ResultsMatrix[linha_matrix, 4] <- round( ((as.numeric(ResultsMatrix[linha_matrix, 3]) / as.numeric(ResultsMatrix[4, 3])) - 1) * 100, 3)
    ResultsMatrix[linha_matrix, 5] <- round( ((as.numeric(ResultsMatrix[linha_matrix, 3]) / as.numeric(ResultsMatrix[linha_matrix_header+1, 3])) -1) * 100, 3)
  }
  
  ## Exatidão total (se a classificação fosse aleatória)
  soma <- 0
  for (class in 1:I){
    soma <- as.numeric(soma) + as.numeric(ConfusionMatrix[class + 3, I + 3]) * as.numeric(ConfusionMatrix[I + 5, class + 1])
  }
  ConfusionMatrix[I + 11, 2] <- soma / as.numeric(ConfusionMatrix[I + 5, I + 3]) ^ 2
  
  ##ÍNDICE KAPPA (GLOBAL)
  ConfusionMatrix[I + 12, 2] <- (as.numeric(ConfusionMatrix[I + 9, 2]) - as.numeric(ConfusionMatrix[I + 11, 2])) / (1 - as.numeric(ConfusionMatrix[I + 11, 2]))
  ResultsMatrix[linha_matrix,6] <- ConfusionMatrix[I + 12, 2]
  
  if (stage == "MF") {
    ResultsMatrix[linha_matrix, 2] <- n_kernel
  }
  
  ##ACURÁCIA DO PRODUTOR
  for (class in 1:I){
    ResultsMatrix[linha_matrix,6+class] <- ConfusionMatrix[I + 6, class+1]
  }
  # -------------------------------------------------------------------------------------------------------------------------------------------
  
  #-------------------- EXPORTANTO A MATRIZ DE CONFUSÃO ---------------------
  write.table(ConfusionMatrix, OutputName_ConfusionMatrix, sep = ",", na = "", row.names = FALSE, col.names = FALSE)
  
  list <- vector("list", 2 )
  list[[1]] <- ResultsMatrix
  list[[2]] <- linha_matrix
  
  return(list)
}


# #################################################################################
# FUNÇÃO PARA CLASSIFICAÇÃO PONTUAL (MÁXIMA VEROSSIMILHANÇA)
# #################################################################################
classificacao_pontual <- function(K, name_classes, Samples_treino_shp, raster_phase1, M, n, PixelSetl, raster_phase2, raster_phase4, Samples_teste_shp){
  print("Iniciando classificação por Máxima Verossimilhança (MLC).")

  respostaImg <- 1 # Imagem Real
  if (respostaImg == 1){ 
    print("Objeto de estudo: Imagem Real.")
    print("Carregando dados...")
    
    I <- K
    
    #Definindo que não há uma classe de fundo de referência, como no caso de imagens sintéticas
    background_class <<- NA
  
    raster_ph1 <- stack(raster_phase1)
  
    # Extraindo os pixels contidos nos poligonos definidos pelos shapefiles
    # Convertendo para uma única matrix
    Samples <- vector("list", I)
    # Criando uma matrix para manter a referenceia de localização da célula
    # Deletando esta coluna da matrix dos valores por banda
    Samples_treino_shp_cellnumber <- vector("list", I)
    Samples_csv <- vector("list", I)
    
    for (class in 1:I){
      Samples[[class]] <- extract(raster_ph1, Samples_treino_shp[[class]], along=TRUE, cellnumbers=TRUE)
      Samples[[class]] <- do.call(rbind, Samples[[class]])
      
      Samples_treino_shp_cellnumber[[class]] <- Samples[[class]][,1]

      Samples[[class]] <- Samples[[class]][,-1] #Deleta a primeira coluna, referent à localização dos pixels na imagem
      Samples_csv[[class]] <- Samples[[class]]
      
      OutputName_SampleMatrix <- paste0("Amostras_treino_", name_classes[[class]], ".csv")
      write.table(Samples[[class]], OutputName_SampleMatrix, sep = ",", na = "", row.names = FALSE, col.names = FALSE)
    }
    
    #Gerando uma máscara para identificar os pixels amostrados e suas respectivas classes
    Samples_mask <- matrix(0, nrow = nrow(raster_ph1), ncol = ncol(raster_ph1), byrow = TRUE)
    Samples_rowCol <- vector("list", I)
    for (class in 1:I){
      Samples_rowCol[[class]] <- rowColFromCell(raster_ph1, Samples_treino_shp_cellnumber[[class]])
      Samples_mask[Samples_rowCol[[class]] ] <- class
    }
    
    # -----------------------------------------------
    # ------------- Exportando amostras -------------
    # -----------------------------------------------
    r <- raster(Samples_mask) #Convertendo a matrix raster_reference para raster
    proj <- projection(raster_ph1) #projeção do raster analisado
    if(is.na(proj) == FALSE){ #Se houver uma projeção (proj != "NA"). Imagem georreferenciada
      projection(r) <- CRS(proj)
    }
    ext <- extent(raster_ph1) #extenção do raster
    rr <- setExtent(r, ext) #atribuindo extensão
    writeRaster(rr, filename = paste0("Samples_treino_mask.tif"), overwrite=TRUE)
    
    
    Samples <- vector("list", I)
    for (class in 1:I){    
      Samples[[class]] <- data.matrix(Samples_csv[[class]])
    }
    
    ##------------------------------- FASE 2 --------------------------------
    ##------------Obtenção de valores de g(x) por pixel rotulado e-----------
    ##--------------modelo, bem como o desvio padrão por classe--------------
    
    raster_ph2 <- stack(raster_phase2)
    
    ##------------------------------- FASE 3 --------------------------------
    ##--------------- Identificação dos modelos representantes --------------
    #                 Faz uso apenas dos produtos gerados nas 
    #                           fases anteriores
    
    ##------------------------------- FASE 4 --------------------------------
    ##--------------------------- Geração da MPAD ---------------------------
    
    # Carrega a imagem que será classificada
    raster_ph4 <<- brick(raster_phase4) # o processamento se mostrou mais rápido ao utilizar o comando brick ao invés de stack
    
    ##-----------------------------------------------------------------------
    ##--------------------------- ANÁLISE DE ERRO ---------------------------
    ##-----------------------------------------------------------------------
    #Gero uma máscara para identificar os pixels amostrados e suas respectivas classes
    raster_reference <- matrix(0, nrow = nrow(raster_ph4), ncol = ncol(raster_ph4), byrow = TRUE)
    
    Samples_shp_rowCol <- vector("list", I)
    for (class in 1:I){
      Samples_shp_rowCol[[class]] <- rowColFromCell(raster_ph4, Samples_treino_shp_cellnumber[[class]])
      raster_reference[Samples_shp_rowCol[[class]] ] <- class
    }
    
    
    # Extraindo os pixels contidos nos poligonos definidos pelos shapefiles
    # Convertendo para uma única matrix
    Samples_teste <- vector("list", I)
    Samples_teste_cellnumber <- vector("list", I)
    
    #Gero uma máscara para identificar os pixels amostrados e suas respectivas classes
    Samples_teste_reference <- matrix(0, nrow = nrow(raster_ph4), ncol = ncol(raster_ph4), byrow = TRUE)
    
    Samples_teste_rowCol <- vector("list", I)
    
    for (class in 1:I){
      Samples_teste[[class]] <- extract(raster_ph4, Samples_teste_shp[[class]], along=TRUE, cellnumbers=TRUE)
      Samples_teste[[class]] <- do.call(rbind, Samples_teste[[class]])
      
      # Criando uma matrix para manter a referenceia de localização da célula
      # Deletando esta coluna da matrix dos valores por banda
      Samples_teste_cellnumber[[class]] <- Samples_teste[[class]][,1]
      Samples_teste[[class]] <- Samples_teste[[class]][,-1]
      
      Samples_teste_rowCol[[class]] <- rowColFromCell(raster_ph4, Samples_teste_cellnumber[[class]])
      Samples_teste_reference[Samples_teste_rowCol[[class]] ] <- class
    }
    
    random_positions <- vector("list", I)
    
    #Define a quantidade de pixels referente a cada classe
    Num_pixels_classe <- vector("list", I)
    menor_length <- 'Primeiro'
    for (class in 1:I){
      Num_pixels_classe[[class]] <- length(Samples_teste[[class]][,1])
      if (Num_pixels_classe[[class]] < menor_length || menor_length == 'Primeiro'){
        menor_length <- Num_pixels_classe[[class]]
        menor_class <- class
      }
    }
    for (class in 1:I){
      random_positions[[class]] <- sample(Samples_teste_cellnumber[[class]], menor_length)
    }
    
    #Gero uma máscara para identificar os pixels amostrados e suas respectivas classes
    Samples_teste_global <- matrix(0, nrow = nrow(raster_ph4), ncol = ncol(raster_ph4), byrow = TRUE)
    
    Samples_teste_global_rowCol <- vector("list", I)
    for (class in 1:I){
      Samples_teste_global_rowCol[[class]] <- rowColFromCell(raster_ph4, random_positions[[class]])
      Samples_teste_global[Samples_teste_global_rowCol[[class]] ] <- class
    }
    
    # Para processamento de imagem real não se utiliza uma máscara de feições por largura para se computar as estatísticas
    # Esta máscara é utilizada apenas quando processado imagem sintética
    respostaMascara <<- 0
    Samples_teste_global <<- Samples_teste_global
    #//FIM. Entrada de dados para imagem Real
  
  }
  
  print(paste0("MLC: ", M, " modelos e ", n, " amostras/modelo"))
  
  OutputName_Map <- "Mapa_classificação_MLC.tif"
  
  #--------- [ResultsMatrix] Matriz a ser exportada contendo uma síntese dos resultados gerados ---------
  RM_nrow <- 3
  RM_ncol <- 8+I
  
  ResultsMatrix <- matrix(nrow =RM_nrow, ncol = RM_ncol)
  
  ResultsMatrix[1,1] <- "Descrição dos resultados obtidos"
  ResultsMatrix[2,1] <- "*IPE: Índice de Permanência Estrutural (%)"
  ResultsMatrix[3,1] <- "Produto de classificação (SEM USO DE MÁSCARA)"
  ResultsMatrix[3,2] <- "Tamanho do kernel (n X n)"
  ResultsMatrix[3,3] <- "Exatidão Global"
  ResultsMatrix[3,4] <- "IPE* (EG[C. Original] / EG[Pontual])"
  ResultsMatrix[3,6] <- "Kappa Global"
  
  col_id <- 6
  for(class_id in 1:I){
    col_id <- col_id + 1
    ResultsMatrix[3,col_id] <- paste0("Ac. Prod - ", name_classes[[class_id]])
  }
  
  OutputName_ResultsMatrix <<- "0Síntese_Resultados.csv"
  
  linha_matrix <- RM_nrow # Contador para manter referência da última linha preenchida
  linha_matrix_header <<- RM_nrow + 1
  #-------------------------------------------------------------------------------------------------------------------------
  
  ##------------------------------------------------------------------------------------------------------------------------
  ##----------------------------------------------------- PROCESSAMENTO ----------------------------------------------------
  ##------------------------------------------------------------------------------------------------------------------------
  
  ##------------------------------------ FASE 1 ------------------------------------
  ##------------------- Construção dos modelos de classificação --------------------
  
  print("MLC: Fase 1 - Gerando modelos de classificação...")
  #--------- Organizando as amostras --------
  
  # Número de faixas espectrais consideradas
  nb <- ncol(Samples[[1]])
  
  #cria uma lista composta por I listas que por sua vez é composta por nb listas. Neste caso, nb=7.
  list_values_band <- vector("list", I)
  for (class in 1:I){
    list_values_band[[class]] <- vector("list", nb)
    for(band in 1:(nb)){ #De 1 até o número total de bandas (7)
      e <- 0
      for(element in 1: (length(Samples[[class]][,1]) )){ #De 1 até o total de elementos em cada lista para cada banda (water: 159, 4 ....)
        e <- e + 1
        list_values_band[[class]][[band]][e] <- Samples[[class]][element,band]
      }
    }
  }
  
  #---------- Definindo uma variável para guardar os parâmetros ("modelos") ----------
  # Criando uma lista
  mean_set <- vector("list", M)
  cov_set <- vector("list", M)
  
  #---------- Gerando os modelos ----------
  for (m in 1:M){ #M representa o número de vezes que g(x) será calculado/repetido (M: número de modelos considerados)
    
    # Lista que receberá as amostras aleatórias
    random_list <- vector("list", I)
    ## Sorteando algumas amostras a partir do conjunto total de amostras
    for (class in 1:I){ 
      random_list[[class]] <- vector ("list", nb)
      for (band in 1:nb){
        random_list[[class]][[band]] <- sample(list_values_band[[class]][[band]], n)
      }
    }
    
    # Calculando o vetor de média e a matriz de covariäncia das amostras para cada classe
    mean_classes <- vector("list", I)
    for (class in 1:I){
      mean_classes[[class]] <- vector ("list", nb)
      for(band in 1:nb){
        #mean vector
        mean_classes[[class]][[band]] <- mean(random_list[[class]][[band]])
      }
    }
    
    # Matriz de covariância
    cov_classes <- vector("list", I)
    for (class in 1:I){
      cov_classes[[class]] <- cov(simplify2array(random_list[[class]])) 
    }
    
    # convertendo variáveis de listas para matrizes
    list_matrix_mean <- vector("list", I)
    for (class in 1:I){
      list_matrix_mean[[class]] <- matrix(unlist(mean_classes[[class]]), byrow = TRUE) 
    }
    
    cov_asList <- vector("list", I)
    for (class in 1:I){
      cov_asList[[class]] <- cov_classes[[class]]
    }
    
    ##-------------------------------------------------------------
    ##----------------------- SAÍDA DE DADOS ----------------------
    ##-------------------------------------------------------------
    mean_set[[m]] <- list_matrix_mean
    cov_set[[m]] <- cov_asList
  }
    
  ##-------------------------------------FASE 2-------------------------------------
  ##------- Nesta fase é obtido valores de g(x) por pixel rotulado e modelo, -------
  ##---------------------- bem como o desvio padrão por classe ---------------------
  print("MLC: Fase 2 - Calculando a variabilidade de cada classe...")
    
  Pixels <- vector("list", I)
  for (i in 1:I){
    Pixels[[i]] <- extract(raster_ph2, PixelSet[[i]])
  }
  
  #---------- Definindo uma variável para guardar os valores obtidos ----------
  # Criando uma matriz
  G_mi_X_pi <- matrix(NA, nrow = 0, ncol = 4)
  
  # Definindo o nome de cada coluna da matriz
  colnames(G_mi_X_pi) <- c("Pixel (p)", "Classe (i)", "Modelo (m)", "g_m,i(x_p,i)")
  
  P <- array(NA, dim = I) # Variável para armazenar a quantidade de pixels para cada classe
  
  
  for (i in 1:I){
    # Definindo a quantidade de pixels #Por classe
    P[i] <- length(Pixels[[i]][,1])
    
    #---------- Calculando as funções discriminantes ----------
    for (p in 1:P[i]){ #P representa o número de pixels
      
      x <- matrix(Pixels[[i]][p, ], nrow = nb, ncol = 1)
      
      for (m in 1:M){ 
        g_x <- log(1/I)-0.5*log(det(cov_set[[m]][[i]])) - 0.5*t(x - mean_set[[m]][[i]]) %*% solve(cov_set[[m]][[i]]) %*% (x - mean_set[[m]][[i]])
        ##----------------------- SAÍDA DE DADOS ----------------------
        ##------------------------- Parte 1/2 -------------------------
        
        #Cria uma nova linha que será adicionada a matriz, contendo os mesmo nomes de colunas
        newRow <- matrix(c(p, i, m, g_x), ncol = 4)
        colnames(newRow) <- colnames(G_mi_X_pi)
        # Adiciona newRow para a matriz
        G_mi_X_pi <- rbind(G_mi_X_pi, newRow)
      }
    }
  }
  
  ##----------- CONTINUAÇÃO FASE 2 ----------
  
  PP <- Reduce("+", P) # Quantidade todal de pixels
  
  #---------- Definindo uma variável para guardar os desvios padrões obtidos ----------
  # Cria uma matriz
  DP_pi<- matrix(NA, nrow = 0, ncol = 3)
  # Definindo o nome de cada coluna da matriz
  colnames(DP_pi) <- c("Pixel (p)", "Classe (i)", "DP(x_p,i)")
  
  for (i in 1:I){
    for (p in 1:P[i]){ #P representa o número de pixels
      # Calcular o desvio padrão dos valores em G_mi_X_pi, por pixel rotulado (pixel p, de classe i)
      DP <- sd(G_mi_X_pi[,4][which((G_mi_X_pi[,1] == p) & (G_mi_X_pi[,2] == i))], na.rm = TRUE) # na.rm -> Should missing values be removed?
      
      # Cria uma nova linha que será adicionada a matriz, contendo os mesmo nomes de colunas
      newRow <- matrix(c(p, i, DP), ncol = 3)
      colnames(newRow) <- colnames(DP_pi)
      # Adiciona newRow para a matriz
      DP_pi <- rbind(DP_pi, newRow)    
    }
  }
  
  #---------- Definindo uma variável para guardar os desvios padrões obtidos ----------
  # Cria uma matriz
  DP_i <- matrix(NA, nrow = 0, ncol = 2)
  # Definindo o nome de cada coluna do dataframe
  colnames(DP_i) <- c("Classe (i)", "DP(x_p,i)")
  
  for (i in 1:I){
    # Calculando o desvio padrão médio dos valores em G_mi_X_pi, por classe
    DP_medio <- mean(DP_pi[,3][which(DP_pi[,2]== i)], na.rm = TRUE) # na.rm -> Should missing values be removed?
    ##-------------------------------------------------------------
    ##----------------------- SAÍDA DE DADOS ----------------------
    ##-------------------------------------------------------------
    
    # Cria uma nova linha que será adicionada ao dataframe, contendo os mesmo nomes de colunas
    newRow <- matrix(c(i, DP_medio), ncol = 2)
    colnames(newRow) <- colnames(DP_i)
    # Adiciona newRow para o dataframe
    DP_i <- rbind(DP_i, newRow)    
  }
  
  DP_i <<- DP_i
  
  ##-------------------------------------FASE 3-------------------------------------
  ##------------------ Identificação dos modelos representantes --------------------
  print("MLC: Fase 3 - Escolhendo um modelo representante para cada classe...")
  
  #--------- Definindo uma variável para guardar os valores médios de g(x) ---------
  # Cria uma matriz
  G_mi <- matrix(NA, nrow = 0, ncol = 3)
  # Definindo o nome de cada coluna do dataframe
  colnames(G_mi) <- c("Modelo (m)", "Classe (i)", "G_m,i")
  
  for (i in 1:I){
    for (m in 1:M){
      # Calculando o valor médio de g(x), por modelo e classe
      g_x <- mean(G_mi_X_pi[,4][which((G_mi_X_pi[,3]== m) & (G_mi_X_pi[,2]== i))], na.rm = TRUE) # na.rm -> Should missing values be removed?
      
      # Cria uma nova linha que será adicionada ao dataframe, contendo os mesmo nomes de colunas
      newRow <- matrix(c(m, i, g_x), ncol = 3)
      colnames(newRow) <- colnames(G_mi)
      # Adiciona newRow para o dataframe
      G_mi <- rbind(G_mi, newRow)    
    }
  }
  
  #--------- Definindo uma variável para guardar os valores médios de g(x) ---------
  # Cria uma matriz
  G_i <- matrix(NA, nrow = 0, ncol = 2)
  # Definindo o nome de cada coluna do dataframe
  colnames(G_i) <- c("Classe (i)", "G_i")
  
  for (i in 1:I){
    # Calcular o valor médio de g(x), por modelo e classe
    g_x <- mean(G_mi[,3][which(G_mi[,2]== i)], na.rm = TRUE) # na.rm -> Should missing values be removed?
    
    # Cria uma nova linha que será adicionada ao dataframe, contendo os mesmo nomes de colunas
    newRow <- matrix(c(i, g_x), ncol = 2)
    colnames(newRow) <- colnames(G_i)
    # Adiciona newRow para o dataframe
    G_i <- rbind(G_i, newRow)
  }
  
  #-- Identificar qual modelo (m) gerou o valor em g_mi mais próximo do valor medio-
  #--------------------------- para cada classe (em g_i) ---------------------------
  
  #--- Definindo uma variável para guardar os valores de diferença ---
  # Cria uma matriz
  Dif <- matrix(NA, nrow = 0, ncol = 5)
  # Definindo o nome de cada coluna do dataframe
  colnames(Dif) <- c("G_mi", "G_i", "Classe (i)", "Modelo (m)", "|Diferença|")
  
  for (j in 1:length(G_mi[,1])){
    # Calcular o valor médio de g(x), por modelo e classe
    D <- abs(G_mi[j,3] - G_i[G_mi[j,2],2])
    
    # Cria uma nova linha que será adicionada ao dataframe, contendo os mesmo nomes de colunas
    newRow <- matrix(c(G_mi[j,3], G_i[G_mi[j,2],2], G_mi[j,2], G_mi[j,1], D), ncol = 5)
    colnames(newRow) <- colnames(Dif)
    # Adiciona newRow para o dataframe
    Dif <- rbind(Dif, newRow)
  }
  
  m_representante <- array(NA, dim = I)
  for (i in 1:I){
    m_representante[i] <- Dif[,4][which( (Dif[,5]== min(Dif[,5][Dif[,3]== i])) & (Dif[,3]== i) )]
  }
  
  # Salva o dataset do R, caso seja precisar reprocessar utilizando os mesmos parâmetros
  save.image(paste0(working_directory,"0Dataset.RData"))
  
  ##-------------------------------------FASE 4-------------------------------------
  ##------------------------------- Geração da MPAD --------------------------------
  print("MLC: Fase 4 - Gerando o mapa de classificação pontual...")
  
  #--------- Cálculo da função MaxVer para toda a imagem, para todas as classes analisadas ---------
  
  #-------------convertendo o raster para matrix para aumentar performance--------------
  
  raster2 <- as.matrix(raster_ph4)
  maxver_output <- matrix(data = NA, nrow = ncell(raster_ph4), ncol = I, byrow = TRUE)
  
  #Calculando partes da equação para favorecer o processamento
  parte1 <- vector("list", I)
  parte2 <- vector("list", I)
  parte3 <- vector("list", I)
  for (i in 1:I){
    parte1[[i]] <- log(1/I)-0.5*log(det(cov_set[[ m_representante[i] ]][[i]])) 
    parte2[[i]] <- mean_set[[ m_representante[i] ]][[i]]
    parte3[[i]] <- solve(cov_set[[ m_representante[i] ]][[i]]) 
  }
  
  #print("MLC: Fase 4 - Calculando matriz de Máxima Verossimilhança...")
  for (i in 1:I){
    b <- calc(raster_ph4, fun <- function(x) { parte1[[i]] - 0.5*t(x - parte2[[i]]) %*% parte3[[i]] %*% (x - parte2[[i]])})
    maxver_output[,i] <- matrix(data = b, ncol = 1, byrow = TRUE)
  }
  maxver_output <<- maxver_output
  #-------------Identificando a classe que cada pixel será associado--------------
  raster_classes <- as.matrix(apply(maxver_output, 1, which.max))
  raster_classes <<- raster_classes
  #--------------------Gerando também o mapa de classificação---------------------
  raster_classes_output <<- matrix(raster_classes, ncol = raster_ph4@ncols, byrow = TRUE)
  
  ##----------------------- SAÍDA DE DADOS ----------------------
  
  #--------------------EXPORTANTO O MAPA DE CLASSIFICAÇÃO --------------------
  Mapa_classificacao <- raster(raster_classes_output) #Convertendo a matrix raster_classes_output para raster
  
  proj <- projection(raster_ph4) #projeção do raster analisado
  if(is.na(proj) == FALSE){ #Se houver uma projeção (proj != "NA"). Imagem georreferenciada
    projection(Mapa_classificacao) <- CRS (proj)
  }
  ext <- extent(raster_ph4) #extenção do raster
  Mapa_classificacao <- setExtent(Mapa_classificacao, ext) #atribuindo extensão
  
  writeRaster(Mapa_classificacao, filename = OutputName_Map, overwrite=TRUE)
  
  #---------------Gerando e exportando também a matriz de confusão----------------
  OutputName_ConfusionMatrix <- paste0("Matriz_", gsub("\\.tif", ".csv", OutputName_Map))
  
  list <- ConfusionMatrix("MLC", OutputName_ConfusionMatrix, ncell(raster_ph4), raster_ph4@ncols, ResultsMatrix, 
                          linha_matrix, raster_classes_output, NULL, NULL)
  
  list <- c(list, Mapa_classificacao)
  return(list)
}



# #################################################################################
# FUNÇÕES PARA CLASSIFICAÇÃO CONTEXTUAL UTILIZANDO O FILTRO DE MAIORIA
# #################################################################################
  
# MÉTODO CONVENCIONAL
filter_function <- function(x, ...) {
  
  i <<- i + 1 # assign to inside FUN's environment # Scoping Assignment
  #célula central
  center <- x[ceiling(length(x)/2)]
  
  #maioria 
  majority <- modal(x, ties = 'NA')
  if (is.na(majority) == TRUE){ # caso não haja uma maioria (majority = 'NA')
    return(center)
  } 
  else{
    return(majority)
  }
}

MajorityFilter <- function(x, w) {
  i <<- 0
  f <- focal(x[[1]], w=w, fun=filter_function, pad=TRUE, i = i, padValue=background_class)
  ## Abordagem adotada para regiões de borda:
  ## LINHAS ADICIONADAS (ACIMA E ABAIXO): compostas por ZERO (padValue=0) ou NA (default)
  ## COLUNAS ADICIONADAS (ESQUERDA E DIREITA): A coluna gerada a esquerda é uma cópida da ÚLTIMA coluna
  ##                                          A coluna gerada a direita é uma cópia da PRIMEIRA coluna
  ##                    0 0 0 0 0 0 0
  ## 1 0 1 0 1          1 1 0 1 0 1 1
  ## 0 2 0 2 0          0 0 2 0 2 0 0
  ## 1 0 2 0 1    ==>   1 1 0 2 0 1 1
  ## 0 1 0 1 2          2 0 1 0 1 2 0
  ## 1 0 2 0 1          1 1 0 2 0 1 1
  ##                    0 0 0 0 0 0 0
  #fim <<- Sys.time()
  return(f)
}

# MÉTODO PROPOSTO (MÁSCARAS MPAD)
filter_function_MPAD <- function(x, ...) {
  # OBSERVAÇÃO: a janela móvel x se desloca em linha, enquanto que a matriz MPAD se desloca em coluna.
  #     SOLUÇÃO: utilizar a transposta da MPAD para continuar utilizando i como índice
  i <<- i + 1 # assign to inside FUN's environment # Scoping Assignment
  
  #célula central
  center <- x[ceiling(length(x)/2)]
  
  #maioria 
  majority <- modal(x, ties = 'NA')
  if (MPAD_transposta[i] == 0){
    if (is.na(majority) == TRUE){ # caso não haja uma maioria (majority = 'NA')
      return(center)
    } 
    else{
      return(majority)
    }
  }
  else{ # Se for Pixel de Alta Discriminabilidade (MPAD == 1)
    return(center)
  }
}

MajorityFilter_MPAD <- function(x, w) {
  i <<- 0
  f <- focal(x[[1]], w=w, fun=filter_function_MPAD, pad=TRUE, i = i, padValue=background_class)
  ## Abordagem adotada para regiões de borda:
  ## LINHAS ADICIONADAS (ACIMA E ABAIXO): compostas por ZERO (padValue=0) ou NA (default)
  ## COLUNAS ADICIONADAS (ESQUERDA E DIREITA): A coluna gerada a esquerda é uma cópida da ÚLTIMA coluna
  ##                                          A coluna gerada a direita é uma cópia da PRIMEIRA coluna
  ##                    0 0 0 0 0 0 0
  ## 1 0 1 0 1          1 1 0 1 0 1 1
  ## 0 2 0 2 0          0 0 2 0 2 0 0
  ## 1 0 2 0 1    ==>   1 1 0 2 0 1 1
  ## 0 1 0 1 2          2 0 1 0 1 2 0
  ## 1 0 2 0 1          1 1 0 2 0 1 1
  ##                    0 0 0 0 0 0 0
  #fim <<- Sys.time()
  return(f)
}

classificacao_MF <- function(seq_C, kernel_list, mlc_map, ResultsMatrix, linha_matrix){
  I <- K
  MPAD_list <- vector("list", ncell(seq_C))
  MPAD_transposta_list <- vector("list", ncell(seq_C))
  primeiro <- 1
  resposta2 <- 1
  
  for (n_kernel in kernel_list){
    w <- matrix(1, nrow = n_kernel, ncol = n_kernel) # estrutura da matrix de convolução
    
    # ----------APLICAÇÃO DO MÉTODO CONVENCIONAL (PARA COMPARAÇÃO A POSTERIORI)----------
    contextual_map <- MajorityFilter(mlc_map, w)
    
    OutputName_ContextualMap_original <- paste0("Mapa_classificação_Contextual",n_kernel, "X",n_kernel,".tif")
    OutputName_ConfusionMatrix <- paste0("Matriz_", gsub("\\.tif", ".csv", OutputName_ContextualMap_original))
    
    list <- ConfusionMatrix("MF", OutputName_ConfusionMatrix, ncell(raster_ph4), raster_ph4@ncols, ResultsMatrix, 
                            linha_matrix, as.matrix(contextual_map), n_kernel, NULL)
    
    ResultsMatrix <- list[[1]] 
    linha_matrix <- list[[2]]
    
    ##----------------- SAÍDA -----------------
    #--------------------EXPORTANTO O MAPA DE CLASSIFICAÇÃO--------------------
    proj <- projection(mlc_map) #projeção do raster analisado
    if(is.na(proj) == FALSE){ #Se houver uma projeção (proj != "NA"). Imagem georreferenciada
      projection(contextual_map) <- CRS (proj)
    }
    ext <- extent(mlc_map) #extenção do raster
    contextual_map <- setExtent(contextual_map, ext) #atribuindo extensão
    
    OutputName_ContextualMap_original <- paste0("Mapa_classificação_Contextual",n_kernel, "X",n_kernel,".tif")
    writeRaster(contextual_map, filename = OutputName_ContextualMap_original, overwrite=TRUE)
    
    # ----------APLICAÇÃO DO MÉTODO PROPOSTO (UTILIZANDO AS MÁSCARAS MPAD) ----------
    resposta <- 1
    
    # Inseri uma linha em branco entre os produtos gerados sem e com uso de máscara
    ResultsMatrix <- rbind(ResultsMatrix, NA)
    linha_matrix <- linha_matrix + 1
    
    # Inseri uma linha com os nomes das colunas
    ResultsMatrix <- rbind(ResultsMatrix, NA)
    linha_matrix <- linha_matrix + 1
    
    ResultsMatrix[linha_matrix,1] <- "Produto de classificação (COM USO DA MPAD)"
    ResultsMatrix[linha_matrix,2] <- "Tamanho do kernel (n X n)"
    #ResultsMatrix[linha_matrix,3] <- "Erro Médio de Classificação"
    ResultsMatrix[linha_matrix,3] <- "Exatidão Global"
    ResultsMatrix[linha_matrix,4] <- "IPE* (EG[C. MPAD] / EG[Pontual])"
    ResultsMatrix[linha_matrix,5] <- "IPE* (EG[C. MPAD] / EG[C. Original])"
    ResultsMatrix[linha_matrix,6] <- "Kappa Global"
    
    col_id <- 6
    for(class_id in 1:I){
      col_id <- col_id + 1
      ResultsMatrix[linha_matrix,col_id] <- paste0("Ac. Prod - ", name_classes[[class_id]])
    }
    
    ResultsMatrix[linha_matrix, 6 + I + 1] <- "Fator C"
    
    c_list <- seq_C # definido acima (juntamente com a lista de kernels)
    c_list_i <- 0
    
    for (c_value in c_list){ 
      print(paste0("MF. Kernel: ", n_kernel, " C: ", c_value))
      c_list_i <- c_list_i + 1
      
      ResultsMatrix <- rbind(ResultsMatrix, NA)
      linha_matrix <- linha_matrix + 1
      
      # Define o valor da constante que multiplicará os desvios padrões
      C <- c_value

      # converte character para número real
      C <- as.double(C)
      
      if (primeiro == 1){ # Caso seja o primeiro kernel analisado deve-se gerar as máscaras MPAD
                          # Caso não seja, não há a necessidade de repetir este processo
        
        MPAD_CxDP <- matrix(data = 0, nrow = raster_ph4@nrows, ncol = raster_ph4@ncols, byrow = TRUE)
        # Contador
        N_PAD_CxDP <-  0 # Número de pixels de alta discriminabilidade
        
        # Indexadores
        linha <- 1
        coluna <- 1
        
        for (j in 1:ncell(raster_ph4)){
          ##------------Identificando a classe referente ao segundo maior valor------------
          maxver_pixel <- maxver_output[j,]
          ##cria uma lista contento os emsmos índices das classes de cobertura analisadas
          classes <- c(1:I)
          ## ID das classes, exceto a classe referente ao maior valor de MaxVer
          classes <- classes[-which.max(maxver_pixel)] 
          #Valores de MaxVer do pixel, exceto o maior valor
          maxver_pixel <- maxver_pixel[-which.max( maxver_pixel )] 
          ## Classe referente ao Maior valor, ou seja, o SEGUNDO maior valor
          class_secondMax <- classes[which.max(maxver_pixel)] 
          
          # --------Identificando os pixels de alta discriminabilidade - PAD -------- 
          # -------- C X DESVIO PADRÃO --------
          if(maxver_output[j, raster_classes[j]] >= maxver_output[j, class_secondMax ] + C * ( (DP_i[raster_classes[j],2])^(2) + (DP_i[class_secondMax, 2])^(2) )^(1/2) ){
            MPAD_CxDP[linha, coluna] <- 1
            N_PAD_CxDP <- N_PAD_CxDP + 1
          }
          # -----------------------------------
          
          # Gambiarra para identificar o número da linha e coluna correspondente ao pixel analisado
          if (coluna == raster_ph4@ncols){
            coluna <- 1
            linha <- linha + 1
          }
          else{
            coluna <- coluna + 1
          }
        }
        MPAD_list[[c_list_i]] <- MPAD_CxDP
        MPAD_transposta_list[[c_list_i]]<- t(MPAD_CxDP)
        MPAD_transposta <<- MPAD_transposta_list[[c_list_i]]
      } else{
        MPAD_CxDP <- MPAD_list[[c_list_i]]
        MPAD_transposta <<- MPAD_transposta_list[[c_list_i]]
      }
      
      ##----------------------- SAÍDA DE DADOS ----------------------
      C2 <- gsub(",", "_", gsub("\\.", "_", C)) # ou gsub("[.]", "_", C))
      OutputName_MPAD_CxDP <- paste0("Máscara_MPAD_", C2, "DP.tif")
      OutputName_ContextualMap_CxDP <- paste0("Mapa_classificação_contextual", n_kernel,"X", n_kernel,"_MPAD_", C2, "DP.tif")
      
      #-------------------------EXPORTANTO A MPAD--------------------------
      MPAD_raster_CxDP <- raster(MPAD_CxDP) #Convertendo a matrix MPAD para raster
      
      proj <- projection(raster_ph4) #projeção do raster analisado
      if(is.na(proj) == FALSE){ #Se houver uma projeção (proj != "NA"). Imagem georreferenciada
        projection(MPAD_raster_CxDP) <- CRS(proj)
      }
      ext <- extent(raster_ph4) #extenção do raster
      MPAD_CxDP <- setExtent(MPAD_raster_CxDP, ext) #atribuindo extensão
      
      writeRaster(MPAD_CxDP, filename = OutputName_MPAD_CxDP, overwrite=TRUE)
      
      ##------------------------------------------------------------------------------------------------------------------------
      ##-------------------------------------------------- FILTRAGEM CONTEXTUAL ------------------------------------------------
      ##------------------------------------------------------------------------------------------------------------------------
      
      w = matrix(1, nrow = n_kernel, ncol = n_kernel) # estrutura da matrix de convolução
      
      ##--------- Organização dos dados em Matriz ----------
      MPAD_CxDP_asMatrix <<- as.matrix(MPAD_CxDP)
      
      r2_CxDP <- MajorityFilter_MPAD(mlc_map, w)
      r2_CxDP_matrix <- as.matrix(r2_CxDP)
      
      
      OutputName_ConfusionMatrix <- paste0("Matriz_", gsub("\\.tif", ".csv", OutputName_ContextualMap_CxDP))
      
      list <- ConfusionMatrix("MF_MPAD", OutputName_ConfusionMatrix, ncell(raster_ph4), raster_ph4@ncols, ResultsMatrix, 
                              linha_matrix, r2_CxDP_matrix, n_kernel, C)
      
      ResultsMatrix <- list[[1]] 
      linha_matrix <- list[[2]]
      
      ##----------------- SAÍDA -----------------
      ResultsMatrix[linha_matrix, 1] <- OutputName_ContextualMap_CxDP
      ResultsMatrix[linha_matrix, 2] <- n_kernel
      
      ResultsMatrix[linha_matrix, 6 + I + 1] <- C
      
      #--------------------EXPORTANTO O MAPA DE CLASSIFICAÇÃO E A MPAD--------------------
      proj <- projection(mlc_map) #projeção do raster analisado
      if(is.na(proj) == FALSE){ #Se houver uma projeção (proj != "NA"). Imagem georreferenciada
        projection(r2_CxDP) <- CRS (proj)
      }
      ext <- extent(mlc_map) #extenção do raster
      r2_CxDP <- setExtent(r2_CxDP, ext) #atribuindo extensão
      
      writeRaster(r2_CxDP, filename = OutputName_ContextualMap_CxDP, overwrite=TRUE)
      
      #removeTmpFiles(h=0) #clear temporary files
      
    } #// FIM: for (c_value in c_list){ 
    primeiro <- 2
    #resposta2 <- readline(prompt="Deseja refazer o processamento utilizando outro kernel para classificação contextual? [Sim: 1; Não: 2]  ")
   
    if (resposta2 == 1 && tail(kernel_list, n=1) != n_kernel){
      ResultsMatrix <- rbind(ResultsMatrix, NA)
      linha_matrix <- linha_matrix + 1
      ResultsMatrix <- rbind(ResultsMatrix, NA)
      linha_matrix <- linha_matrix + 1
      linha_matrix_header <<- linha_matrix
      
      ResultsMatrix[linha_matrix,1] <- "Produto de classificação (SEM USO DE MÁSCARA)"
      ResultsMatrix[linha_matrix,2] <- "Tamanho do kernel (n X n)"
      ResultsMatrix[linha_matrix,3] <- "Exatidão Global"
      ResultsMatrix[linha_matrix,4] <- "IPE* (EG[C. MPAD] / EG[Pontual])"
      ResultsMatrix[linha_matrix,6] <- "Kappa Global"
      
      col_id <- 6
      for(class_id in 1:I){
        col_id <- col_id + 1
        ResultsMatrix[linha_matrix,col_id] <- paste0("Ac. Prod - ", name_classes[[class_id]])
      }
      
    }
  }
  ResultsMatrix <<- ResultsMatrix
  linha_matrix <<- linha_matrix
  write.table(ResultsMatrix, OutputName_ResultsMatrix, sep = ",", na = "", row.names = FALSE, col.names = FALSE)
  
  list <- vector("list", 2)
  list[[1]] <- ResultsMatrix
  list[[2]] <- linha_matrix
  return(list)
}


# #################################################################################
# FUNÇÃO DE INICIALIZAÇÃO DO PROCESSO DE CLASSIFICAÇÃO (DEFINE OS DADOS DE ENTRADA)
# #################################################################################
classificacao <- function(K, name_classes, Samples_treino_shp, raster_phase1, M, n, PixelSetl, raster_phase2, raster_phase4, Samples_teste_shp, seq_C, kernel_list){
  print("Iniciando classificador...")
  
  # ----------Classificação Pontual - Máxima Verossimilihança----------
  list <- classificacao_pontual(K, name_classes, Samples_treino_shp, raster_phase1, M, n, PixelSetl, raster_phase2, raster_phase4, Samples_teste_shp)
  
  ResultsMatrix <<- list[[1]]
  linha_matrix <<- list[[2]]
  mlc_map <<- list[[3]]
  
  backup_Pontual_list <<- list
  
  print("Método contextual: Filtro de Maioria.")
  
  list <- classificacao_MF(seq_C, kernel_list, mlc_map, ResultsMatrix, linha_matrix)
  ResultsMatrix <- list[[1]] 
  linha_matrix <- list[[2]]
    
  return(list[[1]]) #Retornando a matriz de síntese dos resultados
}

#Inicializa o código
ResultsMatrix <- classificacao(K, name_classes, Samples_treino_shp, raster_phase1, J, N, PixelSetl, raster_phase2, raster_phase4, Samples_teste_shp, seq_C, kernel_list)

print("The classification was completed.")
