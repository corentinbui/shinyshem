library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(lubridate)

#-------------------- Fonctions

# ----------------- Preparation des colonnes et de leurs attributs


prep_usines <- function()
{
  noms_usines_artouste <- c("Artouste_Lac", "Artouste_Usine", "Bious", "Pont_de_Camps")
  noms_usines_hourat <- c("Miegebat", "Hourat", "Geteu", "Castet", "Fabreges")
  noms_usines_valentin <- c("Eaux_bonnes", "Assouste", "Espalungue")
  noms_usines_mareges <- c("Mareges_Usine", "Saint_Pierre", "Coindre")
  noms_usines_louron <- c("Lassoula", "Clarabide", "Lapes", "Pont_de_Prat", "Aube", "Pont_d_Estagnou")
  noms_usines_eget <- c("Eget_Usine", "Oule")
  liste_usines <- c(noms_usines_artouste, noms_usines_hourat, noms_usines_valentin, noms_usines_mareges, noms_usines_louron, noms_usines_eget)
  return(liste_usines)
}

prep_col <- function(liste_usines)
{
  liste_prix <- c("Prix_spot", "Prix_OA1C", "Prix_OA2C", "Prix_OA4C", "Prix_OA5C")
  noms_colonnes <- c("Dates", liste_prix, liste_usines)
  return(noms_colonnes)
}

prep_gpmt <- function()
{
  return(liste_gpmt <- c(rep("Artouste", 4), rep("Hourat", 5), rep("Valentin", 3), rep("Mareges", 3), rep("Louron", 6), rep("Eget", 2)))
}

prep_attr <- function(noms_colonnes, liste_gpmt)
{
  liste_tarifs <- c(rep("Prix_spot", 6), "Prix_OA4C", rep("Prix_OA5C", 5), rep("Prix_spot", 6), "Prix_OA4C", "Prix_OA5C", "Prix_OA1C", "Prix_spot", "Prix_OA5C")
  noms_gpmt <- c(NA, "prix_spot", rep("prix_OA", 4), liste_gpmt)
  noms_tarifs <- c(rep(NA, 6), liste_tarifs)
  df_attributs <- data.frame(noms_gpmt, noms_tarifs)
  df_attributs <- t(df_attributs)
  colnames(df_attributs) <- noms_colonnes
  
  return(df_attributs)
}

# -------------- Chargement du fichier de donnees

load_local <- function(path) {
  prod_shem <- read.csv2(path)
  return(prod_shem)
}

prep_shem <- function(prod_shem, liste_usines, noms_col, granu)
{
  colnames(prod_shem) <- noms_col #On renomme les colonnes
  prod_shem[, liste_usines] = apply(prod_shem[, liste_usines], 2, as.numeric) #On force tout en numérique, même si ça échoue lamentablement
  prod_shem <- prod_shem %>% 
    mutate(Dates = as.POSIXct(Dates, format = "%d/%m/%Y %H:%M")) %>%  #Formatage des dates
    mutate(Annee = lubridate::year(Dates)) %>% #Ajout d'une colonne "Annee"
    mutate(!!granu := f_granu(Dates, granu)) %>% #Ajout d'une colonne "granu" (dynamique)
    mutate("temp_peak" = ifelse(hour(Dates) >= 8 & hour(Dates) < 20, "peak", "base")) %>% #peak entre 8h et 20h 365
    mutate("temp_hc_hp" = ifelse(hour(Dates) >= 6 & hour(Dates) < 22, "HP", "HC")) %>% #heures de pointe entre 6 et 22h, heures creuses sinon, 365
    mutate("temp_ete_hiver" = ifelse(month(Dates) >= 4 & month(Dates) <= 10, "été", "hiver")) %>%  #ete d'avril à octobre inclus, hiver sinon
    mutate("temp_super_p" = ifelse((month(Dates) <= 3   |   month(Dates) >= 12)     &     ((hour(Dates) >= 9 & hour(Dates) < 11)  |  (hour(Dates) >= 18 & hour(Dates) < 20))  , "SPP", "")) %>% #heures de super pointe 9h-11h et 18h-20h de décembre à février inclus 
    mutate("Tranche" = paste(temp_peak, temp_hc_hp, temp_ete_hiver, temp_super_p, sep = " ")) %>% #Concaténation des tranches dans une seule colonne
    select(-starts_with("temp")) # Suppression des colonnes temporaires base, peak, etc.
  # prod_shem[is.na(prod_shem)] <- 0 #On remplace les NA par des 0
  # prod_shem[is.character(prod_shem)] <- 0 #On remplace les caractères par des 0
  # prod_shem[!is.numeric(prod_shem)] <- 0
}

# ------------------------ Selection des colonnes

selection_var <- function(selec_col, noms_colonnes, attributs_col, noms = "noms_gpmt")
{
  selec_var <- c()
  for(nom_colonne in noms_colonnes) {
    if(attributs_col[noms, nom_colonne] %in% selec_col) {selec_var <- c(selec_var, nom_colonne)}
  }
  return(selec_var)
}

#---------- Choix de la granularite

f_granu <- function(x, granularity) 
{
  switch(granularity,
         "Annee" = lubridate::year(x),
         "Mois" = lubridate::month(x),
         "Semaine" = lubridate::week(x),
         "Jour" = lubridate::yday(x)
  )
}

#---------- Filtre sur les dates

filter_dates <-function(prod_shem, date_debut, date_fin)
{
  prod_shem %>%
    filter(Dates >= as.POSIXct(date_debut) & Dates <= as.POSIXct(date_fin))
}

#---------- Ajout de colonnes intermédiaires utiles pour le group_by (Année et Granularité)

ajout_col_granu <- function(prod_shem, granu)
{
  prod_shem  %>% 
    mutate(!!granu := f_granu(Dates, granu))
}

#---------- Construction de la liste des colonnes pour group_by

col_groupby <- function(tranche, agreg, granu)
{
  col <- c()
  if(agreg == "Annee") {col <- c("Annee", col)}
  if(tranche == TRUE) {col <- c(col, "Tranche")}
  else{
    ifelse(granu != "Heure", col <- c(col, granu), col <- c("Dates")) #col Dates si on est en granularité horaire : pas de filtre temporel
  }
  return(col)
}

#---------- Filtre avec col groupby

filter_col <- function(prod_shem, f_calcul, liste_var, col)
{
  prod_shem %>%
    group_by_at(vars(!!col)) %>%
    summarise_at(liste_var, f_calcul)
}

#---------- Calcul des valos

calcul_valo <- function(prod_shem_valo, attr_col, liste_usines)
{
  for(nom_col in liste_usines){
    tarif = attr_col["noms_tarifs", nom_col]
    prod_shem_valo[nom_col] <- prod_shem_valo[nom_col] * prod_shem_valo[tarif]
  }
  return(prod_shem_valo)
}

#---------- Calcul des prod ou valo par groupement

calcul_par_grpm <- function(prod_shem, liste_gpmt, usine_ou_gpmt, liste_usines, attr_col)
{
  if(usine_ou_gpmt == "gpmt")
  {
    for(gpmt in liste_gpmt) #Initialisation des colonnes agregation par groupement
    {
      prod_shem <- prod_shem %>%
        mutate(!!gpmt := 0)
    }
    
    for(usine in liste_usines) #Calcul iteratif des prod agregees par groupement
    {
      gpmt_usine = attr_col["noms_gpmt", usine]
      # if(gpmt_usine %in% liste_gpmt) {prod_shem[, gpmt_usine] = prod_shem[, gpmt_usine] + prod_shem[, usine]}
      if(gpmt_usine %in% liste_gpmt) {prod_shem[, gpmt_usine] = rowSums(prod_shem[, c(gpmt_usine, usine)], na.rm = T)} #Somme des colonnes avec rowSums au lieu de simplement l'opérateur + pour avoir l'argument na.rm
    }
  }
  return(prod_shem)
}








#---------------------ShinyApp proprement dite









server <- function(input, output)
{
  
  # ----------------- Preparation des colonnes et de leurs attributs
  
  liste_usines <- prep_usines()
  noms_col <- prep_col(liste_usines)
  liste_gpmt <- prep_gpmt()
  attr_col <- prep_attr(noms_col, liste_gpmt)  
  
  # ------------- Import du fichier de donnees apres appui sur le bouton de telechargement   
  
  prod_shem <- reactive({
    
    # ----------------- Import des donnees de prod --- pas de dplyr à cause du colnames que je sais pas faire
    prod_shem_import <- load_local(input$fichier_data$datapath)
    prod_shem_import <- prep_shem(prod_shem_import, liste_usines, noms_col, input$granu)
    return(prod_shem_import)
  })
  
  # ------------- Sélection des variables pour les tableaux de prod ou de valo  
  
  selec_variables <- reactive({
    # -------------- Initialisation des variables à afficher
    
    liste_prix <- selection_var(selec_col = input$selec_prix, noms_col, attr_col)
    var_selec <- c("Dates", "Tranche", "Jour", "Semaine", "Mois", "Annee") 
    liste_var <- c()
    
    if(input$puissance_ou_energie == "puissance") #Si on calcule en moyenne, on affiche les prix (en somme ça n'a plus de sens)
    {
      var_selec <- c(var_selec, liste_prix)
      liste_var <- liste_prix
    }
    
    # --------------------- Sélection des variables
    
    selec_par_tarif <- selection_var(input$selec_tarif, noms_col, attr_col, "noms_tarifs") #Usines filtrées par tarification
    selec_usines <- selec_par_tarif #Usines filtrées par tarif d'abord, éventuellement par groupement ensuite
    
    if(input$usine_ou_gpmt == "gpmt")
    {
      liste_gpmt <- input$selec_gpmt #Selection des groupements a afficher
      
      # -------------- Selection des variables à afficher
      
      var_selec <- c(var_selec, liste_gpmt) #Variables à afficher à la fin
      liste_var <- c(liste_var, liste_gpmt)  #Variables sur lesquelles effectuer le summarize_at final
    }
    else
    {
      # -------------- Selection des variables à afficher 
      
      selec_par_gpmt <- selection_var(input$selec_gpmt, noms_col, attr_col, "noms_gpmt") #Usines filtrées par groupement
      selec_usines <- intersect(selec_par_gpmt, selec_par_tarif) #Usines filtrées par tarif ET par groupement
      var_selec <- c(var_selec, selec_usines) #Variables à afficher à la fin
      liste_var <- c(liste_var, selec_usines) #Variables sur lesquelles effectuer le summarize_at final
    }
    
    # ----------------- Somme ou moyenne selon si on calcule en energie ou puissance
    
    ifelse(input$puissance_ou_energie == "puissance", f_calcul <- mean, f_calcul <- sum)
    
    # -------------- Construction des colonnes pour les group_by
    
    col <- col_groupby(input$tranche, input$agreg, input$granu)
    
    # --------------- Rangement dans une liste à retourner
    
    liste_retour <- list("var_selec" = var_selec, "liste_var" = liste_var, "f_calcul" = f_calcul, "col_groupby" = col, "selec_usines" = selec_usines)
    return(liste_retour)
  })
  
  # ------------- Affichage du header et du tail du fichier importe dans l'onglet Import des donnees
  
  output$dim_table <- renderText(
    {
      req(input$fichier_data)
      paste("Vous avez importe un tableau de ", dim(prod_shem())[1], "lignes et", dim(prod_shem())[2], "colonnes.") 
    }
  )
  
  output$apercu_debut <- renderText(
    {
      req(input$fichier_data)
      "Apercu debut :"
    }
  )
  
  output$table_output_head <- DT::renderDataTable(DT::datatable(
    {
      req(input$fichier_data)
      head(prod_shem())
    }
  ))
  
  output$apercu_fin <- renderText(
    {
      req(input$fichier_data)
      "Apercu fin :"
    }
  )
  
  output$table_output_tail <- DT::renderDataTable(DT::datatable(
    {
      req(input$fichier_data)
      tail(prod_shem())
    }
  ))
  
  
  # -------------- Affichage de la datatable de prod 
  
  output$datatable_output_prod <- DT::renderDataTable(DT::datatable(
    {
      # -------------- Chargement du tableau de donnees apres appui sur le bouton
      
      req(input$fichier_data)
      prod_shem_prod <- prod_shem()
      
      # --------------- Sélection des variables pour le group_by, pour l'affichage, choix de la fonction calcul
      
      var_selec <- selec_variables()$var_selec
      liste_var <- selec_variables()$liste_var
      f_calcul <- selec_variables()$f_calcul
      col_groupby <- selec_variables()$col_groupby
      selec_usines <- selec_variables()$selec_usines
      
      # -------------- Filtre sur les usines ou les groupements par granularité et tranche tarifaire, agregation par groupement
      
      prod_shem_prod  %>%
        filter_dates(input$debut_fin[1], input$debut_fin[2]) %>%
        calcul_par_grpm(input$selec_gpmt, input$usine_ou_gpmt, selec_usines, attr_col) %>% #attr_col est une variables globales construites en début de script
        filter_col(f_calcul, liste_var, col_groupby) %>% #group_by selon la granularité, les tranches tarifaires, le mode d'agrégation temporelle
        select(one_of(var_selec))
    },
    options = list(pageLength = 53) 
  ) %>% formatRound(columns = -1, digits = 3) #Affichage de 2 chiffres après la virgule, je ne sais pas comment sélectionner toutes les colonnes...
  )
  
  # -------------- Affichage de la datatable de valo 
  
  output$datatable_output_valo <- DT::renderDataTable(DT::datatable(
    {
      # ---------------
      req(input$fichier_data)
      prod_shem_valo <- prod_shem()
      
      # --------------- Sélection des variables pour le group_by, pour l'affichage, choix de la fonction calcul
      
      var_selec <- selec_variables()$var_selec
      liste_var <- selec_variables()$liste_var
      f_calcul <- selec_variables()$f_calcul
      col_groupby <- selec_variables()$col_groupby
      selec_usines <- selec_variables()$selec_usines
      
      # -------------- Filtre sur les usines ou les groupements par granularité et tranche tarifaire
      
      prod_shem_valo %>%
        filter_dates(input$debut_fin[1], input$debut_fin[2]) %>%
        calcul_valo(attr_col, selec_usines) %>% # Calcul des valos
        calcul_par_grpm(input$selec_gpmt, input$usine_ou_gpmt, selec_usines, attr_col) %>% #liste_usines et attr_col sont des variables globales construites en début de script
        filter_col(f_calcul, liste_var, col_groupby) %>% #group_by selon la granularité, les tranches tarifaires, le mode d'agrégation temporelle
        select(one_of(var_selec))
    },
    options = list(pageLength = 24)
  ) %>% formatRound(columns = -1, digits = 3) #Affichage de 2 chiffres après la virgule, je ne sais pas comment sélectionner toutes les colonnes...
  )
}