library(shiny)
library(DT)
library(ggplot2)
library(dplyr)

#-------------------- Fonctions

# ----------------- Preparation des colonnes et de leurs attributs


prep_usines <- function()
{
  noms_usines_ossau <- c("Artouste_Lac", "Artouste_Usine", "Bious", "Pont_de_Camps", "Miegebat", "Hourat", "Geteu", "Castet", "Fabreges")
  noms_usines_valentin <- c("Eaux_bonnes", "Assouste", "Espalungue")
  noms_usines_mareges <- c("Mareges_Usine", "Saint_Pierre", "Coindre")
  noms_usines_louron <- c("Lassoula", "Clarabide", "Lapes", "Pont_de_Prat", "Aube", "Pont_d_Estagnou")
  noms_usines_eget <- c("Eget_Usine", "Oule")
  liste_usines <- c(noms_usines_ossau, noms_usines_valentin, noms_usines_mareges, noms_usines_louron, noms_usines_eget)
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
  return(liste_gpmt <- c(rep("Ossau", 9), rep("Valentin", 3), rep("Mareges", 3), rep("Louron", 6), rep("Eget", 2)))
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

# load_web <- function(file) {
#   path <- "https://raw.githubusercontent.com/corentinbui/shinyshem/master/"
#   prod_shem <- read.csv2(c(path, file))
#   return(prod_shem)
# }

prep_shem <- function(prod_shem, liste_usines, noms_col)
{
  colnames(prod_shem) <- noms_col #On renomme les colonnes
  prod_shem[, liste_usines] = apply(prod_shem[, liste_usines], 2, as.numeric) #On force tout en numérique, même si ça échoue lamentablement
  prod_shem = mutate(prod_shem, Dates = as.POSIXct(Dates, format = "%d/%m/%Y %H:%M")) #Formatage des dates
  # prod_shem[is.na(prod_shem)] <- 0 #On remplace les NA par des 0
  # prod_shem[is.character(prod_shem)] <- 0 #On remplace les caractères par des 0
  # prod_shem[!is.numeric(prod_shem)] <- 0
  return(prod_shem)
}

# ------------------------ Selection des colonnes

selection_var <- function(prix, gpmt, noms_colonnes, attributs_col)
{
  selec_gpmt <- c(prix, gpmt)
  var_selec <- c("Dates", "Annee", "Jour", "Semaine", "Mois") #On sélectionne au moins la date et la granularite si elle existe
  for(nom_colonne in noms_colonnes) {
    if(attributs_col["noms_gpmt", nom_colonne] %in% selec_gpmt) {var_selec <- c(var_selec, nom_colonne)}
  }
  return(var_selec)
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

#---------- Filtre sur la granularite


filter_granu <- function(prod_shem_prod, granu, date_debut, date_fin, f_calcul, var_selec, liste_var)
{
  if(granu != "Heure") {     #avec filtre granuralite
    prod_shem_prod %>% 
      filter(Dates >= as.POSIXct(date_debut) & Dates <= as.POSIXct(date_fin)) %>%
      group_by(Annee = lubridate::year(Dates), !!granu := f_granu(Dates, granu)) %>% #!!granu := permet de nommer dynamiquement la colonne de granularite (jour, semaine...)
      summarise_at(liste_var, f_calcul) %>%
      select(one_of(var_selec))
  }
  else {                     # sans filtre granuralite
    prod_shem_prod %>% 
      filter(Dates >= as.POSIXct(date_debut) & Dates <= as.POSIXct(date_fin)) %>%
      select(one_of(var_selec))
  }
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

calcul_par_grpm <- function(prod_shem, liste_gpmt, liste_usines, attr_col)
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
    # input$action_github #Appui sur le bouton download dans le cas du load_web
    prod_shem_import <- load_local(input$fichier_data$datapath)
    # prod_shem_import <- load_web(file = input$fichier_github)
    prod_shem_import <- prep_shem(prod_shem_import, liste_usines, noms_col)
    return(prod_shem_import)
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
      
      # --------------------- Affichage du tableau filtre sur la granularite et/ou les groupements
      
      if(input$usine_ou_gpmt == "gpmt")
      {
        liste_gpmt <- input$selec_gpmt #Selection des groupements a afficher
        
        # -------------- Calcul des prod par groupement 
        
        prod_shem_prod <- calcul_par_grpm(prod_shem_prod, liste_gpmt, liste_usines, attr_col)
        
        # -------------- Selection des variables à afficher
        
        var_selec <- c("Dates", "Jour", "Semaine", "Mois", liste_gpmt) #Fonction flitre_granu : variables a selectionner
        liste_var <- liste_gpmt  #Fonction flitre_granu : Variables sur lesquelles effectuer le summarize final
      }
      else
      {
        # -------------- Selection des variables à afficher 
        
        var_selec <- selection_var(prix = input$selec_prix, gpmt = input$selec_gpmt, noms_colonnes = noms_col, attributs_col = attr_col)
        liste_var <- liste_usines #Variables sur lesquelles effectuer le summarize final
      }
      
      # ----------------- Somme ou moyenne selon si on calcule en energie ou puissance
      
      ifelse(input$puissance_ou_energie == "puissance", f_calcul <- mean, f_calcul <- sum)
      
      # -------------- On filtre sur les usines ou les groupements
      
      prod_shem_prod <- filter_granu(prod_shem_prod, input$granu, input$debut_fin[1], input$debut_fin[2], f_calcul, var_selec, liste_var)
    },
    options = list(pageLength = 24)
  )
  )
  
  
  # -------------- Affichage de la datatable de valo 
  
  output$datatable_output_valo <- DT::renderDataTable(DT::datatable(
    {
      # ---------------
      req(input$fichier_data)
      prod_shem_valo <- prod_shem()
      
      # -------------- Calcul des valos horaires
      
      prod_shem_valo <- calcul_valo(prod_shem_valo, attr_col, liste_usines)
      
      # --------------------- Affichage du tableau filtre sur la granularite et/ou les groupements
      
      if(input$usine_ou_gpmt == "gpmt")
      {
        liste_gpmt <- input$selec_gpmt #Selection des groupements a afficher
        
        # -------------- Calcul des prod par groupement 
        
        prod_shem_valo <- calcul_par_grpm(prod_shem_valo, liste_gpmt, liste_usines, attr_col)
        
        # -------------- Selection des variables à afficher
        
        var_selec <- c("Dates", "Granularite", liste_gpmt) #Fonction flitre_granu : variables a selectionner
        liste_var <- liste_gpmt  #Fonction flitre_granu : Variables sur lesquelles effectuer le summarize final
      }
      else
      {
        # -------------- Selection des variables à afficher 
        
        var_selec <- selection_var(prix = input$selec_prix, gpmt = input$selec_gpmt, noms_colonnes = noms_col, attributs_col = attr_col)
        liste_var <- liste_usines #Variables sur lesquelles effectuer le summarize final
      }
      
      # -------------- On filtre sur les usines ou les groupements
      
      prod_shem_valo <- filter_granu(prod_shem_valo, input$granu, input$debut_fin[1], input$debut_fin[2], sum, var_selec, liste_var)
      
      
      # # -------------- Selection des variables à afficher 
      # 
      # var_selec <- selection_var(prix = input$selec_prix, gpmt = input$selec_gpmt, noms_colonnes = noms_col, attributs_col = attr_col)
      # 
      # # -------------- Calcul des valos horaires
      # 
      # prod_shem_valo <- calcul_valo(prod_shem_valo, attr_col, liste_usines)
      # 
      # # --------------------- Affichage du tableau avec ou sans filtre granuralite
      # 
      # prod_shem_valo <- filter_granu(prod_shem_valo, input$granu, input$debut_fin[1], input$debut_fin[2], sum, var_selec, liste_usines)
      
    },
    options = list(pageLength = 24)
  )
  )
}