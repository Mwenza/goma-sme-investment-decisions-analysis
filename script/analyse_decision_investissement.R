# =========================================================================
# 0. INFORMATIONS GENERALES -----------------------------------------------
# =========================================================================

###########################################################################
#
# TITRE:
# Décision d'investissement en contexte de conflit: 
# une analyse microéconomique fondée sur la théorie de 
# l'utilité espérée à Goma.
#
# Auteur  : Mwenza Bwira
# Version : 2.0
# Date    : Juillet 2026
#
# Objectif:
# Estimer les dertimants de la décision d'investissement des PME de Goma
# à l'aide d'un modèle Logit. 
###########################################################################

# =========================================================================
# 1. CHARGEMENT DES PACKAGES  ---------------------------------------------
# =========================================================================

packages <- c("tidyverse", 
              "dplyr", "readxl", 
              "janitor", "questionr", "pscl", 
              "psych", "gtsummary", "gmodels", "gt", 
              "flextable","modelsummary", "broom",
              "labelled", "broom.helpers", 
              "marginaleffects","parameters",
              "performance", "pROC", "sandwich",
              "lmtest", "generalhoslem", 
              "officer")

lapply(packages, library, character.only = TRUE)

# =========================================================================
# 2. IMPORTATION DE LA BASE DES DONNEES -----------------------------------
# =========================================================================

# 2.1. Importation

chemin <- "E:/recherche/Décision d'investissement et instabilité/Logit/Décision_dinvestissement_en_contexte_de_conflit.xlsx"

données <- read_excel(chemin)

# 2.2. Exploration de la base des données 

summary(données)
names(données)
str(données)

# =========================================================================
# 3. NETTOYAGE ET PREPARATION DE LA DATA ----------------------------------
# =========================================================================

# 3.1. Suppression des colonnes inutiles
#     ----------------------------------
données <- données[,-c(1,2,3,4,14,15,16,17,18, 26, 
                       27, 28, 29,30,31, 32,33,34,
                       34,38,39,40,41,42,43,44,45,46,47,48)] 

# 3.2. Renommage des variables
#     -------------------------

names(données) <- c("sexe", "Age", "Niv_Etude", "Duree_ActivitE", "Sector",
                    "Taille_Entreprise", "Chiffre_Affaires", "ID_entreprise",
                    "Forme_juridique", "Exposition_au_conflit",
                    "Echelle_exposition", "Pertes_dues_au_Conflit", 
                    "Estimation_Perte", "Perception_climat_securitaire",
                    "Decision_Investir",
                    "Montant_Investi", 
                    "RentabilitE_attendue", "Aversion_Risk",
                    "Echelle_Aversion_Risk")

# 3.3. Création des nouvelles variables  
#      --------------------------------

données <- données |> mutate(AnciennetE = 2026 - Duree_ActivitE)



# 3.4. Factorisation des variables
#      ---------------------------

Variables_factorisees <- c("sexe", "Niv_Etude", 
                           "Sector", "ID_entreprise", 
                           "Forme_juridique", "Exposition_au_conflit",
                           "Echelle_exposition",
                           "Pertes_dues_au_Conflit", 
                           "Perception_climat_securitaire", 
                           "Decision_Investir",
                           "RentabilitE_attendue", 
                           "Aversion_Risk", "Echelle_Aversion_Risk")

données[Variables_factorisees] <- lapply(données[Variables_factorisees],factor)



# 3.5. Verification des doublons
#     --------------------------

sum(duplicated(données))

# 3.6. Labélisation des variables
#     ---------------------------

var_label(données$Niv_Etude) <- "Niveau d'instruction"
var_label(données$Sector) <- "Secteur d'activité"
var_label(données$Taille_Entreprise) <- "Taille de l'entreprise"
var_label(données$Chiffre_Affaires) <- "Chiffre d'affaires"
var_label(données$ID_entreprise) <- "Statut de l'entreprise"
var_label(données$Exposition_au_conflit) <- "Exposition au conflit"
var_label(données$Echelle_exposition) <- "Niveau d'exposition au conflit"
var_label(données$Pertes_dues_au_Conflit) <- "Pertes dues au conflit"
var_label(données$Perception_climat_securitaire) <- "Perception du climat sécuritaire"
var_label(données$Decision_Investir) <- "Décision d'investir"
var_label(données$RentabilitE_attendue) <- "Rentabilité attendue"
var_label(données$Aversion_Risk) <- "Aversion au risque"
var_label(données$Echelle_Aversion_Risk) <- "Niveau de tolérance face au risque"
var_label(données$AnciennetE) <- "Ancienneté"


# =========================================================================
# 4. STATISTIQUES DESCRIPTIVES --------------------------------------------
# =========================================================================

Tableau1 <- données %>%
  tbl_summary(
    include = c(
      sexe,
      Age,
      Niv_Etude,
      Sector,
      Taille_Entreprise,
      Chiffre_Affaires,
      ID_entreprise,
      AnciennetE
    ),
    type = list(
      Age ~ "continuous",
      Taille_Entreprise ~ "continuous",
      Chiffre_Affaires ~ "continuous",
      AnciennetE ~ "continuous"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ± {sd} ({min}-{max})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  )


# ========================================================================= 
# 5. ANALYSE BIVARIEE -----------------------------------------------------
# =========================================================================

Tableau2 <- 
  données %>% 
  tbl_summary(
    by = Decision_Investir,
    include = c("sexe","Age", "Niv_Etude",
                "Sector", "AnciennetE", "Taille_Entreprise",
                "Chiffre_Affaires",
                "ID_entreprise", "Exposition_au_conflit",
                "Echelle_exposition",
                "Pertes_dues_au_Conflit",
                "Perception_climat_securitaire",
                "RentabilitE_attendue",
                "Aversion_Risk",
                "Echelle_Aversion_Risk"),
    type = list(Taille_Entreprise ~ "continuous")
    ) %>% add_p() %>% bold_labels()


Tableau2

# A travers les valeurs p des résultats de ce tableau, nous remarquons
# que les variables ayant une possible liaison avec la décision
# d'investissement sont:
#   * Niveau d'instruction
#   * le secteur d'activité
#   * Ancienneté 
#   * le chiffre d'affaires
#   * la formalité ou non de l'entreprise
#   * Exposition au conflit 
#   * Pertes dues au conflit
#   * la perception du climat sécuritaire 
#   * l'aversion au risuqe

# Il est à noter que compte tenu de la portée théorique de la rentabilité
# attendue dans le cadre théorique de l'utilité espérée, cette variable
# nous l'avons maintenu dans le modèle malgré sa faible association à la 
# variable cible. 



# =========================================================================
# 6. ESTIMATION DU MODELE LOGIT -------------------------------------------
# =========================================================================

# 6.1.  Définition des modalités références
#       -----------------------------------

données$sexe <- relevel(données$sexe, ref = "Femme")
données$Niv_Etude <- relevel(données$Niv_Etude, ref = "Aucun")
données$Sector <- relevel(données$Sector, ref = "Secteur primaire (Agriculture)")
données$ID_entreprise <- relevel(données$ID_entreprise, ref = "Non")
données$Forme_juridique <- relevel(données$Forme_juridique, ref = "Entreprise individuelle")
données$Exposition_au_conflit <- relevel(données$Exposition_au_conflit, ref = "Non")
données$Echelle_exposition <- relevel(données$Echelle_exposition, ref = "Moins important")
données$Pertes_dues_au_Conflit <- relevel(données$Pertes_dues_au_Conflit, ref = "Non")
données$Perception_climat_securitaire <- relevel(données$Perception_climat_securitaire, ref = "Modérément stable")
données$RentabilitE_attendue <- relevel(données$RentabilitE_attendue, ref = "faible")
données$Aversion_Risk <- relevel(
  données$Aversion_Risk,
  ref = "Je suis neutre face aux risques (je prends des risques mesurés)"
)
données$Echelle_Aversion_Risk <- relevel(
  données$Echelle_Aversion_Risk,
  ref = "Moyenne"
)


# 6.2.  Estimation du modèle
#       --------------------

Model_logit <- glm(
  Decision_Investir ~ 
    Niv_Etude + 
    Sector + 
    AnciennetE +
    Chiffre_Affaires  +
    Exposition_au_conflit +
    Echelle_exposition +
    Pertes_dues_au_Conflit +
    Perception_climat_securitaire + 
    RentabilitE_attendue +
    Aversion_Risk, family = binomial(link = "logit"),
  data = données)


Model_logit %>%
  tbl_regression(exponentiate = TRUE) %>%
  add_global_p(type = "II")

tbl_regression(reg, exponentiate = TRUE) %>%
  add_global_p(keep = TRUE)

#   Les résultats de cette commande nous donnent un modèle estimé 
# ayant un problème de séparation complète des données où il y a 
# des modalités qui sont rares. Ce qui déstabilise la bonté du modèle.
# La solution adoptée dans ce cas est la suppression de la variable
# "Echelle d'exposition" qui affiche une erreur standard anormalement grand  
# et n'apporte geure des nouvelles informations étant donné que
# la variable Exposition au conflit existe déjà dans le modèle. 
# Pour la variable rentabilité attendue, nous trouvons utile de la regrouper
# en deux modalités: rentabilité faible et rentabilté elevée cela 
# pour atténuer la séparation complète observée dans ses modalités. 

#   Regroupement des modalités pour Rentabilité attendue
#   ----------------------------------------------------

données$Rentabilite_attendue_rec <- factor(
  ifelse(
    données$RentabilitE_attendue %in% c("Très faible", "faible"),
    "Rentabilité faible",
    "Rentabilité elevée"
  ),
  levels = c("Rentabilité faible", "Rentabilité elevée")
)

#   6.3. Réestimation du modèle
#       -----------------------

Model_logit1 <- glm(
  Decision_Investir ~ 
    Niv_Etude + 
    Sector + 
    AnciennetE +
    Chiffre_Affaires  +
    Exposition_au_conflit +
    Pertes_dues_au_Conflit +
    Perception_climat_securitaire + 
    Rentabilite_attendue_rec +
    Aversion_Risk, family = binomial(link = "logit"),
  data = données)


# =========================================================================
# 7. RESUME DU MODELE -----------------------------------------------------
# =========================================================================

summary(Model_logit1)

# =========================================================================
# 8. ODDS RATIOS ----------------------------------------------------------
# =========================================================================

Tableau_Odds <- odds.ratio(Model_logit1)

# =========================================================================
# 9. DIAGNOSTIC DU MODELE -------------------------------------------------
# =========================================================================

# 9.1. Qualité d'ajustement globale
#      -----------------------------
#   a) LR Test

LR_Test <- lmtest::lrtest(Model_logit1)

#   b) Pseudo R2

Pseudo_R2 <- pscl::pR2(Model_logit1)


# 9.2. Diagnostic ou tests des hypothèses
#       ---------------------------------------
#     a) VIF/Multicolinéarité

VIF_test <- performance::check_collinearity(Model_logit1)

#     b) Pouvoir prédicitf (Hosmer-Lemeshow)

Hosmer_Lemeshow_test <- generalhoslem::logitgof(
  données$Decision_Investir, 
  exp = fitted(Model_logit1), g = 10
)

# 9.3. Pouvoir de discrimination
#       -------------------------
#    a) AUC/ROC

Roc_Auc_test <- roc(
  response = données$Decision_Investir,
  predictor = fitted(Model_logit1),
  levels = c("Non", "Oui"),
  ci = TRUE,
  boot.n = 2000
)


#    b) Matrice de confusion

Proba_predite <- predict(Model_logit1, type = "response")

Pred_class <- ifelse (Proba_predite > 0.5, 1, 0)

Matrice_conf <- table(observé = données$Decision_Investir,
                      prédit = Pred_class)

#    c) Derniers indicateurs

Confusion <- caret::confusionMatrix(
  factor(
    Pred_class,
    levels = c(0,1),
    labels = c("Non","Oui")
  ),
  données$Decision_Investir,
  positive = "Oui"
)
# =========================================================================
# 10. CALCULS DES EFFETS MARGINAUX ----------------------------------------
# =========================================================================

# 10.1. Pour les variables continues
#      -----------------------------

var_cont_dumodel <- c("AnciennetE","Chiffre_Affaires")

Effets_marginaux1 <- avg_slopes(Model_logit, variables = var_cont_dumodel )

# 10.2. Pour les variables categorielles 
#       --------------------------------

Var_cat_dumodel <- c("Niv_Etude ","Sector",
                     "Exposition_au_conflit ", 
                     "Pertes_dues_au_Conflit ", 
                     "Perception_climat_securitaire",
                     "RentabilitE_attendue",
                     "Aversion_Risk"
                     )

Effets_marginaux2 <- avg_comparisons(Model_logit, variables = Var_cat_dumodel)


# =========================================================================
# 11.  EXPORT DES TABLEAUX  -----------------------------------------------
# =========================================================================

#       11.1.  PREPARATION DES TABLEAUX 

#   Tableau 1:  Caracteristiques de l'echantillon 
#               ----------------------------------

Tableau1 <- Tableau1 %>%
  modify_caption(
    "**Tableau 1. Caractéristiques descriptives des répondants et des PME enquêtées**"
  )

#   Tableau 2: analyse bivariée
#                -----------------

Tableau2 <- Tableau2 %>%
  modify_caption(
    "**Tableau 2. Analyse bivariée selon la décision d'investissement**"
  )


#    Tableau 3 : Résultats du modele logit
#               ------------------------

Tableau3 <- tbl_regression(
  Model_logit,
  exponentiate = TRUE
) %>%
  bold_p(t = 0.05) %>%
  bold_labels() %>%
  modify_caption(
    "**Tableau 4. Déterminants de la décision d'investissement des PME de Goma**"
  )

#  Tableau 4: Diagnostic du modele
#             --------------------

Tableau5 <- tibble::tibble(
  Indicateur = c(
    "Test du rapport de vraisemblance (χ²)",
    "p-value (LR test)",
    "Pseudo R² de McFadden",
    "Test de Hosmer-Lemeshow (χ²)",
    "p-value Hosmer-Lemeshow",
    "AUC (ROC)",
    "IC à 95 % de l'AUC",
    "Accuracy",
    "Sensitivity",
    "Specificity",
    "Balanced Accuracy"
  ),
  
  Valeur = c(
    round(LR_Test$Chisq[2], 3),
    format.pval(LR_Test$`Pr(>Chisq)`[2], digits = 3),
    round(Pseudo_R2["McFadden"], 3),
    round(Hosmer_Lemeshow_test$statistic, 3),
    round(Hosmer_Lemeshow_test$p.value, 3),
    round(as.numeric(Roc_Auc_test$auc), 3),
    paste0(
      round(Roc_Auc_test$ci[1],3),
      " - ",
      round(Roc_Auc_test$ci[3],3)
    ),
    round(Confusion$overall["Accuracy"],3),
    round(Confusion$byClass["Sensitivity"],3),
    round(Confusion$byClass["Specificity"],3),
    round(Confusion$byClass["Balanced Accuracy"],3)
  )
)
#   Tableau 5: Effets marginaux
#             -----------------

#   Pour les variables continues: 

Tableau5 <- modelsummary(
  Effets_marginaux1,
  output = "data.frame"
)

#   Pour les variables categorielles: 

Tableau6 <- modelsummary(
  Effets_marginaux2,
  shape = term + contrast ~ model,
  output = "data.frame"
)

#     11.2. EXPORT VERS WORD DES TABLEAUX


save_as_docx(
  "Tableau1" = as_flex_table(Tableau1),
  "Tableau2" = as_flex_table(Tableau2),
  "Tableau3" = as_flex_table(Tableau3),
  "Tableau4" = flextable(Tableau4),
  "Tableau5" = flextable(Tableau5),
  "Tableau6" = flextable(Tableau6),
  path = "E:/recherche/Décision d'investissement et instabilité/Logit/Resultats_article-2.docx"
)


# =========================================================================
# 12. SAUVEGARDE DE L'ENVIRONNEMENT  --------------------------------------
# =========================================================================


