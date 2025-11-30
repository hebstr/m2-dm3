### LABELS ---------------------------------------------------------------------

.labels <- list(
  base = easy_recode(
    sex = c("sex" = "Sexe"),
    weight = c("wtkilo1" = "Poids, kg"),
    cancer = c("ca" = "Cancer"),
    ethnic = c("race" = "Origine ethnique"),
    educ = c("edu" = "Niveau d'études, grade"),
    income = c("income" = "Revenu, k$"),
    insur = c("ninsclas" = "Assurance maladie"),
    transfer = c("transhx" = "Transfert interhospitalier >24h"),
    proba_surv = c("surv2md1" = "Probabilité de survie à 60 jours, %"),
    dnr = c("dnr1" = "Limitation/arrêt des thérapeutiques actives"),
    rhc = c("swang1" = "Cathétérisme droit"),
    score_dasi = c("das2d3pc" = "DASI"),
    score_apa = c("aps1" = "Score APACHE III"),
    score_gcs = c("scoma1" = "Score de Glasgow"),
    pv_temp = c("temp1" = "Température, °C"),
    pv_pam = c("meanbp1" = "Pression artérielle moyenne, mmHg"),
    pv_fc = c("hrt1" = "Fréquence cardiaque, bpm"),
    pv_fr = c("resp1" = "Fréquence respiratoire, rpm"),
    pv_paco2 = c("paco21" = "PaCO2, mmHg"),
    pv_pafi = c("pafi1" = "PaO2/FiO2, mmHg"),
    pv_ph = c("ph1" = "pH"),
    bio_leuco = c("wblc1" = "Leucocytémie, G/L"),
    bio_bili = c("bili1" = "Bilirubinémie, μM"),
    bio_creat = c("crea1" = "Créatininémie, μM"),
    bio_sodium = c("sod1" = "Natrémie, mM"),
    bio_kalium = c("pot1" = "Kaliémie, mM"),
    bio_hema = c("hema1" = "Hématocrite, %")
  ),
  new = list(
    org_fail = "Défaillance d'organe",
    tt_discharge = "Durée d'hospitalisation, jours",
    tt_death = "Durée jusqu'au décès, jours",
    ev_death_30 = "Mortalité à 30 jours",
    ev_death_180 = "Mortalité à 180 jours",
    age_cat = "Âge, années",
    n_diag = "Nb. de diagnostics à l'admission",
    n_comorb = "Nb. de comorbidités"
  )
)

.diag <- c(
  "resp", "card", "neuro", "gastr", "renal",
  "meta", "hema", "seps", "trauma", "ortho"
)

### DF -------------------------------------------------------------------------

df_base <-
read_csv2("data/dm3.csv")[-1] |>
  set_names(tolower) |>
  mutate(across(ends_with("dte"), dmy)) |>
  relocate(!!!.labels$base$name)

df_setup <-
df_base |>
  mutate(across(c(cat1, cat2),
                ~ if_else(str_detect(., "Cancer"), NA, .)),
         across(all_of(.diag),
                ~ if_else(. == "Yes", "1", "0") |>
                  as.numeric()),
         across(ends_with(c("weight", "pam", "fc", "fr")),
                ~ na_if(., 0)),
         rhc = if_else(rhc == "RHC", 1, 0),
         ethnic = str_to_title(ethnic),
         educ = round(educ),
         dnr = if_else(dnr == "Yes", 1, 0),
         score_gcs = 15 - score_gcs * 0.12,
         bio_bili = bio_bili * 17.1,
         bio_creat = bio_creat * 88.4,
         org_fail =
           case_when(if_all(c(cat1, cat2), is.na) ~ "Aucune",
                     !str_detect(cat1, "MOSF") & !is.na(cat1) & is.na(cat2) |
                     !str_detect(cat2, "MOSF") & !is.na(cat2) & is.na(cat1) ~ "Uni",
                     .default = "Multi") |>
           fct_relevel("Aucune", "Uni", "Multi"),
         tt_discharge = as.numeric(dschdte - sadmdte),
         tt_death = as.numeric(dthdte - sadmdte),
         ev_death_30 = case_when(tt_death <= 30 ~ 1, .default = 0),
         ev_death_180 = case_when(tt_death <= 180 ~ 1, .default = 0)) |>
  easy_fct(sex,
           "Homme" = "Male",
           "Femme" = "Female") |>
  easy_fct(ethnic,
           "Blanche" = "White",
           "Noire" = "Black",
           "Autre" = "Other") |>
  easy_fct(cancer,
           "Non" = "No",
           "Localisé" = "Yes",
           "Métastatique" = "Metastatic") |>
  easy_fct(income,
           "<11" = "Under $11k",
           "[11-25)" = "$11-$25k",
           "[25-50)" = "$25-$50k",
           ">=50" = "> $50k") |>
  easy_fct(insur,
           "PV" = "Private",
           "MR" = "Medicare",
           "PVMR" = "Private & Medicare",
           "MD" = "Medicaid",
           "MRMD" = "Medicare & Medicaid",
           "Aucune" = "No insurance") |>
  easy_fct(age,
           45, 75, .btw = 60,
           .name = "age_cat") |>
  easy_fct(educ, 9, 13) |>
  easy_fct(educ,
           "Élémentaire" = "<9",
           "Secondaire" = "[9-13)",
           "Supérieur" = ">=13") |>
  easy_fct(pv_temp, 35.5, 38.5) |>
  easy_fct(pv_ph, 7.38, 7.42) |>
  easy_fct(pv_pam, cut = 65) |>
  easy_fct(pv_paco2, 35, 45) |>
  easy_fct(pv_pafi, 100, 300, .btw = 200) |>
  easy_fct(pv_fr, 12, 20) |>
  easy_fct(pv_fc, 60, 100) |>
  easy_fct(bio_leuco, 7.5, 15) |>
  easy_fct(bio_sodium, 135, 145) |>
  easy_fct(bio_kalium, 3.5, 5) |>
  easy_fct(bio_hema, 40, 50) |>
  rowwise() |>
    mutate(n_diag = sum(across(all_of(.diag))),
           n_comorb = sum(across(ends_with("hx")))) |>
    ungroup() |>
  mutate(across(c(n_diag, n_comorb), as_factor)) |>
  set_variable_labels(!!!with(.labels, c(base$label, new))) |>
  select(where(~ !is.null(label_attribute(.))), -age)

load("data/df_imp.RData")

### OPTS -----------------------------------------------------------------------

set_opts(
  data = easy_descr(df_setup),
  view = easy_view(df_setup),
  parametric = c("weight", "score_apa"),
  acro = acro(
    PV ~ "privée",
    MR ~ "Medicare",
    PVMR ~ "privée + Medicare",
    MD ~ "Medicaid",
    MRMD ~ "Medicare + Medicaid"
  ),
  line = list(
    lty = 2,
    color = set_opts()$color$warm[2],
    alpha = 0.5
  )
)

.miss_data <-
opts$view$data |>
  filter(!is.na(n_miss), variable != "tt_death") |>
  select(variable, label, n_miss, p_miss)

### BV VARS --------------------------------------------------------------------

.bv <-
  list(
    group = list(
      demo = c("sex", "age_cat", "ethnic", "educ", "income", "insur"),
      atcd = c("n_diag", "n_comorb", "cancer", "org_fail"),
      pv = str_subset(opts$data$ql$vars, "pv_"),
      bio = str_subset(opts$data$ql$vars, "bio_")
    ),
    vars = c("ev_death_30", "rhc")
  )

### SP -------------------------------------------------------------------------

sp_vars <-
df_setup |>
  select(-rhc, -starts_with(c("tt", "ev"))) |>
  names()

.sp_model <- logistic_reg() |> set_engine("glm")

.sp_recipe <-
recipe(formula = reformulate(sp_vars, "rhc"), data = df_imp) |>
  step_mutate(rhc = factor(rhc))

df_sp <-
workflow() |>
  add_model(.sp_model) |>
  add_recipe(.sp_recipe) |>
  fit(df_imp) |>
  augment(type.predict = "response", new_data = df_imp) |>
  rename(sp = .pred_1) |>
  mutate(rhc = factor(rhc)) |>
  set_variable_labels(sp = "Score de propension")

df_match_model <- matchit(
  formula = reformulate(sp_vars, "rhc"),
  data = df_sp,
  distance = "logit",
  caliper = 0.002,
  std.caliper = FALSE
)

df_match <- match.data(df_match_model)

compar_df <- list(
  non_ajusté = df_imp,
  ajusté = df_match
)

### AUTO EXEC ------------------------------------------------------------------

# auto_exec()
