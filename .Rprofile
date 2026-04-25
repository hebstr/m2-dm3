source("rv/scripts/rvr.R")
source("rv/scripts/activate.R")
source("~/.Rprofile")

library(tidyverse) |> suppressPackageStartupMessages()
library(tidymodels) |> suppressPackageStartupMessages()
library(rlang, warn.conflicts = FALSE)
library(mice, warn.conflicts = FALSE)
library(glue)
library(labelled)
library(gtsummary)
library(MatchIt)
library(halfmoon)
library(survival)
library(ggsurvfit)
library(patchwork)
library(hebstr)

update_geom_defaults("text", list(family = "Luciole"))

lang_fr()

conflicted::conflict_prefer("filter", "dplyr")
