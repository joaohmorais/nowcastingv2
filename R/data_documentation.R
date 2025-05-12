#' Dengue notifications in Rio de Janeiro municipality, 2023 and 2024.
#'
#' Example dataset for `{nowcastingv2}`: Dengue notifications in Rio de Janeiro municipality, from SINAN DATASUS system (2023 and up until Jan 18 2024), before Rio's 2024 Dengue epidemic.
#'
#' @format A data frame with 24235 rows and 6 columns:
#' \describe{
#'   \item{ID_AGRAVO}{Outcome ID (ICD-10 code)}
#'   \item{ID_MN_RESI}{Patient's municipality IBGE code}
#'   \item{DT_NOTIFIC}{Notification date}
#'   \item{DT_SIN_PRI}{Symptoms onset date}
#'   \item{DT_DIGITA}{Date when notification enters the system}
#'   \item{CLASSI_FIN}{Final classification of the case, according to SINAN categories}
#' }
#' @source Sistema de Informação de Agravos de Notificação (SINAN), DATASUS, Brazil.
#' @examples
#' head(dengue_rio)
"dengue_rio"
