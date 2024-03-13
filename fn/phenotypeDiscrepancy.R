
#' Identify Phenotype Discrepancies
#'
#' This function merges data from the "nda_study_intake" and "sips_scoresheet" datasets,
#' performs necessary data transformations, and identifies subjects with phenotype discrepancies.
#'
#' @details
#' The function uses the \code{dataRequest}, \code{source}, and \code{dataMerge} functions
#' to fetch and merge the required datasets. It then calculates various phenotype-related
#' variables and identifies subjects with discrepancies between the provided phenotype data
#' and calculated values.
#'
#' @return
#' A vector containing source subject IDs with phenotype discrepancies.
#'
#' @examples
#' \dontrun{
#' ids <- phenotypeDiscrepancy()
#' }
#'
#' @import dplyr
#' @importFrom utils view
#'
#' @export
#'
#' @seealso
#' \code{\link{dataRequest}}, \code{\link{source}}, \code{\link{dataMerge}}
#'
#' @references
#' If applicable, include any references or sources for the methodology used in the function.
#'
#' @keywords
#' phenotype data discrepancy identification merging
#'
#' @family
#' Data Processing
#'
#' @note
#' Additional notes or information about the function.
#'
#' @examples
#' ids <- phenotypeDiscrepancy()
#'
#' @return a vector of discrepant source subject IDs
#' @export
phenotypeDiscrepancy <- function(){
  source("api/dataRequest.R")
  source("api/dataMerge.R")
  
  dataRequest("nda_study_intake")
  dataRequest("sips_scoresheet")
  merged_redcap <- dataMerge(nda_study_intake_clean, sips_scoresheet_clean)
  
  
  merged_redcap %>% dplyr::mutate(src_subject_id=as.numeric(src_subject_id),
                                  apss_chr=ifelse(!is.na(apss_status), 
                                                  ifelse(apss_status=="never_apss", "neverchr","chr"),
                                                  "miss"),
                                  bips_chr=ifelse(!is.na(bips_status), 
                                                  ifelse(bips_status=="never_bips", "neverchr","chr"),
                                                  "miss"),
                                  grd_chr=ifelse(!is.na(grd_status), 
                                                 ifelse(grd_status=="never_grd", "neverchr","chr"),
                                                 "miss"),
                                  pops=ifelse(!is.na(pops_status),pops_status,"missing"),
                                  # Below variable requires only one of apss/bips/grd to be present
                                  # and indicate positive chr status. In contrast, all are required
                                  # to prove pt is NOT CHR
                                  sips_chr_status=ifelse(pops=="pops_met","pops_met",
                                                         ifelse(apss_chr=="chr" | bips_chr=="chr" | grd_chr=="chr","lifetime_chr",
                                                                ifelse(apss_chr=="miss" | bips_chr=="miss" | grd_chr=="miss","missing", "neverchr"))),
                                  sips_summary=paste0(apss_status,"/",bips_status,
                                                      "/",grd_status,"/",pops_status),
                                  phenotype_discrepancy=ifelse(is.na(phenotype) | is.na(sips_chr_status),NA,
                                                               ifelse(phenotype=="chr" & visit=="bl" & sips_chr_status=="neverchr",1,
                                                                      ifelse(phenotype!="chr" & visit=="bl" & sips_chr_status=="lifetime_chr",1,0))),
                                  phenotype_missing=ifelse(is.na(phenotype) & 
                                                             sips_chr_status=="missing",1,0)) %>% 
    relocate(src_subject_id, site, visit, sips_chr_status, sips_summary) -> phenotype_check
  
  phenotype_check %>% filter(phenotype_discrepancy==1) -> discrepant
  
  # to inspect discrepancies
  view(discrepant)
  # this function should return a vector of discrepant source subject IDs
  discrepant_ids <- as.vector(discrepant$src_subject_id)
  return(discrepant_ids)
  
}