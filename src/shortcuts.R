if(!require(dplyr)) {install.packages("dplyr")}; library(dplyr);

# negate in
`%!in%` = dplyr::Negate(`%in%`)