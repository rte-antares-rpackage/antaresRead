# badge doc ----
badge_api_ok <- function() {
  "\\ifelse{html}{\\figure{badge_api_ok.svg}{options: alt='Antares API OK'}}{Antares API: \\strong{OK}}"
}
badge_api_no <- function() {
  "\\ifelse{html}{\\figure{badge_api_no.svg}{options: alt='Antares API NO'}}{Antares API: \\strong{NO}}"
}