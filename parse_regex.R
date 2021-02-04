require(ore)
require(stringr)

parse_afstem_regex <- function(string) {
  df<- data.frame(samlet_resultat=NA,
                  for_tekst=NA,
                  for_antal=NA,
                  for_partier=NA,
                  imod_tekst=NA,
                  imod_antal=NA,
                  imod_partier=NA,
                  hverkeneller_tekst=NA,
                  hverkeneller_antal=NA,
                  hverkeneller_partier=NA,
                  kommentar=NA)
  
  vedtaget_med <- ore.search(ore("(?<resten>^Vedtaget med.*)",options="m"),string)$groups$matches
  res <- ore.search(ore("(?<samlet_resultat>([^\\n\\.,](?!\\d|(Forslaget)))*)\\n*(?<resten>.*)",
                        options="m"),string)$groups$matches
  
  if(is.null(vedtaget_med)){
    df$samlet_resultat <- res[1,"samlet_resultat"]
  } else {
    df$samlet_resultat <- NA
    res <- vedtaget_med
  }
  
  res <- ore.search(ore("(?<for_tekst>[^\\d]*(?<for_antal>\\d+)([^\\d\\n,\\(\\)](?!imod))*(?#
                      )(\\((?<for_partier>[^()]*(:?\\([^()]*\\)[^()]*)*)\\))?)\\n*(?<resten>.*)?",
                        options="m"),res[1,"resten"])$groups$matches
  if(!is.null(res)){
    df$for_tekst <- res[1,"for_tekst"]
    df$for_antal <- res[1,"for_antal"]
    df$for_partier <- res[1,"for_partier"]
  }
  
  res <- ore.search(ore("(?<imod_tekst>[^\\d]*(?<imod_antal>\\d+)([^\\d\\n,\\(\\)](?!hverken))*(?#
                      )(\\((?<imod_partier>[^()]*(:?\\([^()]*\\)[^()]*)*)\\))?)?\\n*(?<resten>.*)?",
                        options="m"),res[1,"resten"])$groups$matches
  if(!is.null(res)){
    df$imod_tekst <- res[1,"imod_tekst"]
    df$imod_antal <- res[1,"imod_antal"]
    df$imod_partier <- res[1,"imod_partier"]
  }
  
  res <- ore.search(ore("(?<hverkeneller_tekst>[^\\d]*(?<hverkeneller_antal>\\d+)([^\\.\\n,\\(\\)])*(?#
                      )(\\((?<hverkeneller_partier>[^()]*(:?\\([^()]*\\)[^()]*)*)\\))?)?[\\.\\n]*(?<resten>.*)?",
                        options="m"),res[1,"resten"])$groups$matches
  if(!is.null(res)){
    df$hverkeneller_tekst <- res[1,"hverkeneller_tekst"]
    df$hverkeneller_antal <- res[1,"hverkeneller_antal"]
    df$hverkeneller_partier <- res[1,"hverkeneller_partier"]
    
    df$kommentar <- res[1,"resten"]
  }
  
  return(df)
}

get_get_partier_func <- function(type,pattern,notpattern) {
  return(function(res) get_valid_partilist(res,type,pattern,notpattern))
}

get_partier_for <- get_get_partier_func("for",regex("(for)|(vedtaget)",ignore_case = TRUE),"hverken")
get_partier_imod <- get_get_partier_func("imod","mod","hverken")
get_partier_hverkeneller <- get_get_partier_func("hverkeneller","hverken","(?!)")

get_valid_partilist <- function(res,type,pattern,notpattern){
  tekst <- res[1,str_c(type,"_tekst")]
  antal <- as.numeric(res[1,str_c(type,"_antal")])
  partier <- res[1,str_c(type,"_partier")]
  
  if(str_detect(tekst,pattern) & !str_detect(tekst,notpattern) & !is.na(tekst)){
    if(is.na(antal)) {
      return(NA)
    } else {
      if(antal == 0 & is.na(partier)){
        return("")
      } else if (antal >0 & !is.na(partier)){
        return(partier)
      } else {
        return(NA)
      }
    }
  } else {
    return(NA)
  }
}

get_result <- function(res){
  df<- data.frame(stemmer.for=NA,
                  stemmer.imod=NA,
                  stemmer.hverkeneller=NA,
                  stemmer.kommentar=NA)
  df$stemmer.for <- get_partier_for(res)
  df$stemmer.imod <- get_partier_imod(res)
  df$stemmer.hverkeneller <- get_partier_hverkeneller(res)
  if(is.na(res[1,"hverkeneller_tekst"]))
  {
    df$stemmer.hverkeneller <- ""
  }
  if(is.na(res[1,"kommentar"])){
    df$stemmer.kommentar <- ""
  } else {
    df$stemmer.kommentar <-str_remove_all(res[1,"kommentar"],"(^\\s*)|(\\s*$)")
  }
  df
}
