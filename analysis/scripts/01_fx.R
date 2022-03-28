# wcomment ----------------------------------------------------------------
# Permite agregar comentarios de Word

wcomment <- function(cm, txt = "", author = "PS", date = Sys.Date(), comN = commentN){
  cmt_str <- paste0('<span class="comment-text">[',cm,']{.comment-start id="', comN, '" author="', author, '" date="', date, '"}', txt, '[]{.comment-end id="',commentN,'"}</span>')
  assign("commentN", commentN + 1, envir = .GlobalEnv)
  return(cmt_str)
}

# wtc ---------------------------------------------------------------------
# Calcular centroide ponderado

wtc <- function(g = .data, w = NULL, cl = NULL){
  if (!(is(g,"sf")) | !(w %in% colnames(g))){
    stop(paste("requires an sf object with at a column",w))
  }
  names(g)[names(g) == w] <- "w"
  centers <- st_coordinates(st_centroid(g)) %>% 
    cbind(g) %>% 
    st_drop_geometry() %>% 
    group_by_at(cl) %>% 
    summarise(X = weighted.mean(X, w),
              Y = weighted.mean(Y, w)) %>% 
    as.data.frame() %>% 
    st_as_sf(coords = c("X", "Y"), crs = st_crs(g))
  
  return(centers)
}


# f_seg ---------------------------------------------------------------------

f_seg <- function(db, VAR, grupo = "group", peso = "peso", id = "ID_s"){
  res <- data.frame(Global = mutual_total(data = db, group = grupo,
                                          unit = "ID_s", weight = peso,
                                          se = F)$est[1],
                    Between = mutual_total(data = db, group = grupo,
                                           unit = VAR, weight = peso,
                                           se = F)$est[1],
                    Within =  mutual_total(data = db, group = grupo,
                                           unit = id, weight = peso,
                                           se = F, within = VAR)$est[1])
  return(res)
}
