# wcomment ----------------------------------------------------------------
# Permite agregar comentarios de Word

wcomment <- function(cm, txt = "", author = "PS", date = Sys.Date(), comN = commentN){
  cmt_str <- paste0('<span class="comment-text">[',cm,']{.comment-start id="', comN, '" author="', author, '" date="', date, '"}', txt, '[]{.comment-end id="',commentN,'"}</span>')
  assign("commentN", commentN + 1, envir = .GlobalEnv)
  return(cmt_str)
}

# f_seg ---------------------------------------------------------------------
# Calculo de Mutual Information Index Total + Between + Within

f_seg <- function(db, VAR, grupo = "group", peso = "peso", id = "ID_s"){
  res <- data.frame(Global = segregation::mutual_total(data = db, group = grupo,
                                                       unit = "ID_s", weight = peso,
                                                       se = F)$est[1],
                    Between = segregation::mutual_total(data = db, group = grupo,
                                                        unit = VAR, weight = peso,
                                                        se = F)$est[1],
                    Within =  segregation::mutual_total(data = db, group = grupo,
                                                        unit = id, weight = peso,
                                                        se = F, within = VAR)$est[1])
  return(res)
}
