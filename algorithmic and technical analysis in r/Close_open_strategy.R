
library(rutils)


sym_bols <- rutils::etf_env$sym_bols
names(sym_bols) <- sym_bols


pnl_s<- lapply(sym_bols , function(i){
  
  oh_lc <- log(get(i, rutils::etf_env))
  op_en <- quantmod::Op(oh_lc)
  clos_e <- quantmod::Cl(oh_lc)
  close_open <- (op_en - rutils::lag_it(clos_e, lagg=1, pad_zeros=FALSE))
  cumsum(close_open)
  })



is.list(pnl_s)

NROW(pnl_s)

NROW(pnl_s[[1]])

names(pnl_s)


final_pnls <- sort(
  sapply( sym_bols, function(x) last(pnl_s[[x]])) 
  , decreasing=TRUE)

round(final_pnls, 2)



# Plot log wealth
weal_th <- get(names(final_pnls[1]) ,pnl_s )
col_name <- colnames(weal_th)
dygraphs::dygraph(weal_th, main=paste("Wealth of ", names(final_pnls[1]), "Close-to-Open Strategy")) %>%
  dySeries(name=col_name[1], label=paste(names(final_pnls[1])), strokeWidth=2, col="blue") %>%
  dyLegend(width=600)







