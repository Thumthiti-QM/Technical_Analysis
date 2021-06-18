EWMA strategy parameters
library(rutils)
oh_lc <- rutils::etf_env$VTI
wid_th <- 352
lamb_da <- 0.01


source("ewma_model.R")



position_s <- simu_ewma(oh_lc=oh_lc, lamb_da=lamb_da, wid_th=wid_th)$positions





sum(position_s)
tail(position_s)



sum(ceiling(abs(rutils::diff_it(position_s)/2)))  

lamb_das <- seq(from=0.01, to=0.40, by=0.01)
lamb_das



n_trades <- sapply(lamb_das, function(lamb_da){
  p_s <- simu_ewma(oh_lc=oh_lc,lamb_da=lamb_da, wid_th=wid_th)[, "positions"]
  sum(ceiling(abs(rutils::diff_it(p_s)/2))) 
})


n_trades



x11(width=6, height=5)


plot(x=lamb_das, y=n_trades, t="l", col = "blue", main="Number of Trades
as Function of the Decay Parameter Lambda", xlab = "Decay Parameter Lambda", 
     ylab = "Number of Trades", lwd = 3)


library(HighFreq)

oh_lc <- HighFreq::SPY["2009"]

re_turns <- 60*rutils::diff_it(log(as.numeric(Cl(oh_lc[, 4])))) /
  rutils::diff_it(.index(oh_lc))
re_turns[1] <- 0
price_s <- cumsum(re_turns)

inter_val <- 35^2
inter_log <- log(inter_val)

n_rows <- NROW(price_s)
num_agg <- n_rows %/% inter_val
end_p <- c(0, n_rows - num_agg*inter_val + (0:num_agg)*inter_val)






calc_hurst <- function(re_turns, end_p){
  interval_s <- round(seq.int(from=3, to=30, length.out=9)^2)
  r_s <- sapply(interval_s, function(inter_val) {
    end_p <- rutils::calc_endpoints(price_s,
                                      inter_val=inter_val)
    r_s <- sapply(2:NROW(end_p), function(ep) {
      in_dex <- end_p[ep-1]:end_p[ep]
      diff(range(price_s[in_dex]))/sd(re_turns[in_dex])
      }) # end sapply
    mean(na.omit(r_s))
    }) # end sapply
  rs_log <- log(r_s)
  rs_log <- rs_log - mean(rs_log)
  inter_log <- log(interval_s)
  inter_log <- inter_log - mean(inter_log)
  mod_el <- lm(rs_log ~ inter_log)
  hurs_t <- summary(mod_el)$coeff[2, 1]
  return(hurs_t)
}





calc_hurst(re_turns, end_p)


library(parallel) 
n_boot <- 1e2
n_rows <- NROW(re_turns)





n_cores <- detectCores() - 1
clus_ter <- makeCluster(n_cores)
clusterSetRNGStream(clus_ter, 1121)
clusterExport(clus_ter, c("calc_hurst","price_s"))








  
boot_data <- parLapply(clus_ter, 1:n_boot,
                           function(x, re_turns, n_rows) {
                            sampl_e <- re_turns[sample.int(n_rows, replace=TRUE)]
                            c(hurst=calc_hurst(sampl_e, end_p))
                            
                             }, re_turns=re_turns, n_rows=n_rows) 


boot_data <- rutils::do_call(rbind, boot_data)
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), std_error=sd(x)))


sd(re_turns)/sqrt(n_rows)
stopCluster(clus_ter)




tail(boot_data)
c(mean=mean(boot_data), std_error=sd(boot_data))



densi_ty <- density(boot_data)

plot(density(boot_data[, "hurst"]), lwd=2, xlab="Hurst value",main="Distribution of Bootstrapped Hurst Value", col = "blue")
abline(v=mean(boot_data[, "hurst"]), lwd=2, col="red")
text(x=mean(boot_data), y = 200, 0.1, labels="expected value", pos=3, cex=0.9)


