simu_ewma <- function(oh_lc, lamb_da=0.01, wid_th=251, bid_offer=0.001, tre_nd=1) {
n_rows <- NROW(oh_lc)
# Calculate EWMA prices
weight_s <- exp(-lamb_da*1:wid_th)
weight_s <- weight_s/sum(weight_s)
clos_e <- quantmod::Cl(oh_lc)
ew_ma <- stats::filter(clos_e, filter=weight_s, sides=1, method="convolution")
ew_ma[1:(wid_th-1)] <- ew_ma[wid_th]
# Determine trade dates right after EWMA has crossed prices
indica_tor <- tre_nd*sign(clos_e - as.numeric(ew_ma))
trade_dates <- (rutils::diff_it(indica_tor) != 0)
trade_dates <- which(trade_dates) + 1
trade_dates <- trade_dates[trade_dates < n_rows]
# Calculate positions, either: -1, 0, or 1
position_s <- rep(NA_integer_, n_rows)
position_s[1] <- 0
position_s[trade_dates] <- indica_tor[trade_dates-1]
position_s <- zoo::na.locf(position_s, na.rm=FALSE)
op_en <- quantmod::Op(oh_lc)
close_lag <- rutils::lag_it(clos_e)
pos_lagged <- rutils::lag_it(position_s)
# Calculate daily profits and losses
pnl_s <- rutils::diff_it(clos_e)*position_s
pnl_s[trade_dates] <- pos_lagged[trade_dates]*
(op_en[trade_dates] - close_lag[trade_dates])
pnl_s[trade_dates] <- pnl_s[trade_dates] +
position_s[trade_dates]*
(clos_e[trade_dates] - op_en[trade_dates])
# Calculate transaction costs
cost_s <- 0.5*bid_offer*abs(pos_lagged - position_s)*clos_e
pnl_s <- (pnl_s - cost_s)
# Calculate strategy returns
pnl_s <- cbind(position_s, pnl_s)
colnames(pnl_s) <- c("positions", "pnls")
pnl_s
} #