plot(ecdf(df0date), xaxt='n')
axis.Date(1, at=seq(df0date[1], max(df0date), "years"), format="%Y-%m-%d")
