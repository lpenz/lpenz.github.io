df0cases <- apply(dssim, 2, function(i) sum(i > totalspace))
df0prob  <- df0cases / sum(df0cases)
