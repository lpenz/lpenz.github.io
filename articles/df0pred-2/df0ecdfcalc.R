df0day <- sort(daysleft + today)
df0ecdfunc <- ecdf(df0day)
df0prob <- df0ecdfunc(df0day)
