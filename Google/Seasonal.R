```{python seasonalFun}
# def seas_decomp(series):
#  # set the fig size
#  N,M = 16,10
#  result = seasonal_decompose(trends[series], model = 'additive')
#  rcParams['figure.figsize'] = N,M
#  result.plot()
#  plt.show()
```





## Home Equity Credit Line
```{python decomp}
N,M = 16,10
result = seasonal_decompose(trends['home_equity_credit_line'], model = 'additive')
rcParams['figure.figsize'] = N,M
result.plot()
plt.show()


```


## Home Equity Credit Line
```{python decompHECL}
# # Redo the same thing, but with the known frequency
# seasonal_decompose(df, model='additive', freq=365).plot()
result = seasonal_decompose(trends['home_equity_credit_line'], model = 'additive', freq=7)
print(result)
result.plot();
# plt.show();

```

```{r}
knitr::knit_exit()
```


# py_run_string("import os as os")
# py_run_string("os.environ['QT_QPA_PLATFORM_PLUGIN_PATH'] = 'C:/Users/bbutler/Documents/anaconda3/Library/plugins/platforms'")