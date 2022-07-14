# ipkqg.args

Parse command line arguments of the form "KEY1=VALUE1 KEY2=VALUE2"

Define a command line interface for an R script with this package. 

Example:

```r
# R script "myscript.R"
library(ipkqg.args)
ARGS <- getKVArgs()

number <- arg_int(ARGS, "number")
path <- arg_filename(ARGS, exists = TRUE)
```

Such a script could be called with

`Rscript myscript.R number=3 path=path/to/existing/file`

