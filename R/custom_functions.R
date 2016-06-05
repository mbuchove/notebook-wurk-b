# to emulate docstring 
describe <- function(obj) attr(obj, "help")

catn <- function(...){cat(..., "\n", sep='')}
attr(catn, 'help') <- "a cleaner way to print plain text, includes newline and no separation"
#catn(describe(catn))

#help(pbinom)

