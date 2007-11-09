
match.arg.ext = function(arg, choices, base = 1, several.ok = FALSE, 
    numeric = FALSE, ignore.case = FALSE) 
{
    if (missing(choices)) {
        formal.args <- formals(sys.function(sys.parent()))
        choices <- eval(formal.args[[deparse(substitute(arg))]])
    }
    if (is.character(arg)) {
        if (ignore.case) {
            choices = tolower(choices)
            arg = tolower(arg)
        }
        res = match.arg(arg=arg,choices=choices,several.ok=several.ok)
        if (numeric)  res = which(choices %in% res) + base - 1
    } else if (is.numeric(arg)) {
        if ( (arg<base) | (arg>(length(choices)+base-1)) )
            stop("'arg' should be between ",base," and ",length(choices)+base-1)
        if (numeric) {
            res = arg
        } else {
            res = choices[arg - base + 1]
        }
    } else stop("'arg' should be numeric or character")
    return(res)
}
