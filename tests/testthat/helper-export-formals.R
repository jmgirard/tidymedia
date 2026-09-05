# The AC1 sweep for M112, as a function so the test below and a reviewer
# gathering evidence read the same domain. Returns one row per exported object
# that is a function: its name, its formals as a comma-joined string, and
# whether `call` is among them.
#
# The domain is `getNamespaceExports()` and not `man/*.Rd`: an .Rd file records
# no export status (one of the 82 is marked internal), so a grep over usage
# blocks cannot partition its hits into exported and not.
tm_export_formals <- function(pkg = "tidymedia") {
  ns <- asNamespace(pkg)
  names <- sort(getNamespaceExports(pkg))
  objs <- lapply(names, get, envir = ns)
  keep <- vapply(objs, is.function, logical(1))
  names <- names[keep]
  objs <- objs[keep]
  formal_names <- lapply(objs, function(f) names(formals(f)))
  data.frame(
    export = names,
    formals = vapply(
      formal_names,
      function(x) paste(x, collapse = ", "),
      character(1)
    ),
    has_call = vapply(formal_names, function(x) "call" %in% x, logical(1)),
    stringsAsFactors = FALSE
  )
}
