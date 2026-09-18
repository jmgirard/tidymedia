# M106: the ledger of every namespace function that calls resolve_hw_encoder().
#
# resolve_hw_encoder() is the one place a verb's `video_codec` turns into an
# encoder name, and under a hardware backend it is where this FFmpeg build gets
# asked what it has. A caller that hands it a token nobody checked gets an
# answer about the machine to a question about its own argument -- the defect
# M095 removed from three pipelines and M106 removed from the seam itself.
#
# The DOMAIN is computed, never listed: every function in the installed
# namespace whose own body names the resolver, by all.names(body(f)) -- the
# mechanism tm_symbol_graph() uses (helper-timeout-sweep.R), which reads the
# parsed body, so the name cannot be faked by a comment or a string. A verb or
# pipeline that starts calling the resolver joins the domain on its own and
# fails test-hw-encoder-ledger.R until someone states why it is safe.
#
# Three dispositions, and each is machine-checked against the site's own body
# rather than taken on trust:
#
#   literal        the codec handed to the resolver is a string constant in the
#                  source, so there is no caller token to check
#   checked-above  the codec is a symbol, and a check on that same symbol
#                  appears earlier in the body than the resolver call
#   emit-half      as checked-above, and the check is check_video_codec() --
#                  this is the seam half whose whole job is to hold that order
#
# The sites, and why each is safe:
#
#   emit_video_codec         emit-half      check_video_codec() is the first
#                                           line of the body, above the
#                                           resolver call (M106)
#
# One site since M135. format_for_web_pipeline() (literal: the recipe's own
# "libx264") and anonymize_pipeline() (checked-above: check_token() at the top
# of the body) each called the resolver directly until the `quality` seam gave
# them a reason to go through the emit half instead; both dispositions still
# hold there and are what test-hw-encoder-ledger.R's discrimination cases
# exercise, on bodies that fail them.
tm_hw_encoder_ledger <- function() {
  c(emit_video_codec = "emit-half")
}

# The checkers that count as checking a codec token. check_video_codec() is the
# seam's own half; check_token() is what it calls and what anonymize_pipeline()
# calls directly, with `allow_null = TRUE` for the sentinel.
tm_hw_encoder_checkers <- function() c("check_video_codec", "check_token")

# The resolver's two names (M135): resolve_hw_encoder_info() is the body, which
# also answers whether it fell back, and resolve_hw_encoder() is the one-string
# wrapper every older caller keeps. A site reaching either is in the domain; the
# wrapper itself is the resolver's other door, not a caller, and is left out.
tm_hw_encoder_resolvers <- function() {
  c("resolve_hw_encoder", "resolve_hw_encoder_info")
}

# The computed domain: namespace functions whose own body names the resolver.
tm_hw_encoder_sites <- function() {
  ns <- asNamespace("tidymedia")
  objs <- mget(ls(ns, all.names = TRUE), envir = ns, ifnotfound = list(NULL))
  fns <- objs[vapply(objs, is.function, logical(1))]
  sites <- names(fns)[vapply(
    fns,
    function(f) any(tm_hw_encoder_resolvers() %in% all.names(body(f))),
    logical(1)
  )]
  tm_sort_c(setdiff(sites, tm_hw_encoder_resolvers()))
}

# Every call in a body, in source order.
#
# Order is the whole point -- "checked above" is a claim about position -- so
# this recurses into each language object's elements in the order they appear
# rather than collecting a set. A `{` block, an `if`, a function call's
# arguments: all are calls whose elements are visited left to right.
tm_calls_in_order <- function(x) {
  if (!is.call(x)) return(list())
  out <- list(x)
  for (el in as.list(x)) {
    if (is.call(el)) out <- c(out, tm_calls_in_order(el))
  }
  out
}

# Where in that order the resolver is called, and with what first argument.
tm_hw_encoder_resolve_calls <- function(f) {
  calls <- tm_calls_in_order(body(f))
  is_resolve <- vapply(
    calls,
    function(cl) is.name(cl[[1]]) &&
      as.character(cl[[1]]) %in% tm_hw_encoder_resolvers(),
    logical(1)
  )
  lapply(which(is_resolve), function(i) list(at = i, arg = calls[[i]][[2]]))
}

# Whether `sym` is checked by one of `checkers` at a position before `before`.
tm_hw_encoder_checked_before <- function(f, sym, before,
                                         checkers = tm_hw_encoder_checkers()) {
  calls <- tm_calls_in_order(body(f))
  if (before < 2) return(FALSE)
  any(vapply(seq_len(before - 1L), function(i) {
    cl <- calls[[i]]
    length(cl) >= 2 && is.name(cl[[1]]) &&
      as.character(cl[[1]]) %in% checkers && identical(cl[[2]], sym)
  }, logical(1)))
}

# Does the site's own body bear out its stated disposition?
#
# Returns TRUE or a character string saying what it found instead, so a failing
# expectation reports the reason rather than `FALSE is not TRUE`.
tm_hw_encoder_disposition_holds <- function(name, disposition) {
  f <- get(name, envir = asNamespace("tidymedia"))
  sites <- tm_hw_encoder_resolve_calls(f)
  if (length(sites) == 0) {
    return(paste0(name, ": no resolve_hw_encoder() call found in the body"))
  }
  for (site in sites) {
    if (disposition == "literal") {
      if (!is.character(site$arg)) {
        return(paste0(name, ": codec is ", deparse(site$arg),
                      ", not a string constant"))
      }
      next
    }
    if (!is.name(site$arg)) {
      return(paste0(name, ": codec is ", deparse(site$arg), ", not a symbol"))
    }
    checkers <- if (disposition == "emit-half") {
      "check_video_codec"
    } else {
      tm_hw_encoder_checkers()
    }
    if (!tm_hw_encoder_checked_before(f, site$arg, site$at, checkers)) {
      return(paste0(name, ": ", deparse(site$arg), " is not checked by ",
                    paste(checkers, collapse = "/"), " above the resolver"))
    }
  }
  TRUE
}
