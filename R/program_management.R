
# Config directory ---------------------------------------------------------

# Where a remembered program location lives. `tools::R_user_dir()` is the one
# user config location CRAN policy sanctions (M097); it honors
# `R_USER_CONFIG_DIR`, which is how the suite keeps its writes out of the
# user's real configuration.
tm_config_dir <- function() {
  tools::R_user_dir("tidymedia", "config")
}

# Where a location set before M097 was written. Delegated to the library that
# wrote it rather than reconstructed by hand: the legacy layout differs per
# platform, and a single-platform suite cannot catch a second copy of those
# rules drifting. The only place the package computes it (M097 AC5).
tm_legacy_config_dir <- function() {
  rappdirs::user_config_dir("tidymedia", "R")
}

tm_config_file <- function(program, dir = tm_config_dir()) {
  file.path(dir, glue("{program}_location.txt"))
}

# Data directory -----------------------------------------------------------

# Where `install_on_win()` puts a downloaded FFmpeg build when the caller names
# no directory. `tools::R_user_dir()` is the one user data location CRAN policy
# sanctions (M098); it honors `R_USER_DATA_DIR`, which is how the suite keeps
# its writes out of the user's real data directory. The `ffmpeg` subdirectory
# is the extraction root: `archive_extract(strip_components = 1)` unpacks the
# build's contents into it, so the binaries land under `bin/` beneath it.
tm_install_dir <- function() {
  file.path(tools::R_user_dir("tidymedia", "data"), "ffmpeg")
}

# find_program() ----------------------------------------------------------

#' Find the location of a dependency program
#'
#' Returns the location of the requested program as a string.
#'
#' The program is looked up on the `PATH` first. When it is not there, the
#' location remembered by [set_program()] is read from
#' `tools::R_user_dir("tidymedia", "config")`; a location remembered by a
#' version of tidymedia before 0.2.0 was written to
#' `rappdirs::user_config_dir("tidymedia", "R")`, and that file is read only
#' when no file exists in the current directory.
#'
#' @param program A string indicating which program to find
#' @return Either a string indicating whether the requested program was found or
#'   `NULL` if the program could not be found.
#' @seealso [set_program()] to point tidymedia at a binary in a non-standard
#'   location, and [install_on_win()] to download FFmpeg on Windows.
#' @family program management functions
#' @examples
#' # Returns the path to the binary, or NULL with a warning if it is not found
#' find_ffmpeg()
#' find_mediainfo()
find_program <- function(program = c("ffmpeg", "ffprobe", "ffplay", "mediainfo")) {
  
  # Validate arguments
  program <- rlang::arg_match(program)

  # First, look for program in path
  location <- Sys.which(program)
  
  if (location == "") {
    # If program not found, look for a user config file: the R_user_dir()
    # location first, then the pre-M097 one, so a location set before
    # the move is still found. The read falls back; nothing is migrated.
    config <- tm_config_file(program)
    if (!file.exists(config)) {
      config <- tm_config_file(program, tm_legacy_config_dir())
    }
    # If a user config file exists, read it in
    if (file.exists(config)) {
      location <- readLines(config)
      # Verify that the location in the user config file is valid
      if (Sys.which(location) == "") {
        cli::cli_warn(c(
          "{program} was configured at {.file {location}} but that file no \\
           longer seems to exist.",
          "i" = "Use {.fn set_{program}} to point tidymedia at it again."
        ))
        location <- NULL
      }
    } else {
      # If config file not found, return NULL value and warning
      location <- NULL
      cli::cli_warn(c(
        "Failed to find {program}.",
        "i" = "Check that it is installed and, if necessary, use \\
               {.fn set_{program}}."
      ))
    }
  }
  
  location
}

# find_mediainfo() --------------------------------------------------------

#' @rdname find_program
#' @export
find_mediainfo <- function() {
  find_program("mediainfo")
}

# find_ffmpeg() -----------------------------------------------------------

#' @rdname find_program
#' @export
find_ffmpeg <- function() {
  find_program("ffmpeg")
}

# find_ffprobe() -----------------------------------------------------------

#' @rdname find_program
#' @export
find_ffprobe <- function() {
  find_program("ffprobe")
}

# find_ffplay() -----------------------------------------------------------

#' @rdname find_program
#' @export
find_ffplay <- function() {
  find_program("ffplay")
}

# run_program() -----------------------------------------------------------

# Run a resolved program with an argument vector and return its stdout as a
# character vector. Every token in `args` is passed through `shQuote()` so the
# CLI receives arguments verbatim without shell interpolation: file paths and
# MediaInfo `--Inform` strings containing spaces, quotes, `;`, `%`, or `$` are
# safe. This is the internal counterpart to the Layer 0 escape hatches
# `mediainfo()` / `ffprobe()`, which stay raw-string by design (D002); the
# structured readers build clean token vectors and route them through here.
#
# Callers pass tokens unquoted (one vector element per CLI argument); do not
# pre-quote. `location` is the output of a `find_*()` call; a missing binary
# aborts rather than shelling out to nothing.
#
# `input` and `stderr` pass through to system2(): ffm_run() sets `input = ""`
# so FFmpeg cannot drain the parent's stdin (see ffmpeg()) and `stderr = ""`
# so encode progress/errors stream to the console; the metadata readers keep
# the quiet defaults (stderr discarded).
run_program <- function(location, args, program = "the program",
                        input = NULL, stderr = FALSE,
                        call = rlang::caller_env()) {
  if (is.null(location) || is.na(location) || !nzchar(location)) {
    cli::cli_abort("Could not locate {program}.", call = call)
  }
  # shQuote()'s default is sh-style quoting on every OS; on Windows the child
  # process parses its command line with cmd-style (double-quote) rules, so
  # the type must follow the platform or spaced paths break there.
  quote_type <- if (.Platform$OS.type == "windows") "cmd" else "sh"
  # `suppress = TRUE` reproduces the suppressWarnings() this replaces: every
  # caller here has always seen warnings discarded, and the timeout is the only
  # new thing that reaches them (M69/D047). The limit is 0 -- no limit, byte-for
  # -byte today's behavior -- unless the caller set `tidymedia.timeout`.
  limit <- resolve_timeout(call = call)
  guard_timeout(
    program, limit,
    system2(location, args = shQuote(args, type = quote_type), stdout = TRUE,
            stderr = stderr, input = input, timeout = limit),
    suppress = TRUE,
    call = call
  )
}

# set_program() ------------------------------------------------------------

#' Set the location of a dependency program
#'
#' The location is remembered across sessions in a file named
#' `<program>_location.txt` under `tools::R_user_dir("tidymedia", "config")`,
#' which [find_program()] reads whenever the program is not on the `PATH`.
#' Once this file exists, a location remembered by a version of tidymedia
#' before 0.2.0 is no longer read.
#'
#' @param program A string indicating which program to set the location for.
#' @param location A string containing the location of the program.
#' @return A logical indicating whether the program location was set properly.
#'
#' @seealso [find_program()] to locate a configured binary, and
#'   [install_on_win()] to download FFmpeg on Windows.
#' @family program management functions
#' @examples
#' \dontrun{
#' # Point tidymedia at a binary in a non-standard location
#' set_mediainfo("C:/Program Files/MediaInfo/mediainfo.exe")
#' }
#' @export
set_program <- function(program = c("ffmpeg", "ffprobe", "ffplay", "mediainfo"),
                         location) {
  
  # Validate arguments
  program <- rlang::arg_match(program)
  rlang::check_string(location)
  if (Sys.which(location) == "") {
    cli::cli_abort("Can't find an executable at {.file {location}}.")
  }
  
  # Find where to save user configuration data (tools::R_user_dir(), M097)
  config_dir <- tm_config_dir()
  config_file <- tm_config_file(program, config_dir)
  
  # Create configuration directory if needed
  if (!dir.exists(config_dir)) dir.create(config_dir, recursive = TRUE)
  
  # Save location to user configuration file
  writeLines(location, config_file)

  # Pointing at a different binary invalidates everything remembered about the
  # old one, so the session memo goes with it (M67/D044). Unconditional across
  # all four programs: cheap, and it cannot be forgotten when a second
  # capability memo is added later.
  forget_ffmpeg_capabilities()
}

# set_mediainfo() ---------------------------------------------------------

#' @rdname set_program
#' @export
set_mediainfo <- function(location) {
  set_program("mediainfo", location)
}

# set_ffmpeg() ------------------------------------------------------------

#' @rdname set_program
#' @export
set_ffmpeg <- function(location) {
  set_program("ffmpeg", location)
}

#' @rdname set_program
#' @export
set_ffprobe <- function(location) {
  set_program("ffprobe", location)
}

#' @rdname set_program
#' @export
set_ffplay <- function(location) {
  set_program("ffplay", location)
}


# tm_confirm() ------------------------------------------------------------

# Ask the caller to approve an action before it happens, and refuse rather
# than assume consent where there is no one to ask (D080). The seam carries no
# caller's argument name of its own: the caller supplies the bullets that name
# its own escape hatch, so a second caller whose hatch is spelled differently
# inherits no wrong hint (M38/M40).
#
# `prompt` arrives already formatted -- the caller runs its own values through
# cli, so a path containing braces cannot be re-interpolated here (M44).
#
# `rlang::is_interactive()` decides whether anyone can be asked; it honors
# `rlang::local_interactive()` and the `rlang_interactive` option, which is how
# the suite reaches the refusal branch. `utils::menu()` gates on
# `base::interactive()` instead, which those do not move, so the ask branch is
# reachable in a test only through a mock of `menu()`.
tm_confirm <- function(prompt, ..., call = rlang::caller_env()) {
  refuse <- function() {
    cli::cli_abort(
      c("Can't ask for confirmation in a non-interactive session.", ...),
      class = "tidymedia_confirmation_unavailable",
      call = call
    )
  }
  if (!rlang::is_interactive()) refuse()
  # The two predicates can disagree: `rlang::is_interactive()` honors the
  # `rlang_interactive` option, `base::interactive()` does not, and `menu()`
  # refuses on the latter. A caller who has set that option in a session with
  # no console reaches this line and would otherwise get menu()'s own
  # unclassed error, with none of the refusal's information; routing that
  # failure through refuse() keeps one contract for "no one can be asked".
  answer <- tryCatch(
    utils::menu(c("Yes", "No"), title = prompt),
    error = function(cnd) refuse()
  )
  # menu() returns 0 when the reader answers nothing, which is a decline.
  answer == 1L
}

# Make a string safe to hand to cli as message text: cli interpolates `{...}`
# in every bullet, so a path containing braces is escaped rather than
# evaluated (M44).
tm_cli_escape <- function(x) {
  gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)
}


# install_on_win() --------------------------------------------------------

# The programs an install registers, and where the extracted build puts each
# one. Named once so the prompt cannot promise a different set of writes from
# the one the call makes.
tm_install_registers <- c("ffmpeg", "ffprobe", "ffplay")

# Of those three, the two the package itself calls. `ffplay` is reachable only
# through find_ffplay()/set_ffplay() and nothing in tidymedia invokes it, so a
# build that omits it is an install worth completing; a build with no `ffmpeg`
# or no `ffprobe` is not (M102 AC4).
tm_install_required <- c("ffmpeg", "ffprobe")

# The build install_on_win() fetches when the caller names no URL. Named once
# so the sidecar decision compares against the same string the download uses,
# rather than against a second copy of it that could drift.
tm_default_download_url <-
  "https://www.gyan.dev/ffmpeg/builds/ffmpeg-release-essentials.7z"

# Where a source publishes its archive's digest: the archive URL with
# `.sha256` appended, which is what gyan.dev writes beside every build.
tm_sidecar_url <- function(download_url) {
  paste0(download_url, ".sha256")
}

# Pull a SHA-256 out of a published sidecar body. Three spellings are in the
# wild and all three are accepted: a bare digest on its own, `sha256sum`'s
# "<hex>  <file>" pair, and OpenSSL's "SHA256(<file>)= <hex>". Hex case is not
# significant in any of them, so the digest comes back lowercased and every
# comparison downstream can be a plain identical().
#
# Returns NULL where no line matches, rather than aborting. Every helper this
# milestone adds returns instead of aborting, deliberately: install_on_win()
# raises all of its classed refusals in its own body, which is what makes the
# census AC6 derives from that body's exits exactly true rather than nearly so.
tm_parse_sidecar <- function(lines) {
  if (is.null(lines)) return(NULL)
  patterns <- c(
    # A digest alone, or first of two whitespace-separated fields.
    "^[[:space:]]*([0-9a-fA-F]{64})([[:space:]]|$)",
    "[Ss][Hh][Aa]256[[:space:]]*\\([^)]*\\)[[:space:]]*=[[:space:]]*([0-9a-fA-F]{64})"
  )
  for (line in lines) {
    for (pattern in patterns) {
      found <- regmatches(line, regexec(pattern, line))[[1]]
      if (length(found)) return(tolower(found[[2]]))
    }
  }
  NULL
}

# The SHA-256 of a file on disk. `digest` reads the file in chunks rather than
# into memory, which matters for a several-hundred-megabyte build (D081).
# Returns NULL where the file cannot be read. `digest()` aborts with a bare
# simpleError on a path that is not there, which would leave install_on_win()
# through an exit carrying no class of its own -- and one AC6's census cannot
# see, since it is neither a return() nor a cli_abort() node.
tm_archive_digest <- function(path) {
  tryCatch(
    tolower(digest::digest(path, algo = "sha256", file = TRUE)),
    error = function(cnd) NULL
  )
}

# Fetch `url` to `destfile`. `utils::download.file()`'s contract allows two
# shapes of failure -- signalling a condition of its own, and returning a
# non-zero status -- and this covers both in one value: TRUE where the file
# arrived, the signalled condition where there was one, and NULL where the
# status was non-zero and nothing was signalled. The caller turns anything but
# TRUE into its own classed refusal, retaining the condition as `parent` where
# one exists.
tm_fetch <- function(url, destfile) {
  tryCatch(
    {
      status <- utils::download.file(url = url, destfile = destfile, mode = "wb")
      # Status 0 is not a promise that a readable file arrived: a mirror can
      # report success and leave nothing behind, and the caller downstream
      # would then meet a bare simpleError from whatever reads the file. The
      # existence test is what makes the TRUE above mean what this comment
      # says it means.
      if (identical(as.integer(status), 0L) && file.exists(destfile)) {
        TRUE
      } else {
        NULL
      }
    },
    error = function(cnd) cnd
  )
}

# Every entry under `dir`, recursively, as a data frame of path, size and
# mtime -- the three fields D046's created-or-changed comparison reads. Paths
# are relative to `dir` so two snapshots of the same directory compare
# directly, directories are included because a failed extraction creates them
# as readily as it creates files, and dotfiles are included because a caller's
# hidden file is as much theirs as a visible one.
#
# Size and time come from one `file.info()` stat per entry, and the frame is
# ordered by path so a comparison never turns on the order the filesystem
# happened to list.
tm_dir_snapshot <- function(dir) {
  paths <- sort(list.files(
    dir,
    recursive = TRUE, all.files = TRUE, include.dirs = TRUE, no.. = TRUE
  ))
  info <- file.info(file.path(dir, paths), extra_cols = FALSE)
  data.frame(
    path = paths,
    size = as.numeric(info$size),
    mtime = as.numeric(info$mtime),
    isdir = as.logical(info$isdir),
    stringsAsFactors = FALSE
  )
}

# What the two snapshots show this extraction added, split into the two sets
# the removal treats differently.
#
# A DIRECTORY qualifies only where it is new, and only the topmost new one of
# a chain: a pre-existing directory's mtime moves the instant an entry lands
# inside it -- measured 2026-09-02, 19:13:23.663 before and 19:13:24.848 after
# one file was written into it -- so a merely-changed directory removed
# recursively would take the caller's own untouched entries with it. Its added
# children are removed one by one instead, which reaches the same debris
# without the collateral (M103).
#
# A FILE qualifies where it is new OR where its size or mtime moved, which is
# D046's rule unchanged: the zero-byte truncation is the case that rule exists
# to clear, and a pre-existing file the extraction overwrote is a file this
# run wrote.
tm_snapshot_added <- function(before, after) {
  known <- before$path
  changed <- vapply(
    seq_len(nrow(after)),
    function(i) {
      j <- match(after$path[i], known)
      is.na(j) ||
        !identical(after$size[i], before$size[j]) ||
        !identical(after$mtime[i], before$mtime[j])
    },
    logical(1)
  )
  created_dirs <- after$path[!(after$path %in% known) & after$isdir]
  list(
    files = after$path[changed & !after$isdir],
    dirs = created_dirs[!(dirname(created_dirs) %in% created_dirs)]
  )
}

# A thin wrapper over `unlink()`, a seam of its own so the suite can make a
# removal fail without making the filesystem fail. `expand = FALSE` is D046's
# rule: a name holding `*`, `?` or `[` costs no neighbour.
tm_unlink <- function(path, recursive = FALSE) {
  unlink(path, recursive = recursive, expand = FALSE)
}

# Remove what the extraction added under `dir`, and name what would not go.
#
# Directories first and recursively, so the deep chain a stripped absolute
# path produces costs one call rather than one per level; then the added files
# that are still there, which is every file outside a removed directory. A
# third snapshot decides what survived: `unlink()` reports one status for a
# whole subtree and no names at all, so the only honest answer to "what is
# still there" is another look at the directory. Only entries the removal
# TARGETED can be reported -- an untouched file the caller put there is not a
# leftover (M103).
tm_remove_added <- function(dir, before, after) {
  added <- tm_snapshot_added(before, after)
  targeted <- c(added$dirs, added$files)
  if (!length(targeted)) return(character(0))
  sweep <- function() {
    still <- tm_dir_snapshot(dir)$path
    for (path in added$dirs) {
      if (path %in% still) tm_unlink(file.path(dir, path), recursive = TRUE)
    }
    still <- tm_dir_snapshot(dir)$path
    for (path in added$files) {
      if (path %in% still) tm_unlink(file.path(dir, path))
    }
    sort(targeted[targeted %in% tm_dir_snapshot(dir)$path])
  }
  survived <- sweep()
  # A second pass, and only where the first left something. Windows will not
  # delete a file another handle still holds, and the handle in question is
  # the one `archive_extract()` was writing the failed entry through: it is
  # not an R connection, so nothing here can close it by name. `gc()` is the
  # one lever R has -- it runs the finalizers of any external pointer the
  # extraction left unreferenced -- and the pause is for a hold that is
  # transient rather than leaked, which a scanner or an indexer produces.
  # Both are free on the succeeding path, which never reaches this function,
  # and on the platforms where the first sweep already worked.
  if (length(survived)) {
    gc()
    Sys.sleep(0.1)
    survived <- sweep()
  }
  survived
}

# The chain of directories `dir.create(path, recursive = TRUE)` would have to
# make for `path` to exist, outermost first, or none where it already does.
#
# Read BEFORE the create rather than after, because after is too late to tell
# a directory the call made from one it found -- and a partially successful
# recursive create, which makes the parents and then fails on the leaf, leaves
# a state no later look can attribute.
tm_missing_ancestors <- function(path) {
  out <- character(0)
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  while (!dir.exists(path)) {
    out <- c(path, out)
    parent <- dirname(path)
    if (identical(parent, path)) break
    path <- parent
  }
  out
}

# Remove the directories `tm_missing_ancestors()` named, deepest first, and
# stop at the first that is not empty.
#
# Emptiness is the guard rather than a record of what was written: a directory
# the call created but something else has since put a file in is not the
# call's to delete, and stopping rather than skipping is what keeps the
# removal from reaching around a kept directory to its parent.
tm_remove_created_dirs <- function(dirs) {
  for (dir in rev(dirs)) {
    if (!dir.exists(dir)) next
    if (length(list.files(dir, all.files = TRUE, no.. = TRUE))) break
    if (!identical(tm_unlink(dir, recursive = TRUE), 0L)) break
  }
  invisible(NULL)
}

# Unpack `archive` into `dir`. A two-slot list: `files`, the paths the
# extraction produced relative to `dir`, or NULL where libarchive refused; and
# `leftovers`, the entries a refused extraction wrote and the cleanup could
# not remove.
#
# The list replaces the bare NULL M102 returned because a failed unpack now
# has two things to say rather than one. R drops attributes on NULL, so
# carrying the leftovers alongside a NULL return was never open; the caller
# tests `is.null(produced$files)` instead (M103).
#
# The file list is returned rather than dropped because it is the only honest
# answer to what THIS extraction produced: `archive_extract()` writes into a
# directory it does not clear, so a later look at `dir` sees a previous
# install's files as readily as this one's (M102 AC4).
#
# The condition is dropped rather than returned, which is the one place this
# milestone deliberately loses information: libarchive's message is a C++
# source location and an internal function name (`archive_extract.cpp:140
# archive_read_open1(): Unrecognized archive format`), which tells the reader
# of an R error nothing they can act on. The caller's refusal names the archive
# and the directory instead (M102 AC3).
tm_unpack <- function(archive, dir) {
  # The connection is opened here rather than left to `archive_extract()`,
  # which opens `file(archive, "rb")` itself and closes it only on the paths
  # that reach the end of the read. A libarchive failure inside
  # `archive_read_data_block()` leaves it open -- measured 2026-09-02, one
  # leaked connection for the corrupt-payload fixture, none for the
  # unrecognized-format one and none on the succeeding path -- and Windows
  # will not delete a file something still holds open, so that leak is what
  # left the downloaded archive behind after a failed unpack (M102 AC3).
  con <- tryCatch(file(archive, "rb"), error = function(cnd) NULL)
  if (is.null(con)) return(list(files = NULL, leftovers = character(0)))
  on.exit(tm_close(con), add = TRUE)
  before <- tm_dir_snapshot(dir)
  produced <- tryCatch(
    as.character(archive::archive_extract(con, dir = dir, strip_components = 1)),
    error = function(cnd) NULL
  )
  if (!is.null(produced)) {
    return(list(files = produced, leftovers = character(0)))
  }
  # The connection is closed BEFORE the removal rather than left to the exit
  # handler: it is the archive's, not the destination's, but the same rule
  # that made it worth owning -- Windows will not delete a file something
  # still holds open -- says to hold no handle the cleanup does not need. The
  # handler stays, and finds nothing left to close.
  tm_close(con)
  list(
    files = NULL,
    leftovers = tm_remove_added(dir, before, tm_dir_snapshot(dir))
  )
}

# Which of `programs` the extraction produced, read off the file list
# `tm_unpack()` returned rather than off the install directory. The paths are
# matched where the install will look for them -- `bin/<program>.exe` under
# the install directory, which is what tm_install_binary() builds -- with
# separators normalized, because a path is what libarchive reports and the
# comparison should not turn on which slash it used, and case folded, because
# the target filesystem does not distinguish them either.
tm_extracted_programs <- function(files, programs) {
  found <- tolower(sub("^[.]/", "", gsub("\\\\", "/", as.character(files))))
  programs[tolower(paste0("bin/", programs, ".exe")) %in% found]
}

# Close `con` where it is still open, and do nothing where the callee already
# closed it: `isOpen()` itself errors on a connection that has been destroyed,
# so the test and the close share one handler.
tm_close <- function(con) {
  invisible(tryCatch(if (isOpen(con)) close(con), error = function(cnd) NULL))
}

tm_install_binary <- function(install_dir, program) {
  file.path(install_dir, "bin", paste0(program, ".exe"))
}

# What the install would do, one item per line: the published digest where
# one will be fetched, the archive to be fetched, the directory it unpacks
# into, and each remembered-location file it may overwrite. `sidecar_url` is
# NULL on the paths that fetch no digest, so the prompt names every fetch the
# call makes and only those (M101; M102 AC1/AC2). The overwrite lines are what
# the call MAY write: which of the three the archive turns out to contain is
# not knowable before it is unpacked, so the prompt names all three and the
# install says out loud which one it skipped (M102 AC4).
# Every caller-supplied value goes through a cli field, which does
# not recurse into the value, so a directory containing braces is shown rather
# than evaluated (M44); the result is stripped of styling and hyperlinks so
# what reaches `menu()` is the plain text a test can read back.
#
# Both the prompt and the non-interactive refusal are built from this, so a
# caller who is told to pass `confirm = FALSE` has been shown the same items
# the prompt would have named.
tm_install_details <- function(download_url, install_dir, programs,
                               sidecar_url = NULL) {
  line <- function(...) cli::ansi_strip(cli::format_inline(..., .envir = parent.frame()))
  c(
    if (!is.null(sidecar_url)) line("Download: {.url {sidecar_url}}"),
    line("Download: {.url {download_url}}"),
    line("Unpack into: {.file {install_dir}}"),
    vapply(
      programs,
      function(p) line("Overwrite remembered location: {.file {tm_config_file(p)}}"),
      character(1),
      USE.NAMES = FALSE
    )
  )
}

# The consent prompt: the question, then the items, one per line.
tm_install_prompt <- function(download_url, install_dir, programs,
                              sidecar_url = NULL) {
  paste(
    c(
      "tidymedia is about to install FFmpeg. Proceed?",
      paste0("* ", tm_install_details(download_url, install_dir, programs,
                                      sidecar_url))
    ),
    collapse = "\n"
  )
}

#' Install FFmpeg on Windows
#'
#' Downloads an FFmpeg archive, extracts it, and updates the package's user
#' config files to point to the component executable files. Because the call
#' downloads a third-party build and overwrites remembered program locations,
#' it asks for confirmation first and does nothing at all until it has it.
#'
#' The archive is checked against a SHA-256 digest before anything is unpacked,
#' and no program location is remembered unless the extraction actually
#' produced that program. For the package's own default source the digest is
#' fetched from `<download_url>.sha256`, which is what gyan.dev publishes
#' beside each build; for any other source, pass `archive_checksum`. Because
#' the digest travels from the same host over the same connection as the
#' archive, this catches a corrupted or truncated download, not a compromised
#' source.
#'
#' A refusal leaves the install directory as the call found it. Files a failed
#' extraction wrote are removed, files that were already there are kept, and a
#' directory the call created is removed again; anything that could not be
#' removed is named in the error rather than left for the caller to discover.
#' The one exception is `tidymedia_program_not_extracted`, where the archive
#' unpacked successfully but did not contain a required program: that error
#' says so, and the unpacked files stay where they are.
#'
#' @param download_url A string indicating the location of the FFmpeg
#'   installation archive. If `NULL`, will default to the latest static
#'   essentials release from gyan.dev, a `.7z` archive.
#' @param install_dir A string indicating a directory to install FFmpeg to. If
#'   `NULL`, will default to the `ffmpeg` subdirectory of
#'   `tools::R_user_dir("tidymedia", "data")`, the user data directory CRAN
#'   policy sanctions.
#' @param confirm A logical indicating whether to ask for confirmation before
#'   downloading and installing anything. Defaults to `TRUE`. The prompt names
#'   the archive to be downloaded, the directory it will be unpacked into, and
#'   the remembered program locations it may overwrite. Where there is no one
#'   to ask, the call aborts rather than assume consent, naming those same
#'   items; pass `confirm = FALSE` to install without being asked.
#' @param archive_checksum A string giving the archive's expected SHA-256
#'   digest as 64 hexadecimal characters, in either case. Defaults to `NULL`.
#'   A digest supplied here is used on every source, and no digest is fetched.
#'   Where it is `NULL` and `download_url` is not the package's own default,
#'   nothing is verified and the call says so.
#' @return A logical indicating whether the installation was successful.
#'   `FALSE` is returned by a declined confirmation and by a failure to create
#'   the install directory. Five other outcomes abort with a condition of their
#'   own rather than returning: a download that did not deliver
#'   (`tidymedia_download_unavailable`), a published digest that could not be
#'   fetched or read (`tidymedia_checksum_unavailable`), a digest that did not
#'   match the downloaded archive (`tidymedia_checksum_mismatch`), an archive
#'   that could not be unpacked (`tidymedia_archive_unreadable`), and a
#'   required program the archive did not contain
#'   (`tidymedia_program_not_extracted`). Every one of these leaves the install
#'   directory as the call found it, except the last, which leaves the files
#'   the archive did unpack.
#' @seealso [set_program()] to register an existing binary, and [find_ffmpeg()]
#'   to check what is currently configured.
#' @family program management functions
#' @examples
#' \dontrun{
#' # Download and install a static FFmpeg build (Windows)
#' install_on_win()
#' }
#' @export
install_on_win <- function(download_url = NULL,
                           install_dir = NULL,
                           confirm = TRUE,
                           archive_checksum = NULL) {

  rlang::check_bool(confirm)
  # The two path-shaped arguments are checked here for the same reason the
  # other two are: without it a non-string reaches `dir.create()` or
  # `download.file()` and leaves through a bare `simpleError`, which is an
  # exit carrying no class of its own and one the AC6 census cannot see --
  # it is neither a `return()` nor a `cli_abort()` node (M102).
  rlang::check_string(download_url, allow_null = TRUE)
  rlang::check_string(install_dir, allow_null = TRUE)
  check_sha256(archive_checksum, allow_null = TRUE)

  if (is.null(download_url)) {
    download_url <- tm_default_download_url
  }
  if (is.null(install_dir)) {
    install_dir <- tm_install_dir()
  }
  # Which digest, if any, this archive gets checked against.
  #
  # A digest the caller supplied wins on every source, the package's own
  # default included: someone holding a digest already is never sent to fetch
  # a second copy of one. Otherwise only the default source has a sidecar to
  # fetch, and the test is against the RESOLVED url -- so a caller who types
  # that address out in full is verified exactly like one who named nothing.
  sidecar_url <- NULL
  if (is.null(archive_checksum) &&
      identical(download_url, tm_default_download_url)) {
    sidecar_url <- tm_sidecar_url(download_url)
  }
  # A caller-named source with no digest to check against is installed anyway
  # -- refusing would leave such a caller no route at all -- but never
  # silently, and never after the fact: it is said above the prompt, so what
  # a caller approves includes knowing that nothing will check the download.
  if (is.null(archive_checksum) && is.null(sidecar_url)) {
    cli::cli_inform(c(
      "!" = "The archive at {.url {download_url}} will not be verified.",
      "i" = "Pass {.arg archive_checksum} to check it against a digest you
             have."
    ))
  }
  # The consent sits above the first write, so a declined call creates no
  # directory, downloads nothing, and overwrites no remembered location
  # (D080). It asks about the RESOLVED values: a caller who named neither
  # argument is told what the defaults came out to. The sidecar is named here
  # for the same reason the archive is -- the prompt names every fetch the
  # call makes, and only the fetches it makes.
  if (confirm) {
    details <- tm_install_details(
      download_url, install_dir, tm_install_registers, sidecar_url
    )
    approved <- tm_confirm(
      tm_install_prompt(
        download_url, install_dir, tm_install_registers, sidecar_url
      ),
      "i" = "This install would have done the following:",
      rlang::set_names(tm_cli_escape(details), rep("*", length(details))),
      "i" = "Pass {.code confirm = FALSE} to install without being asked."
    )
    if (!approved) return(FALSE)
  }
  # What this call is about to make, and the promise to take it back again.
  # The handler is registered ABOVE the create because the create itself can
  # half-succeed -- `recursive = TRUE` makes the parents and then fails on the
  # leaf -- and every refusal below here has to leave the directory as the
  # call found it (M103 AC3). It is disarmed only once a program has been
  # registered, and it is a no-op wherever the directory holds anything: a
  # directory that already existed is never in `created_dirs` at all.
  created_dirs <- tm_missing_ancestors(install_dir)
  registered <- FALSE
  on.exit(if (!registered) tm_remove_created_dirs(created_dirs), add = TRUE)
  if (!dir.exists(install_dir)) {
    status <- dir.create(install_dir, recursive = TRUE)
    if (status == FALSE) return(FALSE)
  }
  # Both temporary files go on the exit handler rather than being unlinked at
  # the end: every abort below happens between here and there, and a refusal
  # that leaves a several-hundred-megabyte download behind is a bug the
  # succeeding path would never show (M102 AC3).
  if (!is.null(sidecar_url)) {
    sidecar_file <- tempfile()
    on.exit(unlink(sidecar_file), add = TRUE)
    # The digest is fetched BEFORE the archive: a source that cannot produce
    # one refuses in a second rather than after a long download.
    fetched <- tm_fetch(sidecar_url, sidecar_file)
    if (!isTRUE(fetched)) {
      cli::cli_abort(
        "Can't fetch the published digest at {.url {sidecar_url}}.",
        class = "tidymedia_checksum_unavailable",
        parent = if (rlang::is_condition(fetched)) fetched
      )
    }
    # The read is guarded for the same reason the fetch is: `download.file()`
    # reporting status 0 is not a promise that a readable file arrived, and a
    # bare "cannot open connection" here would be the one exit on this path
    # carrying no class of its own.
    body <- tryCatch(
      readLines(sidecar_file, warn = FALSE),
      error = function(cnd) NULL,
      warning = function(cnd) NULL
    )
    archive_checksum <- tm_parse_sidecar(body)
    if (is.null(archive_checksum)) {
      cli::cli_abort(
        c(
          "Can't read a SHA-256 digest from {.url {sidecar_url}}.",
          "i" = "Expected 64 hexadecimal characters, on their own or in a
                 {.code sha256sum} or {.code SHA256(file)=} line."
        ),
        class = "tidymedia_checksum_unavailable"
      )
    }
  }
  tf <- tempfile()
  on.exit(unlink(tf), add = TRUE)
  fetched <- tm_fetch(download_url, tf)
  if (!isTRUE(fetched)) {
    cli::cli_abort(
      "Can't download the archive at {.url {download_url}}.",
      class = "tidymedia_download_unavailable",
      parent = if (rlang::is_condition(fetched)) fetched
    )
  }
  if (!is.null(archive_checksum)) {
    found <- tm_archive_digest(tf)
    # A download that cannot be read is a download that did not deliver, and
    # it refuses as one: reporting a mismatch against a digest that could not
    # be computed would name a cause that is not the cause.
    if (is.null(found)) {
      cli::cli_abort(
        c(
          "Can't download the archive at {.url {download_url}}.",
          "i" = "The download reported success but nothing readable arrived,
                 so its digest could not be computed."
        ),
        class = "tidymedia_download_unavailable"
      )
    }
    if (!identical(found, tolower(archive_checksum))) {
      cli::cli_abort(
        c(
          "The downloaded archive does not match its SHA-256 digest.",
          "i" = "Expected {.val {tolower(archive_checksum)}}.",
          "x" = "Downloaded {.val {found}}."
        ),
        class = "tidymedia_checksum_mismatch"
      )
    }
  }
  produced <- tm_unpack(tf, install_dir)
  if (is.null(produced$files)) {
    # The directories this call made come back before the message is built,
    # not after it: the caller reads the message once, and a line naming a
    # directory the exit handler is about to delete would be false by the time
    # they went to look. Where the unpack left something behind the directory
    # holds it, so the removal stops of its own accord -- the test below is
    # the belt to that braces (M103 AC7).
    if (!length(produced$leftovers)) tm_remove_created_dirs(created_dirs)
    left <- file.path(install_dir, produced$leftovers)
    cli::cli_abort(
      c(
        "Can't unpack the downloaded archive.",
        "i" = "Archive: {.file {tf}}.",
        if (length(left)) {
          c(
            "i" = "Install directory: {.file {install_dir}}.",
            "!" = "{cli::qty(length(left))}{?This entry/These entries} could not
                   be removed and {cli::qty(length(left))}{?is/are} still
                   there: {.file {left}}."
          )
        } else if (!dir.exists(install_dir)) {
          c("i" = "This call created the install directory and has removed it
                   again; nothing was left behind.")
        } else {
          c(
            "i" = "Install directory: {.file {install_dir}}.",
            "i" = "Nothing was left behind; the directory holds what it held
                   when this call started."
          )
        }
      ),
      class = "tidymedia_archive_unreadable"
    )
  }
  # Register what the extraction actually produced, and nothing else: a
  # remembered location pointing at a file the archive never contained is a
  # worse state than no remembered location at all. What THIS extraction
  # produced is read off its own file list, never off the install directory:
  # `install_dir` defaults to one stable path across installs and the
  # extraction does not clear it, so a directory listing would count a
  # previous run's binaries as this build's. The required programs are
  # checked before the first write, so a build missing one leaves every
  # existing remembered location as it was.
  unpacked <- tm_extracted_programs(produced$files, tm_install_registers)
  absent_required <- setdiff(tm_install_required, unpacked)
  if (length(absent_required)) {
    cli::cli_abort(
      c(
        "The archive did not produce {.and {.file {absent_required}}}.",
        "i" = "Looked for {.and {.file {paste0(\"bin/\", absent_required,
               \".exe\")}}} under {.file {install_dir}}.",
        "i" = "Nothing was registered; whatever the archive did unpack is
               still in that directory."
      ),
      class = "tidymedia_program_not_extracted"
    )
  }
  absent_optional <- setdiff(tm_install_registers, unpacked)
  if (length(absent_optional)) {
    cli::cli_inform(c(
      "i" = "The archive did not produce {.and {.file {absent_optional}}};
             no location was remembered for {cli::qty(length(absent_optional))}
             {?it/them}."
    ))
  }
  registered <- TRUE
  for (program in unpacked) {
    set_program(program, tm_install_binary(install_dir, program))
  }

  TRUE
}
