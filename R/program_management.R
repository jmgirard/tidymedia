
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
      # The recovery this machine actually has. `install_on_win()` is named
      # only where it runs and only for the programs it registers: on a Mac, or
      # for mediainfo, it would send the caller at a call that refuses them.
      # Both facts are read from the installer's own seam and its own list
      # (`tm_os()`, `tm_install_registers`) rather than restated here, so the
      # advice cannot drift from what the installer does (M115).
      bullets <- c(
        "Failed to find {program}.",
        "i" = "Check that it is installed, then use {.fn set_{program}} to \\
               point tidymedia at it."
      )
      if (identical(tm_os(), "windows") && program %in% tm_install_registers) {
        bullets <- c(bullets, "i" = "Or run {.fn install_on_win} to download \\
                                     FFmpeg and remember where it landed.")
      }
      cli::cli_warn(bullets)
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

# program_status() --------------------------------------------------------

#' Report which dependency programs tidymedia can find
#'
#' Looks up all four programs tidymedia knows about and returns one row for
#' each: where it resolved to, and what version it reported. Nothing is
#' installed, written, or changed by the call.
#'
#' A program that cannot be found gets `NA` in both columns rather than a
#' warning, so the answer for four programs arrives as one table instead of a
#' pile of messages. The lookup is [find_program()]'s: the `PATH` first, then a
#' location remembered by [set_program()], and finally a location a version of
#' tidymedia before 0.2.0 remembered under
#' `rappdirs::user_config_dir("tidymedia", "R")`.
#'
#' The version is whatever the binary reports for its own version flag, so it
#' is the FFmpeg build number for `ffmpeg`, `ffprobe` and `ffplay`, and the
#' MediaInfo library version for `mediainfo`. A program that resolves but
#' cannot be asked -- because the call fails, or because
#' `options(tidymedia.timeout = )` ended it -- has a location and an `NA`
#' version.
#'
#' @return A tibble with one row per program and three columns: `program`, the
#'   program's name; `location`, the resolved path or `NA`; and `version`, the
#'   version the binary reported or `NA`.
#' @seealso [find_program()] for one program at a time, [set_program()] to
#'   point tidymedia at a binary in a non-standard location, and
#'   [unset_program()] to forget one it remembered.
#' @family program management functions
#' @examplesIf nzchar(Sys.which("ffmpeg"))
#' # One row per program; NA where the program was not found
#' program_status()
#' @export
program_status <- function() {
  programs <- tm_programs()
  # find_program() warns once per program it cannot resolve, and once more for
  # a remembered location whose binary is gone. A report over four programs
  # answers with NA in a column instead; the caller asked which programs are
  # missing, so being told is not news (M113).
  locations <- lapply(programs, function(program) {
    suppressWarnings(find_program(program))
  })
  # The version probe's own timeout warning is NOT suppressed: it says the
  # limit ended the probe, which is a different fact from the program being
  # absent and is the one D048 made audible.
  versions <- tool_versions(programs, locations, call = rlang::current_env())
  tibble::tibble(
    program = programs,
    location = vapply(locations, tm_na_string, character(1)),
    # unname(): tool_versions() answers a list named by program, and a named
    # column prints its names back at the reader in every row label.
    version = unname(vapply(versions, tm_na_string, character(1)))
  )
}

# A resolved location or a captured version, as one string or NA. NULL is what
# find_program() returns when it finds nothing; "" is what Sys.which() returns.
tm_na_string <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    return(NA_character_)
  }
  as.character(x)
}

# unset_program() ---------------------------------------------------------

#' Forget the location of a dependency program
#'
#' Removes the location [set_program()] remembered for a program, so that
#' [find_program()] goes back to answering from the `PATH`. Both places a
#' location can live are cleared: the file under
#' `tools::R_user_dir("tidymedia", "config")` and, where one is still there, the
#' file a version of tidymedia before 0.2.0 wrote under
#' `rappdirs::user_config_dir("tidymedia", "R")`.
#'
#' Forgetting a location does not remove the program itself, and it does not
#' change what is on the `PATH`. A program tidymedia found on the `PATH` is
#' still found afterwards. A location remembered by a version before 0.2.0 is
#' cleared as well, so it is not left behind for [find_program()] to answer
#' with once the current file is gone.
#'
#' There is nothing to confirm: deleting the remembered location is the whole of
#' what the call does. Calling it for a program with nothing remembered warns
#' and returns `FALSE` rather than failing -- the state you asked for is already
#' the state you have.
#'
#' @param program A string naming which program to forget the location for:
#'   one of `"ffmpeg"`, `"ffprobe"`, `"ffplay"` or `"mediainfo"`. There is
#'   no default: the call deletes a file, and D079's rule for this package
#'   keeps a member of the set out of the default position, so a call that
#'   names no program refuses rather than picking one.
#' @return Invisibly, `TRUE` where a remembered location was removed and `FALSE`
#'   where there was none to remove.
#' @seealso [set_program()] to remember a location, and [program_status()] to
#'   see what tidymedia currently finds.
#' @family program management functions
#' @examples
#' \dontrun{
#' # Forget a location set_program() remembered, so that find_program() goes
#' # back to answering from the PATH
#' unset_program("mediainfo")
#' }
#' @export
unset_program <- function(program) {
  tm_unset_program(program, call = rlang::current_env())
}

# tm_unset_program(): unset_program()'s body, with `call` threaded, for the
# reason M112 records at tm_set_program() -- `call` names the environment a
# refusal is reported from and has no business in an exported signature or in
# the Rd usage line a reader copies from. It carries no default here either.
tm_unset_program <- function(program, call) {
  program <- rlang::arg_match(
    program,
    values = tm_programs(),
    error_arg = "program",
    error_call = call
  )

  # Both files, in the order find_program() reads them. Deleting only the
  # current one would leave a pre-0.2.0 location that find_program() then
  # starts answering with, which is the opposite of forgetting.
  files <- c(
    tm_config_file(program),
    tm_config_file(program, tm_legacy_config_dir())
  )
  present <- files[file.exists(files)]

  if (!length(present)) {
    cli::cli_warn(
      c(
        "No remembered location to forget for {program}.",
        "i" = "Use {.fn set_{program}} to remember one."
      ),
      class = "tidymedia_no_remembered_location",
      tm_program = program,
      call = call
    )
    return(invisible(FALSE))
  }

  # Through the tm_unlink() seam, and answered by a third look at the
  # filesystem rather than by the removal's own return value: `unlink()`
  # reports one status for the whole call and no names, so which files are
  # still there is a question only the filesystem can answer (M103's shape).
  tm_unlink(present)
  left <- present[file.exists(present)]
  if (length(left)) {
    cli::cli_abort(
      c(
        "Can't remove the remembered location for {program}.",
        "x" = "{.file {left}}",
        "i" = "Check that you can write to the directory holding it."
      ),
      class = "tidymedia_location_not_removed",
      tm_program = program,
      tm_files = left,
      call = call
    )
  }

  # What tidymedia remembers about an FFmpeg build was memoized against the
  # binary the forgotten location named, so it goes with it -- the same reason
  # set_program() drops it when it points at a different binary (M67/D044).
  forget_ffmpeg_capabilities()

  invisible(TRUE)
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
#' Because the call writes a file that outlives the session, it asks for
#' confirmation first and writes nothing until it has it. The prompt names the
#' location as you typed it -- which is what gets written -- and the full path
#' of the file that would record it. Declining leaves the config directory
#' exactly as it was. In a session with no one to ask, the call refuses rather
#' than assume consent; pass \code{confirm = FALSE} to write without being
#' asked, which is what an unattended script wants.
#'
#' @param program A string indicating which program to set the location for.
#' @param location A string containing the location of the program.
#' @param confirm Whether to ask before writing the remembered location.
#'   \code{TRUE} (the default) asks and, in a non-interactive session, refuses.
#'   \code{FALSE} writes without asking.
#' @return Invisibly, \code{TRUE} where the location was written and
#'   \code{FALSE} where the caller declined to write it.
#'
#' @seealso [find_program()] to locate a configured binary, and
#'   [install_on_win()] to download FFmpeg on Windows.
#' @family program management functions
#' @examples
#' \dontrun{
#' # Point tidymedia at a binary in a non-standard location; asks first
#' set_mediainfo("C:/Program Files/MediaInfo/mediainfo.exe")
#'
#' # In an unattended script, where there is no one to ask
#' set_mediainfo("C:/Program Files/MediaInfo/mediainfo.exe", confirm = FALSE)
#' }
#' @export
set_program <- function(program = c("ffmpeg", "ffprobe", "ffplay", "mediainfo"),
                        location, confirm = TRUE) {
  tm_set_program(program, location, confirm = confirm,
                 call = rlang::current_env())
}

# tm_set_program(): set_program()'s body, with `call` threaded.
#
# The split exists so `call` stops appearing in an exported signature and in
# the Rd usage line a reader copies from: it is the environment a refusal is
# reported from, which only tidymedia's own front doors have a value for
# (M112, superseding M110's decision to leave it a published formal).
#
# Every refusal below carries `call` -- the argument checkers included, not
# only the abort sites (M100). D074's siting has each export refuse its own
# arguments; this seam reaches the same outcome by naming the frame, because
# the not-found and consent refusals sit below the config-path resolution and
# the prompt build and so cannot be re-called at five front doors without
# duplicating both bodies (M110).
#
# `call` carries no default. Every caller is one of the five exports below and
# has its own frame to pass; a default would hand a site that forgot it this
# helper's frame, which is the blame the split exists to remove.
tm_set_program <- function(program = c("ffmpeg", "ffprobe", "ffplay", "mediainfo"),
                           location, confirm, call) {

  program <- rlang::arg_match(program, error_arg = "program", error_call = call)
  rlang::check_string(location, call = call)
  rlang::check_bool(confirm, call = call)
  if (Sys.which(location) == "") {
    cli::cli_abort("Can't find an executable at {.file {location}}.",
                   class = "tidymedia_program_not_found", call = call,
                   tm_program = program, tm_location = location)
  }
  
  # Find where to save user configuration data (tools::R_user_dir(), M097)
  config_dir <- tm_config_dir()
  config_file <- tm_config_file(program, config_dir)
  
  # Consent comes before anything is created or written, so a decline -- and a
  # refusal for want of anyone to ask -- leaves the config directory exactly as
  # it was found. The refusal names the same two items the prompt would have,
  # and the escape hatch by the argument's own name (M38/M40: the seam carries
  # no argument name of its own).
  if (confirm) {
    details <- tm_set_details(program, location, config_dir)
    approved <- tm_confirm(
      tm_set_prompt(program, location, config_dir),
      call = call,
      "i" = "Pass {.code confirm = FALSE} to set the location without being asked.",
      "i" = tm_cli_escape(details[[1]]),
      "i" = tm_cli_escape(details[[2]])
    )
    if (!approved) return(invisible(FALSE))
  }
  
  # Create configuration directory if needed
  if (!dir.exists(config_dir)) dir.create(config_dir, recursive = TRUE)
  
  # Save location to user configuration file
  writeLines(location, config_file)

  # Pointing at a different binary invalidates everything remembered about the
  # old one, so the session memo goes with it (M67/D044). Unconditional across
  # all four programs: cheap, and it cannot be forgotten when a second
  # capability memo is added later.
  forget_ffmpeg_capabilities()

  invisible(TRUE)
}

# set_mediainfo() ---------------------------------------------------------

#' @rdname set_program
#' @export
set_mediainfo <- function(location, confirm = TRUE) {
  tm_set_program("mediainfo", location, confirm = confirm,
                 call = rlang::current_env())
}

# set_ffmpeg() ------------------------------------------------------------

#' @rdname set_program
#' @export
set_ffmpeg <- function(location, confirm = TRUE) {
  tm_set_program("ffmpeg", location, confirm = confirm,
                 call = rlang::current_env())
}

#' @rdname set_program
#' @export
set_ffprobe <- function(location, confirm = TRUE) {
  tm_set_program("ffprobe", location, confirm = confirm,
                 call = rlang::current_env())
}

#' @rdname set_program
#' @export
set_ffplay <- function(location, confirm = TRUE) {
  tm_set_program("ffplay", location, confirm = confirm,
                 call = rlang::current_env())
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


# Platform ------------------------------------------------------------------

# Which operating system this session is running on, as a lowercase name.
#
# The seam `install_on_win()`'s platform gate reads, and the only place the
# package asks. `Sys.info()` is the source: its `sysname` is the kernel name
# uname reports -- `Windows`, `Darwin`, `Linux`, `FreeBSD`, `SunOS` -- which
# is what a message naming the caller's platform wants, and lowercasing it is
# the whole normalization. R documents `Sys.info()` as possibly unimplemented,
# in which case it returns `NULL`, so `.Platform$OS.type` is the fallback: it
# is always one of `windows` or `unix`, which is coarser than the gate would
# like but still answers the one question the gate asks. A host that cannot
# say more than `unix` is refused like any other non-Windows host and gets no
# package-manager route, only `set_program()` (M108's gate).
#
# Both sources are arguments so the fallback branch can be fired by a test: on
# every machine the suite runs on, `Sys.info()` is implemented, so a branch
# reading it directly would be unreachable code. No caller passes either one.
tm_os <- function(info = Sys.info(), os_type = .Platform$OS.type) {
  if (is.null(info)) {
    return(tolower(os_type))
  }
  tolower(info[["sysname"]])
}


# install_on_win() --------------------------------------------------------

# The programs an install registers, and where the extracted build puts each
# one. Named once so the prompt cannot promise a different set of writes from
# the one the call makes.
tm_install_registers <- c("ffmpeg", "ffprobe", "ffplay")

# Where a caller who is not on Windows gets FFmpeg instead. One line each,
# because that is the whole of the answer on those two platforms, and the
# package keeps no installer for either (GP1: each would need its own source,
# digest format and architecture matrix). A platform not named here -- FreeBSD,
# Solaris, or the coarse `unix` the seam falls back to -- is still refused, and
# is told only to point tidymedia at a build it already has: the package has no
# idea what that machine's package manager is, and advice it cannot stand
# behind is worse than none.
tm_install_routes <- c(
  darwin = "brew install ffmpeg",
  linux = "sudo apt-get install ffmpeg"
)

# The name a caller would recognize, for the uname words that are not one.
# `tm_os()` speaks uname's vocabulary, which is what the condition carries and
# what a bug report wants; a macOS caller told only "darwin" has to know that
# is their machine. Platforms whose uname word is already the familiar name --
# `linux`, `windows`, `freebsd` -- are absent and get no parenthetical.
tm_os_names <- c(
  darwin = "macOS",
  sunos = "Solaris"
)

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
#
# An entry `file.info()` cannot stat -- a broken symlink is the reachable
# case, measured 2026-09-02 -- comes back NA in every field. The NAs are kept
# rather than filled in here: this frame is a record of what was seen, and two
# NAs compare identical, so an unstattable entry the caller already had reads
# as unchanged, which is right. Deciding what an NA MEANS is the comparison's
# job, and tm_snapshot_added() does it (M103).
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
# A DIRECTORY qualifies only where it is new -- where nothing was at its path
# before, or what was there was not a directory -- and only the topmost new
# one of a chain: a pre-existing directory's mtime moves the instant an entry
# lands inside it -- measured 2026-09-02, 19:13:23.663 before and 19:13:24.848 after
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
  # An entry `file.info()` could not stat carries an NA `isdir`, and NA is the
  # one value the two subscripts below cannot take: `x[NA]` selects
  # NA_character_, which is neither a path the removal can delete nor a path
  # the report can name, so such an entry would be silently dropped by both.
  # It counts as a file, which is both what a broken symlink is on disk and
  # the safe reading -- a file is removed by name, where a directory would be
  # removed with everything under it (M103).
  isdir <- !is.na(after$isdir) & after$isdir
  # A directory is CREATED where it did not exist AS A DIRECTORY before, not
  # merely where its path is new. The two readings differ on one case and it
  # is a reachable one: a path the caller held as a file and this extraction
  # replaced with a directory is in `before`, so the path-only reading calls
  # it pre-existing, while its `isdir` keeps it out of the file bucket -- so
  # it lands in neither, and is neither removed nor named. The type is what
  # the removal turns on, so the type is what the comparison asks about
  # (M103).
  was_dir <- !is.na(before$isdir) & before$isdir
  prior_dir <- after$path %in% before$path[was_dir]
  # One `match()` over the whole column rather than one per row: the per-row
  # form rebuilt the hash table on every iteration, measured at 0.50 s on 6000
  # unchanged entries, and this frame is walked three times per removal sweep.
  j <- match(after$path, before$path)
  same <- function(x, y) {
    eq <- x == y
    ifelse(is.na(eq), is.na(x) & is.na(y), eq)
  }
  # A type change counts alongside size and mtime, so a directory that became
  # a file is a changed file even where the two happen to stat alike.
  changed <- is.na(j) |
    !same(after$size, before$size[j]) |
    !same(after$mtime, before$mtime[j]) |
    !same(after$isdir, before$isdir[j])
  created_dirs <- after$path[isdir & !prior_dir]
  list(
    files = after$path[changed & !isdir],
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
    kept_dirs <- added$dirs[added$dirs %in% still]
    for (path in added$files) {
      if (!(path %in% still)) next
      # A file under a created directory the recursive call could not remove
      # is left where it is. Two reasons, and either alone would be enough:
      # it is already covered by that directory's own name in the report, and
      # the path may not resolve where it looks like it does. `list.files()`
      # descends THROUGH a directory symlink, so a symlink the extraction
      # created reads as a created directory whose children are the link
      # target's -- outside this destination entirely. The recursive
      # `unlink()` on the link removes the LINK, so on a platform that can
      # delete it there is nothing left to walk; where that call fails, this
      # loop would otherwise delete the target's files, outside the directory
      # and unnamed (M103 review pass 3).
      if (any(startsWith(path, paste0(kept_dirs, "/")))) next
      tm_unlink(file.path(dir, path))
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
# stop at the first that is not empty. Returns the ones still standing, so a
# caller building a message can say what this call created and could not take
# back rather than guess from `dir.exists()` alone (M103 review pass 3).
#
# Emptiness is the guard rather than a record of what was written: a directory
# the call created but something else has since put a file in is not the
# call's to delete, and stopping rather than skipping is what keeps the
# removal from reaching around a kept directory to its parent.
tm_remove_created_dirs <- function(dirs) {
  remaining <- dirs
  for (dir in rev(dirs)) {
    if (dir.exists(dir)) {
      if (length(list.files(dir, all.files = TRUE, no.. = TRUE))) break
      if (!identical(tm_unlink(dir, recursive = TRUE), 0L)) break
    }
    remaining <- setdiff(remaining, dir)
  }
  invisible(remaining)
}

# Unpack `archive` into `dir`. A three-slot list: `files`, the paths the
# extraction produced relative to `dir`, or NULL where libarchive refused;
# `leftovers`, the entries a refused extraction wrote and the cleanup could
# not remove; and `removed_yours`, the entries the caller already had that
# the cleanup removed because the failed extraction had written over them.
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
  if (is.null(con)) {
    return(list(files = NULL, leftovers = character(0), removed_yours = character(0)))
  }
  on.exit(tm_close(con), add = TRUE)
  before <- tm_dir_snapshot(dir)
  produced <- tryCatch(
    as.character(archive::archive_extract(con, dir = dir, strip_components = 1)),
    error = function(cnd) NULL
  )
  if (!is.null(produced)) {
    return(list(
      files = produced, leftovers = character(0), removed_yours = character(0)
    ))
  }
  # The connection is closed BEFORE the removal rather than left to the exit
  # handler: it is the archive's, not the destination's, but the same rule
  # that made it worth owning -- Windows will not delete a file something
  # still holds open -- says to hold no handle the cleanup does not need. The
  # handler stays, so on this path the connection has two closers: the second
  # one is a no-op, because `tm_close()` guards on `isOpen()` inside a
  # `tryCatch()`. Two closers rather than one is the price of closing early
  # without also having to prove no path below reaches the handler; M102 AC3's
  # "leaves no connection open" is what both of them serve.
  tm_close(con)
  after <- tm_dir_snapshot(dir)
  leftovers <- tm_remove_added(dir, before, after)
  # Which of the caller's OWN entries the cleanup removed. D082 removes a
  # pre-existing file the failed extraction wrote over -- what it holds
  # afterwards is nothing the caller put there -- and that is the one
  # deletion a refusal cannot describe as leaving the directory as it found
  # it. Read off the destination rather than predicted: an entry that was
  # there before and is not there now is one this call took (M103 review
  # pass 3).
  yours <- intersect(tm_snapshot_added(before, after)$files, before$path)
  list(
    files = NULL,
    leftovers = leftovers,
    removed_yours = setdiff(yours, tm_dir_snapshot(dir)$path)
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

# Which of the paths `tm_unpack()` reported are under `dir` now. The file list
# is what libarchive says it wrote, and it is not the same question as what is
# there: a build the antivirus quarantines between the extraction and the
# check is listed and gone. Everything below the extraction asks THIS set
# rather than the list, so one meaning of "produced" holds across the whole
# path and no refusal describes a directory it has not looked at (M105).
# Separators are normalized the way `tm_extracted_programs()` normalizes them.
# Case is NOT: that helper folds case to match a reported path against a
# program name, which is a comparison between two strings, while this one asks
# the filesystem, which answers for the target's own rules -- case-insensitive
# on Windows, the only platform this runs on (M105 review F12).
tm_files_on_disk <- function(files, dir) {
  rel <- sub("^[.]/", "", gsub("\\\\", "/", as.character(files)))
  rel[file.exists(file.path(dir, rel))]
}

# Close `con` where it is still open, and do nothing where the callee already
# closed it: `isOpen()` itself errors on a connection that has been destroyed,
# so the test and the close share one handler.
tm_close <- function(con) {
  invisible(tryCatch(if (isOpen(con)) close(con), error = function(cnd) NULL))
}

# `path.expand()` here rather than at either reader. Two callers ask about
# this path and they do not agree about `~` on their own: `file.info()` expands
# it and `Sys.which()` does not, so `install_on_win(install_dir = "~/ffmpeg")`
# gave a registration check that refused a good build and blamed the archive
# for it (M104 review F1). Expanding once, where the path is built, is what
# keeps the check and the `set_program()` call that follows it asking about the
# same file -- expanding inside the check alone would only move the failure
# into the loop, which is the partial registration M104 exists to stop.
tm_install_binary <- function(install_dir, program) {
  path.expand(file.path(install_dir, "bin", paste0(program, ".exe")))
}

# Whether a path the extraction produced can be handed to set_program(): it
# resolves the way set_program() itself will ask (`Sys.which()`, at the abort
# this check exists to make unreachable from here), it is a file rather than a
# directory, and it holds bytes.
#
# The two tests beyond parity are what parity cannot promise on the one
# platform this install runs on. Windows has no executable bit, so a truncated
# `ffmpeg.exe` of zero length resolves there and would be remembered as a
# working program. Whether `Sys.which()` answers for a DIRECTORY named
# `ffmpeg.exe` is a platform behaviour measured here only on macOS (2026-09-03,
# where it does not), so the directory is refused by a test of its own rather
# than by inference from that measurement.
#
# What it deliberately does not do is RUN the program: that would be the first
# probe in this seam to execute a downloaded binary, and it would turn a slow
# or blocked spawn into an install failure (M104).
#
# It answers one logical per path. Both readers are already vectorized, so the
# elementwise `&` is what the caller wanted anyway -- the whole produced set is
# asked in one call rather than through a `vapply()` -- and it is what retires
# the `!is.na(info$size)` clause the scalar `&&` needed: `size` is NA exactly
# where `isdir` is, and `FALSE & NA` is FALSE, so the first clause already
# decides an absent path. The answer is unnamed: `Sys.which()` names its
# result and `file.info()` names its rows, and neither name belongs to this
# question (M105).
tm_usable_binary <- function(path) {
  info <- file.info(path, extra_cols = FALSE)
  !is.na(info$isdir) & !info$isdir & info$size > 0 &
    unname(Sys.which(path)) != ""
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

# What a set_program() call would do, one item per line: the location as the
# caller typed it, and the file that would record it. The typed string is what
# gets written, so it is what the prompt names -- never a path `Sys.which()`
# resolved it to, which is not what lands in the file.
#
# Every value goes through a cli field, which does not recurse into the value,
# so a location containing braces is shown rather than evaluated (M44); the
# result is stripped of styling and hyperlinks so what reaches `menu()` is the
# plain text a test can read back. Same shape as tm_install_details() above,
# for the same reasons.
#
# Both the prompt and the non-interactive refusal are built from this, so a
# caller who is told to pass `confirm = FALSE` has been shown the same items
# the prompt would have named.
tm_set_details <- function(program, location, dir = tm_config_dir()) {
  line <- function(...) cli::ansi_strip(cli::format_inline(..., .envir = parent.frame()))
  c(
    line("Remember this location: {.file {location}}"),
    line("By writing: {.file {tm_config_file(program, dir)}}")
  )
}

# The consent prompt: the question, then the items, one per line.
tm_set_prompt <- function(program, location, dir = tm_config_dir()) {
  paste(
    c(
      paste0("tidymedia is about to remember where ", program, " is. Proceed?"),
      paste0("* ", tm_set_details(program, location, dir))
    ),
    collapse = "\n"
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
#' This call installs on Windows only. On any other platform it refuses before
#' it says, asks, writes, or downloads anything, and the error names the
#' platform it found. Elsewhere FFmpeg comes from the system's own package
#' manager -- `brew install ffmpeg` on macOS, `sudo apt-get install ffmpeg` on
#' Linux -- and [set_program()] points tidymedia at a build that is already
#' installed, on every platform.
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
#' Every program the extraction produced is checked before any location is
#' remembered: the path has to resolve the way an executable does, and what is
#' there has to be a file rather than a directory, and not be empty. The
#' program itself is never run, so a build that unpacks and then cannot
#' execute -- the wrong architecture, say -- passes this check. Where a
#' required program fails it, nothing at all is registered and the error names
#' each failed program and its full path; where an optional one fails it, the
#' install completes and says which program it skipped.
#'
#' A refusal leaves the install directory as the call found it. Files a failed
#' extraction wrote are removed, a directory the call created is removed
#' again, and anything already in the directory is left alone -- with one
#' deliberate exception: a file of yours the failed extraction wrote over is
#' removed with the rest of the debris, because what it holds after a failed
#' extraction is nothing you put there. The error names that file by full
#' path, so a refusal never reports a directory as untouched when it took
#' something of yours out of it.
#'
#' Removal is best-effort, and on Windows it does not always succeed. Where an
#' extraction fails part-way, the library that was writing the file is still
#' holding it open, and Windows will not delete a file something holds. Those
#' entries are named in the error by full path, so a refusal on Windows can
#' leave files behind -- the error tells you which. A directory this call
#' created and could not remove again is named the same way.
#'
#' Two refusals sit outside that rule, both of them below a successful
#' extraction: `tidymedia_program_not_extracted`, where a required program is
#' not at the path it would be installed to, and `tidymedia_program_unusable`,
#' where a file is at that path and cannot be used. Each says so, and the
#' unpacked files stay where they are. What the extraction produced is read
#' from the archive's own file list and from the install directory together,
#' so a path the archive listed and did not leave behind -- an unpacked
#' program an antivirus quarantined, say -- is refused as a program that is
#' not there rather than as one that cannot be used, and the error says the
#' extraction reported writing it. It is the unpacked files that put these
#' refusals outside the rule, so where none of them are there the rule applies
#' to `tidymedia_program_not_extracted` like any other: a directory this call
#' created is removed again, and the error says so instead.
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
#'   the install directory. Eight other outcomes abort with a condition of
#'   their own rather than returning: a call made on a platform this function
#'   does not install for (`tidymedia_wrong_platform`), a confirmation that
#'   could not be asked for because the session has no one to ask
#'   (`tidymedia_confirmation_unavailable`), a download that did not deliver
#'   (`tidymedia_download_unavailable`), a published digest that could not be
#'   fetched or read (`tidymedia_checksum_unavailable`), a digest that did not
#'   match the downloaded archive (`tidymedia_checksum_mismatch`), an archive
#'   that could not be unpacked (`tidymedia_archive_unreadable`), a
#'   required program that is not at the path it would be installed to
#'   (`tidymedia_program_not_extracted`), and a required program the archive
#'   produced in a form that cannot be used (`tidymedia_program_unusable`).
#'   Every one of these aims to leave the install directory as the call found
#'   it, except the last two, which leave the files the archive did unpack --
#'   and `tidymedia_program_not_extracted` is back inside the rule where none
#'   of the archive's files are there. Removal is best-effort: on Windows a
#'   partly-written file cannot be deleted while the extraction library still
#'   holds it, and the error names what it could not remove. See Details.
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

  # The one platform this function installs for. The gate is an allow-list --
  # anything that is not `windows` is refused -- so a host the routing table
  # has never heard of is refused rather than sent to download a Windows build
  # it cannot run. It sits BELOW the four argument checks, because an argument
  # mistake is worth reporting on any machine and a cheap value refusal comes
  # first (D036, D043), and ABOVE every cost: nothing has been said to the
  # caller, asked of them, written, or fetched by the time it fires.
  platform <- tm_os()
  if (!identical(platform, "windows")) {
    # Single-bracket, so a platform the table does not name gives NA rather
    # than a subscript error.
    route <- unname(tm_install_routes[platform])
    known <- unname(tm_os_names[platform])
    # The two bullets below are written as a pair per platform rather than as
    # one line plus an optional one: with no route to name, "Then point
    # tidymedia at it" would promise a step that is not there and refer to
    # nothing, which is what the platforms this allow-list exists to serve --
    # FreeBSD, Solaris, the coarse `unix` fallback -- would read.
    advice <- if (is.na(route)) {
      c("i" = "Point tidymedia at an FFmpeg build you already have with
               {.fun set_program}.")
    } else {
      c(
        "i" = "Install FFmpeg with {.code {route}}.",
        "i" = "Then point tidymedia at it with {.fun set_program}, or at a
               build you already have."
      )
    }
    cli::cli_abort(
      c(
        "{.fun install_on_win} installs FFmpeg on Windows only.",
        "x" = if (is.na(known)) {
          "This session is running on {platform}."
        } else {
          "This session is running on {platform} ({known})."
        },
        advice
      ),
      class = "tidymedia_wrong_platform",
      tm_platform = platform
    )
  }

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
  # call found it (M103 AC3). It is disarmed the moment the extraction
  # produces FILES, which is one step earlier than "once a program has been
  # registered": everything below an extraction that wrote something is
  # outside the rule, because those files are in that directory and
  # `tidymedia_program_not_extracted` tells the caller so. Disarming at
  # registration instead let that abort delete the directory its own message
  # pointed at (M103 review pass 1); disarming on a merely SUCCESSFUL
  # extraction let it keep an empty directory this call created, under the
  # same message, wherever the archive produced nothing -- single-segment
  # entries, every one stripped by `strip_components = 1` (M103 review pass
  # 2). The carve-out is written over the files a successful extraction
  # leaves, so where it leaves none it does not reach. It is also a no-op
  # wherever the directory holds anything: a directory that already existed is
  # never in `created_dirs` at all.
  created_dirs <- tm_missing_ancestors(install_dir)
  unpacked_here <- FALSE
  on.exit(if (!unpacked_here) tm_remove_created_dirs(created_dirs), add = TRUE)
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
    kept_created <- created_dirs
    if (!length(produced$leftovers)) {
      kept_created <- tm_remove_created_dirs(created_dirs)
    }
    # `cli_vec()` with the truncation off, because AC7 promises every entry by
    # name: cli abbreviates a vector in a message at 20 by default -- measured
    # 2026-09-02 on 25 paths, entries 19 through 23 came back as an ellipsis --
    # and a caller told about 20 of 25 files still on their disk would have to
    # go hunting for the rest. A long list is the lesser cost, and it is a
    # list nothing but a failed install can produce (M103 AC7).
    left <- cli::cli_vec(
      file.path(install_dir, produced$leftovers), list("vec-trunc" = Inf)
    )
    # The caller's own entries the cleanup removed, and the directories this
    # call created and could not take back: the two states the plain "holds
    # what it held" sentence used to describe wrongly (M103 review pass 3).
    yours <- cli::cli_vec(
      file.path(install_dir, produced$removed_yours), list("vec-trunc" = Inf)
    )
    kept <- cli::cli_vec(kept_created, list("vec-trunc" = Inf))
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
        } else if (length(kept)) {
          c(
            "i" = "Install directory: {.file {install_dir}}.",
            "!" = "Nothing the extraction wrote is still there, but this call
                   created {cli::qty(length(kept))}{?this directory/these
                   directories} and could not remove {cli::qty(length(kept))}
                   {?it/them} again: {.file {kept}}."
          )
        } else if (length(yours)) {
          c(
            "i" = "Install directory: {.file {install_dir}}.",
            "!" = "Nothing the extraction wrote is still there, but the failed
                   extraction had written over {cli::qty(length(yours))}
                   {?this file/these files} of yours, which {cli::qty(length(yours))}
                   {?was/were} removed with it: {.file {yours}}."
          )
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
  # From here the extraction has succeeded and its files are in the install
  # directory, so nothing below may take that directory back (M103 AC4). What
  # counts as "its files" is the reported list intersected with the disk, and
  # every question below asks that one set: a list entry holding nothing is a
  # file no caller can be sent to, and a directory holding none of them is one
  # this call may still take back (M105).
  on_disk <- tm_files_on_disk(produced$files, install_dir)
  unpacked_here <- length(on_disk) > 0L
  # Register what the extraction actually produced, and nothing else: a
  # remembered location pointing at a file the archive never contained is a
  # worse state than no remembered location at all. Which paths are candidates
  # is still read off this extraction's own file list, never off a listing of
  # the install directory: `install_dir` defaults to one stable path across
  # installs and the extraction does not clear it, so a directory listing
  # would count a previous run's binaries as this build's. Each of those paths
  # is then asked whether it is there. That narrows the candidates to this
  # build's own list; it does not prove the file AT such a path was written by
  # this extraction rather than left by an earlier one, which `file.exists()`
  # cannot tell and nothing below asks it to (M105 review F6). The required
  # programs are checked before the first write, so
  # a build missing one leaves every existing remembered location as it was.
  unpacked <- tm_extracted_programs(on_disk, tm_install_registers)
  absent_required <- setdiff(tm_install_required, unpacked)
  if (length(absent_required)) {
    # The required programs the extraction REPORTED and did not leave behind,
    # told apart from the ones it never claimed: they are the same refusal --
    # neither is a program this install can register -- but only one of them
    # has a cause the caller can act on (M105).
    vanished <- intersect(
      absent_required, tm_extracted_programs(produced$files, tm_install_required)
    )
    # The directories this call made come back before the message is built,
    # for the same reason the unreadable-archive refusal does it there: the
    # caller reads the message once, and it has to describe the state they
    # will find. `tm_remove_created_dirs()` stops at the first directory that
    # is not empty, so an extraction that wrote files keeps its directory of
    # its own accord and the guard below is what decides the wording.
    kept_created <- created_dirs
    if (!length(on_disk)) {
      kept_created <- tm_remove_created_dirs(created_dirs)
    }
    kept <- cli::cli_vec(kept_created, list("vec-trunc" = Inf))
    # Where every missing program is one the extraction REPORTED, "did not
    # produce" would contradict the line below it, which says the extraction
    # reported writing exactly those paths. The archive listed them; what it
    # did not do is leave them behind (M105 review F2).
    headline <- if (length(vanished) == length(absent_required)) {
      "The archive did not leave behind {.and {.file {absent_required}}}."
    } else {
      "The archive did not produce {.and {.file {absent_required}}}."
    }
    # An extraction that reported no files at all and one that reported files
    # and left none of them are the same disposition and not the same event,
    # and only the second has a report to be absent from. Saying "none of the
    # files the extraction reported are there" of an empty report implies a
    # report that never happened, which is the wording M103 had exact before
    # this milestone re-pointed the guard (M105 review F3).
    nothing_there <- if (length(produced$files)) {
      "None of the files the extraction reported are there."
    } else {
      "The archive produced no files at all."
    }
    cli::cli_abort(
      c(
        headline,
        "i" = "Looked for {.and {.file {tm_install_binary(install_dir,
               absent_required)}}}.",
        if (length(vanished)) {
          c("!" = "The extraction reported writing {.and {.file
                   {tm_install_binary(install_dir, vanished)}}}, but
                   {cli::qty(length(vanished))}{?it is/they are} not there.
                   Antivirus quarantine after extraction is the usual cause.")
        },
        if (length(on_disk)) {
          c("i" = "Nothing was registered; the files the extraction did
                   produce are in {.file {install_dir}}.")
        } else if (!dir.exists(install_dir)) {
          c("i" = "{nothing_there} Nothing was registered, and this call has
                   removed the install directory it created.")
        } else if (length(kept)) {
          c("i" = "{nothing_there} Nothing was registered, and this call
                   created {cli::qty(length(kept))}{?this directory/these
                   directories} and could not remove {cli::qty(length(kept))}
                   {?it/them} again: {.file {kept}}.")
        } else {
          c("i" = "{nothing_there} Nothing was registered, and the install
                   directory holds what it held when this call started.")
        }
      ),
      class = "tidymedia_program_not_extracted"
    )
  }
  # Every path the extraction produced is asked whether it can be used BEFORE
  # the first set_program() call, so a build carrying a program the caller
  # cannot run leaves every existing remembered location as it was rather than
  # registering the ones that happened to come first. The whole set is
  # partitioned once here; the two branches below dispose of the required and
  # the optional halves (M104 AC1).
  unusable <- unpacked[!tm_usable_binary(tm_install_binary(install_dir, unpacked))]
  unusable_required <- intersect(tm_install_required, unusable)
  if (length(unusable_required)) {
    # Nothing here touches the install directory. This sits below a successful
    # extraction, so the archive's files are in that directory and D082's rule
    # stops at exactly that boundary: a caller told the build cannot be used
    # is left the build to look at.
    cli::cli_abort(
      c(
        "The archive produced {.and {.file {unusable_required}}}, but
         {cli::qty(length(unusable_required))}{?it/they} cannot be used.",
        "i" = "Checked {.and {.file {tm_install_binary(install_dir,
               unusable_required)}}}.",
        "i" = "A produced program is registered only where its path resolves
               to a file that is not empty; the file itself is never run.",
        "i" = "Nothing was registered; whatever the archive unpacked is still
               in {.file {install_dir}}."
      ),
      class = "tidymedia_program_unusable"
    )
  }
  absent_optional <- setdiff(tm_install_registers, unpacked)
  unusable_optional <- setdiff(unusable, tm_install_required)
  # One message where both kinds occur in a call, and a distinct sentence for
  # each: the archive-did-not-produce wording is false of a program the
  # archive DID produce, and a caller sent looking for a file that is on their
  # disk is a worse state than no message at all (M104 AC3).
  if (length(absent_optional) || length(unusable_optional)) {
    cli::cli_inform(c(
      if (length(absent_optional)) {
        c("i" = "The archive did not produce {.and {.file {absent_optional}}};
                 no location was remembered for
                 {cli::qty(length(absent_optional))}{?it/them}.")
      },
      if (length(unusable_optional)) {
        c("i" = "The archive produced {.and {.file {unusable_optional}}}, but
                 what it produced could not be used, so no location was
                 remembered for {cli::qty(length(unusable_optional))}
                 {?it/them}: {.file {tm_install_binary(install_dir,
                 unusable_optional)}}.")
      }
    ))
  }
  # `confirm = FALSE`: the install's own prompt above already named every one
  # of these overwrites by full path, so asking again here would ask a second
  # time for consent already given -- once per program, at that.
  for (program in setdiff(unpacked, unusable)) {
    set_program(program, tm_install_binary(install_dir, program), confirm = FALSE)
  }

  TRUE
}
