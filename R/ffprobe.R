
# ffprobe() ---------------------------------------------------------------

#' Send a command to the FFprobe program
#' 
#' Probe a media file for information.
#' 
#' @param command A string containing the command to send to FFprobe.
#' @return A string containing the text output by FFprobe.
#' @export
ffprobe <- function(command) {
  assert_that(rlang::is_character(command, n = 1))
  out <- system(glue('"{find_ffprobe()}" {command}'), intern = TRUE)
  out
}


# probe_all() -------------------------------------------------------------

#' Look up all information about a media file using FFprobe
#'
#' Create a list object containing information about a media file's container
#' and streams.
#'
#' @param infile A string indicating the location (file path or web link) of a
#'   media file to probe.
#' @param convert A logical indicating whether to convert purely numerical
#'   values to integers or doubles.
#' @return A list object containing two inner objects: `container` which is a
#'   data frame describing the media file's container and `streams` which is a
#'   data frame describing the media file's video and audio streams.
#' @export
probe_all <- function(infile, convert = FALSE) {
  args <- dplyr::if_else(convert, true = "", false = "-pretty")
  # Probe container
  command <- glue('-i "{infile}" -v quiet -show_format -of default=nw=1 {args}') 
  container <- ffprobe(command) |> format_probe()
  if (convert == TRUE) {
    container <- 
      container |> 
      dplyr::mutate(
        across(c(nb_streams, nb_programs), as.integer),
        across(c(start_time, duration, size, bit_rate), as.double)
      )
  }
  # Probe streams
  streams <- vector(mode = "list", length = container$nb_streams)
  for (i in 1:container$nb_streams) {
    command <- glue('-i "{infile}" -v quiet -show_streams -select_streams {i-1}',
                    ' -of default=nw=1 {args}')
    streams[[i]] <- ffprobe(command) |> format_probe()
  }
  streams <- 
    dplyr::bind_rows(streams) |> 
    dplyr::mutate(filename = container$filename, .before = 1)
  if (convert == TRUE) {
    streams <- 
      streams |> 
      dplyr::mutate(
        dplyr::across(dplyr::everything(), dplyr::na_if, y = "N/A"),
        dplyr::across(
          c(index, width, height, coded_width, coded_height, duration_ts,
            bits_per_raw_sample, nb_frames, nb_read_frames, nb_read_packets, 
            sample_rate, channels, bits_per_sample), 
          as.integer
        ),
        across(c(start_time, duration, bit_rate, max_bit_rate), as.double)
      )
  }
  # Combine into list and return
  list(container = container, streams = streams)
}

# probe_container() -------------------------------------------------------

#' Shortcut functions for probing specific information
#'
#' Return just the data frame describing the media file's container via
#' `probe_container()`. Return just the data frame describing the media file's
#' streams via `probe_streams()`. Return just the rows of the data frame
#' describing the media file's video streams via `probe_video()`. Return just
#' the rows of the data frame describing the media file's audio streams via
#' `probe_audio()`. Each of these functions must be given either the output of
#' `probe_all()` or the location of a media file; note that the former approach
#' can save time when working with larger files.
#'
#' @param probe A list object created by `probe_all()`. Must be `NULL` if
#'   `infile` is not `NULL`.
#' @param infile A string indicating the location (file path or web link) to a
#'   media file. Must be `NULL` if `probe` is `NULL`.
#' @return A data frame containing only the requested information.
#' @export
probe_container <- function(probe = NULL, infile = NULL) {
  assert_that(is.null(probe) + is.null(infile) == 1)
  if (!is.null(infile)) df <- probe_all(infile)
  probe$container
}

# probe_streams() ---------------------------------------------------------

#' @rdname probe_container
#' @export
probe_streams <- function(probe = NULL, infile = NULL) {
  assert_that(is.null(probe) + is.null(infile) == 1)
  if (!is.null(infile)) df <- probe_all(infile)
  probe$streams
}

# probe_video() -----------------------------------------------------------

#' @rdname probe_container
#' @export
probe_video <- function(probe = NULL, infile = NULL) {
  assert_that(is.null(probe) + is.null(infile) == 1)
  if (!is.null(infile)) df <- probe_all(infile)
  probe$streams |> filter(codec_type == "video")
}

# probe_audio() -----------------------------------------------------------

#' @rdname probe_container
#' @export
probe_audio <- function(probe = NULL, infile = NULL) {
  assert_that(is.null(probe) + is.null(infile) == 1)
  if (!is.null(infile)) df <- probe_all(infile)
  probe$streams |> filter(codec_type == "audio")
}

# format_probe() ----------------------------------------------------------

# Turn the text output from FFprobe into a named dataframe
format_probe <- function(x) {
  tibble(x) |> 
    separate(x, into = c("key", "value"), sep = "=") |> 
    pivot_wider(names_from = "key", values_from = "value")
}


# convert_fractions() -----------------------------------------------------

#' Convert string fractions to doubles
#'
#' This is useful for columns such as frame rates, which FFprobe often lists as
#' fractions such as "30000 / 1001" (this would convert to 29.97003). 
#' 
#' @param x A character vector containing fractions to evaluate.
#' @return A numeric vector with each fraction evaluated to a double.
#' @export
convert_fractions <- function(x) {
  purrr::map_dbl(x, ~eval(parse(text = .x)))
}
