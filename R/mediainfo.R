
# find_mediainfo() --------------------------------------------------------

# Look for the MediaInfo CLI program in path and then in environmental variables
# If not found, prompt the user to install and save an environmental variable
find_mediainfo <- function() {
  mediainfo <- Sys.which("mediainfo")
  if (mediainfo == "") {
    mediainfo <- Sys.getenv("TIDYMEDIA_MEDIAINFO")
    assert_that(
      mediainfo != "", 
      msg = paste0(
        "MediaInfo CLI not found. On Linux, check that it is installed. ",
        "On Windows, check that it is installed and then use set_mediainfo()."
      )
    )
  }
  mediainfo
}

# set_mediainfo() ---------------------------------------------------------

#' Set the MediaInfo location
#'
#' Save the location of the MediaInfo CLI to an environmental variable, which
#' will persist across sessions. This is currently necessary on Windows (and
#' Mac?) but not on Linux platforms.
#'
#' @param path *Required.* A string indicating the location of the MediaInfo CLI
#'   program.
#' @export
set_mediainfo <- function(path) {
  assert_that(rlang::is_character(path, n = 1))
  assert_that(
    Sys.which(path) != "", 
    msg = "Could not find path try again"
  )
  Sys.setenv(TIDYMEDIA_MEDIAINFO = path)
}

# info_query() ------------------------------------------------------------

#' Query information from MediaInfo
#'
#' Create a new row tibble that contains information about a media file. The
#' information to be extracted can be either provided as a \code{section} and
#' multiple \code{paramaters} or as a \code{template} file.
#'
#' @param filename *Required.* A string indicating the file path of a media
#'   file.
#' @param section *Optional.* Either a string indicating the MediaInfo section
#'   from which to query the \code{parameters} or \code{NULL} to use a
#'   \code{template} file instead (default = \code{NULL}). Note that querying
#'   from multiple sections at once requires using a \code{template} file.
#' @param parameters *Optional.* Either a vector of one or more strings
#'   indicating the MediaInfo parameters to query from \code{section} or
#'   \code{NULL} to use a \code{template} file instead (default = \code{NULL}).
#' @param template *Optional.* Either a string indicating the file path of a
#'   MediaInfo template file or \code{NULL} to query by section and parameters
#'   instead (default = \code{NULL}). Note that parameters in the template file
#'   should be separated by \code{ & }.
#' @param names *Optional.* A vector of one or more strings indicating the names
#'   of the variables in the returned tibble; must be the same length as
#'   \code{parameters} or the number of parameters requested in the
#'   \code{template} file (default = the names of the strings in
#'   \code{parameters}).
#' @param mediainfo *Optional.* A string indicating the location of the MediaInfo CLI
#'   program (default = will use \code{find_mediainfo()}).
#' @param ... Currently ignored.
#' @return A row tibble containing each parameter as a separate variable.
#' @export
info_query <- function(filename, 
                       section = NULL, 
                       parameters = NULL,
                       template = NULL,
                       names = parameters,
                       mediainfo = find_mediainfo(),
                       ...) {
  
  # Validate arguments
  assert_that(rlang::is_character(filename, n = 1))
  assert_that(rlang::is_character(section, n = 1) || rlang::is_null(section))
  assert_that(rlang::is_character(parameters) || rlang::is_null(parameters))
  assert_that(length(parameters) >= 1 || rlang::is_null(parameters))
  assert_that(rlang::is_character(template, n = 1) || rlang::is_null(template))
  assert_that(rlang::is_character(mediainfo, n = 1))
  assert_that(Sys.which(mediainfo) != "")
  
  # Query information from mediainfo into a string
  if (rlang::is_null(template)) {
    output_str <- 
      system(
        glue(
          '"{mediainfo}" "--Inform={section};',
          '{paste0(sprintf("%%%s%%", parameters), collapse = " & ")}"',
          ' "{filename}"'
        ),
        intern = TRUE
      ) 
  } else {
    output_str <- 
      system(
        glue('"{mediainfo}" "--Inform=file://{template}" "{filename}"'),
        intern = TRUE
      )
  }
  
  # Tidy the returned string into a tibble
  df <- 
    tidyr::separate(
      tibble::tibble(value = output_str),
      col = value, 
      into = names, 
      sep = " & ",
      convert = TRUE,
      remove = TRUE
    )
  
  #TODO: Update below once where() is exported by tidyselect
  where <- utils::globalVariables("where")
  
  # Replace empty strings with NA
  df <- dplyr::mutate(
    df,
    dplyr::across(
      .cols = where(rlang::is_character), 
      .fns = ~dplyr::na_if(., "")
    )
  )
  
  df
}

# info_summary() ----------------------------------------------------------

#' MediaInfo Summary
#'
#' Query common summary information about a media file from MediaInfo and store
#' it in a row tibble. This is a convenience wrapper to call \code{info_query}
#' with several built-in templates.
#'
#' @param filename *Required.* A string containing the path to a media file.
#' @param style *Required.* Either \code{"full"} or \code{"brief"}, which
#'   determines which built-in template to use, i.e., which parameters to query
#'   and how to name them (default = "full"). The "full" style includes a long
#'   list of parameters from the General, Video, and Audio sections; variables
#'   are named as {Section}_{Parameter} using the parameter names from MediaInfo
#'   (with the exception that symbol characters are removed). The "brief" style
#'   includes a handful of common parameters with shorter names.
#' @param mediainfo *Optional.* A string pointing to the MediaInfo CLI program.
#'   By default, it will be looked up using \code{find_mediainfo()}.
#' @param ... Other arguments to be passed on to \code{info_query()}
#' @return A row tibble containing many variables summarizing the
#' @export
info_summary <- function(filename, 
                         style = c("full", "brief"),
                         mediainfo = find_mediainfo(), 
                         ...) {
  
  style <- match.arg(style)
  
  if (style == "full") {
    names <- c(
      "General_CompleteName", "General_Format", "General_FileSizeString", 
      "General_FileSize", "General_DurationString", "General_Duration", 
      "Video_Format", "Video_FormatVersion", "Video_FormatProfile", 
      "Video_CodecID", "Video_DurationString", "Video_Duration", 
      "Video_BitRateMode", "Video_BitRateString", "Video_BitRate",
      "Video_Width", "Video_Height", "Video_DisplayAspectRatioString",
      "Video_DisplayAspectRatio", "Video_FrameRateMode", 
      "Video_FrameRateString", "Video_FrameRate", "Video_FrameCount", 
      "Video_Standard", "Video_ScanType", "Video_StreamSizeString", 
      "Video_StreamSize", "Audio_Format", "Audio_FormatVersion", 
      "Audio_FormatProfile", "Audio_CodecID", "Audio_DurationString", 
      "Audio_Duration", "Audio_BitRateMode", "Audio_BitRateString", 
      "Audio_BitRate", "Audio_Channels", "Audio_ChannelPositions", 
      "Audio_SamplingRateString", "Audio_SamplingRate", 
      "Audio_StreamSizeString", "Audio_SteamSize"
    )
  } else if (style == "brief") {
    names <- c(
      "Path", "Format", "FileSize", "Duration",
      "Width", "Height", "FrameRate", "VideoBitRate",
      "Channels", "SamplingRate", "AudioBitRate"
    )
  }
  
  info_query(
    filename = filename,
    template = system.file(
      glue("mediainfo_template_{style}.txt"), 
      package = "tidymedia"
    ),
    names = names,
    mediainfo = mediainfo,
    ...    
  )
}
