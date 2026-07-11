# Read a Batch Provenance Manifest

Retrieve the reproducibility manifest recorded by
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
when it was run with `manifest = TRUE`. The manifest is a tibble with
one row per job capturing the compiled command, the FFmpeg and FFprobe
versions used, a run timestamp, the input and output paths, and the
output file size — plus input/output md5 checksums when the batch was
run with `checksums = TRUE`. Together with the reproducible command this
is what turns a batch run into an auditable record.

## Usage

``` r
ffm_manifest(x, path = NULL)
```

## Arguments

- x:

  A tibble returned by
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  with `manifest = TRUE`.

- path:

  An optional file path. When supplied, the manifest is also written
  there as CSV (via
  [`utils::write.csv`](https://rdrr.io/r/utils/write.table.html), no row
  names) and returned invisibly.

## Value

The manifest
[tibble](https://tibble.tidyverse.org/reference/tibble-package.html);
invisibly when `path` is written.

## See also

[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
which records the manifest.

Other verification functions:
[`verify_media()`](https://jmgirard.github.io/tidymedia/reference/verify_media.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
jobs <- tibble::tibble(input = video, output = tempfile(fileext = ".mp3"))
res <- ffm_batch(jobs, manifest = TRUE, .f = function(input, output, ...) {
  ffm_files(input, output) |> ffm_drop("video")
})
ffm_manifest(res)
#> # A tibble: 1 × 7
#>   command      input output output_size ffmpeg_version ffprobe_version timestamp
#>   <chr>        <chr> <chr>        <dbl> <chr>          <chr>           <chr>    
#> 1 "-y -i \"/h… /hom… /tmp/…        8898 6.1.1-3ubuntu5 6.1.1-3ubuntu5  2026-07-…
```
