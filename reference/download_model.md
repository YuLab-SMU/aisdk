# Download Model from Hugging Face

Download a quantized model from Hugging Face Hub.

## Usage

``` r
download_model(repo_id, filename, dest_dir = NULL, quiet = FALSE)
```

## Arguments

- repo_id:

  The Hugging Face repository ID (e.g., "TheBloke/Llama-2-7B-GGUF").

- filename:

  The specific file to download.

- dest_dir:

  Destination directory. Defaults to "~/.cache/aisdk/models".

- quiet:

  Suppress download progress.

## Value

Path to the downloaded file.
