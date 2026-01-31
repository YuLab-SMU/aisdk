# Create Image Content

Creates an image content object for a multimodal message. automatically
handles URLs and local files (converted to base64).

## Usage

``` r
content_image(image_path, media_type = "auto", detail = "auto")
```

## Arguments

- image_path:

  Path to a local file or a URL.

- media_type:

  MIME type of the image (e.g., "image/jpeg", "image/png"). If NULL,
  attempts to guess from the file extension.

- detail:

  Image detail suitable for some models (e.g., "auto", "low", "high").

## Value

A list representing the image content in OpenAI-compatible format.
