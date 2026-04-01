#' @title Image APIs
#' @description
#' High-level helpers for image analysis and image generation workflows.
#' @name image_api
NULL

#' @title Analyze Image
#' @description
#' Analyze an image with a multimodal language model.
#' @param model A `LanguageModelV1` object or `provider:model` string.
#' @param image Image source accepted by [input_image()].
#' @param prompt Prompt describing the analysis task.
#' @param system Optional system prompt.
#' @param registry Optional provider registry.
#' @param ... Additional arguments passed to [generate_text()].
#' @return A `GenerateResult`.
#' @export
analyze_image <- function(model,
                          image,
                          prompt,
                          system = NULL,
                          registry = NULL,
                          ...) {
  generate_text(
    model = model,
    prompt = list(list(
      role = "user",
      content = list(
        input_text(prompt),
        input_image(image)
      )
    )),
    system = system,
    registry = registry,
    ...
  )
}

#' @title Extract Structured Data From Image
#' @description
#' Extract schema-constrained structured data from an image using a multimodal language model.
#' @param model A `LanguageModelV1` object or `provider:model` string.
#' @param image Image source accepted by [input_image()].
#' @param schema A `z_schema` object used as `response_format`.
#' @param prompt Prompt describing the extraction task.
#' @param system Optional system prompt.
#' @param registry Optional provider registry.
#' @param ... Additional arguments passed to [generate_text()].
#' @return A `GenerateResult`.
#' @export
extract_from_image <- function(model,
                               image,
                               schema,
                               prompt = "Extract the requested structured information from this image.",
                               system = NULL,
                               registry = NULL,
                               ...) {
  generate_text(
    model = model,
    prompt = list(list(
      role = "user",
      content = list(
        input_text(prompt),
        input_image(image)
      )
    )),
    system = system,
    response_format = schema,
    registry = registry,
    ...
  )
}

#' @title Generate Images
#' @description
#' Generate images using an image generation model.
#' @param model An `ImageModelV1` object or `provider:model` string.
#' @param prompt Prompt describing the desired image.
#' @param output_dir Directory where generated images should be written. Defaults to `tempdir()`.
#' @param registry Optional provider registry.
#' @param ... Additional arguments passed to the model implementation.
#' @return A `GenerateImageResult`.
#' @export
generate_image <- function(model,
                           prompt,
                           output_dir = tempdir(),
                           registry = NULL,
                           ...) {
  model <- resolve_model(model, registry, type = "image")
  result <- model$do_generate_image(c(
    list(prompt = prompt, output_dir = output_dir),
    list(...)
  ))
  result$images <- finalize_image_artifacts(result$images, output_dir = output_dir, prefix = "generated_image")
  result
}

#' @title Edit Images
#' @description
#' Edit images using an image editing model.
#' @param model An `ImageModelV1` object or `provider:model` string.
#' @param image Image source or provider-specific image input.
#' @param prompt Editing instructions.
#' @param mask Optional mask image.
#' @param output_dir Directory where edited images should be written. Defaults to `tempdir()`.
#' @param registry Optional provider registry.
#' @param ... Additional arguments passed to the model implementation.
#' @return A `GenerateImageResult`.
#' @export
edit_image <- function(model,
                       image,
                       prompt = NULL,
                       mask = NULL,
                       output_dir = tempdir(),
                       registry = NULL,
                       ...) {
  model <- resolve_model(model, registry, type = "image")
  result <- model$do_edit_image(c(
    list(image = image, prompt = prompt, mask = mask, output_dir = output_dir),
    list(...)
  ))
  result$images <- finalize_image_artifacts(result$images, output_dir = output_dir, prefix = "edited_image")
  result
}
