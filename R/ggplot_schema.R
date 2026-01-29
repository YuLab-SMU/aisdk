# ============================================================================
# ggplot Schema System - Enhanced for Frontend Rendering
# ============================================================================
#
# Design Philosophy:
# 1. Static definitions for known structures (predictable, fast)
# 2. Dynamic extraction for runtime-dependent values (accurate, complete)
# 3. Explicit empty/null semantics for frontend precision
# 4. Render hints for frontend optimization
#
# ============================================================================

# ----------------------------------------------------------------------------
# Empty Value Semantics (matching ggplot2's internal `empty()` function)
# ----------------------------------------------------------------------------

#' @title Check if Value is Empty (ggplot2 semantics)
#' @description
#' Mirrors ggplot2's internal `empty()` function behavior.
#' Returns TRUE if: NULL, 0 rows, 0 cols, or waiver object.
#' @param x Value to check.
#' @return Logical.
#' @keywords internal
is_ggplot_empty <- function(x) {
  if (is.null(x)) return(TRUE)
  if (inherits(x, "waiver")) return(TRUE)

  if (is.data.frame(x) && (nrow(x) == 0 || ncol(x) == 0)) return(TRUE)
  FALSE
}

#' @title Create Empty-Aware Schema Wrapper
#' @description
#' Wraps a schema to explicitly handle empty values.
#' Adds `_empty` metadata for frontend rendering decisions.
#' @param schema Base z_schema.
#' @param empty_behavior How frontend should handle empty: "skip", "placeholder", "inherit".
#' @return Enhanced z_schema.
#' @keywords internal
z_empty_aware <- function(schema, empty_behavior = "skip") {
  schema$`_empty_behavior` <- empty_behavior
  schema
}

# ----------------------------------------------------------------------------
# Aesthetic Mapping Schema (Enhanced)
# ----------------------------------------------------------------------------

#' @title Known Aesthetic Types
#' @description Registry of known aesthetics with their expected types.
#' @keywords internal
KNOWN_AESTHETICS <- list(
  # Position aesthetics
  x = list(type = "any", description = "X position"),
  y = list(type = "any", description = "Y position"),
  xmin = list(type = "number", description = "Minimum X"),
  xmax = list(type = "number", description = "Maximum X"),
  ymin = list(type = "number", description = "Minimum Y"),
  ymax = list(type = "number", description = "Maximum Y"),
  xend = list(type = "number", description = "End X position"),
  yend = list(type = "number", description = "End Y position"),


  # Color aesthetics
  colour = list(type = "color", description = "Outline color"),
  color = list(type = "color", description = "Outline color (alias)"),
  fill = list(type = "color", description = "Fill color"),
  alpha = list(type = "number", description = "Transparency (0-1)", min = 0, max = 1),

  # Size/shape aesthetics

  size = list(type = "number", description = "Size in mm", min = 0),
  linewidth = list(type = "number", description = "Line width in mm", min = 0),
  shape = list(type = "shape", description = "Point shape (0-25 or character)"),
  linetype = list(type = "linetype", description = "Line type (0-6 or name)"),

  # Text aesthetics
  label = list(type = "string", description = "Text label"),
  hjust = list(type = "number", description = "Horizontal justification (0-1)"),
  vjust = list(type = "number", description = "Vertical justification (0-1)"),
  angle = list(type = "number", description = "Rotation angle in degrees"),
  family = list(type = "string", description = "Font family"),
  fontface = list(type = "string", description = "Font face (plain, bold, italic, bold.italic)"),

  # Grouping
  group = list(type = "any", description = "Grouping variable"),

  # Statistical
  weight = list(type = "number", description = "Weight for statistical calculations")
)

#' @title Aesthetic Mapping Schema
#' @description Schema for ggplot2 aesthetic mappings (aes).
#' @param known_only If TRUE, only include known aesthetics in schema.
#' @return A z_object schema.
#' @export
z_aes_mapping <- function(known_only = FALSE) {
  if (known_only) {
    # Build strict schema from known aesthetics
    props <- lapply(names(KNOWN_AESTHETICS), function(aes_name) {
      info <- KNOWN_AESTHETICS[[aes_name]]
      z_string(description = info$description, nullable = TRUE)
    })
    names(props) <- names(KNOWN_AESTHETICS)
    do.call(z_object, c(props, list(.required = character(0), .additional_properties = FALSE)))
  } else {
    # Flexible schema allowing any aesthetic
    schema <- z_any_object(description = "Aesthetic mappings: variable names or expressions mapped to visual properties")
    schema$`_render_hint` <- "aes_editor"
    schema$`_known_aesthetics` <- names(KNOWN_AESTHETICS)
    class(schema) <- c("z_schema", "z_aes_mapping", "list")
    schema
  }
}

# ----------------------------------------------------------------------------
# Position Adjustment Schema (Enhanced with type-specific params)
# ----------------------------------------------------------------------------

#' @title Known Position Types with Parameters
#' @keywords internal
KNOWN_POSITIONS <- list(
  identity = list(
    description = "No position adjustment",
    params = list()
  ),
  jitter = list(
    description = "Random noise to avoid overplotting",
    params = list(
      width = list(type = "number", default = 0.4, description = "Amount of horizontal jitter"),
      height = list(type = "number", default = 0.4, description = "Amount of vertical jitter"),
      seed = list(type = "integer", nullable = TRUE, description = "Random seed for reproducibility")
    )
  ),
  dodge = list(
    description = "Dodge overlapping objects side-to-side",
    params = list(
      width = list(type = "number", default = 0.9, description = "Dodging width"),
      preserve = list(type = "enum", values = c("total", "single"), default = "total", description = "Preserve strategy")
    )
  ),
  dodge2 = list(
    description = "Dodge with variable widths",
    params = list(
      width = list(type = "number", default = 0.9, description = "Dodging width"),
      preserve = list(type = "enum", values = c("total", "single"), default = "total"),
      padding = list(type = "number", default = 0.1, description = "Padding between elements"),
      reverse = list(type = "boolean", default = FALSE, description = "Reverse order")
    )
  ),
  stack = list(
    description = "Stack overlapping objects on top of each other",
    params = list(
      vjust = list(type = "number", default = 1, description = "Vertical adjustment"),
      reverse = list(type = "boolean", default = FALSE, description = "Reverse stacking order")
    )
  ),
  fill = list(
    description = "Stack and normalize to unit height",
    params = list(
      vjust = list(type = "number", default = 1, description = "Vertical adjustment"),
      reverse = list(type = "boolean", default = FALSE, description = "Reverse stacking order")
    )
  ),
  nudge = list(
    description = "Nudge points by fixed amount",
    params = list(
      x = list(type = "number", default = 0, description = "Horizontal nudge"),
      y = list(type = "number", default = 0, description = "Vertical nudge")
    )
  )
)

#' @title Position Adjustment Schema
#' @description Schema for position adjustments with type-specific parameters.
#' @param position_type Optional specific position type for strict schema.
#' @return A z_object schema.
#' @export
z_position <- function(position_type = NULL) {
  if (!is.null(position_type) && position_type %in% names(KNOWN_POSITIONS)) {
    # Build type-specific schema
    pos_info <- KNOWN_POSITIONS[[position_type]]
    props <- list(
      type = z_enum(position_type, description = pos_info$description)
    )
    for (param_name in names(pos_info$params)) {
      p <- pos_info$params[[param_name]]
      props[[param_name]] <- switch(p$type,
        "number" = z_number(description = p$description, default = p$default, nullable = isTRUE(p$nullable)),
        "integer" = z_integer(description = p$description, default = p$default, nullable = isTRUE(p$nullable)),
        "boolean" = z_boolean(description = p$description, default = p$default),
        "enum" = z_enum(p$values, description = p$description, default = p$default),
        z_string(description = p$description, nullable = TRUE)
      )
    }
    do.call(z_object, c(props, list(.required = "type")))
  } else {
    # Generic position schema
    schema <- z_object(
      type = z_enum(names(KNOWN_POSITIONS), description = "Position adjustment type", default = "identity"),
      width = z_number(description = "Width for dodge/jitter", nullable = TRUE),
      height = z_number(description = "Height for jitter", nullable = TRUE),
      padding = z_number(description = "Padding between elements", nullable = TRUE),
      preserve = z_enum(c("total", "single"), description = "Preserve strategy", nullable = TRUE),
      reverse = z_boolean(description = "Reverse order", nullable = TRUE),
      .additional_properties = TRUE,
      .required = "type"
    )
    schema$`_known_types` <- names(KNOWN_POSITIONS)
    schema$`_render_hint` <- "position_selector"
    schema
  }
}

# ----------------------------------------------------------------------------
# Geom Registry - Dynamic Parameter Discovery
# ----------------------------------------------------------------------------

#' @title Extract Geom Parameters from ggproto Object
#' @description
#' Dynamically extracts parameter information from a ggplot2 geom.
#' This handles the "scattered definitions" problem by reading from source.
#' @param geom_name Name of the geom (e.g., "point", "line").
#' @return List with default_aes, required_aes, optional_aes, extra_params.
#' @export
extract_geom_params <- function(geom_name) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }

  # Construct the Geom class name
  geom_class_name <- paste0("Geom", tools::toTitleCase(geom_name))

  # Try to get the geom from ggplot2 namespace

  geom_obj <- tryCatch({
    get(geom_class_name, envir = asNamespace("ggplot2"))
  }, error = function(e) NULL)

  if (is.null(geom_obj) || !inherits(geom_obj, "Geom")) {
    return(NULL)
  }

  result <- list(
    name = geom_name,
    class = geom_class_name,
    default_aes = list(),
    required_aes = character(0),
    optional_aes = character(0),
    extra_params = character(0),
    `_source` = "ggplot2"
  )

  # Extract default_aes
  if (!is.null(geom_obj$default_aes)) {
    default_aes <- geom_obj$default_aes
    result$default_aes <- lapply(names(default_aes), function(aes_name) {
      val <- default_aes[[aes_name]]
      # Handle quosures and expressions safely
      if (rlang::is_quosure(val)) {
        val <- tryCatch({
          rlang::eval_tidy(val)
        }, error = function(e) {
          # If evaluation fails (e.g., theme element reference), return as expression string
          rlang::quo_text(val)
        })
      }
      list(
        name = aes_name,
        default = if (is.null(val)) NULL else as.character(val),
        type = if (is.null(val)) "null" else typeof(val)
      )
    })
    names(result$default_aes) <- names(default_aes)
  }

  # Extract required_aes
  if (!is.null(geom_obj$required_aes)) {
    result$required_aes <- geom_obj$required_aes
  }

  # Extract optional_aes
  if (!is.null(geom_obj$optional_aes)) {
    result$optional_aes <- geom_obj$optional_aes
  }

  # Extract extra_params (non-aesthetic parameters)
  if (!is.null(geom_obj$extra_params)) {
    result$extra_params <- geom_obj$extra_params
  }

  # Try to get the geom function for additional parameter info
  geom_func_name <- paste0("geom_", geom_name)
  geom_func <- tryCatch({
    get(geom_func_name, envir = asNamespace("ggplot2"))
  }, error = function(e) NULL)

  if (!is.null(geom_func) && is.function(geom_func)) {
    # Extract function parameters
    func_args <- formals(geom_func)
    result$func_params <- names(func_args)
  }

  result
}

#' @title Build Geom-Specific Layer Schema
#' @description
#' Creates a precise schema for a specific geom type.
#' @param geom_name Name of the geom.
#' @return A z_object schema tailored to the geom.
#' @export
z_geom_layer <- function(geom_name) {
  params <- extract_geom_params(geom_name)

  if (is.null(params)) {
    # Fallback to generic layer schema
    return(z_layer())
  }

  # Build mapping schema with required/optional hints
  mapping_props <- list()
  for (aes_name in params$required_aes) {
    info <- KNOWN_AESTHETICS[[aes_name]]
    desc <- if (!is.null(info)) info$description else paste("Required:", aes_name)
    mapping_props[[aes_name]] <- z_string(description = desc)
  }
  for (aes_name in params$optional_aes) {
    info <- KNOWN_AESTHETICS[[aes_name]]
    desc <- if (!is.null(info)) info$description else paste("Optional:", aes_name)
    mapping_props[[aes_name]] <- z_string(description = desc, nullable = TRUE)
  }

  mapping_schema <- if (length(mapping_props) > 0) {
    do.call(z_object, c(mapping_props, list(
      .required = params$required_aes,
      .additional_properties = TRUE
    )))
  } else {
    z_aes_mapping()
  }

  # Build params schema from default_aes
  params_props <- list()
  for (aes_name in names(params$default_aes)) {
    aes_info <- params$default_aes[[aes_name]]
    known <- KNOWN_AESTHETICS[[aes_name]]

    if (!is.null(known)) {
      params_props[[aes_name]] <- switch(known$type,
        "number" = z_number(
          description = known$description,
          nullable = TRUE,
          minimum = known$min,
          maximum = known$max
        ),
        "color" = z_string(description = known$description, nullable = TRUE),
        "shape" = z_string(description = known$description, nullable = TRUE),
        "linetype" = z_string(description = known$description, nullable = TRUE),
        z_string(description = known$description, nullable = TRUE)
      )
    } else {
      params_props[[aes_name]] <- z_string(
        description = paste("Aesthetic:", aes_name),
        nullable = TRUE
      )
    }
  }

  params_schema <- if (length(params_props) > 0) {
    do.call(z_object, c(params_props, list(
      .required = character(0),
      .additional_properties = TRUE
    )))
  } else {
    z_any_object(description = "Constant parameters")
  }

  # Build the layer schema
  schema <- z_object(
    geom = z_enum(geom_name, description = paste("Geometry type:", geom_name)),
    stat = z_string(description = "Statistical transformation", default = "identity"),
    mapping = mapping_schema,
    data = z_empty_aware(
      z_dataframe(.description = "Layer-specific data", .min_rows = 0),
      empty_behavior = "inherit"
    ),
    position = z_position(),
    params = params_schema,
    show_legend = z_boolean(description = "Show in legend", nullable = TRUE),
    inherit_aes = z_boolean(description = "Inherit aesthetics from plot", default = TRUE),
    .required = c("geom")
  )

  # Add metadata for frontend
  schema$`_geom_info` <- list(
    required_aes = params$required_aes,
    optional_aes = params$optional_aes,
    default_aes = params$default_aes
  )
  schema$`_render_hint` <- "layer_editor"

  schema
}

#' @title Guide Schema
#' @description Schema for guides (legend/axis).
#' @return A z_object schema.
#' @export
z_guide <- function() {
  z_object(
    aesthetic = z_string(description = "Aesthetic name, e.g. 'colour', 'fill'"),
    type = z_string(description = "Guide type, e.g., 'colorbar', 'legend', 'axis'", default = "legend"),
    title = z_string(description = "Title text", nullable = TRUE),
    position = z_string(description = "Position, e.g., 'left', 'right', 'bottom', 'top'", nullable = TRUE),
    direction = z_string(description = "Direction, e.g., 'horizontal', 'vertical'", nullable = TRUE),
    reverse = z_boolean(description = "Reverse order", nullable = TRUE),
    order = z_integer(description = "Order of guide", nullable = TRUE),
    .additional_properties = TRUE
  )
}

#' @title Layer Schema
#' @description Schema for a single ggplot2 layer.
#' @return A z_object schema.
#' @export
z_layer <- function() {
  z_object(
    geom = z_string(description = "Geometry type, e.g., 'point', 'line', 'bar'"),
    stat = z_string(description = "Statistical transformation, e.g., 'identity', 'smooth'", default = "identity"),
    mapping = z_aes_mapping(),
    data = z_dataframe(
      .description = "Layer-specific data. If null, inherits from plot data.",
      .min_rows = 0
    ),
    position = z_position(),
    params = z_any_object(description = "Constant parameters, e.g., {color: 'red', size: 3}"),
    show_legend = z_boolean(description = "Whether to show legend for this layer", nullable = TRUE),
    inherit_aes = z_boolean(description = "Whether to inherit aesthetics from plot", default = TRUE)
  )
}

#' @title Scale Schema
#' @description Schema for a scale definition.
#' @return A z_object schema.
#' @export
z_scale <- function() {
  z_object(
    aesthetic = z_string(description = "The aesthetic this scale applies to, e.g., 'x', 'y', 'color'"),
    type = z_string(description = "Scale type, e.g., 'continuous', 'discrete', 'log10'"),
    name = z_string(description = "Scale name (axis label or legend title)", nullable = TRUE),
    breaks = z_array(z_any_object(), description = "Major breaks", min_items = 0),
    labels = z_array(z_string(), description = "Labels for breaks", min_items = 0),
    limits = z_array(z_any_object(), description = "Limits of the scale", min_items = 2, max_items = 2)
  )
}

#' @title Coordinate System Schema
#' @description Schema for coordinate system.
#' @return A z_object schema.
#' @export
z_coord <- function() {
  z_object(
    type = z_string(description = "Coordinate type, e.g., 'cartesian', 'polar', 'flip'", default = "cartesian"),
    limits_x = z_array(z_number(), description = "X axis limits", min_items = 2, max_items = 2, nullable = TRUE),
    limits_y = z_array(z_number(), description = "Y axis limits", min_items = 2, max_items = 2, nullable = TRUE),
    expand = z_boolean(description = "Whether to add expansion padding", default = TRUE)
  )
}

# ----------------------------------------------------------------------------
# Theme Schema (Enhanced with element types)
# ----------------------------------------------------------------------------

#' @title Theme Element Types
#' @description Registry of theme element types and their properties.
#' @keywords internal
THEME_ELEMENT_TYPES <- list(
  element_text = list(
    description = "Text element styling",
    props = list(
      family = list(type = "string", description = "Font family"),
      face = list(type = "enum", values = c("plain", "italic", "bold", "bold.italic"), description = "Font face"),
      colour = list(type = "color", description = "Text color"),
      size = list(type = "number", description = "Font size in points"),
      hjust = list(type = "number", description = "Horizontal justification (0-1)"),
      vjust = list(type = "number", description = "Vertical justification (0-1)"),
      angle = list(type = "number", description = "Rotation angle"),
      lineheight = list(type = "number", description = "Line height multiplier"),
      margin = list(type = "margin", description = "Margins around text"),
      inherit.blank = list(type = "boolean", description = "Inherit blank from parent")
    )
  ),
  element_line = list(
    description = "Line element styling",
    props = list(
      colour = list(type = "color", description = "Line color"),
      linewidth = list(type = "number", description = "Line width"),
      linetype = list(type = "linetype", description = "Line type"),
      lineend = list(type = "enum", values = c("round", "butt", "square"), description = "Line end style"),
      arrow = list(type = "arrow", description = "Arrow specification"),
      inherit.blank = list(type = "boolean", description = "Inherit blank from parent")
    )
  ),
  element_rect = list(
    description = "Rectangle element styling",
    props = list(
      fill = list(type = "color", description = "Fill color"),
      colour = list(type = "color", description = "Border color"),
      linewidth = list(type = "number", description = "Border width"),
      linetype = list(type = "linetype", description = "Border line type"),
      inherit.blank = list(type = "boolean", description = "Inherit blank from parent")
    )
  ),
  element_blank = list(
    description = "Blank element (removes the element)",
    props = list()
  ),
  unit = list(
    description = "Unit specification",
    props = list(
      value = list(type = "number", description = "Numeric value"),
      units = list(type = "enum", values = c("pt", "cm", "mm", "inches", "lines", "npc"), description = "Unit type")
    )
  ),
  margin = list(
    description = "Margin specification (top, right, bottom, left)",
    props = list(
      t = list(type = "number", description = "Top margin"),
      r = list(type = "number", description = "Right margin"),
      b = list(type = "number", description = "Bottom margin"),
      l = list(type = "number", description = "Left margin"),
      unit = list(type = "string", description = "Unit (pt, cm, mm, etc.)", default = "pt")
    )
  )
)

#' @title Theme Component Hierarchy
#' @description Defines the hierarchical structure of theme components.
#' @keywords internal
THEME_HIERARCHY <- list(
  # Top-level components
  line = list(element = "element_line", description = "All line elements"),
  rect = list(element = "element_rect", description = "All rect elements"),
  text = list(element = "element_text", description = "All text elements"),
  title = list(element = "element_text", description = "All title elements", inherits = "text"),

  # Axis components
  axis.line = list(element = "element_line", description = "Axis lines", inherits = "line"),
  axis.line.x = list(element = "element_line", inherits = "axis.line"),
  axis.line.y = list(element = "element_line", inherits = "axis.line"),
  axis.text = list(element = "element_text", description = "Axis tick labels", inherits = "text"),
  axis.text.x = list(element = "element_text", inherits = "axis.text"),
  axis.text.y = list(element = "element_text", inherits = "axis.text"),
  axis.title = list(element = "element_text", description = "Axis titles", inherits = "title"),
  axis.title.x = list(element = "element_text", inherits = "axis.title"),
  axis.title.y = list(element = "element_text", inherits = "axis.title"),
  axis.ticks = list(element = "element_line", description = "Axis tick marks", inherits = "line"),
  axis.ticks.x = list(element = "element_line", inherits = "axis.ticks"),
  axis.ticks.y = list(element = "element_line", inherits = "axis.ticks"),
  axis.ticks.length = list(element = "unit", description = "Length of tick marks"),

  # Legend components
  legend.background = list(element = "element_rect", description = "Legend background", inherits = "rect"),
  legend.key = list(element = "element_rect", description = "Legend key background"),
  legend.key.size = list(element = "unit", description = "Legend key size"),
  legend.key.height = list(element = "unit", inherits = "legend.key.size"),
  legend.key.width = list(element = "unit", inherits = "legend.key.size"),
  legend.text = list(element = "element_text", description = "Legend item labels", inherits = "text"),
  legend.title = list(element = "element_text", description = "Legend title", inherits = "title"),
  legend.position = list(element = "position", description = "Legend position"),
  legend.direction = list(element = "enum", values = c("horizontal", "vertical")),
  legend.box = list(element = "enum", values = c("horizontal", "vertical")),
  legend.margin = list(element = "margin", description = "Legend margin"),
  legend.spacing = list(element = "unit", description = "Spacing between legends"),

  # Panel components
  panel.background = list(element = "element_rect", description = "Panel background", inherits = "rect"),
  panel.border = list(element = "element_rect", description = "Panel border"),
  panel.grid = list(element = "element_line", description = "Grid lines", inherits = "line"),
  panel.grid.major = list(element = "element_line", inherits = "panel.grid"),
  panel.grid.minor = list(element = "element_line", inherits = "panel.grid"),
  panel.grid.major.x = list(element = "element_line", inherits = "panel.grid.major"),
  panel.grid.major.y = list(element = "element_line", inherits = "panel.grid.major"),
  panel.grid.minor.x = list(element = "element_line", inherits = "panel.grid.minor"),
  panel.grid.minor.y = list(element = "element_line", inherits = "panel.grid.minor"),
  panel.spacing = list(element = "unit", description = "Spacing between panels"),
  panel.spacing.x = list(element = "unit", inherits = "panel.spacing"),
  panel.spacing.y = list(element = "unit", inherits = "panel.spacing"),

  # Plot components
  plot.background = list(element = "element_rect", description = "Plot background", inherits = "rect"),
  plot.title = list(element = "element_text", description = "Plot title", inherits = "title"),
  plot.subtitle = list(element = "element_text", description = "Plot subtitle", inherits = "title"),
  plot.caption = list(element = "element_text", description = "Plot caption", inherits = "title"),
  plot.tag = list(element = "element_text", description = "Plot tag", inherits = "title"),
  plot.margin = list(element = "margin", description = "Plot margins"),

  # Strip (facet label) components
  strip.background = list(element = "element_rect", description = "Facet strip background", inherits = "rect"),
  strip.background.x = list(element = "element_rect", inherits = "strip.background"),
  strip.background.y = list(element = "element_rect", inherits = "strip.background"),
  strip.text = list(element = "element_text", description = "Facet strip text", inherits = "text"),
  strip.text.x = list(element = "element_text", inherits = "strip.text"),
  strip.text.y = list(element = "element_text", inherits = "strip.text"),
  strip.placement = list(element = "enum", values = c("inside", "outside"))
)

#' @title Create Element Schema
#' @description Creates a schema for a specific theme element type.
#' @param element_type One of the THEME_ELEMENT_TYPES names.
#' @return A z_object schema.
#' @keywords internal
z_theme_element <- function(element_type) {
  if (!element_type %in% names(THEME_ELEMENT_TYPES)) {
    return(z_any_object(description = paste("Unknown element type:", element_type)))
  }

  elem_info <- THEME_ELEMENT_TYPES[[element_type]]

  if (length(elem_info$props) == 0) {
    # element_blank has no properties
    schema <- z_object(
      `_type` = z_enum(element_type, description = elem_info$description),
      .required = "_type"
    )
    return(schema)
  }

  props <- list(
    `_type` = z_enum(element_type, description = elem_info$description)
  )

  for (prop_name in names(elem_info$props)) {
    p <- elem_info$props[[prop_name]]
    props[[prop_name]] <- switch(p$type,
      "string" = z_string(description = p$description, nullable = TRUE),
      "number" = z_number(description = p$description, nullable = TRUE),
      "boolean" = z_boolean(description = p$description, nullable = TRUE),
      "color" = z_string(description = p$description, nullable = TRUE),
      "linetype" = z_string(description = p$description, nullable = TRUE),
      "enum" = z_enum(p$values, description = p$description, nullable = TRUE),
      "margin" = z_theme_element("margin"),
      "unit" = z_theme_element("unit"),
      z_any_object(description = p$description)
    )
  }

  do.call(z_object, c(props, list(.required = "_type", .additional_properties = TRUE)))
}

#' @title Theme Schema
#' @description
#' Schema for theme settings with full hierarchy support.
#' @param flat If TRUE, returns flat structure. If FALSE, returns hierarchical.
#' @return A z_object schema.
#' @export
z_theme <- function(flat = TRUE) {
  if (flat) {
    # Build flat schema with all theme components
    props <- list()
    for (comp_name in names(THEME_HIERARCHY)) {
      comp_info <- THEME_HIERARCHY[[comp_name]]
      desc <- comp_info$description
      if (is.null(desc)) desc <- paste("Theme component:", comp_name)

      if (comp_info$element %in% names(THEME_ELEMENT_TYPES)) {
        # Use element-specific schema
        elem_schema <- z_theme_element(comp_info$element)
        elem_schema$description <- desc
        props[[comp_name]] <- elem_schema
      } else if (comp_info$element == "position") {
        props[[comp_name]] <- z_string(description = desc, nullable = TRUE)
      } else if (comp_info$element == "enum") {
        props[[comp_name]] <- z_enum(comp_info$values, description = desc, nullable = TRUE)
      } else {
        props[[comp_name]] <- z_any_object(description = desc)
      }
    }

    schema <- do.call(z_object, c(props, list(
      .required = character(0),
      .additional_properties = TRUE
    )))
    schema$`_render_hint` <- "theme_editor"
    schema$`_hierarchy` <- THEME_HIERARCHY
    schema
  } else {
    # Hierarchical structure for tree-based UI
    schema <- z_any_object(description = "Theme settings with hierarchical structure")
    schema$`_render_hint` <- "theme_tree"
    schema$`_hierarchy` <- THEME_HIERARCHY
    schema$`_element_types` <- THEME_ELEMENT_TYPES
    schema
  }
}

#' @title Facet Schema
#' @description Schema for faceting.
#' @return A z_object schema.
#' @export
z_facet <- function() {
  z_object(
    type = z_string(description = "Facet type: 'null', 'wrap', 'grid'", default = "null"),
    facets = z_array(z_string(), description = "Variables to facet by"),
    nrow = z_integer(description = "Number of rows", nullable = TRUE),
    ncol = z_integer(description = "Number of columns", nullable = TRUE),
    scales = z_string(description = "Should scales be fixed? 'fixed', 'free', 'free_x', 'free_y'", default = "fixed")
  )
}

#' @title GGPlot Object Schema
#' @description Top-level schema for a ggplot object.
#' @return A z_object schema.
#' @export
z_ggplot <- function() {
  z_object(
    data = z_dataframe(
      .description = "Global plot data",
      .min_rows = 0
    ),
    mapping = z_aes_mapping(),
    layers = z_array(z_layer(), description = "List of plot layers"),
    scales = z_array(z_scale(), description = "List of scale definitions"),
    guides = z_array(z_guide(), description = "List of guides"),
    coord = z_coord(),
    theme = z_theme(),
    facet = z_facet(),
    labels = z_object(
      title = z_string(nullable = TRUE),
      subtitle = z_string(nullable = TRUE),
      caption = z_string(nullable = TRUE),
      x = z_string(nullable = TRUE),
      y = z_string(nullable = TRUE),
      .additional_properties = TRUE
    )
  )
}

z_ggplot_build <- function() {
  z_object(
    layers = z_array(z_any_object(), description = "Built layer data"),
    panel_params = z_array(z_any_object(), description = "Panel parameters")
  )
}

z_ggplot_render <- function() {
  z_object(
    widths = z_array(z_string(), description = "GTable widths"),
    heights = z_array(z_string(), description = "GTable heights"),
    structure = z_any_object(description = "GTable layout structure")
  )
}

# ----------------------------------------------------------------------------
# Enhanced ggplot Object Conversion
# ----------------------------------------------------------------------------

#' @title Convert ggplot Object to Schema-Compliant Structure
#' @description
#' Converts a ggplot object to a JSON-serializable structure with
#' precise empty value handling and render hints for frontend.
#' @param plot A ggplot object.
#' @param include_data Whether to include data in output.
#' @param include_render_hints Whether to include frontend render hints.
#' @return A list structure matching z_ggplot schema.
#' @export
ggplot_to_z_object <- function(plot, include_data = TRUE, include_render_hints = TRUE) {
  mapping_to_list <- function(mapping) {
    if (is.null(mapping)) return(list())
    if (length(mapping) == 0) return(list())
    out <- lapply(mapping, function(x) {
      if (is.character(x)) return(x)
      rlang::as_label(rlang::get_expr(x))
    })
    if (!is.null(names(mapping))) names(out) <- names(mapping)
    out
  }

  df_to_records <- function(df) {
    if (is_ggplot_empty(df)) {
      return(list(`_empty` = TRUE, `_reason` = "no_data", rows = list()))
    }
    records <- lapply(seq_len(nrow(df)), function(i) {
      as.list(df[i, , drop = FALSE])
    })
    list(
      `_empty` = FALSE,
      rows = sanitize_for_json(records),
      `_nrow` = nrow(df),
      `_ncol` = ncol(df),
      `_colnames` = names(df)
    )
  }

  ggproto_name <- function(obj, prefix) {
    cls <- class(obj)[1]
    name <- gsub(paste0("^", prefix), "", cls)
    tolower(name)
  }

  scale_type <- function(scale) {
    cls <- paste(class(scale), collapse = " ")
    if (grepl("Continuous", cls)) return("continuous")
    if (grepl("Discrete", cls)) return("discrete")
    "unknown"
  }

  # --- Data ---
  plot_data <- if (include_data && is.data.frame(plot$data)) {
    df_to_records(plot$data)
  } else {
    list(`_empty` = is_ggplot_empty(plot$data), `_included` = FALSE)
  }

  plot_mapping <- mapping_to_list(plot$mapping)

  # --- Layers with geom-specific info ---
  layers <- lapply(seq_along(plot$layers), function(i) {
    layer <- plot$layers[[i]]

    geom_name <- ggproto_name(layer$geom, "Geom")
    stat_name <- ggproto_name(layer$stat, "Stat")

    # Get geom info for render hints
    geom_info <- extract_geom_params(geom_name)

    layer_data <- list(`_empty` = TRUE, `_included` = FALSE)
    if (include_data && !is.null(layer$data) && is.data.frame(layer$data)) {
      layer_data <- df_to_records(layer$data)
    } else if (is_ggplot_empty(layer$data)) {
      layer_data <- list(`_empty` = TRUE, `_reason` = "inherits_from_plot")
    }

    params <- sanitize_for_json(c(layer$aes_params, layer$geom_params, layer$stat_params))

    position_obj <- layer$position
    position_name <- ggproto_name(position_obj, "Position")
    position_list <- list(type = position_name)

    # Extract position parameters dynamically
    if (position_name %in% names(KNOWN_POSITIONS)) {
      pos_params <- KNOWN_POSITIONS[[position_name]]$params
      for (pname in names(pos_params)) {
        if (exists(pname, envir = position_obj)) {
          position_list[[pname]] <- position_obj[[pname]]
        }
      }
    }

    result <- list(
      `_index` = i,
      geom = geom_name,
      stat = stat_name,
      mapping = mapping_to_list(layer$mapping),
      data = layer_data,
      position = position_list,
      params = params,
      show_legend = if (!is.null(layer$show.legend)) layer$show.legend else NULL,
      inherit_aes = if (!is.null(layer$inherit.aes)) layer$inherit.aes else TRUE
    )

    # Add render hints
    if (include_render_hints && !is.null(geom_info)) {
      result$`_render_hints` <- list(
        required_aes = geom_info$required_aes,
        optional_aes = geom_info$optional_aes,
        has_required_mapping = all(geom_info$required_aes %in% c(
          names(mapping_to_list(layer$mapping)),
          names(plot_mapping)
        ))
      )
    }

    result
  })

  # --- Scales ---
  scales <- list()
  if (!is.null(plot$scales) && length(plot$scales$scales) > 0) {
    scales <- lapply(plot$scales$scales, function(scale) {
      list(
        aesthetic = if (!is.null(scale$aesthetics)) scale$aesthetics[[1]] else NULL,
        type = scale_type(scale),
        name = scale$name,
        breaks = if (!is.null(scale$breaks) && !is.function(scale$breaks)) as.list(scale$breaks) else list(),
        labels = if (!is.null(scale$labels) && !is.function(scale$labels)) as.list(scale$labels) else list(),
        limits = if (!is.null(scale$limits) && !is.function(scale$limits)) as.list(scale$limits) else NULL,
        `_class` = class(scale)[1]
      )
    })
  }

  # --- Guides ---
  guides <- list()
  if (!is.null(plot$guides)) {
    guides_source <- plot$guides
    if (!is.null(guides_source$guides) && is.list(guides_source$guides)) {
      guides_source <- guides_source$guides
    }

    if (is.list(guides_source) && length(guides_source) > 0) {
      guides <- lapply(names(guides_source), function(aes) {
        g <- guides_source[[aes]]
        res <- list(aesthetic = aes)

        if (is.character(g)) {
          res$type <- g
        } else if (inherits(g, "ggproto") || inherits(g, "guide")) {
          res$type <- ggproto_name(g, "Guide")
          if (!is.null(g$title)) res$title <- as.character(g$title)
          if (!is.null(g$position)) res$position <- as.character(g$position)
          if (!is.null(g$direction)) res$direction <- as.character(g$direction)
          if (!is.null(g$reverse)) res$reverse <- as.logical(g$reverse)
          if (!is.null(g$order)) res$order <- as.integer(g$order)
        } else {
          res$type <- "unknown"
        }
        res
      })
    }
  }

  # --- Coord ---
  coord <- list(
    type = ggproto_name(plot$coordinates, "Coord"),
    limits_x = NULL,
    limits_y = NULL,
    expand = if (!is.null(plot$coordinates$expand)) plot$coordinates$expand else TRUE
  )

  # --- Facet ---
  facet_obj <- plot$facet
  facet_type <- ggproto_name(facet_obj, "Facet")
  facet_facets <- list()
  facet_nrow <- NULL
  facet_ncol <- NULL
  facet_scales <- "fixed"
  if (!is.null(facet_obj$params)) {
    if (!is.null(facet_obj$params$facets)) {
      facet_facets <- lapply(facet_obj$params$facets, function(x) {
        if (is.null(x)) return(NULL)
        if (rlang::is_quosure(x) || rlang::is_symbol(x) || rlang::is_call(x)) {
          rlang::as_label(rlang::get_expr(x))
        } else {
          as.character(x)
        }
      })
    }
    if (!is.null(facet_obj$params$nrow)) facet_nrow <- facet_obj$params$nrow
    if (!is.null(facet_obj$params$ncol)) facet_ncol <- facet_obj$params$ncol
    if (!is.null(facet_obj$params$scales)) facet_scales <- facet_obj$params$scales
  }

  # --- Labels ---
  labels <- plot$labels
  labels_out <- list(
    title = rlang::`%||%`(labels$title, NULL),
    subtitle = rlang::`%||%`(labels$subtitle, NULL),
    caption = rlang::`%||%`(labels$caption, NULL),
    x = rlang::`%||%`(labels$x, NULL),
    y = rlang::`%||%`(labels$y, NULL)
  )

  # --- Theme (enhanced extraction) ---
  theme_out <- extract_theme_values(plot$theme)

  # --- Build output ---
  out <- list(
    `_schema_version` = "2.0",
    `_empty_semantics` = list(
      null_is_empty = TRUE,
      zero_rows_is_empty = TRUE,
      waiver_is_empty = TRUE
    ),
    data = plot_data,
    mapping = plot_mapping,
    layers = layers,
    scales = scales,
    guides = guides,
    coord = coord,
    theme = theme_out,
    facet = list(
      type = if (!is.null(facet_type)) facet_type else "null",
      facets = facet_facets,
      nrow = facet_nrow,
      ncol = facet_ncol,
      scales = facet_scales
    ),
    labels = labels_out
  )

  # Add render hints for frontend
  if (include_render_hints) {
    out$`_render_hints` <- list(
      has_data = !is_ggplot_empty(plot$data),
      layer_count = length(plot$layers),
      has_facets = facet_type != "null",
      geom_types = unique(sapply(layers, function(l) l$geom)),
      scale_aesthetics = sapply(scales, function(s) s$aesthetic)
    )
  }

  sanitize_for_json(out)
}

#' @title Extract Theme Values
#' @description
#' Extracts theme values with proper element type handling.
#' @param theme A ggplot2 theme object.
#' @return A list of theme values.
#' @keywords internal
extract_theme_values <- function(theme) {
  if (!inherits(theme, "theme")) {
    return(list(`_empty` = TRUE))
  }

  result <- list()

  for (elem_name in names(theme)) {
    elem <- theme[[elem_name]]

    if (is.null(elem)) {
      result[[elem_name]] <- list(`_type` = "null")
    } else if (inherits(elem, "element_blank")) {
      result[[elem_name]] <- list(`_type` = "element_blank")
    } else if (inherits(elem, "element_text")) {
      result[[elem_name]] <- list(
        `_type` = "element_text",
        family = elem$family,
        face = elem$face,
        colour = elem$colour,
        size = elem$size,
        hjust = elem$hjust,
        vjust = elem$vjust,
        angle = elem$angle,
        lineheight = elem$lineheight
      )
    } else if (inherits(elem, "element_line")) {
      result[[elem_name]] <- list(
        `_type` = "element_line",
        colour = elem$colour,
        linewidth = elem$linewidth,
        linetype = elem$linetype,
        lineend = elem$lineend
      )
    } else if (inherits(elem, "element_rect")) {
      result[[elem_name]] <- list(
        `_type` = "element_rect",
        fill = elem$fill,
        colour = elem$colour,
        linewidth = elem$linewidth,
        linetype = elem$linetype
      )
    } else if (inherits(elem, "unit")) {
      result[[elem_name]] <- list(
        `_type` = "unit",
        value = as.numeric(elem),
        units = attr(elem, "unit")
      )
    } else if (inherits(elem, "margin")) {
      result[[elem_name]] <- list(
        `_type` = "margin",
        t = as.numeric(elem[1]),
        r = as.numeric(elem[2]),
        b = as.numeric(elem[3]),
        l = as.numeric(elem[4]),
        unit = attr(elem, "unit")
      )
    } else {
      # Fallback for unknown types
      result[[elem_name]] <- tryCatch(
        sanitize_for_json(elem),
        error = function(e) list(`_type` = "unknown", `_class` = class(elem)[1])
      )
    }
  }

  result
}

ggplot_parse_code <- function(code, envir = globalenv(), include_data = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    rlang::abort("ggplot2 package is required")
  }
  parsed <- parse(text = code)
  plot <- eval(parsed, envir = envir)
  if (!inherits(plot, "ggplot")) {
    rlang::abort("code must evaluate to a ggplot object")
  }
  ggplot_to_z_object(plot, include_data = include_data)
}

ggplot_parse_json <- function(text) {
  if (is.list(text)) return(text)
  obj <- safe_parse_json(text)
  if (is.null(obj)) {
    rlang::abort("invalid ggplot JSON")
  }
  obj
}

ggplot_build_to_z_object <- function(plot, include_data = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    rlang::abort("ggplot2 package is required")
  }
  if (!inherits(plot, "ggplot")) {
    rlang::abort("plot must be a ggplot object")
  }
  built <- ggplot2::ggplot_build(plot)
  list(
    plot = ggplot_to_z_object(plot, include_data = include_data),
    built = list(
      layers = sanitize_for_json(built$data),
      panel_params = sanitize_for_json(built$layout$panel_params)
    )
  )
}

ggplot_gtable_layout <- function(plot) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    rlang::abort("ggplot2 package is required")
  }
  if (!inherits(plot, "ggplot")) {
    rlang::abort("plot must be a ggplot object")
  }
  built <- ggplot2::ggplot_build(plot)
  gtable <- ggplot2::ggplot_gtable(built)
  list(
    widths = as.list(sapply(gtable$widths, as.character)),
    heights = as.list(sapply(gtable$heights, as.character)),
    structure = sanitize_for_json(gtable$layout)
  )
}

# ============================================================================
# Frontend-Ready Export Functions
# ============================================================================

#' @title Export ggplot as Frontend-Ready JSON
#' @description
#' Exports a ggplot object as JSON optimized for frontend rendering.
#' Addresses all frontend feedback:
#' - Strict scalar typing (no {} for missing values)
#' - Structured units with pre-calculated pixel values
#' - Stable IDs for React keys
#' - Consistent Array of Structures pattern
#'
#' @param plot A ggplot object.
#' @param width Plot width in pixels.
#' @param height Plot height in pixels.
#' @param include_data Whether to include data.
#' @param include_built Whether to include ggplot_build() output.
#' @param pretty Format JSON with indentation.
#' @return JSON string optimized for frontend.
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' json <- ggplot_to_frontend_json(p, width = 800, height = 600)
#' }
ggplot_to_frontend_json <- function(plot,
                                     width = 800,
                                     height = 600,
                                     include_data = TRUE,
                                     include_built = FALSE,
                                     pretty = FALSE) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    rlang::abort("ggplot2 package is required")
  }
  if (!inherits(plot, "ggplot")) {
    rlang::abort("plot must be a ggplot object")
  }

  plot_dims <- list(width_px = width, height_px = height)

  # Convert plot to structured object
  result <- ggplot_to_frontend_object(plot, include_data, plot_dims)

  # Optionally include built data

  if (include_built) {
    built <- ggplot2::ggplot_build(plot)
    result$built <- list(
      layers = lapply(built$data, function(df) {
        sanitize_for_json(df, plot_dims = plot_dims)
      }),
      panel_params = sanitize_for_json(built$layout$panel_params, plot_dims = plot_dims)
    )
  }

  # Add stable IDs
  result <- add_stable_ids(result, prefix = "plot")

  # Add metadata
  result$`_meta` <- list(
    schema_version = "2.1",
    width_px = width,
    height_px = height,
    dpi = DEFAULT_DPI,
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
    empty_semantics = list(
      null_means = "missing_or_not_set",
      empty_array_means = "no_items",
      `_empty_true_means` = "inherits_or_computed"
    )
  )

  jsonlite::toJSON(result, auto_unbox = TRUE, null = "null", pretty = pretty)
}

#' @title Convert ggplot to Frontend-Friendly Object
#' @description Internal function for structured conversion.
#' @keywords internal
ggplot_to_frontend_object <- function(plot, include_data, plot_dims) {

  # Helper functions
  mapping_to_list <- function(mapping) {
    if (is.null(mapping) || length(mapping) == 0) return(list())
    out <- lapply(mapping, function(x) {
      if (is.character(x)) return(x)
      rlang::as_label(rlang::get_expr(x))
    })
    if (!is.null(names(mapping))) names(out) <- names(mapping)
    out
  }

  ggproto_name <- function(obj, prefix) {
    cls <- class(obj)[1]
    tolower(gsub(paste0("^", prefix), "", cls))
  }

  # --- Data (Array of Structures) ---
  plot_data <- if (include_data && is.data.frame(plot$data) && nrow(plot$data) > 0) {
    list(
      `_empty` = FALSE,
      `_nrow` = nrow(plot$data),
      `_ncol` = ncol(plot$data),
      columns = names(plot$data),
      rows = sanitize_for_json(plot$data, plot_dims = plot_dims)
    )
  } else {
    list(`_empty` = TRUE, `_reason` = if (is.null(plot$data)) "null" else "no_rows")
  }

  # --- Layers ---
  layers <- lapply(seq_along(plot$layers), function(i) {
    layer <- plot$layers[[i]]
    geom_name <- ggproto_name(layer$geom, "Geom")
    stat_name <- ggproto_name(layer$stat, "Stat")

    # Layer data
    layer_data <- if (include_data && !is.null(layer$data) && is.data.frame(layer$data) && nrow(layer$data) > 0) {
      list(
        `_empty` = FALSE,
        rows = sanitize_for_json(layer$data, plot_dims = plot_dims)
      )
    } else {
      list(`_empty` = TRUE, `_reason` = "inherits_from_plot")
    }

    # Position with structured params
    position_obj <- layer$position
    position_name <- ggproto_name(position_obj, "Position")
    position_list <- list(type = position_name)

    if (position_name %in% names(KNOWN_POSITIONS)) {
      for (pname in names(KNOWN_POSITIONS[[position_name]]$params)) {
        val <- tryCatch(position_obj[[pname]], error = function(e) NULL)
        if (!is.null(val)) {
          position_list[[pname]] <- val
        }
      }
    }

    # Params - ensure no {} for missing scalars
    params <- c(layer$aes_params, layer$geom_params, layer$stat_params)
    params <- sanitize_for_json(params, plot_dims = plot_dims)

    # Geom info for render hints
    geom_info <- extract_geom_params(geom_name)

    list(
      `_index` = i,
      `_id` = generate_stable_id("layer", i, geom_name, prefix = "layer"),
      geom = geom_name,
      stat = stat_name,
      mapping = mapping_to_list(layer$mapping),
      data = layer_data,
      position = position_list,
      params = params,
      show_legend = if (is.null(layer$show.legend)) NULL else layer$show.legend,
      inherit_aes = if (is.null(layer$inherit.aes)) TRUE else layer$inherit.aes,
      `_geom_info` = if (!is.null(geom_info)) list(
        required_aes = geom_info$required_aes,
        optional_aes = geom_info$optional_aes
      ) else NULL
    )
  })

  # --- Scales ---
  scales <- if (!is.null(plot$scales) && length(plot$scales$scales) > 0) {
    lapply(seq_along(plot$scales$scales), function(i) {
      scale <- plot$scales$scales[[i]]
      cls <- paste(class(scale), collapse = " ")
      scale_type <- if (grepl("Continuous", cls)) "continuous" else if (grepl("Discrete", cls)) "discrete" else "unknown"

      list(
        `_id` = generate_stable_id("scale", i, scale$aesthetics[[1]], prefix = "scale"),
        aesthetic = if (!is.null(scale$aesthetics)) scale$aesthetics[[1]] else NULL,
        type = scale_type,
        name = if (is.null(scale$name)) NULL else scale$name,
        breaks = if (!is.null(scale$breaks) && !is.function(scale$breaks)) as.list(scale$breaks) else list(),
        labels = if (!is.null(scale$labels) && !is.function(scale$labels)) as.list(scale$labels) else list(),
        limits = if (!is.null(scale$limits) && !is.function(scale$limits)) as.list(scale$limits) else NULL
      )
    })
  } else {
    list()
  }

  # --- Coord ---
  coord <- list(
    `_id` = generate_stable_id("coord", class(plot$coordinates)[1], prefix = "coord"),
    type = ggproto_name(plot$coordinates, "Coord"),
    limits_x = NULL,
    limits_y = NULL,
    expand = if (is.null(plot$coordinates$expand)) TRUE else plot$coordinates$expand
  )

  # --- Facet ---
  facet_obj <- plot$facet
  facet_type <- ggproto_name(facet_obj, "Facet")
  facet_vars <- list()
  if (!is.null(facet_obj$params$facets)) {
    facet_vars <- lapply(facet_obj$params$facets, function(x) {
      if (is.null(x)) return(NULL)
      if (rlang::is_quosure(x) || rlang::is_symbol(x) || rlang::is_call(x)) {
        rlang::as_label(rlang::get_expr(x))
      } else {
        as.character(x)
      }
    })
  }

  facet <- list(
    `_id` = generate_stable_id("facet", facet_type, prefix = "facet"),
    type = facet_type,
    facets = facet_vars,
    nrow = facet_obj$params$nrow,
    ncol = facet_obj$params$ncol,
    scales = if (is.null(facet_obj$params$scales)) "fixed" else facet_obj$params$scales
  )

  # --- Theme (with structured units) ---
  theme_out <- extract_theme_for_frontend(plot$theme, plot_dims)

  # --- Labels ---
  labels <- list(
    title = plot$labels$title,
    subtitle = plot$labels$subtitle,
    caption = plot$labels$caption,
    x = plot$labels$x,
    y = plot$labels$y
  )

  # --- Guides ---
  guides <- extract_guides(plot$guides)

  # Build result
  list(
    data = plot_data,
    mapping = mapping_to_list(plot$mapping),
    layers = layers,
    scales = scales,
    guides = guides,
    coord = coord,
    facet = facet,
    theme = theme_out,
    labels = labels,
    `_render_hints` = list(
      has_data = !is_ggplot_empty(plot$data),
      layer_count = length(plot$layers),
      has_facets = facet_type != "null",
      geom_types = unique(sapply(layers, function(l) l$geom))
    )
  )
}

#' @title Extract Theme for Frontend
#' @description Extracts theme with structured units and pixel values.
#' @keywords internal
extract_theme_for_frontend <- function(theme, plot_dims) {
  if (!inherits(theme, "theme")) {
    return(list(`_empty` = TRUE))
  }

  result <- list()

  for (elem_name in names(theme)) {
    elem <- theme[[elem_name]]
    result[[elem_name]] <- sanitize_theme_element(elem, plot_dims)
  }

  result
}

#' @title Sanitize Theme Element for Frontend
#' @keywords internal
sanitize_theme_element <- function(elem, plot_dims) {
  if (is.null(elem)) {
    return(NULL)  # Explicit null, not {}
  }

  if (inherits(elem, "element_blank")) {
    return(list(`_type` = "element_blank"))
  }

  if (inherits(elem, "element_text")) {
    return(list(
      `_type` = "element_text",
      family = if (is.null(elem$family)) NULL else elem$family,
      face = if (is.null(elem$face)) NULL else elem$face,
      colour = if (is.null(elem$colour)) NULL else elem$colour,
      size = if (is.null(elem$size)) NULL else elem$size,
      size_px = if (is.null(elem$size)) NULL else round(elem$size * DEFAULT_DPI / 72, 2),
      hjust = if (is.null(elem$hjust)) NULL else elem$hjust,
      vjust = if (is.null(elem$vjust)) NULL else elem$vjust,
      angle = if (is.null(elem$angle)) NULL else elem$angle,
      lineheight = if (is.null(elem$lineheight)) NULL else elem$lineheight,
      margin = if (is.null(elem$margin)) NULL else sanitize_for_json(elem$margin, plot_dims = plot_dims)
    ))
  }

  if (inherits(elem, "element_line")) {
    return(list(
      `_type` = "element_line",
      colour = if (is.null(elem$colour)) NULL else elem$colour,
      linewidth = if (is.null(elem$linewidth)) NULL else elem$linewidth,
      linewidth_px = if (is.null(elem$linewidth)) NULL else round(elem$linewidth * DEFAULT_DPI / 72, 2),
      linetype = if (is.null(elem$linetype)) NULL else elem$linetype,
      lineend = if (is.null(elem$lineend)) NULL else elem$lineend
    ))
  }

  if (inherits(elem, "element_rect")) {
    return(list(
      `_type` = "element_rect",
      fill = if (is.null(elem$fill)) NULL else elem$fill,
      colour = if (is.null(elem$colour)) NULL else elem$colour,
      linewidth = if (is.null(elem$linewidth)) NULL else elem$linewidth,
      linewidth_px = if (is.null(elem$linewidth)) NULL else round(elem$linewidth * DEFAULT_DPI / 72, 2),
      linetype = if (is.null(elem$linetype)) NULL else elem$linetype
    ))
  }

  if (inherits(elem, "unit") || inherits(elem, "margin")) {
    return(sanitize_for_json(elem, plot_dims = plot_dims))
  }

  # Scalar values
  if (is.atomic(elem) && length(elem) == 1) {
    if (is.na(elem)) return(NULL)
    return(elem)
  }

  # Fallback
  sanitize_for_json(elem, plot_dims = plot_dims)
}

#' @title Extract Guides
#' @keywords internal
extract_guides <- function(guides_obj) {
  if (is.null(guides_obj)) return(list())

  guides_source <- guides_obj
  if (!is.null(guides_source$guides) && is.list(guides_source$guides)) {
    guides_source <- guides_source$guides
  }

  if (!is.list(guides_source) || length(guides_source) == 0) {
    return(list())
  }

  lapply(names(guides_source), function(aes) {
    g <- guides_source[[aes]]
    res <- list(
      `_id` = generate_stable_id("guide", aes, prefix = "guide"),
      aesthetic = aes
    )

    if (is.character(g)) {
      res$type <- g
    } else if (inherits(g, "ggproto") || inherits(g, "guide")) {
      cls <- class(g)[1]
      res$type <- tolower(gsub("^Guide", "", cls))
      res$title <- if (!is.null(g$title)) as.character(g$title) else NULL
      res$position <- if (!is.null(g$position)) as.character(g$position) else NULL
      res$direction <- if (!is.null(g$direction)) as.character(g$direction) else NULL
      res$reverse <- if (!is.null(g$reverse)) as.logical(g$reverse) else NULL
      res$order <- if (!is.null(g$order)) as.integer(g$order) else NULL
    } else {
      res$type <- "unknown"
    }

    res
  })
}

# =============================================================================
# Helper Functions
# =============================================================================

#' @keywords internal
DEFAULT_DPI <- 72

#' @title Add Stable IDs to Nested List
#' @description
#' Recursively traverses a list and adds `_id` fields where missing
#' based on content hashing.
#' @param x List to process.
#' @param prefix Optional prefix for IDs.
#' @return Modified list.
#' @keywords internal
add_stable_ids <- function(x, prefix = NULL) {
  if (!is.list(x)) return(x)
  
  # Recurse first
  res <- lapply(x, function(val) {
    if (is.list(val) && !inherits(val, "z_schema")) {
      add_stable_ids(val, prefix = prefix)
    } else {
      val
    }
  })
  
  # If it looks like a component that needs an ID and doesn't have one
  if (length(res) > 0 && !is.null(names(res)) && !any(names(res) == "_id")) {
    content_summary <- paste(names(res), collapse = "_")
    res$`_id` <- generate_stable_id("component", content_summary, prefix = prefix)
  }
  
  res
}


#' @title Sanitize Object for JSON Serialization
#' @description
#' Standardizes R objects for consistent JSON serialization, especially
#' for ggplot2 elements like units and margins.
#' @param x Object to sanitize.
#' @param plot_dims Optional list with width and height in inches.
#' @return A sanitized list or vector.
#' @keywords internal
sanitize_for_json <- function(x, plot_dims = list(width = 8, height = 6)) {
  if (is.null(x)) return(NULL)
  
  if (inherits(x, "margin")) {
    # Convert margin to numeric vector (t, r, b, l)
    return(as.numeric(x))
  }
  
  if (inherits(x, "unit")) {
    # Convert units to numeric (px or inches etc.)
    return(as.numeric(x))
  }
  
  if (is.list(x)) {
    return(lapply(x, sanitize_for_json, plot_dims = plot_dims))
  }
  
  if (is.atomic(x)) {
    if (length(x) > 1) return(as.list(x))
    return(x)
  }
  
  # Fallback: convert to character to avoid serialization errors
  as.character(x)
}

#' @title Generate Stable ID
#' @description
#' Generates a stable unique identifier for a plot element.
#' @param type Type of element (e.g., "layer", "guide").
#' @param ... Components to include in the ID hash.
#' @param prefix Optional prefix for the ID.
#' @return A stable ID string.
#' @importFrom digest digest
#' @keywords internal
generate_stable_id <- function(type, ..., prefix = NULL) {
  components <- c(as.character(type), as.character(list(...)))
  # Use digest from the package
  hash <- substr(digest::digest(paste(components, collapse = "_")), 1, 8)
  if (!is.null(prefix)) {
    paste0(prefix, "_", hash)
  } else {
    hash
  }
}

