# Wrap Reactive Tools

Wraps reactive tools to inject reactiveValues and session into their
execute functions. Call this in your Shiny server before passing tools
to aiChatServer.

## Usage

``` r
wrap_reactive_tools(tools, rv, session)
```

## Arguments

- tools:

  List of Tool objects, possibly including ReactiveTool objects.

- rv:

  The reactiveValues object to inject.

- session:

  The Shiny session object to inject.

## Value

List of wrapped Tool objects ready for use.
