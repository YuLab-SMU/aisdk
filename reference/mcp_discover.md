# Distributed MCP Ecosystem

Service discovery and dynamic composition for MCP (Model Context
Protocol) servers. Implements mDNS/DNS-SD discovery, skill negotiation,
and hot-swapping of tools at runtime.

Factory function to create an MCP discovery instance.

## Usage

``` r
mcp_discover(registry_url = NULL)
```

## Arguments

- registry_url:

  Optional URL for remote skill registry.

## Value

An McpDiscovery object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create discovery instance
discovery <- mcp_discover()

# Scan local network
services <- discovery$scan_network()

# Register a known endpoint
discovery$register("my-server", "localhost", 3000)

# List all discovered endpoints
discovery$list_endpoints()
} # }
```
