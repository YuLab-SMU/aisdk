# MCP Discovery Class

R6 class for discovering MCP servers on the local network using
mDNS/DNS-SD (Bonjour) protocol.

## Public fields

- `discovered`:

  List of discovered MCP endpoints.

- `registry_url`:

  URL of the remote skill registry.

## Methods

### Public methods

- [`McpDiscovery$new()`](#method-McpDiscovery-new)

- [`McpDiscovery$scan_network()`](#method-McpDiscovery-scan_network)

- [`McpDiscovery$register()`](#method-McpDiscovery-register)

- [`McpDiscovery$query_capabilities()`](#method-McpDiscovery-query_capabilities)

- [`McpDiscovery$list_endpoints()`](#method-McpDiscovery-list_endpoints)

- [`McpDiscovery$search_registry()`](#method-McpDiscovery-search_registry)

- [`McpDiscovery$print()`](#method-McpDiscovery-print)

- [`McpDiscovery$clone()`](#method-McpDiscovery-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new MCP Discovery instance.

#### Usage

    McpDiscovery$new(registry_url = NULL)

#### Arguments

- `registry_url`:

  Optional URL for remote skill registry.

#### Returns

A new McpDiscovery object.

------------------------------------------------------------------------

### Method `scan_network()`

Scan the local network for MCP servers.

#### Usage

    McpDiscovery$scan_network(timeout_seconds = 5, service_type = "_mcp._tcp")

#### Arguments

- `timeout_seconds`:

  How long to scan for services.

- `service_type`:

  The mDNS service type to look for.

#### Returns

A data frame of discovered services.

------------------------------------------------------------------------

### Method `register()`

Register a known MCP endpoint manually.

#### Usage

    McpDiscovery$register(name, host, port, capabilities = NULL)

#### Arguments

- `name`:

  Service name.

- `host`:

  Hostname or IP address.

- `port`:

  Port number.

- `capabilities`:

  Optional list of capabilities.

#### Returns

Self (invisibly).

------------------------------------------------------------------------

### Method `query_capabilities()`

Query a discovered server for its capabilities.

#### Usage

    McpDiscovery$query_capabilities(host, port)

#### Arguments

- `host`:

  Hostname or IP.

- `port`:

  Port number.

#### Returns

A list of server capabilities.

------------------------------------------------------------------------

### Method `list_endpoints()`

List all discovered MCP endpoints.

#### Usage

    McpDiscovery$list_endpoints()

#### Returns

A data frame of endpoints.

------------------------------------------------------------------------

### Method `search_registry()`

Search the remote registry for skills.

#### Usage

    McpDiscovery$search_registry(query)

#### Arguments

- `query`:

  Search query.

#### Returns

A data frame of matching skills.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for McpDiscovery.

#### Usage

    McpDiscovery$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    McpDiscovery$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
