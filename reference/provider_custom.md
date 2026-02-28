# Custom Provider Factory

A dynamic factory for creating custom provider instances. This allows
users to instantiate a model provider at runtime by configuring the
endpoint (`base_url`), credentials (`api_key`), network protocol/routing
(`api_format`), and specific capabilities (`use_max_completion_tokens`),
without writing a new Provider class.
