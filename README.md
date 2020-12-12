# webmention

Types and functions for working with
[Webmention](https://www.w3.org/TR/webmention), which pass the
[webmention.rocks](https://webmention.rocks/) test suite.

- `webmention` function constructs a valid `Webmention` type.
- `notify` function sends a `Webmention`, handling endpoint discovery.
- `verify` function verifies a `Webmention` source mentions its target.
