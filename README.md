# Pandora (v1.0.0)

A simple library for building, parsing, serializing, traversing xml.
Currently the xml traversing component is one the weaker side.

I mostly used it for parse & serialize XML payloads from AWS.

## Limitations

### No schema validation

The only thing you should be aware of is this library has no
intention to support validating a xml document against a schema,
it'll parse any inlined DTD information, but it won't do anything
with it.

### Only UTF-8 support

Currently only UTF-8 is supported, a large amount of work will be
necessary to change that, but happy to open up support to different
encoding.

