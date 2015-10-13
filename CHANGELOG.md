# 0.3

#### Breaking changes

- Changed the interface to manipulate `WavefrontOBJ`. It’s now a dedicated type with `Vector`
  instead of `DList`, which is way better.

# 0.2

#### Non-breaking changes

- Added more verbose documentation everywhere.

#### Breaking changes

- Removed `ctxtName`. It was an old function used to implement user-defined
  objects, but since we have `Element`, we don’t those anymore.

### 0.1.0.2

- Changed the loop of `tokenize` from `many1` to `untilEnd` (internal parser in Token.hs). That’s
  due to the fact `many1` silently ignores failures while `untilEnd` does not.
- Changed implementation of `tokenize` to use `choice`, which is implemented exactly as we had.
- Removed `identifier` and use `name` instead to relax conditions on formatting names.

### 0.1.0.1

- Added forgotten Codec.Wavefront.

# 0.1

- Initial revision.
