#### 0.7.0.2

- Added support for GHC 8.

#### 0.7.0.1

- Added support for `dlist-0.8`.

## 0.7

- Changed `Element` constructor by adding `elSmoothingGroup`.

## 0.6

- `Face` has just a single constructor now; pattern synonmys are available to pattern match against
  `Triangle` and `Quad`.

## 0.5.1

- Exported all useful constructors and symbols.

# 0.5

#### Breaking changes

- `objFaces` now contain structured faces of type `Face`. A `Face` can be:
  * a `Triangle` ;
  * a `Quad` ;
  * an arbritrary `Polygon`.
  Whatever the shape of the face, it holds several `FaceIndex` used to reference locations, normals
  and texture coordinates.
- Ditto for `objLines`.

### 0.4.0.1

- Fixed a bug in the implementation of `untilEnd`.

# 0.4

- Removed most modules from the exposed interface. Everything can be found in Codec.Wavefront.
- Changed internal structures of a few types. The structure of those types shouldn’t be used in the
  interface, so a few functions to access them was provided.

# 0.3

#### Breaking changes

- Changed the interface to manipulate `WavefrontOBJ`. It’s now a dedicated type with `Vector`
  instead of `DList`, which is way better.

# 0.2

#### Non-breaking changes

- Added more verbose documentation everywhere.

#### Breaking changes

- Removed `ctxtName`. It was an old function used to implement user-defined
  objects, but since we have `Element`, we don’t need those anymore.

### 0.1.0.2

- Changed the loop of `tokenize` from `many1` to `untilEnd` (internal parser in Token.hs). That’s
  due to the fact `many1` silently ignores failures while `untilEnd` does not.
- Changed implementation of `tokenize` to use `choice`, which is implemented exactly as we had.
- Removed `identifier` and use `name` instead to relax conditions on formatting names.

### 0.1.0.1

- Added forgotten Codec.Wavefront.

# 0.1

- Initial revision.
