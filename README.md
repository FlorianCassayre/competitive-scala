competitive-scala
===

My personal repertoire of datastructures and utilities written in pure Scala.

* All implementations are pure, with the exception of randomized algorithms which rely on an impure random number generator oracle.
* All implementations are asymptotically optimal up to a `log^k(n)` factor. Most of the time the chosen algorithm is indeed optimal, however the operations on the underlying immutable datastructures may require additional time (compared to their impure counterpart), thus the log factor.
* All implementations are self-contained: no external libraries are used and files are isolated from each other.

## Repertoire

#### Geometry

* [Point in polygon](src/main/scala/competitivescala/geometry/PointPolygon.scala)

#### Graphs

* [Maximum bipartite matching (unweighted)](src/main/scala/competitivescala/graphs/bipartite/BipartiteMatchingUnweighted.scala)
* [Minimum spanning tree (undirected)](src/main/scala/competitivescala/graphs/MinimumSpanningTree.scala)
* [Shortest path](src/main/scala/competitivescala/graphs/ShortestPath.scala)
  * Single source, positive weights, optional early stopping (Dijkstra)
  * Single source, negative weights and negative loop detection (Bellman-Ford)
* [Topological sort](src/main/scala/competitivescala/graphs/TopologicalSort.scala)
* [Union-find data structure](src/main/scala/competitivescala/graphs/UnionFind.scala)

#### Number theory

* [Combinatorics](src/main/scala/competitivescala/numbers/Combinatorics.scala)
  * Count combinations and permutations
* [Fast Fourier transform (FFT)](src/main/scala/competitivescala/numbers/FFT.scala)
  * Transformation and its inverse
  * Convolution
* [Matrix arithmetic](src/main/scala/competitivescala/numbers/MatrixArithmetic.scala)
  * Dimensions, identity and rotations
  * Multiplication and element-wise operations
  * Gaussian elimination and inverse
* [Modular arithmetic](src/main/scala/competitivescala/numbers/ModularArithmetic.scala)
  * GCD and LCM
  * Modular inverse, Bézout identity and Chinese remainder
  * Fast exponentiation, sum of powers
  * Randomized Miller-Rabin primality test
  * Discrete logarithm
* [Rational arithmetic](src/main/scala/competitivescala/numbers/RationalArithmetic.scala)

#### Arrays, sequences and strings

* [Arithmetic expression parser (shunting-yard)](src/main/scala/competitivescala/strings/parsing/ShuntingYard.scala)
* [JSON parser (recursive)](src/main/scala/competitivescala/strings/parsing/JsonParser.scala)
* [Run-length encoding/decoding](src/main/scala/competitivescala/strings/RunLength.scala)
* [String searching](src/main/scala/competitivescala/strings/StringSearch.scala)
  * Longest prefix also a suffix
  * Knuth-Morris-Pratt (KMP) search
  * Palindrome search
* [Suffixes](src/main/scala/competitivescala/strings/SuffixArray.scala)
  * Suffix array
  * Burrows-Wheeler transform and its inverse
  * Rotation array
  * Longest common prefix (LCP) array

#### Other utilities

* [Binary search](src/main/scala/competitivescala/utils/BinarySearch.scala)
  * Binary search and its four variants
* [Cartesian coordinates](src/main/scala/competitivescala/utils/Vectors.scala)
  * Points and vectors
  * Axis-aligned bounding boxes (AABB)
* [Fixed point and orbit](src/main/scala/competitivescala/utils/FixedPoint.scala)
* [Hexagonal grid](src/main/scala/competitivescala/utils/HexGrid.scala)
* [Memoization](src/main/scala/competitivescala/utils/Memoization.scala)
* [Rhombic dodecahedral grid](src/main/scala/competitivescala/utils/RhombicDodecahedralGrid.scala)
* [Roman numerals from/to integer](src/main/scala/competitivescala/utils/RomanNumerals.scala)
* [SAT solving (DPLL)](src/main/scala/competitivescala/utils/SAT.scala)
* [Space filling curves](src/main/scala/competitivescala/utils/SpaceFillingCurves.scala)
  * Binary index curve
  * Gray code curve
  * Hilbert curve
* [State](src/main/scala/competitivescala/utils/State.scala)
