# Morpheus GraphQL Benchmark

this Package Provides Benchmark for Morpheus GrapHQL

## Parsing Type System

|                      | GraphQL (mean) | GraphQL (std dev) | Morpheus (mean) | Morpheus (std dev) |
| -------------------- | -------------- | ----------------- | --------------- | ------------------ |
| Mythology Schema     | 1.087 ms       | 81.11 μs          | 406.3 μs        | 15.54 μs           |
| Long Descriptions    | 10.59 ms       | 200.1 μs          | 489.1 μs        | 15.40 μs           |
| Star Wars Schema     | 85.95 ms       | 2.369 ms          | 42.65 ms        | 1.425 ms           |
| Nested Wrapper Types | 1.487 s        | 4.659 ms          | 58.37 μs        | 2.127 μs           |
