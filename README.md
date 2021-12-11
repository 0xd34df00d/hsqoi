# hsqoi

Haskell implementatin of the [QOI image format](https://phoboslab.org/log/2021/11/qoi-fast-lossless-image-compression).

## Benchmarks

The baseline is the [reference C code](https://github.com/phoboslab/qoi.git) @ [`e9069e`](https://github.com/phoboslab/qoi/commit/e9069e11a43d779b418679c7a50b2ec14f652085).

Taking a single big `.png` file (5616×3744) and taking a minimum of 5 runs gives, on Core i7 3930k @ 4.0 GHz:

| Implementation                   | Decoding, ms | , % of best C | Encoding, ms | , % of best C |
|----------------------------------|--------------|---------------|--------------|---------------|
| C, gcc 11, `-O3 -march=native`   | 228          | 117%          | 264          | 102%          |
| C, gcc 11, `-O3`                 | 230          | 118%          | 260          | 100%          |
| C, clang 13, `-O3 -march=native` | 211          | 108%          | 356          | 137%          |
| C, clang 13, `-O3`               | 195          | 100%          | 341          | 131%          |
| Haskell                          | 184          |  94%          | 240          |  90%          |

On Ryzen 3700X:

| Implementation                   | Decoding, ms | , % of best C | Encoding, ms | , % of best C |
|----------------------------------|--------------|---------------|--------------|---------------|
| C, gcc 11, `-O3 -march=native`   | 179          | 121%          | 198          | 101%          |
| C, gcc 11, `-O3`                 | 174          | 118%          | 196          | 100%          |
| C, clang 13, `-O3 -march=native` | 158          | 107%          | 266          | 136%          |
| C, clang 13, `-O3`               | 148          | 100%          | 252          | 129%          |
| Haskell                          | 148          | 100%          | 190          |  97%          |
