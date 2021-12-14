# hsqoi

Haskell implementatin of the [QOI image format](https://phoboslab.org/log/2021/11/qoi-fast-lossless-image-compression).

## Benchmarks

The baseline is the [reference C code](https://github.com/phoboslab/qoi.git) @ [`e9069e`](https://github.com/phoboslab/qoi/commit/e9069e11a43d779b418679c7a50b2ec14f652085).

Taking a single big `.png` photo (5616×3744), with either 3 (RGB) or 4 (RGBA) channels
and taking a minimum of 5 runs gives, on Core i7 3930k @ 4.0 GHz:

| Implementation                   | Decoding, ms | , % of best C | Encoding, ms | , % of best C |
|----------------------------------|--------------|---------------|--------------|---------------|
| C, gcc 11, `-O3 -march=native`   | 228          | 117%          | 264          | 102%          |
| C, gcc 11, `-O3`                 | 230          | 118%          | 260          | 100%          |
| C, clang 13, `-O3 -march=native` | 211          | 108%          | 356          | 137%          |
| C, clang 13, `-O3`               | 195          | 100%          | 341          | 131%          |
| Haskell, 3-channel               | 172          |  88%          | 226          |  87%          |
| Haskell, 4-channel               | 187          |  96%          | 209          |  80%          |

Similarly for a [5120x2880 artwork](https://raw.githubusercontent.com/KDE/breeze/6d4fe7781790c69758be380324262261699894f7/wallpapers/Next/contents/images/5120x2880.png):

| Implementation                   | Decoding, ms | , % of best C | Encoding, ms | , % of best C |
|----------------------------------|--------------|---------------|--------------|---------------|
| C, gcc 11, `-O3 -march=native`   | 90           | 130%          | 73           | 102%          |
| C, gcc 11, `-O3`                 | 88           | 128%          | 71           | 100%          |
| C, clang 13, `-O3 -march=native` | 69           | 100%          | 124          | 175%          |
| C, clang 13, `-O3`               | 70           | 101%          | 124          | 175%          |
| Haskell, 3-channel               | 55           |  79%          | 70           |  99%          |
| Haskell, 4-channel               | 54           |  78%          | 72           | 102%          |

On Ryzen 3700X, for the photo:

| Implementation                   | Decoding, ms | , % of best C | Encoding, ms | , % of best C |
|----------------------------------|--------------|---------------|--------------|---------------|
| C, gcc 11, `-O3 -march=native`   | 179          | 121%          | 198          | 101%          |
| C, gcc 11, `-O3`                 | 174          | 118%          | 196          | 100%          |
| C, clang 13, `-O3 -march=native` | 158          | 107%          | 266          | 136%          |
| C, clang 13, `-O3`               | 148          | 100%          | 252          | 129%          |
| Haskell, 3-channel               | 132          |  89%          | 191          |  97%          |
| Haskell, 4-channel               | 145          |  98%          | 141          |  72%          |

For the artwork:

| Implementation                   | Decoding, ms | , % of best C | Encoding, ms | , % of best C |
|----------------------------------|--------------|---------------|--------------|---------------|
| C, gcc 11, `-O3 -march=native`   |  68          | 117%          |  56          | 102%          |
| C, gcc 11, `-O3`                 |  66          | 114%          |  55          | 100%          |
| C, clang 13, `-O3 -march=native` |  60          | 103%          |  96          | 175%          |
| C, clang 13, `-O3`               |  58          | 100%          |  90          | 164%          |
| Haskell, 3-channel               |  48          |  83%          |  54          |  98%          |
| Haskell, 4-channel               |  48          |  83%          |  52          |  95%          |
