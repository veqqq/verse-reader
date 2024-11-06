
Inspired by [https://github.com/layeh/kjv](https://github.com/layeh/kjv) and [https://github.com/lukesmithxyz/kjv](https://github.com/lukesmithxyz/kjv)

### To Use

- `raco exe -o kjv bible-parse.rkt` builds
- `./kjv gen 1:1` to show verse. It shows full books, allows partial names phil, matt etc.

- `racket interpreted-bible-parse.rkt gen 1` to validate new data. #ToDo: File's currently hardcoded, have both versions accept a file argument and and use that for program name, usage etc.

### To Do
- potentially change format to have chapter and verse together as 1:1, (even book), maybe faster execution as you just match to the exact line instead of checking multiple elements? 
- many #ToDo:s littered about in `bible-parse.rkt`, soon binaries and further books will be available.

- change BoM etc. abbreviations to remove spaces: 1 ne -> 1ne

### Texts

From [layeh](https://github.com/layeh):

- Clementine Vulgate
- O.T. Septuagint and [SBL Greek New Testament](https://en.wikipedia.org/wiki/SBL_Greek_New_Testament)

From [Luke Smith](https://github.com/LukeSmithxyz):

- KJV + Apocrophya (n.b. I eliminated 1 line in Sirach: "A Prayer to...")

From [Mormon Documentation Project](https://github.com/mormon-documentation-project/lds-scriptures/):
- Book of Mormon (and soon others) (n.b. Nephi 3 and 4 are corrupted in almost all online sources; my initial attempts to scrape the whole book failed here too. I was able to easily fix the .csv here though!)

### Performance


Now with hyperfine (and my processor seems to be dying):

|                                                  | time                | user     | sys      | size (MB) |
| ------------------------------------------------ | ------------------- | ---------| -------- | ----------|
| Current                                          | 222.1 ms ±   2.9 ms ms  | 168.9 ms | 52.6 ms | 11.2      |
| Common Lisp `bible-parse.lisp`                   | 10.0 ms ±   2.3 ms  | 6.5 ms   | 3.8 ms   | 63.6      |
| Golang with embed    ([commit](https://github.com/veqqq/verse-reader/commit/30d78e839e5a284606605e245b67324595d7091d))                    | 30.8 ms ±   2.4 ms  | 34.1 ms  | 10.8 ms  | 7.6       |
| Golang `bible-parse.go` data structs in file     | 3.6 ms ±   1.1 ms   | 1.3 ms   | 2.1 ms   | 9.1       |
| My Comp. parse                                   | 268.6 ms ±  10.4 ms | 207.4 ms | 61.3 ms  | 10.7      |
| Ryan's Comp. parse                               | 275.3 ms ±  10.3 ms | 221.5 ms | 53.9 ms  | 12        |
| Ryan's lines                                     | 798.6 ms ±  21.1 ms | 727.5 ms | 70.7 ms  | 11.6      |
| Interp.                                          | 647.2 ms ±  6.0 ms  | 608.2 ms | 43.8 ms  |           |
| [reference](https://github.com/lukesmithxyz/kjv) | 158.2 ms ±  33.5 ms | 203.3 ms | 29.8 ms  | 1.6       |

1.11.24

|                                                  | real   | user   | sys        | size (MB) |
| ------------------------------------------------ | ------ | ------ | ---------- | ----------|
| current (bookless)                               | 0.285s | 0.229s | 0.056s     | 11.43     |
| [reference](https://github.com/lukesmithxyz/kjv) | 0.114s | 0.139s | 0.023s     |           |
| Interp.                                          | 0.757s | 0.683s | 0.074s     |           |
| R. lines                                         | 0.777s | 0.704s | 0.074s     |           |
| R.comp. parse                                    | 0.293s | 0.234s | 0.064s     | 12        |
| My comp. parse                                   | 0.292s | 0.216s | 0.076s     | 12        |


- current: `time ./kjvr gen 1`
- reference in bash/awk:`time PAGER=cat kjv gen 1`
- interpreted with file: `time racket interpreted-bible-parse.rkt gen 1`
- [Ryan](https://www.reddit.com/r/Racket/comments/1gh0z38/how_to_embed_data_from_file_into_static_binary/lux9aob/)'s line version note, extremely fast fast compilation!
- [Ryan](https://www.reddit.com/r/Racket/comments/1gh0z38/how_to_embed_data_from_file_into_static_binary/lux9aob/)'s comp parse version, 3-4s compilation and great execution. Now the base.
- [my old](#dc9063a9a27227f3f2848f8c98974825c2fd58b6) comp time attempt without `#:prefab`



| **Data Cleaners**                                | time                | user     | sys      | size (MB) |
| ------------------------------------------------ | ------------------- | ---------| -------- | ----------|
| lds-scrip-extractor.rkt   w/ optimizations --cs  | 1.062 s ±  0.013 s  | 1.004 s  | 00.057 s | 9.4       |
| lds-scrip-extractor.rkt                          | 1.843 s ±  0.724 s  | 1.730 s  | 0.104 s  | 12.1      |
| lds-scrip-extractor.rkt --orig-exe      --cs     | 1.080 s ±  0.022 s  | 1.024 s  | 0.057 s  | 55.3      |
| lds-scrip-extractor.lisp                         | 393.8 ms ±  53.4 ms | 291.2 ms | 44.4 ms  | 39.8      |
| lds-scrip-extractor.lisp  (speed 3) (safety 0)   | 287.1 ms ±  32.5 ms | 188.6 ms | 25.2 ms  | 39.7      |