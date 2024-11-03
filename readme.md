
Inspired by [https://github.com/layeh/kjv](https://github.com/layeh/kjv) and [https://github.com/lukesmithxyz/kjv](https://github.com/lukesmithxyz/kjv)

### To Use

- `raco exe -o kjv bible-parse.rkt` builds
- `./kjv gen 1:1` to show verse. It shows full books, allows partial names phil, matt etc.

- `racket interpreted-bible-parse.rkt gen 1` to validate new data. #ToDo: File's currently hardcoded, have both versions accept a file argument and and use that for program name, usage etc.

### To Do
- potentially change format to have chapter and verse together as 1:1, (even book), maybe faster execution as you just match to the exact line instead of checking multiple elements? 
- many #ToDo:s littered about in `bible-parse.rkt`, soon binaries and further books will be available.

### Texts

From [layeh](https://github.com/layeh):

- Clementine Vulgate
- O.T. Septuagint and [SBL Greek New Testament](https://en.wikipedia.org/wiki/SBL_Greek_New_Testament)

From [Luke Smith](https://github.com/LukeSmithxyz):

- KJV + Apocrophya (n.b. I eliminated 1 line in Sirach: "A Prayer to...")


### Performance


3.11.24 now with hyperfine (and my processor seems to be dy):

|                                                  | time                | user     | sys      | size (MB) |
| ------------------------------------------------ | ------------------- | ---------| -------- | ----------|
| Current (bookless)                               | 249.6 ms ±  9.0 ms  | 199.2 ms | 50.3 ms  | 11.6      |
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
