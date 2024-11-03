
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


3.11.24:

|                                                  | real   | user   | sys        | size (MB) |
| ------------------------------------------------ | ------ | ------ | ---------- | ----------|
| Current (bookless)                               | 0.390s | 0.290s | 0.100s     | 11.6      |
| R. comp. parse                                   | 0.420s | 0.307s | 0.113s     | 12.0      |
| My comp. parse                                   | 0.421s | 0.306s | 0.115s     | 12.0      |

These are 2.11.24. 3.11.24 on the same things are slower, worrying, perhaps my computer's dying....

|                                                  | real   | user   | sys        |
| ------------------------------------------------ | ------ | ------ | ---------- |
| current                                          | 0.293s | 0.234s | 0.064s     |
| [reference](https://github.com/lukesmithxyz/kjv) | 0.114s | 0.139s | 0.023s     |
| interp.                                          | 0.757s | 0.683s | 0.074s     |
| R. lines                                         | 0.777s | 0.704s | 0.074s     |
| R.comp. parse                                    | 0.293s | 0.234s | 0.064s     |
| My comp. parse                                   | 0.292s | 0.216s | 0.076s     |


- current: `time ./kjvr gen 1`
- reference in bash/awk:`time PAGER=cat kjv gen 1`
- interpreted with file: `time racket interpreted-bible-parse.rkt gen 1`
- [Ryan](https://www.reddit.com/r/Racket/comments/1gh0z38/how_to_embed_data_from_file_into_static_binary/lux9aob/)'s line version note, extremely fast fast compilation!
- [Ryan](https://www.reddit.com/r/Racket/comments/1gh0z38/how_to_embed_data_from_file_into_static_binary/lux9aob/)'s comp parse version, 3-4s compilation and great execution. Now the base.
- [my old](#dc9063a9a27227f3f2848f8c98974825c2fd58b6) comp time attempt without `#:prefab`