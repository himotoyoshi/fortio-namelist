Examples
========

Small, self-contained scripts showing how to use `fortio-namelist`.
They are meant to be read and run, not to serve as a test suite.

Run them from this directory:

    ruby 01_read.rb

(If the gem is not installed, run them against the source tree instead:
`ruby -I../lib 01_read.rb`)

| file | what it shows |
| ---- | ------------- |
| [sample.nml](sample.nml)                       | the namelist file the scripts read |
| [01_read.rb](01_read.rb)                       | `parse` - namelist file to Hash, whole file or selected groups, from a String or an IO |
| [02_write.rb](02_write.rb)                     | `dump` - Hash to namelist string, writing it to a file, round trip |
| [03_format_options.rb](03_format_options.rb)   | every format option of `dump` side by side |
| [04_scan.rb](04_scan.rb)                       | `scan` - group/variable names and line numbers without reading the values |
| [05_filter.rb](05_filter.rb)                   | `filter` - read, modify, write back in one step |

A good order is 01 -> 02 -> 03; 04 and 05 are independent of each other.
