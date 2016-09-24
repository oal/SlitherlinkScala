# Slitherlink Scala, DAT233
University of Agder, fall 2016

Made by:
- André Torsvik
- Olav Andreas Lindekleiv
- André Hauge Jegtvolden

### Puzzles
Puzzles are placed in the "input" directory, and are written to "output" as soon as all puzzles in a file are solved.

### How to run it
First, cd into the SlitherlinkScala directory, then run:

```
mkdir out
scalac -cp . src/*.scala -d out/
scala -cp out Runner
```