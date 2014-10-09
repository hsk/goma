# Goma Programming Language

Goma is next generation low level Programming Language.

Goma translate to c++.

## install

    $ omake

## hello world

hello.goma

```
include stdio.h
main():int = {
  printf("hello world\n")
  return 0
}
```

    $ ./gomac hello.goma hello.cpp
    $ g++ hello.cpp
    $ ./a.out
    hello world!

hello.cpp

```
#include <stdio.h>
int main() {
  printf("hello world!\n");
  return 0;
}
```

