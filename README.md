# Goma Programming Language

Goma is next generation low level Programming Language.

Goma translate to c++.

## install

    $ make

## hello world

example/hello.goma

```
include stdio.h
main():int = {
  printf("hello world\n")
  return 0
}
```

    $ ./gomac example/hello.goma example/hello.cpp
    $ g++ example/hello.cpp -o hello
    $ ./hello
    hello world!

example/hello.cpp

```
#include <stdio.h>
int main() {
  printf("hello world!\n");
  return 0;
}
```

