package main

import "fmt"
import "time"

func fib(a int) int {
  if a < 2 {
    return 1
  } else {
    return fib(a-2)+fib(a-1)
  }
}

type Fib interface {
  fib() int
}

type Int struct {x int}

func (p *Int) fib() int {
  if p.x < 2 {
    return 1
  } else {
    p1 := Int{p.x - 2}
    p2 := Int{p.x - 1}
    return p1.fib() + p2.fib()
  }
}

func main() {

  start := time.Now()
  result := fib(40)
  fmt.Printf("fib %d %d %s\n", 40, result, time.Now().Sub(start))

  start = time.Now()
  i:= Int{20}
  i.x = 40
  result = i.fib()
  fmt.Printf("fib %d %d %s\n", i.x, result, time.Now().Sub(start))

}
