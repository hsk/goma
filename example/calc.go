package main

import "fmt"
import "time"

type EInt struct {x int }
type EAdd struct {x E; y E}
type EMul struct {x E; y E}

type E interface { eval() int }
func (p *EInt)     eval() int { return p.x }
func (p *EAdd)     eval() int { return p.x.eval() + p.y.eval() }
func (p *EMul)     eval() int { return p.x.eval() * p.y.eval() }

func main() {

  i:= EInt{1}
  fmt.Printf("eval 1 = %d\n", i.eval())

  add:=EAdd{&EInt{1}, &EInt{2}}
  fmt.Printf("eval 1 + 2 = %d\n", add.eval())

  mul:=EMul{&EAdd{&EInt{1},&EInt{2}}, &EInt{111}}
  fmt.Printf("eval (1 + 2) * 111 = %d\n", mul.eval())

}
