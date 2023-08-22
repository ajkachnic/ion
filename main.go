package main

import (
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"os"

	"github.com/chzyer/readline"
)

const version = "0.0.1"

const helpMessage = `ion is a tiny scripting langauge.

Usage:
  ion <file> [args]
`

var debugAst = flag.Bool("debug-ast", false, "print AST")
var debugBytecode = flag.Bool("debug-bytecode", false, "print bytecode")

func main() {
	flag.Usage = func() {
		fmt.Printf(helpMessage)
		flag.PrintDefaults()
	}

	flag.Parse()

	args := flag.Args()

	if len(args) == 0 {
		repl()
	} else {
		runFile(args[0])
	}
}

func runFile(path string) {
	content, err := ioutil.ReadFile(path)
	if err != nil {
		fmt.Println(err.Error())
		os.Exit(1)
	}

	tokenizer := newTokenizer(string(content))
	tokens := tokenizer.Tokenize()

	parser := NewParser(tokens)
	ast, err := parser.parse()

	if err != nil {
		fmt.Println(err.Error())
		os.Exit(1)
	}

	if *debugAst {
		for _, node := range ast {
			fmt.Println(node)
		}
	}
	context := NewContext(path)
	context.LoadBuiltins()

	compiler := NewCompiler(&context)
	err = compiler.compileProgram(ast)
	globals := make([]Value, GlobalsSize)

	if *debugBytecode {
		fmt.Println(compiler.bytecode().instructions)
	}

	if err != nil {
		fmt.Println(err.Error())
		os.Exit(1)
	}

	vm := NewVM(compiler.bytecode(), globals, &context)
	if err = vm.run(); err != nil {
		fmt.Println(err.Error())
		os.Exit(1)
	}
}

func repl() {
	rl, err := readline.New(">> ")
	if err != nil {
		panic(err)
	}
	defer rl.Close()

	symbolTable := NewSymbolTable()
	constants := []Value{}
	globals := make([]Value, GlobalsSize)

	context := NewContext("<stdin>")
	context.LoadBuiltins()

	for i, v := range context.builtins {
		symbolTable.defineBuiltin(i, v.name)
	}

	for {
		text, err := rl.Readline()

		if err == io.EOF {
			break
		} else if err != nil {
			fmt.Println(err)
			break
		}

		tok := newTokenizer(text)
		tokens := tok.Tokenize()

		parser := NewParser(tokens)
		ast, err := parser.parse()

		if err != nil {
			fmt.Println(err.Error())
			continue
		}

		for _, node := range ast {
			fmt.Println(node)
		}

		compiler := NewCompiler(&context)
		compiler.symbolTable = symbolTable
		compiler.constants = constants

		err = compiler.compileProgram(ast)

		fmt.Println(compiler.currentInstructions())

		if err != nil {
			fmt.Println(err.Error())
			continue
		}

		vm := NewVM(compiler.bytecode(), globals, &context)
		if err = vm.run(); err != nil {
			fmt.Println(err.Error())
			continue
		}

		fmt.Println(vm.lastPoppedStackElem())
		constants = vm.constants
	}
}
