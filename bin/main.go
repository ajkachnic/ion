package main

import (
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"strings"

	"github.com/fatih/color"
	"github.com/reeflective/readline"

	"github.com/ajkachnic/ion/core"
	"github.com/ajkachnic/ion/modules"
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
	ast, err := core.Parse(string(content))

	if err != nil {
		fmt.Println(err.Error())
		os.Exit(1)
	}

	if *debugAst {
		for _, node := range ast {
			fmt.Println(node)
		}
	}
	context := core.NewContext(path)
	context.LoadBuiltins()

	modules.Initialize(&context)

	compiler, e := core.Compile(&context, ast)

	if *debugBytecode {
		fmt.Println(compiler.Bytecode().Instructions)
	}

	if e != nil {
		fmt.Println(e.ErrorWithContext(string(content)))
		os.Exit(1)
	}

	err = core.Execute(&context, compiler.Bytecode(), nil)
	if err != nil {
		fmt.Println(err.Error())
		os.Exit(1)
	}
}

func repl() {
	rl := readline.NewShell()
	rl.Prompt.Primary(func() string { return "> " })
	rl.SyntaxHighlighter = highlight

	symbolTable := core.NewSymbolTable()
	constants := []core.Value{}
	globals := make([]core.Value, core.GlobalsSize)

	context := core.NewContext("<stdin>")
	context.LoadBuiltins()

	modules.Initialize(&context)

	for i, v := range context.Builtins {
		symbolTable.DefineBuiltin(i, v.Name)
	}

	for {
		text, err := rl.Readline()

		if err == io.EOF {
			break
		} else if err != nil {
			fmt.Println(err)
			break
		}

		ast, err := core.Parse(text)

		if err != nil {
			fmt.Println(err.Error())
			continue
		}

		if *debugAst {
			for _, node := range ast {
				fmt.Println(node)
			}
		}

		compiler, e := core.CompileWithContext(&context, ast, symbolTable, constants)

		if *debugBytecode {
			fmt.Println(compiler.Bytecode().Instructions)
		}

		if e != nil {
			fmt.Println(e.ErrorWithContext(text))
			continue
		}

		vm := core.NewVM(compiler.Bytecode(), globals, &context)
		if err = vm.Run(); err != nil {
			fmt.Println(err.Error())
			continue
		}

		fmt.Println(vm.LastPoppedStackElem())
		constants = vm.Constants
	}
}

func highlight(line []rune) string {
	tokenizer := core.NewTokenizer(string(line))
	tokens := tokenizer.Tokenize()

	builder := strings.Builder{}

	i := uint(0)
	for tok_i, token := range tokens {
		if uint(token.Pos.Offset) > i {
			builder.WriteString(string(line)[i:token.Pos.Offset])
		}
		if tok_i+1 == len(tokens) && line[len(line)-1] != ',' {
			continue
		}

		switch token.Kind {
		case core.STRING_LITERAL:
			builder.WriteString(color.GreenString("\"%s\"", token.Payload))
		case core.NUMBER_LITERAL:
			builder.WriteString(color.MagentaString(token.Payload))
		case core.IDENTIFIER:
			builder.WriteString(token.Payload)
		default:
			builder.WriteString(token.String())
		}

		i = uint(token.Pos.Offset) + token.Length

	}

	return builder.String()
}
