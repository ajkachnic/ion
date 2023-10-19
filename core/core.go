package core

func Parse(source string) ([]astNode, error) {
	tokenizer := NewTokenizer(source)
	tokens := tokenizer.Tokenize()

	parser := NewParser(tokens)
	return parser.parse()
}

func Compile(ctx *Context, ast []astNode) (*Compiler, *compileError) {
	compiler := NewCompiler(ctx)
	compiler.compileProgram(ast)

	return &compiler, nil
}

func CompileWithContext(ctx *Context, ast []astNode, symbols *SymbolTable, constants []Value) (*Compiler, *compileError) {
	compiler := NewCompiler(ctx)
	compiler.symbolTable = symbols
	compiler.constants = constants
	compiler.compileProgram(ast)

	return &compiler, nil
}

func Interpret(ctx *Context, source string) error {
	ast, err := Parse(source)
	if err != nil {
		return err
	}

	compiler, err := Compile(ctx, ast)
	if err != nil {
		return err
	}
	globals := make([]Value, GlobalsSize)

	vm := NewVM(compiler.Bytecode(), globals, ctx)
	return vm.Run()
}

func Execute(ctx *Context, code *Bytecode, _globals *[]Value) error {
	globals := make([]Value, GlobalsSize)
	if _globals != nil {
		globals = *_globals
	}

	vm := NewVM(code, globals, ctx)
	return vm.Run()
}
