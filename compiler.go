package main

import (
	"fmt"
	"strings"
)

type compileError struct {
	reason string
	position
}

func (e compileError) Error() string {
	return fmt.Sprintf("Compile error %s: %s", e.position, e.reason)
}

const LINE_CONTEXT = 5

func (e compileError) ErrorWithContext(source string) string {
	builder := strings.Builder{}
	sourceLines := strings.Split(source, "\n")
	line := e.position.line - 1  // index at 0
	column := e.position.col - 1 // index at 0

	topContext := LINE_CONTEXT
	if line < LINE_CONTEXT {
		topContext = line
	}

	for i, content := range sourceLines[line-topContext : line] {
		builder.WriteString(fmt.Sprintf("\x1b[2;90m%3d|\x1b[0;0m %s\n", line+1-topContext+i, content))
	}

	builder.WriteString(fmt.Sprintf("\x1b[2;90m%3d|\x1b[0;0m ", line+1))

	builder.WriteString(sourceLines[line][0:column])
	builder.WriteString("\x1b[31m")
	builder.WriteByte(sourceLines[line][column])
	builder.WriteString("\x1b[0m")
	builder.WriteString(sourceLines[line][column+1:])
	builder.WriteByte('\n')

	builder.WriteString(strings.Repeat(" ", column+5))
	builder.WriteString("\x1b[31m^\x1b[0m\n")
	builder.WriteString("\x1b[31m")
	builder.WriteString(e.reason)
	builder.WriteString("\x1b[0;0m")

	return builder.String()
}

type symbolScope string

const (
	LocalScope   symbolScope = "LOCAL"
	GlobalScope  symbolScope = "GLOBAL"
	BuiltinScope symbolScope = "BUILTIN"
	FreeScope    symbolScope = "FREE"
)

type CompilationScope struct {
	instructions        Instructions
	lastInstruction     emittedInstruction
	previousInstruction emittedInstruction
}

type Symbol struct {
	name  string
	scope symbolScope
	index int
}

type SymbolTable struct {
	outer       *SymbolTable
	store       map[string]Symbol
	freeSymbols []Symbol
}

func NewSymbolTable() *SymbolTable {
	return &SymbolTable{store: make(map[string]Symbol), freeSymbols: []Symbol{}}
}

func NewEnclosedSymbolTable(outer *SymbolTable) *SymbolTable {
	s := NewSymbolTable()
	s.outer = outer
	return s
}

func (s *SymbolTable) define(name string) Symbol {
	symbol := Symbol{name: name, index: len(s.store)}
	if s.outer == nil {
		symbol.scope = GlobalScope
	} else {
		symbol.scope = LocalScope
	}

	s.store[name] = symbol
	return symbol
}

func (s *SymbolTable) defineBuiltin(index int, name string) Symbol {
	symbol := Symbol{name: name, index: index, scope: BuiltinScope}
	s.store[name] = symbol
	return symbol
}

func (s *SymbolTable) defineFree(original Symbol) Symbol {

	s.freeSymbols = append(s.freeSymbols, original)

	symbol := Symbol{
		name:  original.name,
		index: len(s.freeSymbols) - 1,
		scope: FreeScope,
	}

	s.store[original.name] = symbol

	// fmt.Println("free ", s.freeSymbols)

	return symbol
}

func (s *SymbolTable) resolve(name string) (Symbol, bool) {
	obj, ok := s.store[name]
	if !ok && s.outer != nil {
		obj, ok = s.outer.resolve(name)
		if !ok {
			return obj, ok
		}

		if obj.scope == GlobalScope || obj.scope == BuiltinScope {
			return obj, ok
		}

		free := s.defineFree(obj)
		return free, true
	}

	return obj, ok
}

func (c *Compiler) loadSymbol(s Symbol) {
	switch s.scope {
	case GlobalScope:
		c.emit(OpGetGlobal, s.index)
	case LocalScope:
		c.emit(OpGetLocal, s.index)
	case BuiltinScope:
		c.emit(OpGetBuiltin, s.index)
	case FreeScope:
		c.emit(OpGetFree, s.index)
	}
}

type Bytecode struct {
	instructions Instructions
	constants    []Value
}

type Compiler struct {
	constants   []Value
	symbolTable *SymbolTable

	scopes     []CompilationScope
	scopeIndex int
}

type emittedInstruction struct {
	opcode   Opcode
	position int
}

func NewCompiler(context *context) Compiler {
	scope := CompilationScope{
		instructions:        Instructions{},
		lastInstruction:     emittedInstruction{},
		previousInstruction: emittedInstruction{},
	}

	symbolTable := NewSymbolTable()

	for i, builtin := range context.builtins {
		symbolTable.defineBuiltin(i, builtin.name)
	}

	return Compiler{
		constants:   []Value{},
		symbolTable: symbolTable,
		scopes:      []CompilationScope{scope},
		scopeIndex:  0,
	}
}

func (c *Compiler) bytecode() *Bytecode {
	return &Bytecode{
		instructions: c.currentInstructions(),
		constants:    c.constants,
	}
}

func (c *Compiler) enterScope() {
	scope := CompilationScope{
		instructions:        Instructions{},
		lastInstruction:     emittedInstruction{},
		previousInstruction: emittedInstruction{},
	}
	c.scopes = append(c.scopes, scope)
	c.scopeIndex++
	c.symbolTable = NewEnclosedSymbolTable(c.symbolTable)
}

func (c *Compiler) leaveScope() Instructions {
	instructions := c.currentInstructions()

	c.scopes = c.scopes[:len(c.scopes)-1]
	c.scopeIndex--

	c.symbolTable = c.symbolTable.outer

	return instructions
}

func (c *Compiler) currentInstructions() Instructions {
	return c.scopes[c.scopeIndex].instructions
}

func (c *Compiler) compileProgram(ast []astNode) *compileError {
	for _, node := range ast {
		if err := c.compile(node, true); err != nil {
			return err
		}
	}

	return nil
}

func (c *Compiler) compile(node astNode, topLevel bool) *compileError {
	switch node := node.(type) {
	case blockNode:
		for i, e := range node.exprs {
			// Don't pop the last statement
			shouldPop := len(node.exprs)-1 != i

			err := c.compile(e, shouldPop)
			if err != nil {
				return err
			}
		}
	case forExprNode:
		loopStart := len(c.currentInstructions())
		var increment astNode = nil
		switch cond := node.cond.(type) {
		case binaryNode:
			// iterator loop
			if cond.op == inKeyword {
				return c.compileInLoop(node, cond)
			} else {
				if err := c.compile(cond, false); err != nil {
					return err
				}
			}
		case blockNode:
			// c-style for loop
			if len(cond.exprs) == 3 {
				if err := c.compile(cond.exprs[0], false); err != nil {
					return err
				}
				loopStart = len(c.currentInstructions())

				if err := c.compile(cond.exprs[1], false); err != nil {
					return err
				}

				increment = cond.exprs[2]
			} else {
				if err := c.compile(node.cond, false); err != nil {
					return err
				}
			}
		default:
			// while loop
			if err := c.compile(node.cond, false); err != nil {
				return err
			}
		}

		exitJump := c.emit(OpJumpNotTruthy, 0xffff)

		if err := c.compile(node.body, false); err != nil {
			return err
		}
		c.emit(OpPop)

		if increment != nil {
			if err := c.compile(increment, false); err != nil {
				return err
			}
		}

		c.emitLoop(loopStart)

		c.patchJump(exitJump, len(c.currentInstructions()))

		c.emit(OpNull)

	case ifExprNode:
		if err := c.compile(node.cond, false); err != nil {
			return err
		}

		jumpNotTruthy := c.emit(OpJumpNotTruthy, 0xffff)

		if err := c.compile(node.then, false); err != nil {
			return err
		}

		if c.lastInstructionIs(OpPop) {
			c.removeLastPop()
		}

		jumpPos := c.emit(OpJump, 0xffff)

		c.patchJump(jumpNotTruthy, len(c.currentInstructions()))

		if node.else_ == nil {
			c.emit(OpNull)
		} else {
			if err := c.compile(node.else_, false); err != nil {
				return err
			}

			if c.lastInstructionIs(OpPop) {
				c.removeLastPop()
			}

		}

		c.patchJump(jumpPos, len(c.currentInstructions()))
	case fnNode:
		c.enterScope()

		for _, p := range node.params {
			c.symbolTable.define(p)
		}

		if err := c.compile(node.body, true); err != nil {
			return err
		}

		if c.lastInstructionIs(OpPop) {
			lastPos := c.scopes[c.scopeIndex].lastInstruction.position
			c.replaceInstruction(lastPos, makeOpcode(OpReturnValue))

			c.scopes[c.scopeIndex].lastInstruction.opcode = OpReturnValue
		}

		freeSymbols := c.symbolTable.freeSymbols
		numLocals := len(c.symbolTable.store)

		instructions := c.leaveScope()

		for _, s := range freeSymbols {
			c.loadSymbol(s)
		}

		compiledFn := FunctionValue{
			instructions: instructions,
			numLocals:    numLocals,
			numParams:    len(node.params),
		}

		c.emit(OpClosure, c.addConstant(compiledFn), len(freeSymbols))

	case fnCallNode:
		if err := c.compile(node.fn, false); err != nil {
			return err
		}

		for _, a := range node.args {
			if err := c.compile(a, false); err != nil {
				return err
			}
		}

		c.emit(OpCall, len(node.args))

	case binaryNode:
		if err := c.compile(node.left, false); err != nil {
			return err
		}

		if err := c.compile(node.right, false); err != nil {
			return err
		}

		switch node.op {
		case plus:
			c.emit(OpAdd)
		case minus:
			c.emit(OpSub)
		case divide:
			c.emit(OpDiv)
		case times:
			c.emit(OpMul)
		case eq:
			c.emit(OpEq)
		case neq:
			c.emit(OpNotEq)
		case less:
			c.emit(OpLess)
		case greater:
			c.emit(OpGreater)
		case leq:
			c.emit(OpLeq)
		case geq:
			c.emit(OpGeq)
		default:
			return &compileError{
				reason:   fmt.Sprintf("unknown operator %s", token{kind: node.op}),
				position: node.pos(),
			}
		}
	case unaryNode:
		if err := c.compile(node.right, false); err != nil {
			return err
		}

		switch node.op {
		case minus:
			c.emit(OpNegate)
		case not:
			c.emit(OpNot)
		default:
			return &compileError{
				reason:   fmt.Sprintf("unknown unary operator: %s", token{kind: node.op}),
				position: node.pos(),
			}
		}

	case assignmentNode:
		switch left := node.left.(type) {
		case identifierNode:
			var symbol Symbol
			if node.isSet {
				resolved, ok := c.symbolTable.resolve(left.payload)

				if !ok {
					return &compileError{
						reason:   "variable is not defined",
						position: left.pos(),
					}
				}

				symbol = resolved
			} else if stored, ok := c.symbolTable.store[left.payload]; ok {
				symbol = stored
			} else {
				// we need to define the symbol before compiling the right side (recursion)
				symbol = c.symbolTable.define(left.payload)
			}

			if err := c.compile(node.right, false); err != nil {
				return err
			}
			if symbol.scope == GlobalScope {
				c.emit(OpSetGlobal, symbol.index)
			} else {
				c.emit(OpSetLocal, symbol.index)
			}
		default:
			return &compileError{
				reason:   fmt.Sprintf("unknown assignment target: %s", left),
				position: left.pos(),
			}
		}

	case identifierNode:
		symbol, ok := c.symbolTable.resolve(node.payload)
		// fmt.Println("resolving symbol: ", node.payload)
		// fmt.Println(c.symbolTable.freeSymbols)
		if !ok {
			return &compileError{
				reason:   fmt.Sprintf("unknown variable: %s", node.payload),
				position: node.pos(),
			}
		}

		c.loadSymbol(symbol)

	case returnNode:
		if err := c.compile(node.inner, false); err != nil {
			return err
		}

		c.emit(OpReturnValue)

	case intNode:
		integer := IntValue(node.payload)
		c.emit(OpConstant, c.addConstant(integer))
	case floatNode:
		float := FloatValue(node.payload)
		c.emit(OpConstant, c.addConstant(float))
	case boolNode:
		if node.payload {
			c.emit(OpTrue)
		} else {
			c.emit(OpFalse)
		}
	case nullNode:
		c.emit(OpNull)
	case listNode:
		for _, e := range node.items {
			if err := c.compile(e, false); err != nil {
				return err
			}
		}

		c.emit(OpArray, len(node.items))
	case stringNode:
		str := StringValue(node.payload)
		c.emit(OpConstant, c.addConstant(&str))
	case objectNode:
		for _, entry := range node.entries {
			if ident, ok := entry.key.(identifierNode); ok {
				value := StringValue(ident.payload)
				c.emit(OpConstant, c.addConstant(&value))
			} else if err := c.compile(entry.key, false); err != nil {
				return err
			}

			if err := c.compile(entry.val, false); err != nil {
				return err
			}
		}

		c.emit(OpHash, len(node.entries)*2)
	case propertyAccessNode:
		if err := c.compile(node.left, false); err != nil {
			return err
		}
		if ident, ok := node.right.(identifierNode); ok {
			value := StringValue(ident.payload)
			c.emit(OpConstant, c.addConstant(&value))
		} else if err := c.compile(node.right, false); err != nil {
			return err
		}

		c.emit(OpIndex)
	default:
		return &compileError{
			reason:   fmt.Sprintf("unknown node: %s", node.String()),
			position: node.pos(),
		}
	}

	if topLevel {
		c.emit(OpPop)
	}

	return nil
}

func (c *Compiler) removeLastPop() {
	last := c.scopes[c.scopeIndex].lastInstruction
	previous := c.scopes[c.scopeIndex].previousInstruction

	old := c.currentInstructions()
	new := old[:last.position]

	c.scopes[c.scopeIndex].instructions = new
	c.scopes[c.scopeIndex].lastInstruction = previous
}

func (c *Compiler) emit(op Opcode, operands ...int) int {
	ins := makeOpcode(op, operands...)
	pos := c.addInstruction(ins)

	c.setLastInstruction(op, pos)

	return pos
}

func (c *Compiler) compileInLoop(node forExprNode, cond binaryNode) *compileError {
	name, ok := cond.left.(identifierNode)
	if !ok {
		return &compileError{
			reason:   "expected identifier in for..in loop",
			position: cond.left.pos(),
		}
	}

	symbol := c.symbolTable.define(name.payload)

	// actual iterator
	if err := c.compile(cond.right, false); err != nil {
		return err
	}
	c.emit(OpIterate)

	loopStart := len(c.currentInstructions())

	c.emit(OpIterateNext)

	jump := c.emit(OpJumpNotTruthy, 0xffff)

	c.emit(OpSetGlobal, symbol.index)

	if err := c.compile(node.body, true); err != nil {
		return err
	}

	c.emit(OpPop) // pop body

	c.emitLoop(loopStart)
	c.patchJump(jump, len(c.currentInstructions()))

	c.emit(OpPop) // pop iterator
	c.emit(OpPop) // pop condition

	c.emit(OpNull) // pop condition

	return nil
}

func (c *Compiler) emitLoop(loopStart int) {
	offset := len(c.currentInstructions()) - loopStart

	c.emit(OpLoop, offset)
}

func (c *Compiler) setLastInstruction(op Opcode, pos int) {
	previous := c.scopes[c.scopeIndex].lastInstruction
	last := emittedInstruction{opcode: op, position: pos}

	c.scopes[c.scopeIndex].previousInstruction = previous
	c.scopes[c.scopeIndex].lastInstruction = last
}

func (c *Compiler) patchJump(position int, operand int) {
	op := Opcode(c.currentInstructions()[position])
	newInstruction := makeOpcode(op, operand)

	for i := 0; i < len(newInstruction); i++ {
		c.scopes[c.scopeIndex].instructions[position+i] = newInstruction[i]
	}
}

func (c *Compiler) addInstruction(ins []byte) int {
	posNewInstruction := len(c.currentInstructions())
	updatedInstructions := append(c.currentInstructions(), ins...)

	c.scopes[c.scopeIndex].instructions = updatedInstructions

	return posNewInstruction
}

func (c *Compiler) addConstant(value Value) int {
	switch value := value.(type) {
	case IntValue, FloatValue, *StringValue:
		// lookup constants to reduce duplicate compilation
		for i, constant := range c.constants {
			if constant != nil && constant.Eq(value) {
				return i
			}
		}
	}
	c.constants = append(c.constants, value)

	return len(c.constants) - 1
}

func (c *Compiler) lastInstructionIs(op Opcode) bool {
	if len(c.currentInstructions()) == 0 {
		return false
	}

	return c.scopes[c.scopeIndex].lastInstruction.opcode == op
}

func (c *Compiler) replaceInstruction(pos int, newInstruction []byte) {
	for i := 0; i < len(newInstruction); i++ {
		c.scopes[c.scopeIndex].instructions[pos+i] = newInstruction[i]
	}
}
