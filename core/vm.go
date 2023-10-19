package core

import (
	"encoding/binary"
	"fmt"
)

const MaxFrames = 1024
const StackSize = 2048
const GlobalsSize = 65536

type Frame struct {
	cl          *Closure
	ip          int
	basePointer int
}

func newFrame(cl *Closure, basePointer int) *Frame {
	return &Frame{cl: cl, ip: -1, basePointer: basePointer}
}

func (f *Frame) instructions() Instructions {
	return f.cl.fn.instructions
}

type VM struct {
	Constants []Value
	globals   []Value

	stack []Value
	sp    int // stack pointer

	frames      []*Frame
	framesIndex int

	context *Context
}

func NewVM(bytecode *Bytecode, globals []Value, context *Context) *VM {
	mainFn := &FunctionValue{instructions: bytecode.Instructions}
	mainClosure := &Closure{fn: mainFn}
	mainFrame := newFrame(mainClosure, 0)

	frames := make([]*Frame, MaxFrames)
	frames[0] = mainFrame

	return &VM{
		Constants: bytecode.Constants,
		globals:   globals,

		frames:      frames,
		framesIndex: 1,

		stack:   make([]Value, StackSize),
		sp:      0,
		context: context,
	}
}

func (vm *VM) currentFrame() *Frame {
	return vm.frames[vm.framesIndex-1]
}

func (vm *VM) pushFrame(f *Frame) {
	vm.frames[vm.framesIndex] = f
	vm.framesIndex++
}

func (vm *VM) popFrame() *Frame {
	vm.framesIndex--
	return vm.frames[vm.framesIndex]
}

func (vm *VM) push(v Value) error {
	if vm.sp >= StackSize {
		return fmt.Errorf("stack overflow")
	}

	vm.stack[vm.sp] = v
	vm.sp++

	return nil
}

func (vm *VM) pop() Value {
	v := vm.stack[vm.sp-1]
	vm.sp--
	return v
}

func (vm *VM) peek() Value {
	v := vm.stack[vm.sp-1]
	return v
}

func (vm *VM) Run() error {
	var ip int
	var ins Instructions
	var op Opcode

	for vm.currentFrame().ip < len(vm.currentFrame().instructions())-1 {
		vm.currentFrame().ip++

		ip = vm.currentFrame().ip
		ins = vm.currentFrame().instructions()
		op = Opcode(ins[ip])

		switch op {
		case OpConstant:
			constIndex := int(vm.readU16(true))

			if err := vm.push(vm.Constants[constIndex]); err != nil {
				return err
			}
		case OpTrue:
			if err := vm.push(BoolValue(true)); err != nil {
				return err
			}
		case OpFalse:
			if err := vm.push(BoolValue(false)); err != nil {
				return err
			}
		case OpNull:
			if err := vm.push(null); err != nil {
				return err
			}
		case OpArray:
			count := int(vm.readU16(true))

			array := vm.buildList(vm.sp-count, vm.sp)
			vm.sp = vm.sp - count

			if err := vm.push(array); err != nil {
				return err
			}
		case OpHash:
			count := int(vm.readU16(true))

			object, err := vm.buildObject(vm.sp-count, vm.sp)
			if err != nil {
				return err
			}

			vm.sp = vm.sp - count

			if err := vm.push(object); err != nil {
				return err
			}
		case OpCall:
			numArgs := vm.readU8(true)

			if err := vm.executeCall(int(numArgs)); err != nil {
				return err
			}
		case OpClosure:
			constIndex := int(vm.readU16(true))
			numFree := int(vm.readU8(true))

			if err := vm.pushClosure(constIndex, numFree); err != nil {
				return err
			}
		case OpReturnValue:
			returnValue := vm.pop()

			frame := vm.popFrame()
			vm.sp = frame.basePointer - 1

			if err := vm.push(returnValue); err != nil {
				return err
			}

		case OpIterate:
			iterable, ok := vm.pop().(Iterable)
			if !ok {
				return fmt.Errorf("value is not iterable: %T", iterable)
			}

			iter := iterable.Iter()

			if err := vm.push(iter); err != nil {
				return err
			}
		case OpIterateNext:
			iter := vm.peek().(Iterator)
			next, empty := iter.Next()

			var value = next

			if next == nil {
				value = null
			}

			if err := vm.push(value); err != nil {
				return err
			}
			if err := vm.push(BoolValue(empty)); err != nil {
				return err
			}
		case OpJump:
			pos := int(vm.readU16(false))
			vm.currentFrame().ip = pos - 1
		case OpLoop:
			pos := int(vm.readU16(false))
			vm.currentFrame().ip -= pos + 1

		case OpJumpNotTruthy:
			pos := int(vm.readU16(true))

			condition := vm.pop()
			if !condition.Truthy() {
				vm.currentFrame().ip = pos - 1
			}
		case OpPop:
			vm.pop()
		case OpAdd, OpSub, OpMul, OpDiv, OpEq, OpNotEq, OpGreater, OpGeq, OpLess, OpLeq:
			if err := vm.executeBinary(op); err != nil {
				return err
			}
		case OpNegate, OpNot:
			if err := vm.executeUnary(op); err != nil {
				return err
			}
		case OpIndex:
			right := vm.pop()
			left := vm.pop()

			if err := vm.executeIndex(left, right); err != nil {
				return err
			}

		case OpGetBuiltin:
			builtinIndex := int(vm.readU8(true))

			definition := vm.context.Builtins[builtinIndex]

			if err := vm.push(definition); err != nil {
				return err
			}
		case OpSetGlobal:
			globalIndex := int(vm.readU16(true))

			vm.globals[globalIndex] = vm.peek()
		case OpGetGlobal:
			globalIndex := int(vm.readU16(true))

			if err := vm.push(vm.globals[globalIndex]); err != nil {
				return err
			}
		case OpSetLocal:
			localIndex := int(vm.readU8(true))

			frame := vm.currentFrame()
			vm.stack[frame.basePointer+int(localIndex)] = vm.pop()
		case OpGetLocal:
			localIndex := int(vm.readU8(true))

			frame := vm.currentFrame()

			if err := vm.push(vm.stack[frame.basePointer+int(localIndex)]); err != nil {
				return err
			}
		case OpGetFree:
			freeIndex := int(vm.readU8(true))
			currentClosure := vm.currentFrame().cl

			if err := vm.push(currentClosure.free[freeIndex]); err != nil {
				return err
			}
		default:
			return fmt.Errorf("unknown opcode: %s", definitions[op].name)
		}
	}

	return nil
}

func (vm *VM) readU16(increment bool) uint16 {
	ip := vm.currentFrame().ip
	ins := vm.currentFrame().instructions()
	value := binary.BigEndian.Uint16(ins[ip+1:])

	if increment {
		vm.currentFrame().ip += 2
	}

	return value
}

func (vm *VM) readU8(increment bool) uint8 {
	ip := vm.currentFrame().ip
	ins := vm.currentFrame().instructions()
	value := ins[ip+1]

	if increment {
		vm.currentFrame().ip++
	}

	return value
}

func (vm *VM) pushClosure(constIndex int, numFree int) error {
	constant := vm.Constants[constIndex]
	function, ok := constant.(FunctionValue)
	if !ok {
		return fmt.Errorf("not a function: %+v", constant)
	}

	free := make([]Value, numFree)
	for i := 0; i < numFree; i++ {
		free[i] = vm.stack[vm.sp-numFree+i]
	}
	vm.sp = vm.sp - numFree

	closure := &Closure{fn: &function, free: free}
	return vm.push(closure)
}

func (vm *VM) executeCall(numArgs int) error {
	callee := vm.stack[vm.sp-1-numArgs]
	switch callee := callee.(type) {
	case *Closure:
		return vm.callClosure(callee, numArgs)
	case BuiltinFnValue:
		args := vm.stack[vm.sp-numArgs : vm.sp]
		result, err := (callee.Fn)(args)

		if err != nil {
			return err
		}

		vm.sp = vm.sp - numArgs - 1

		if err := vm.push(result); err != nil {
			return err
		}
	default:
		return fmt.Errorf("calling non-function and non-builtin: %T", callee)
	}
	return nil
}

func (vm *VM) callClosure(cl *Closure, numArgs int) error {
	if cl.fn.numParams != numArgs {
		return fmt.Errorf("wrong number of arguments: want=%d, got=%d", cl.fn.numParams, numArgs)
	}

	frame := newFrame(cl, vm.sp-numArgs)
	vm.pushFrame(frame)

	vm.sp = frame.basePointer + cl.fn.numLocals

	return nil
}

func (vm *VM) executeUnary(op Opcode) error {
	a := vm.pop()
	var result Value

	switch op {
	case OpNot:
		result = BoolValue(!a.Truthy())
	case OpNegate:
		switch a := a.(type) {
		case IntValue:
			result = -a
		case FloatValue:
			result = -a
		default:
			return fmt.Errorf("unsupported type for negation: %T", a)
		}
	default:
		return fmt.Errorf("unknown unary operator: %d", op)
	}

	return vm.push(result)
}

func (vm *VM) executeBinary(op Opcode) error {
	b := vm.pop()
	a := vm.pop()

	switch op {
	case OpEq:
		return vm.push(BoolValue(a.Eq(b)))
	case OpNotEq:
		return vm.push(BoolValue(!a.Eq(b)))
	}

	switch b := b.(type) {
	case *StringValue:
		aValue, ok := a.(*StringValue)
		if !ok {
			return fmt.Errorf("unsupported types for binary operation: %T %T", a, b)
		}

		switch op {
		case OpAdd:
			value := StringValue(string(*aValue) + string(*b))
			return vm.push(&value)
		default:
			return fmt.Errorf("unsupported binary operator %d for types: %T %T", op, a, b)
		}
	case IntValue:
		aValue, ok := a.(IntValue)
		if !ok {
			aValue, ok := a.(FloatValue)

			if !ok {
				return fmt.Errorf("unsupported types for binary operation: %T %T", a, b)
			}

			return vm.executeBinaryFloat(op, aValue, FloatValue(b))
		}

		return vm.executeBinaryInt(op, aValue, b)

	case FloatValue:
		aValue, ok := a.(FloatValue)
		if !ok {
			aValue, ok := a.(IntValue)

			if !ok {
				return fmt.Errorf("unsupported types for binary operation: %T %T", a, b)
			}

			return vm.executeBinaryFloat(op, FloatValue(aValue), b)
		}

		return vm.executeBinaryFloat(op, aValue, b)
	}

	return nil
}

func (vm *VM) executeBinaryFloat(op Opcode, a FloatValue, b FloatValue) error {
	var result Value
	switch op {
	case OpAdd:
		result = a + b
	case OpSub:
		result = a - b
	case OpMul:
		result = a * b
	case OpDiv:
		result = a / b
	case OpGreater:
		result = BoolValue(a > b)
	case OpGeq:
		result = BoolValue(a >= b)
	case OpLess:
		result = BoolValue(a < b)
	case OpLeq:
		result = BoolValue(a <= b)

	default:
		return fmt.Errorf("invariant: invalid binary operation")
	}

	return vm.push(result)
}

func (vm *VM) executeBinaryInt(op Opcode, a IntValue, b IntValue) error {
	var result Value
	switch op {
	case OpAdd:
		result = a + b
	case OpSub:
		result = a - b
	case OpMul:
		result = a * b
	case OpDiv:
		result = a / b
	case OpGreater:
		result = BoolValue(a > b)
	case OpGeq:
		result = BoolValue(a >= b)
	case OpLess:
		result = BoolValue(a < b)
	case OpLeq:
		result = BoolValue(a <= b)
	default:
		return fmt.Errorf("invariant: invalid binary operation")
	}

	return vm.push(result)
}

func (vm *VM) executeIndex(left Value, right Value) error {
	switch left := left.(type) {
	case ListValue:
		index, ok := right.(IntValue)
		if !ok {
			return fmt.Errorf("index must be an integer")
		}

		if index < 0 || index >= IntValue(len(left)) {
			return vm.push(null)
		}

		indexed := left[index]
		if indexed == nil {
			return vm.push(null)
		}

		return vm.push(left[index])
	case ObjectValue:
		hashKey, ok := right.(Hashable)
		if !ok {
			return fmt.Errorf("unhashable type: %T", right)
		}

		pair, ok := left[hashKey.HashKey()]
		if !ok {
			return vm.push(null)
		}

		return vm.push(pair.value)
	default:
		return fmt.Errorf("index operator not supported for type: %T", left)
	}
}

func (vm *VM) buildList(start int, end int) Value {
	elements := make([]Value, end-start)

	for i := start; i < end; i++ {
		elements[i-start] = vm.stack[i]
	}

	return ListValue(elements)
}

func (vm *VM) buildObject(start int, end int) (Value, error) {
	pairs := make(map[HashKey]ObjectPair)

	for i := start; i < end; i += 2 {
		key := vm.stack[i]
		value := vm.stack[i+1]

		hashKey, ok := key.(Hashable)
		if !ok {
			return nil, fmt.Errorf("unhashable type: %T", key)
		}

		pairs[hashKey.HashKey()] = ObjectPair{hashKey, value}
	}

	return ObjectValue(pairs), nil
}

func (vm *VM) LastPoppedStackElem() Value {
	return vm.stack[vm.sp]
}
