package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"math"
	"sort"
	"strconv"
	"strings"
)

func (c *context) requireArgLen(fnName string, args []Value, count int) *runtimeError {
	if len(args) < count {
		return &runtimeError{
			reason: fmt.Sprintf("%s requires %d arguments, got %d", fnName, count, len(args)),
		}
	}

	return nil
}

type stackEntry struct {
	name string
	position
}

func (e stackEntry) String() string {
	if e.name != "" {
		return fmt.Sprintf("  in fn %s %s", e.name, e.position)
	}
	return fmt.Sprintf("  in anonymous fn %s", e.position)
}

type runtimeError struct {
	reason string
	position
	stackTrace []stackEntry
}

func (e *runtimeError) Error() string {
	trace := make([]string, len(e.stackTrace))
	for i, entry := range e.stackTrace {
		trace[i] = entry.String()
	}
	return fmt.Sprintf("Runtime error %s: %s\n%s", e.position, e.reason, strings.Join(trace, "\n"))
}

type context struct {
	// directory containing the root file of this context, used for loading
	// other modules with relative paths / URLs
	rootPath string

	builtins  []BuiltinFnValue
	modules   []module
	importMap map[string]Value
}

func NewContext(rootPath string) context {
	return context{
		rootPath:  rootPath,
		builtins:  []BuiltinFnValue{},
		modules:   []module{},
		importMap: make(map[string]Value),
	}
}

type builtinFn func([]Value) (Value, *runtimeError)

type BuiltinFnValue struct {
	name string
	fn   builtinFn
}

func (v BuiltinFnValue) String() string {
	return fmt.Sprintf("%s := () -> { <native fn> }", v.name)
}

func (v BuiltinFnValue) Eq(u Value) bool {
	if w, ok := u.(BuiltinFnValue); ok {
		return v.name == w.name
	}
	return false
}

func (v BuiltinFnValue) Truthy() bool {
	return true
}

func (c *context) LoadFunc(name string, fn builtinFn) {
	c.builtins = append(c.builtins, BuiltinFnValue{
		name: name,
		fn:   fn,
	})
}

func (c *context) LoadBuiltins() {
	c.LoadModules()

	c.LoadFunc("int", c.builtinInt)
	c.LoadFunc("float", c.builtinFloat)
	c.LoadFunc("string", c.builtinString)
	c.LoadFunc("type", c.builtinType)
	c.LoadFunc("len", c.builtinLen)
	c.LoadFunc("print", c.builtinPrint)
	c.LoadFunc("import", c.builtinImport)
}

func (c *context) builtinType(args []Value) (Value, *runtimeError) {
	if err := c.requireArgLen("type", args, 1); err != nil {
		return nil, err
	}

	switch args[0].(type) {
	case NullValue:
		value := StringValue("null")
		return &value, nil
	case IntValue:
		value := StringValue("int")
		return &value, nil
	case FloatValue:
		value := StringValue("float")
		return &value, nil
	case BoolValue:
		value := StringValue("bool")
		return &value, nil
	case *StringValue:
		value := StringValue("string")
		return &value, nil
	case ListValue:
		value := StringValue("list")
		return &value, nil
	case ObjectValue:
		value := StringValue("object")
		return &value, nil
	case FunctionValue, BuiltinFnValue:
		value := StringValue("function")
		return &value, nil
	}

	panic("Unreachable: unknown runtime value")
}

func (c *context) builtinInt(args []Value) (Value, *runtimeError) {
	if err := c.requireArgLen("int", args, 1); err != nil {
		return nil, err
	}

	switch arg := args[0].(type) {
	case IntValue:
		return arg, nil
	case FloatValue:
		return IntValue(math.Floor(float64(arg))), nil
	case *StringValue:
		n, err := strconv.ParseInt(string(*arg), 10, 64)
		if err != nil {
			return null, nil
		}
		return IntValue(n), nil
	default:
		return null, nil
	}
}

func (c *context) builtinFloat(args []Value) (Value, *runtimeError) {
	if err := c.requireArgLen("float", args, 1); err != nil {
		return nil, err
	}

	switch arg := args[0].(type) {
	case IntValue:
		return FloatValue(arg), nil
	case FloatValue:
		return arg, nil
	case *StringValue:
		f, err := strconv.ParseFloat(string(*arg), 64)
		if err != nil {
			return null, nil
		}
		return FloatValue(f), nil
	default:
		return null, nil
	}
}

func (c *context) builtinString(args []Value) (Value, *runtimeError) {
	if err := c.requireArgLen("string", args, 1); err != nil {
		return nil, err
	}

	switch arg := args[0].(type) {
	case IntValue:
		value := StringValue(strconv.FormatInt(int64(arg), 10))
		return &value, nil
	case FloatValue:
		value := StringValue(strconv.FormatFloat(float64(arg), 'f', -1, 64))
		return &value, nil
	case *StringValue:
		return arg, nil
	default:
		return null, nil
	}
}

func (c *context) builtinLen(args []Value) (Value, *runtimeError) {
	if err := c.requireArgLen("len", args, 1); err != nil {
		return nil, err
	}

	switch arg := args[0].(type) {
	case *StringValue:
		return IntValue(len(*arg)), nil
	case ListValue:
		return IntValue(len(arg)), nil
	case ObjectValue:
		return IntValue(len(arg)), nil
	default:
		return nil, &runtimeError{
			reason: fmt.Sprintf("%s does not support a len() call", arg),
		}
	}
}

func (c *context) builtinPrint(args []Value) (Value, *runtimeError) {
	for _, arg := range args {
		switch arg := arg.(type) {
		case *StringValue:
			fmt.Print(string(*arg))
		default:
			fmt.Print(arg)
		}
	}
	fmt.Println()
	return null, nil
}

func (c *context) builtinImport(args []Value) (Value, *runtimeError) {
	if err := c.requireArgLen("import", args, 1); err != nil {
		return nil, err
	}
	arg, ok := args[0].(*StringValue)
	if !ok {
		return nil, &runtimeError{
			reason: fmt.Sprintf("import requires a string, got %s", args[0]),
		}
	}

	name := string(*arg)

	if value, ok := c.importMap[name]; ok {
		return value, nil
	}

	for _, module := range c.modules {
		if module.name == string(*arg) {
			value := make(ObjectValue, len(module.items))

			for k, v := range module.items {
				str := StringValue(k)
				value[str.HashKey()] = ObjectPair{&str, v}
			}

			c.importMap[name] = value

			return value, nil
		}
	}

	return nil, &runtimeError{
		reason: fmt.Sprintf("module %s not found", name),
	}
}

type module struct {
	name  string
	items map[string]Value
}

func (c *context) LoadModule(name string, items map[string]Value) {
	c.modules = append(c.modules, module{
		name:  name,
		items: items,
	})
}

func (c *context) LoadModules() {
	c.LoadModule("math", map[string]Value{
		"pi": FloatValue(math.Pi),

		"ceil": BuiltinFnValue{
			"ceil",
			c.mathCeil,
		},
		"floor": BuiltinFnValue{
			"floor",
			c.mathFloor,
		},
		"sin": BuiltinFnValue{
			"sin",
			c.mathSin,
		},
	})

	c.LoadModule("json", map[string]Value{
		"parse": BuiltinFnValue{
			"parse",
			c.jsonParse,
		},
		"serialize": BuiltinFnValue{
			"serialize",
			c.jsonSerialize,
		},
	})
}

func (c *context) mathCeil(args []Value) (Value, *runtimeError) {
	if err := c.requireArgLen("math.ceil", args, 1); err != nil {
		return nil, err
	}

	switch arg := args[0].(type) {
	case IntValue:
		return arg, nil
	case FloatValue:
		return IntValue(math.Ceil(float64(arg))), nil
	default:
		return nil, &runtimeError{
			reason: fmt.Sprintf("math.ceil does not support type %s", arg),
		}
	}
}

func (c *context) mathFloor(args []Value) (Value, *runtimeError) {
	if err := c.requireArgLen("math.floor", args, 1); err != nil {
		return nil, err
	}

	switch arg := args[0].(type) {
	case IntValue:
		return arg, nil
	case FloatValue:
		return IntValue(math.Floor(float64(arg))), nil
	default:
		return nil, &runtimeError{
			reason: fmt.Sprintf("math.floor does not support type %s", arg),
		}
	}
}

func (c *context) mathSin(args []Value) (Value, *runtimeError) {
	if err := c.requireArgLen("math.sin", args, 1); err != nil {
		return nil, err
	}

	switch arg := args[0].(type) {
	case IntValue:
		return FloatValue(math.Sin(float64(arg))), nil
	case FloatValue:
		return FloatValue(math.Sin(float64(arg))), nil
	default:
		return nil, &runtimeError{
			reason: fmt.Sprintf("math.sin does not support type %s", arg),
		}
	}
}

func (c *context) jsonParse(args []Value) (Value, *runtimeError) {
	if err := c.requireArgLen("json.parse", args, 1); err != nil {
		return nil, err
	}

	arg, ok := args[0].(*StringValue)
	if !ok {
		return nil, &runtimeError{
			reason: fmt.Sprintf("json.parse requires a string, got %s", args[0]),
		}
	}

	var data interface{}
	if err := json.Unmarshal([]byte(*arg), &data); err != nil {
		return nil, &runtimeError{
			reason: fmt.Sprintf("json.parse: %s", err),
		}
	}

	return c.convertJSON(data), nil
}

func (c *context) jsonSerialize(args []Value) (Value, *runtimeError) {
	if err := c.requireArgLen("json.serialize", args, 1); err != nil {
		return nil, err
	}

	arg := args[0]

	builder := bytes.Buffer{}
	err := c.serializeValue(arg, &builder)
	if err != nil {
		return nil, &runtimeError{
			reason: err.Error(),
		}
	}

	value := StringValue(builder.Bytes())

	return &value, nil
}

func (c *context) serializeValue(value Value, builder *bytes.Buffer) *runtimeError {
	switch value := value.(type) {
	case NullValue:
		builder.WriteString("null")
	case IntValue:
		builder.WriteString(strconv.FormatInt(int64(value), 10))
	case FloatValue:
		builder.WriteString(strconv.FormatFloat(float64(value), 'f', -1, 64))
	case BoolValue:
		if value {
			builder.WriteString("true")
		} else {
			builder.WriteString("false")
		}
	case *StringValue:
		builder.WriteString(strconv.Quote(string(*value)))
	case ListValue:
		builder.WriteString("[")
		for i, item := range value {
			if i > 0 {
				builder.WriteString(",")
			}
			if err := c.serializeValue(item, builder); err != nil {
				return err
			}
		}
		builder.WriteString("]")
	case ObjectValue:
		builder.WriteString("{")
		i := 0

		entries := make([]ObjectPair, 0, len(value))

		for _, pair := range value {
			entries = append(entries, pair)
		}

		sort.SliceStable(entries, func(i, j int) bool {
			return entries[i].key.String() < entries[j].key.String()
		})

		for _, pair := range entries {
			if i > 0 {
				builder.WriteString(",")
			}
			str, ok := pair.key.(*StringValue)
			if !ok {
				return &runtimeError{
					reason: fmt.Sprintf("json.serialize: unsupported type %s", pair.key),
				}
			}

			builder.WriteString(strconv.Quote(string(*str)))
			builder.WriteString(":")
			if err := c.serializeValue(pair.value, builder); err != nil {
				return err
			}
			i++
		}
		builder.WriteString("}")
	default:
		return &runtimeError{
			reason: fmt.Sprintf("json.serialize: unsupported type %s", value),
		}
	}
	return nil
}

func (c *context) convertJSON(data interface{}) Value {
	switch data := data.(type) {
	case bool:
		return BoolValue(data)
	case float64:
		return FloatValue(data)
	case string:
		value := StringValue(data)
		return &value
	case []interface{}:
		value := make(ListValue, len(data))
		for i, item := range data {
			value[i] = c.convertJSON(item)
		}
		return value
	case map[string]interface{}:
		value := make(ObjectValue, len(data))
		for k, v := range data {
			str := StringValue(k)
			value[str.HashKey()] = ObjectPair{&str, c.convertJSON(v)}
		}
		return value
	case nil:
		return null
	}
	panic("json.parse: unexpected type")
}
