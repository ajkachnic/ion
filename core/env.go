package core

import (
	"bytes"
	"encoding/json"
	"fmt"
	"math"
	"sort"
	"strconv"
	"strings"
)

func (c *Context) RequireArgLen(fnName string, args []Value, count int) *RuntimeError {
	if len(args) < count {
		return &RuntimeError{
			Reason: fmt.Sprintf("%s requires %d arguments, got %d", fnName, count, len(args)),
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

type RuntimeError struct {
	Reason string
	position
	stackTrace []stackEntry
}

func (e *RuntimeError) Error() string {
	trace := make([]string, len(e.stackTrace))
	for i, entry := range e.stackTrace {
		trace[i] = entry.String()
	}
	return fmt.Sprintf("Runtime error %s: %s\n%s", e.position, e.Reason, strings.Join(trace, "\n"))
}

type Context struct {
	// directory containing the root file of this context, used for loading
	// other modules with relative paths / URLs
	RootPath string

	Builtins  []BuiltinFnValue
	Modules   []module
	ImportMap map[string]Value
}

func NewContext(rootPath string) Context {
	return Context{
		RootPath:  rootPath,
		Builtins:  []BuiltinFnValue{},
		Modules:   []module{},
		ImportMap: make(map[string]Value),
	}
}

type builtinFn func([]Value) (Value, *RuntimeError)

type BuiltinFnValue struct {
	Name string
	Fn   builtinFn
}

func (v BuiltinFnValue) String() string {
	return fmt.Sprintf("%s := () -> { <native fn> }", v.Name)
}

func (v BuiltinFnValue) Eq(u Value) bool {
	if w, ok := u.(BuiltinFnValue); ok {
		return v.Name == w.Name
	}
	return false
}

func (v BuiltinFnValue) Truthy() bool {
	return true
}

func (c *Context) LoadFunc(name string, fn builtinFn) {
	c.Builtins = append(c.Builtins, BuiltinFnValue{
		Name: name,
		Fn:   fn,
	})
}

func (c *Context) LoadBuiltins() {
	// c.LoadModules()

	c.LoadFunc("int", c.builtinInt)
	c.LoadFunc("float", c.builtinFloat)
	c.LoadFunc("string", c.builtinString)
	c.LoadFunc("type", c.builtinType)
	c.LoadFunc("len", c.builtinLen)
	c.LoadFunc("print", c.builtinPrint)
	c.LoadFunc("import", c.builtinImport)
}

func (c *Context) builtinType(args []Value) (Value, *RuntimeError) {
	if err := c.RequireArgLen("type", args, 1); err != nil {
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

func (c *Context) builtinInt(args []Value) (Value, *RuntimeError) {
	if err := c.RequireArgLen("int", args, 1); err != nil {
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

func (c *Context) builtinFloat(args []Value) (Value, *RuntimeError) {
	if err := c.RequireArgLen("float", args, 1); err != nil {
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

func (c *Context) builtinString(args []Value) (Value, *RuntimeError) {
	if err := c.RequireArgLen("string", args, 1); err != nil {
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

func (c *Context) builtinLen(args []Value) (Value, *RuntimeError) {
	if err := c.RequireArgLen("len", args, 1); err != nil {
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
		return nil, &RuntimeError{
			Reason: fmt.Sprintf("%s does not support a len() call", arg),
		}
	}
}

func (c *Context) builtinPrint(args []Value) (Value, *RuntimeError) {
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

func (c *Context) builtinImport(args []Value) (Value, *RuntimeError) {
	if err := c.RequireArgLen("import", args, 1); err != nil {
		return nil, err
	}
	arg, ok := args[0].(*StringValue)
	if !ok {
		return nil, &RuntimeError{
			Reason: fmt.Sprintf("import requires a string, got %s", args[0]),
		}
	}

	name := string(*arg)

	if value, ok := c.ImportMap[name]; ok {
		return value, nil
	}

	for _, module := range c.Modules {
		if module.name == string(*arg) {
			value := make(ObjectValue, len(module.items))

			for k, v := range module.items {
				str := StringValue(k)
				value[str.HashKey()] = ObjectPair{&str, v}
			}

			c.ImportMap[name] = value

			return value, nil
		}
	}

	return nil, &RuntimeError{
		Reason: fmt.Sprintf("module %s not found", name),
	}
}

type module struct {
	name  string
	items map[string]Value
}

func (c *Context) LoadModule(name string, items map[string]Value) {
	c.Modules = append(c.Modules, module{
		name:  name,
		items: items,
	})
}

func (c *Context) LoadModules() {
	// c.LoadModule("math", map[string]Value{
	// 	"pi": FloatValue(math.Pi),

	// 	"ceil": BuiltinFnValue{
	// 		"ceil",
	// 		c.mathCeil,
	// 	},
	// 	"floor": BuiltinFnValue{
	// 		"floor",
	// 		c.mathFloor,
	// 	},
	// 	"sin": BuiltinFnValue{
	// 		"sin",
	// 		c.mathSin,
	// 	},
	// })

	// c.LoadModule("json", map[string]Value{
	// 	"parse": BuiltinFnValue{
	// 		"parse",
	// 		c.jsonParse,
	// 	},
	// 	"serialize": BuiltinFnValue{
	// 		"serialize",
	// 		c.jsonSerialize,
	// 	},
	// })
}

func (c *Context) mathCeil(args []Value) (Value, *RuntimeError) {
	if err := c.RequireArgLen("math.ceil", args, 1); err != nil {
		return nil, err
	}

	switch arg := args[0].(type) {
	case IntValue:
		return arg, nil
	case FloatValue:
		return IntValue(math.Ceil(float64(arg))), nil
	default:
		return nil, &RuntimeError{
			Reason: fmt.Sprintf("math.ceil does not support type %s", arg),
		}
	}
}

func (c *Context) mathFloor(args []Value) (Value, *RuntimeError) {
	if err := c.RequireArgLen("math.floor", args, 1); err != nil {
		return nil, err
	}

	switch arg := args[0].(type) {
	case IntValue:
		return arg, nil
	case FloatValue:
		return IntValue(math.Floor(float64(arg))), nil
	default:
		return nil, &RuntimeError{
			Reason: fmt.Sprintf("math.floor does not support type %s", arg),
		}
	}
}

func (c *Context) mathSin(args []Value) (Value, *RuntimeError) {
	if err := c.RequireArgLen("math.sin", args, 1); err != nil {
		return nil, err
	}

	switch arg := args[0].(type) {
	case IntValue:
		return FloatValue(math.Sin(float64(arg))), nil
	case FloatValue:
		return FloatValue(math.Sin(float64(arg))), nil
	default:
		return nil, &RuntimeError{
			Reason: fmt.Sprintf("math.sin does not support type %s", arg),
		}
	}
}

func (c *Context) jsonParse(args []Value) (Value, *RuntimeError) {
	if err := c.RequireArgLen("json.parse", args, 1); err != nil {
		return nil, err
	}

	arg, ok := args[0].(*StringValue)
	if !ok {
		return nil, &RuntimeError{
			Reason: fmt.Sprintf("json.parse requires a string, got %s", args[0]),
		}
	}

	var data interface{}
	if err := json.Unmarshal([]byte(*arg), &data); err != nil {
		return nil, &RuntimeError{
			Reason: fmt.Sprintf("json.parse: %s", err),
		}
	}

	return c.convertJSON(data), nil
}

func (c *Context) jsonSerialize(args []Value) (Value, *RuntimeError) {
	if err := c.RequireArgLen("json.serialize", args, 1); err != nil {
		return nil, err
	}

	arg := args[0]

	builder := bytes.Buffer{}
	err := c.serializeValue(arg, &builder)
	if err != nil {
		return nil, &RuntimeError{
			Reason: err.Error(),
		}
	}

	value := StringValue(builder.Bytes())

	return &value, nil
}

func (c *Context) serializeValue(value Value, builder *bytes.Buffer) *RuntimeError {
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
				return &RuntimeError{
					Reason: fmt.Sprintf("json.serialize: unsupported type %s", pair.key),
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
		return &RuntimeError{
			Reason: fmt.Sprintf("json.serialize: unsupported type %s", value),
		}
	}
	return nil
}

func (c *Context) convertJSON(data interface{}) Value {
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
