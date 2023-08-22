package main

import (
	"bytes"
	"fmt"
	"hash/fnv"
	"math"
	"sort"
	"strconv"
	"strings"
	"time"
)

type ValueType int

const (
	NullType ValueType = iota
	BoolType
	IntType
	FloatType
	StringType
	ListType
	ObjectType
	FunctionType
)

type Value interface {
	String() string
	Eq(v Value) bool
	Truthy() bool
}

type NullValue struct{}

func (v NullValue) String() string {
	return "null"
}

func (v NullValue) Eq(other Value) bool {
	_, ok := other.(NullValue)
	return ok
}

func (v NullValue) Truthy() bool {
	return false
}

var null = NullValue{}

type StringValue []byte

func (v *StringValue) String() string {
	return strconv.Quote(string(*v))
}

func (v *StringValue) Eq(other Value) bool {
	if w, ok := other.(*StringValue); ok {
		return bytes.Equal(*v, *w)
	}

	return false
}

func (v *StringValue) Truthy() bool {
	return len(*v) > 0
}

type IntValue int64

func (v IntValue) String() string {
	return strconv.FormatInt(int64(v), 10)
}
func (v IntValue) Eq(u Value) bool {
	if w, ok := u.(IntValue); ok {
		return v == w
	} else if w, ok := u.(FloatValue); ok {
		return FloatValue(v) == w
	}

	return false
}

func (v IntValue) Truthy() bool {
	return v != 0
}

type FloatValue float64

func (v FloatValue) String() string {
	return strconv.FormatFloat(float64(v), 'g', -1, 64)
}
func (v FloatValue) Eq(u Value) bool {
	if w, ok := u.(FloatValue); ok {
		return v == w
	} else if w, ok := u.(IntValue); ok {
		return v == FloatValue(w)
	}

	return false
}

func (v FloatValue) Truthy() bool {
	return v != 0
}

type BoolValue bool

func (v BoolValue) String() string {
	if v {
		return "true"
	}
	return "false"
}

func (v BoolValue) Eq(u Value) bool {
	if w, ok := u.(BoolValue); ok {
		return v == w
	}

	return u.Truthy() == bool(v)
}

func (v BoolValue) Truthy() bool {
	return bool(v)
}

type FunctionValue struct {
	instructions Instructions
	numLocals    int
	numParams    int
}

func (v FunctionValue) String() string {
	return v.instructions.String()
}

func (v FunctionValue) Eq(u Value) bool {
	return false
}

func (v FunctionValue) Truthy() bool {
	return true
}

type ObjectPair struct {
	key   Hashable
	value Value
}

type ObjectValue map[HashKey]ObjectPair

func (v ObjectValue) String() string {
	entryStrings := []string{}
	entries := []ObjectPair{}
	for _, entry := range v {
		entries = append(entries, entry)
	}

	sort.SliceStable(entries, func(i, j int) bool {
		return entries[i].key.String() < entries[j].key.String()
	})

	for _, entry := range entries {
		var key string
		if entryKey, ok := entry.key.(*StringValue); ok {
			key = string(*entryKey)
		} else {
			key = entry.key.String()
		}
		entryStrings = append(entryStrings, key+": "+entry.value.String())
	}

	return "{ " + strings.Join(entryStrings, ", ") + " }"
}

func (v ObjectValue) Eq(u Value) bool {
	if w, ok := u.(ObjectValue); ok {
		if len(v) != len(w) {
			return false
		}
		for key, entry := range v {
			value := entry.value
			if wvalue, ok := w[key]; !ok || !value.Eq(wvalue.value) {
				return false
			}
		}

		return true
	}

	return false
}

func (v ObjectValue) Truthy() bool {
	return true
}

type ListValue []Value

func (v ListValue) String() string {
	if len(v) == 0 {
		return "[]"
	}
	items := make([]string, len(v))
	for i, item := range v {
		items[i] = item.String()
	}
	return "[ " + strings.Join(items, ", ") + " ]"
}

func (v ListValue) Eq(u Value) bool {
	if w, ok := u.(ListValue); ok {
		if len(v) != len(w) {
			return false
		}
		for i, value := range v {
			if !value.Eq(w[i]) {
				return false
			}
		}

		return true
	}

	return false
}

func (v ListValue) Truthy() bool {
	return true
}

type HashKey struct {
	kind  ValueType
	value uint64
}

func (v BoolValue) HashKey() HashKey {
	if v {
		return HashKey{BoolType, 1}
	}
	return HashKey{BoolType, 0}
}

func (v IntValue) HashKey() HashKey {
	return HashKey{IntType, uint64(v)}
}

func (v StringValue) HashKey() HashKey {
	hash := fnv.New64a()

	hash.Write([]byte(v))

	return HashKey{StringType, hash.Sum64()}
}

// TODO: we want hash(2.0) == hash(2)
// ref: https://docs.python.org/3/library/stdtypes.html#hashing-of-numeric-types
func (v FloatValue) HashKey() HashKey {
	return HashKey{FloatType, math.Float64bits(float64(v))}
}

type Hashable interface {
	Value
	HashKey() HashKey
}

type BuiltinValue func(args ...Value) Value

func (v BuiltinValue) String() string {
	return "[native code]"
}

func (v BuiltinValue) Eq(u Value) bool {
	return false
}

func (v BuiltinValue) Truthy() bool {
	return true
}

var Builtins = []struct {
	name    string
	builtin BuiltinValue
}{
	{
		"len",
		func(args ...Value) Value {
			if len(args) != 1 {
				// TODO: this is stupid, don't panic
				panic("wrong number of arguments")
			}

			switch v := args[0].(type) {
			case *StringValue:
				return IntValue(len(string(*v)))
			case ListValue:
				return IntValue(len(v))
			case ObjectValue:
				return IntValue(len(v))
			}

			return null
		},
	},
	{
		"time",
		func(args ...Value) Value {
			return FloatValue(float64(time.Now().UnixMilli()) / 1000)
		},
	},
	{
		"print",
		func(args ...Value) Value {
			if len(args) != 1 {
				// TODO: this is stupid, don't panic
				panic("wrong number of arguments")
			}

			switch v := args[0].(type) {
			case *StringValue:
				fmt.Println(string(*v))
			default:
				fmt.Println(v.String())
			}

			return null
		},
	},
}

func getBuiltinByName(name string) BuiltinValue {
	for _, def := range Builtins {
		if def.name == name {
			return def.builtin
		}
	}
	return nil
}
