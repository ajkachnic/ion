package modules

import (
	"fmt"
	"math"

	"github.com/ajkachnic/ion/core"
)

type _math struct {
	ctx *core.Context
}

func loadMath(ctx *core.Context) error {
	// wrapper struct to allow access to the context
	c := &_math{ctx: ctx}

	ctx.LoadModule("math", map[string]core.Value{
		"pi": core.FloatValue(math.Pi),
		"e":  core.FloatValue(math.E),

		"ceil":  core.BuiltinFnValue{"ceil", c.ceil},
		"floor": core.BuiltinFnValue{"floor", c.floor},
		"sin":   core.BuiltinFnValue{"sin", c.sin},
	})
	return nil
}

func (c *_math) ceil(args []core.Value) (core.Value, *core.RuntimeError) {
	if err := c.ctx.RequireArgLen("math.ceil", args, 1); err != nil {
		return nil, err
	}

	switch arg := args[0].(type) {
	case core.IntValue:
		return arg, nil
	case core.FloatValue:
		return core.IntValue(int64(math.Ceil(float64(arg)))), nil
	default:
		return nil, &core.RuntimeError{
			Reason: fmt.Sprintf("math.ceil does not support type %s", arg),
		}
	}
}

func (c *_math) floor(args []core.Value) (core.Value, *core.RuntimeError) {
	if err := c.ctx.RequireArgLen("math.floor", args, 1); err != nil {
		return nil, err
	}

	switch arg := args[0].(type) {
	case core.IntValue:
		return arg, nil
	case core.FloatValue:
		return core.IntValue(int64(math.Floor(float64(arg)))), nil
	default:
		return nil, &core.RuntimeError{
			Reason: fmt.Sprintf("math.floor does not support type %s", arg),
		}
	}
}

func (c *_math) sin(args []core.Value) (core.Value, *core.RuntimeError) {
	if err := c.ctx.RequireArgLen("math.sin", args, 1); err != nil {
		return nil, err
	}

	switch arg := args[0].(type) {
	case core.IntValue:
		return core.FloatValue(math.Sin(float64(arg))), nil
	case core.FloatValue:
		return core.FloatValue(math.Sin(float64(arg))), nil
	default:
		return nil, &core.RuntimeError{
			Reason: fmt.Sprintf("math.sin does not support type %s", arg),
		}
	}
}
