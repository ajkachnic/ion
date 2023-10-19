package modules

import "github.com/ajkachnic/ion/core"

func Initialize(ctx *core.Context) error {
	loadMath(ctx)

	return nil
}
