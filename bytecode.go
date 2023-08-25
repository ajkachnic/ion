package main

import (
	"bytes"
	"encoding/binary"
	"fmt"
)

type Instructions []byte
type Opcode byte

func (ins Instructions) String() string {
	var out bytes.Buffer

	i := 0
	for i < len(ins) {
		def, err := lookupOpcode(ins[i])
		if err != nil {
			fmt.Fprintf(&out, "ERROR: %s\n", err)
			continue
		}

		operands, read := readOperands(def, ins[i+1:])

		fmt.Fprintf(&out, "\x1b[33m%04d\x1b[0m %s\n", i, ins.fmtInstruction(def, operands))

		i += 1 + read
	}

	return out.String()
}

func (ins Instructions) fmtInstruction(def *Definition, operands []int) string {
	operandCount := len(def.operandWidths)

	if len(operands) != operandCount {
		return fmt.Sprintf("ERROR: operand len %d does not match defined %d\n",
			len(operands), operandCount)
	}

	switch operandCount {
	case 0:
		return def.name
	case 1:
		return fmt.Sprintf("%s %d", def.name, operands[0])
	case 2:
		return fmt.Sprintf("%s %d %d", def.name, operands[0], operands[1])
	}

	return fmt.Sprintf("ERROR: unhandled operandCount for %s\n", def.name)
}

type Definition struct {
	name          string
	operandWidths []int
}

const (
	OpConstant Opcode = iota
	OpPop
	OpAdd
	OpSub
	OpMul
	OpDiv
	OpNegate
	OpNot
	OpClosure

	OpJump
	OpLoop // negative jump
	OpJumpNotTruthy
	OpIterate
	OpIterateNext

	OpTrue
	OpFalse
	OpNull
	OpArray
	OpHash
	OpCall
	OpReturnValue
	OpReturn

	OpIndex
	OpGetGlobal
	OpSetGlobal
	OpGetLocal
	OpSetLocal
	OpGetBuiltin
	OpGetFree

	OpEq
	OpNotEq
	OpGreater
	OpLess
	OpGeq
	OpLeq
)

var definitions = map[Opcode]*Definition{
	OpConstant:    {"OpConstant", []int{2}},
	OpPop:         {"OpPop", []int{}},
	OpAdd:         {"OpAdd", []int{}},
	OpSub:         {"OpSub", []int{}},
	OpMul:         {"OpMul", []int{}},
	OpDiv:         {"OpDiv", []int{}},
	OpNegate:      {"OpNegate", []int{}},
	OpNot:         {"OpNot", []int{}},
	OpTrue:        {"OpTrue", []int{}},
	OpFalse:       {"OpFalse", []int{}},
	OpNull:        {"OpNull", []int{}},
	OpArray:       {"OpArray", []int{2}},
	OpHash:        {"OpHash", []int{2}},
	OpCall:        {"OpCall", []int{1}},
	OpReturnValue: {"OpReturnValue", []int{}},
	OpReturn:      {"OpReturn", []int{}},
	OpClosure:     {"OpClosure", []int{2, 1}},

	OpJump:          {"OpJump", []int{2}},
	OpLoop:          {"OpLoop", []int{2}},
	OpJumpNotTruthy: {"OpJumpNotTruthy", []int{2}},
	OpIterate:       {"OpIterate", []int{}},
	OpIterateNext:   {"OpIterateNext", []int{}},

	OpIndex:      {"OpIndex", []int{}},
	OpGetGlobal:  {"OpGetGlobal", []int{2}},
	OpSetGlobal:  {"OpSetGlobal", []int{2}},
	OpGetLocal:   {"OpGetLocal", []int{1}},
	OpSetLocal:   {"OpSetLocal", []int{1}},
	OpGetBuiltin: {"OpGetBuiltin", []int{1}},
	OpGetFree:    {"OpGetFree", []int{1}},

	OpEq:      {"OpEq", []int{}},
	OpNotEq:   {"OpNotEq", []int{}},
	OpGreater: {"OpGreater", []int{}},
	OpLess:    {"OpLess", []int{}},
	OpGeq:     {"OpGeq", []int{}},
	OpLeq:     {"OpLeq", []int{}},
}

func lookupOpcode(op byte) (*Definition, error) {
	def, ok := definitions[Opcode(op)]
	if !ok {
		return nil, fmt.Errorf("opcode %d undefined", op)
	}

	return def, nil
}

func readOperands(def *Definition, instructions Instructions) ([]int, int) {
	operands := make([]int, len(def.operandWidths))
	offset := 0

	for i, width := range def.operandWidths {
		switch width {
		case 1:
			operands[i] = int(instructions[offset])
		case 2:
			operands[i] = int(binary.BigEndian.Uint16(instructions[offset:]))
		}

		offset += width
	}

	return operands, offset
}

func makeOpcode(op Opcode, operands ...int) []byte {
	def, ok := definitions[op]
	if !ok {
		return []byte{}
	}

	instructionLen := 1
	for _, w := range def.operandWidths {
		instructionLen += w
	}

	instruction := make([]byte, instructionLen)
	instruction[0] = byte(op)

	offset := 1
	for i, o := range operands {
		width := def.operandWidths[i]
		switch width {
		case 1:
			instruction[offset] = byte(o)
		case 2:
			binary.BigEndian.PutUint16(instruction[offset:], uint16(o))
		}
		offset += width
	}

	return instruction
}
