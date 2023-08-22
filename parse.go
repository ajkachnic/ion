package main

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"
)

type astNode interface {
	String() string
	pos() position
}

type stringNode struct {
	payload []byte
	tok     *token
}

func (n stringNode) String() string {
	return fmt.Sprintf("%s", strconv.Quote(string(n.payload)))
}

func (n stringNode) pos() position {
	return n.tok.pos
}

type intNode struct {
	payload int64
	tok     *token
}

func (n intNode) String() string {
	return strconv.FormatInt(n.payload, 10)
}

func (n intNode) pos() position {
	return n.tok.pos
}

type floatNode struct {
	payload float64
	tok     *token
}

func (n floatNode) String() string {
	return strconv.FormatFloat(n.payload, 'g', -1, 64)
}

func (n floatNode) pos() position {
	return n.tok.pos
}

type nullNode struct {
	tok *token
}

func (n nullNode) String() string {
	return "null"
}

func (n nullNode) pos() position {
	return n.tok.pos
}

type boolNode struct {
	payload bool
	tok     *token
}

func (n boolNode) String() string {
	return strconv.FormatBool(n.payload)
}

func (n boolNode) pos() position {
	return n.tok.pos
}

type listNode struct {
	items []astNode
	tok   *token
}

func (n listNode) String() string {
	items := make([]string, len(n.items))
	for i, item := range n.items {
		items[i] = item.String()
	}

	return "[" + strings.Join(items, ", ") + "]"
}

func (n listNode) pos() position {
	return n.tok.pos
}

type fnNode struct {
	params []string
	body   astNode
	tok    *token
}

func (n fnNode) String() string {
	return "(" + strings.Join(n.params, ", ") + ") -> " + n.body.String()
}

func (n fnNode) pos() position {
	return n.tok.pos
}

type objectEntry struct {
	key astNode
	val astNode
}

func (n objectEntry) String() string {
	return n.key.String() + ": " + n.val.String()
}

type objectNode struct {
	entries []objectEntry
	tok     *token
}

func (n objectNode) String() string {
	entryStrings := make([]string, len(n.entries))
	for i, ent := range n.entries {
		entryStrings[i] = ent.String()
	}
	return "{ " + strings.Join(entryStrings, ", ") + " }"
}
func (n objectNode) pos() position {
	return n.tok.pos
}

type identifierNode struct {
	payload string
	tok     *token
}

func (n identifierNode) String() string {
	return n.payload
}
func (n identifierNode) pos() position {
	return n.tok.pos
}

type assignmentNode struct {
	left  astNode
	right astNode
	tok   *token
	isSet bool
}

func (n assignmentNode) String() string {
	if n.isSet {
		return fmt.Sprintf("%s = %s", n.left.String(), n.right.String())
	}
	return fmt.Sprintf("%s := %s", n.left.String(), n.right.String())
}

func (n assignmentNode) pos() position {
	return n.tok.pos
}

type propertyAccessNode struct {
	left  astNode
	right astNode
	tok   *token
}

func (n propertyAccessNode) String() string {
	return "(" + n.left.String() + "." + n.right.String() + ")"
}
func (n propertyAccessNode) pos() position {
	return n.tok.pos
}

type unaryNode struct {
	op    tokenKind
	right astNode
	tok   *token
}

func (n unaryNode) String() string {
	opTok := token{kind: n.op}
	return opTok.String() + n.right.String()
}
func (n unaryNode) pos() position {
	return n.tok.pos
}

type binaryNode struct {
	op    tokenKind
	left  astNode
	right astNode
	tok   *token
}

func (n binaryNode) String() string {
	opTok := token{kind: n.op}
	return "(" + n.left.String() + " " + opTok.String() + " " + n.right.String() + ")"
}
func (n binaryNode) pos() position {
	return n.tok.pos
}

type fnCallNode struct {
	fn   astNode
	args []astNode
	tok  *token
}

func (n fnCallNode) String() string {
	argStrings := make([]string, len(n.args))
	for i, arg := range n.args {
		argStrings[i] = arg.String()
	}

	return fmt.Sprintf("%s(%s)", n.fn, strings.Join(argStrings, ", "))
}
func (n fnCallNode) pos() position {
	return n.tok.pos
}

type ifExprNode struct {
	cond  astNode
	then  astNode
	else_ astNode
	tok   *token
}

func (n ifExprNode) String() string {
	if n.else_ == nil {
		return fmt.Sprintf("if %s %s", n.cond, n.then)
	}
	return fmt.Sprintf("if %s %s else %s", n.cond, n.then, n.else_)
}

func (n ifExprNode) pos() position {
	return n.tok.pos
}

type blockNode struct {
	exprs []astNode
	tok   *token
}

func (n blockNode) String() string {
	exprStrings := make([]string, len(n.exprs))
	for i, ex := range n.exprs {
		exprStrings[i] = ex.String()
	}
	return "{ " + strings.Join(exprStrings, ", ") + " }"
}

func (n blockNode) pos() position {
	return n.tok.pos
}

type parser struct {
	tokens        []token
	index         int
	minBinaryPrec []int
}

type parseError struct {
	reason string
	pos    position
}

func (e parseError) Error() string {
	return fmt.Sprintf("Parse error at %s: %s", e.pos.String(), e.reason)
}

func NewParser(tokens []token) parser {
	return parser{
		tokens:        tokens,
		index:         0,
		minBinaryPrec: []int{0},
	}
}

func (p *parser) lastMinPrec() int {
	return p.minBinaryPrec[len(p.minBinaryPrec)-1]
}

func (p *parser) pushMinPrec(prec int) {
	p.minBinaryPrec = append(p.minBinaryPrec, prec)
}

func (p *parser) popMinPrec() {
	p.minBinaryPrec = p.minBinaryPrec[:len(p.minBinaryPrec)-1]
}

func (p *parser) isEOF() bool {
	return p.index == len(p.tokens)
}

func (p *parser) peek() token {
	return p.tokens[p.index]
}

func (p *parser) next() token {
	tok := p.tokens[p.index]

	if p.index < len(p.tokens) {
		p.index++
	}

	return tok
}

func (p *parser) back() {
	if p.index > 0 {
		p.index--
	}
}

func (p *parser) expect(kind tokenKind) (token, error) {
	tok := token{kind: kind}

	if p.isEOF() {
		return token{kind: unknown}, parseError{
			reason: fmt.Sprintf("Unexpected end of input, expected %s", tok),
			pos:    tok.pos,
		}
	}

	next := p.next()
	if next.kind != kind {
		return token{kind: unknown}, parseError{
			reason: fmt.Sprintf("Unexpected token\x1b[34;1m%s\x1b[0;0m, expected %s", next, tok),
			pos:    next.pos,
		}
	}

	return next, nil
}

func (p *parser) readUntil(kind tokenKind) []token {
	tokens := []token{}
	for !p.isEOF() && p.peek().kind != kind {
		tokens = append(tokens, p.next())
	}
	return tokens
}

func (p *parser) parseAssignment(left astNode) (astNode, error) {
	if p.peek().kind != assign &&
		p.peek().kind != set {
		return left, nil
	}

	next := p.next()
	node := assignmentNode{
		isSet: next.kind != assign,
		left:  left,
		tok:   &next,
	}

	right, err := p.parseNode()
	if err != nil {
		return nil, err
	}
	node.right = right

	return node, nil
}

// parseSubNode parses independent  "terms", like unary and binary expressions.
func (p *parser) parseSubNode() (astNode, error) {
	p.pushMinPrec(0)
	defer p.popMinPrec()

	node, err := p.parseUnit()
	if err != nil {
		return nil, err
	}

	for !p.isEOF() {
		switch p.peek().kind {
		case dot:
			next := p.next()
			right, err := p.parseUnit()
			if err != nil {
				return nil, err
			}

			node = propertyAccessNode{left: node, right: right, tok: &next}
		case leftParen:
			next := p.next()
			args := []astNode{}

			for !p.isEOF() && p.peek().kind != rightParen {
				arg, err := p.parseNode()
				if err != nil {
					return nil, err
				}

				args = append(args, arg)

				if _, err = p.expect(comma); err != nil {
					return nil, err
				}
			}

			if _, err := p.expect(rightParen); err != nil {
				return nil, err
			}

			node = fnCallNode{
				fn:   node,
				args: args,
				tok:  &next,
			}
		default:
			return node, nil
		}
	}

	return node, nil
}

func (p *parser) parseNode() (astNode, error) {
	node, err := p.parseSubNode()
	if err != nil {
		return nil, err
	}

	for !p.isEOF() && p.peek().kind != comma {
		switch p.peek().kind {
		case assign, set:
			return p.parseAssignment(node)
		case plus, minus, times, divide, modulus, and, or, greater, less, eq, geq, leq, neq, dotdot:
			minPrec := p.lastMinPrec()
			for {
				if p.isEOF() {
					return nil, parseError{
						reason: "Incomplete binary expression",
						pos:    p.peek().pos,
					}
				}

				peeked := p.peek()
				op := peeked.kind
				prec := infixOpPrecedence(op)
				if prec <= minPrec {
					break
				}
				p.next() // eat the operator

				if p.isEOF() {
					return nil, parseError{
						reason: fmt.Sprintf("Incomplete binary expression with %s", token{kind: op}),
						pos:    p.peek().pos,
					}
				}

				p.pushMinPrec(prec)
				right, err := p.parseNode()
				if err != nil {
					return nil, err
				}
				p.popMinPrec()

				node = binaryNode{
					op:    op,
					left:  node,
					right: right,
					tok:   &peeked,
				}

			}
			return node, nil
		default:
			return node, nil
		}
	}

	return node, nil
}

func infixOpPrecedence(op tokenKind) int {
	switch op {
	case modulus:
		return 80
	case times, divide:
		return 50
	case plus, minus:
		return 40
	case less, greater, leq, geq:
		return 35
	case eq, neq:
		return 30
	case and:
		return 20
	// case xor:
	// 	return 15
	case or:
		return 10
	case dotdot:
		return 5
	default:
		return -1
	}
}

// parseUnit parses a single unit (smallest syntactic "unit"), like
// literals, function literals, grouped expresssiions, etc.
func (p *parser) parseUnit() (astNode, error) {
	tok := p.next()
	switch tok.kind {
	case stringLiteral:
		return p.parseString(tok)
	case numberLiteral:
		return p.parseNumber(tok)
	case trueLiteral:
		return boolNode{payload: true, tok: &tok}, nil
	case falseLiteral:
		return boolNode{payload: false, tok: &tok}, nil
	case nullLiteral:
		return nullNode{tok: &tok}, nil
	case identifier:
		if p.peek().kind == singleArrow {
			return p.parseFunctionLiteral(tok)
		} else {
			return identifierNode{payload: tok.payload, tok: &tok}, nil
		}
	case minus, not:
		right, err := p.parseSubNode()
		if err != nil {
			return nil, err
		}

		return unaryNode{op: tok.kind, right: right, tok: &tok}, nil
	case leftBracket:
		p.pushMinPrec(0)
		defer p.popMinPrec()

		nodes := []astNode{}
		for !p.isEOF() && p.peek().kind != rightBracket {
			node, err := p.parseNode()
			if err != nil {
				return nil, err
			}
			if _, err := p.expect(comma); err != nil {
				return nil, err
			}

			nodes = append(nodes, node)
		}

		if _, err := p.expect(rightBracket); err != nil {
			return nil, err
		}

		return listNode{items: nodes, tok: &tok}, nil
	case leftBrace:
		p.pushMinPrec(0)
		defer p.popMinPrec()

		// empty {} is an empty object
		if p.peek().kind == rightBrace {
			p.next()
			return objectNode{entries: []objectEntry{}, tok: &tok}, nil
		}

		firstExpr, err := p.parseNode()
		if err != nil {
			return nil, err
		}

		if p.isEOF() {
			return nil, parseError{
				reason: fmt.Sprintf("Unexpected end of input inside block or object"),
				pos:    tok.pos,
			}
		}

		if p.peek().kind == colon {
			// it's an object
			p.next()
			valExpr, err := p.parseNode()

			if err != nil {
				return nil, err
			}
			if _, err := p.expect(comma); err != nil {
				return nil, err
			}

			entries := []objectEntry{
				{key: firstExpr, val: valExpr},
			}

			for !p.isEOF() && p.peek().kind != rightBrace {
				key, err := p.parseNode()
				if err != nil {
					return nil, err
				}
				if _, err := p.expect(colon); err != nil {
					return nil, err
				}

				val, err := p.parseNode()
				if err != nil {
					return nil, err
				}
				if _, err := p.expect(comma); err != nil {
					return nil, err
				}

				entries = append(entries, objectEntry{
					key: key,
					val: val,
				})
			}

			if _, err := p.expect(rightBrace); err != nil {
				return nil, err
			}

			return objectNode{entries: entries, tok: &tok}, nil
		}

		exprs := []astNode{firstExpr}
		if _, err := p.expect(comma); err != nil {
			return nil, err
		}

		for !p.isEOF() && p.peek().kind != rightBrace {
			expr, err := p.parseNode()
			if err != nil {
				return nil, err
			}
			if _, err := p.expect(comma); err != nil {
				return nil, err
			}

			exprs = append(exprs, expr)
		}
		if _, err := p.expect(rightBrace); err != nil {
			return nil, err
		}

		return blockNode{exprs: exprs, tok: &tok}, nil

	case leftParen:
		// can be grouped expression or function literal
		p.pushMinPrec(0)
		defer p.popMinPrec()

		startIndex := p.index - 1
		exprs := []astNode{}

		for !p.isEOF() && p.peek().kind != rightParen {
			expr, err := p.parseNode()
			if err != nil {
				return nil, err
			}
			if _, err := p.expect(comma); err != nil {
				return nil, err
			}

			exprs = append(exprs, expr)
		}

		if _, err := p.expect(rightParen); err != nil {
			return nil, err
		}

		if p.peek().kind == singleArrow {
			p.index = startIndex // backtrack to the start of the parens
			return p.parseFunctionLiteral(tok)
		}

		// if len(exprs) == 1 {
		// 	return exprs[0], nil
		// }

		return blockNode{exprs: exprs, tok: &tok}, nil
	case ifKeyword:
		p.pushMinPrec(0)
		defer p.popMinPrec()

		cond, err := p.parseNode()
		if err != nil {
			return nil, err
		}

		if p.peek().kind != leftBrace {
			return nil, parseError{
				reason: fmt.Sprintf("Expected block after if condition"),
				pos:    p.peek().pos,
			}
		}

		then, err := p.parseNode()
		if err != nil {
			return nil, err
		}

		if p.peek().kind == elseKeyword {
			p.next()
			else_, err := p.parseNode()
			if err != nil {
				return nil, err
			}
			return ifExprNode{cond: cond, then: then, else_: else_, tok: &tok}, nil
		}

		return ifExprNode{cond: cond, then: then, else_: nil, tok: &tok}, nil
	}

	return nil, parseError{
		reason: fmt.Sprintf("Unexpected token %s at start of unit", tok),
		pos:    tok.pos,
	}
}

// (a, b) => {}
// ^
// | parser is here (tok)
//
// a => {}
// ^
// | (tok)
func (p *parser) parseFunctionLiteral(tok token) (astNode, error) {
	p.pushMinPrec(0)
	defer p.popMinPrec()

	args := []string{}

	// parse args
	if tok.kind == identifier {
		args = append(args, tok.payload)
	} else if tok.kind == leftParen {
		p.next() // eat the left paren
		for !p.isEOF() && p.peek().kind != rightParen {
			arg, err := p.expect(identifier)
			if err != nil {
				return nil, err
			}
			args = append(args, arg.payload)

			if _, err := p.expect(comma); err != nil {
				return nil, err
			}
		}

		if _, err := p.expect(rightParen); err != nil {
			return nil, err
		}
	}

	// invariant
	if _, err := p.expect(singleArrow); err != nil {
		return nil, err
	}

	body, err := p.parseNode()
	if err != nil {
		return nil, err
	}

	return fnNode{
		params: args,
		body:   body,
		tok:    &tok,
	}, nil
}

func (p *parser) parseString(tok token) (astNode, error) {
	builder := bytes.Buffer{}
	runes := []rune(tok.payload)

	for i := 0; i < len(runes); i++ {
		ch := runes[i]

		if ch == '\\' {
			if i+1 >= len(runes) {
				break
			}
			i += 1
			ch = runes[i]

			switch ch {
			case 't':
				_ = builder.WriteByte('\t')
			case 'n':
				_ = builder.WriteByte('\n')
			case 'r':
				_ = builder.WriteByte('\r')
			case 'f':
				_ = builder.WriteByte('\f')
			case 'x':
				if i+2 >= len(runes) {
					_ = builder.WriteByte('x')
					continue
				}

				hexCode, err := strconv.ParseUint(string(runes[i+1])+string(runes[i+2]), 16, 8)
				if err == nil {
					i += 2
					_ = builder.WriteByte(uint8(hexCode))
				} else {
					_ = builder.WriteByte('x')
				}
			default:
				_, _ = builder.WriteRune(ch)
			}
		} else {
			_, _ = builder.WriteRune(ch)
		}
	}

	bytes := builder.Bytes()

	return stringNode{payload: bytes, tok: &tok}, nil
}

func (p *parser) parseNumber(tok token) (astNode, error) {
	if strings.ContainsRune(tok.payload, '.') {
		f, err := strconv.ParseFloat(tok.payload, 64)
		if err != nil {
			return nil, parseError{reason: err.Error(), pos: tok.pos}
		}
		return floatNode{
			payload: f,
			tok:     &tok,
		}, nil
	}
	n, err := strconv.ParseInt(tok.payload, 10, 64)
	if err != nil {
		return nil, parseError{reason: err.Error(), pos: tok.pos}
	}
	return intNode{
		payload: n,
		tok:     &tok,
	}, nil
}

func (p *parser) parse() ([]astNode, error) {
	nodes := []astNode{}

	for !p.isEOF() {
		node, err := p.parseNode()
		if err != nil {
			return nodes, err
		}

		if _, err = p.expect(comma); err != nil {
			return nodes, err
		}

		nodes = append(nodes, node)
	}

	return nodes, nil
}
