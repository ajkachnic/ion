package main

import (
	"fmt"
	"strings"
	"unicode"
)

type tokenKind int

const (
	unknown tokenKind = iota
	comment

	// language tokens
	comma
	dot
	dotdot
	colon
	leftParen
	rightParen
	leftBracket
	rightBracket
	leftBrace
	rightBrace
	assign // :=
	set    // =
	singleArrow
	doubleArrow

	// binary operators
	plus
	minus
	times
	divide
	modulus
	and
	or
	greater
	less
	eq
	geq
	leq
	neq

	// unary operators
	not
	negate

	// keywords
	breakKeyword
	ifKeyword
	elseKeyword
	forKeyword
	inKeyword
	returnKeyword

	// literals
	identifier
	trueLiteral
	falseLiteral
	stringLiteral
	numberLiteral
	nullLiteral
)

type position struct {
	line     int
	col      int
	filename string
}

func (p position) String() string {
	return fmt.Sprintf("[%d:%d]", p.line, p.col)
}

type token struct {
	kind    tokenKind
	pos     position
	payload string
}

func (t token) String() string {
	switch t.kind {
	case comma:
		return ","
	case dot:
		return "."
	case dotdot:
		return ".."
	case colon:
		return ":"
	case leftParen:
		return "("
	case rightParen:
		return ")"
	case leftBracket:
		return "["
	case rightBracket:
		return "]"
	case leftBrace:
		return "{"
	case rightBrace:
		return "}"
	case assign:
		return ":="
	case set:
		return "="
	case singleArrow:
		return "->"
	case doubleArrow:
		return "=>"

	case plus:
		return "+"
	case minus:
		return "-"
	case times:
		return "*"
	case divide:
		return "/"
	case modulus:
		return "%"
	case and:
		return "&&"
	case or:
		return "||"
	case greater:
		return ">"
	case less:
		return "<"
	case eq:
		return "=="
	case geq:
		return ">="
	case leq:
		return "<="
	case neq:
		return "!="

	case not:
		return "!"
	case negate:
		return "-"

	case breakKeyword:
		return "break"
	case ifKeyword:
		return "if"
	case elseKeyword:
		return "else"
	case forKeyword:
		return "for"
	case inKeyword:
		return "in"
	case returnKeyword:
		return "return"

	case identifier:
		return fmt.Sprintf("var(%s)", t.payload)
	case trueLiteral:
		return "true"
	case falseLiteral:
		return "false"
	case nullLiteral:
		return "null"
	case stringLiteral:
		return fmt.Sprintf("string(%s)", t.payload)
	case numberLiteral:
		return fmt.Sprintf("number(%s)", t.payload)

	default:
		return "<unknown>"
	}
}

type tokenizer struct {
	source   []rune
	index    int
	filename string
	line     int
	col      int
}

func newTokenizer(source string) tokenizer {
	return tokenizer{
		source:   []rune(source),
		index:    0,
		filename: "<input>",
		line:     1,
		col:      1,
	}
}

func (t *tokenizer) isEOF() bool {
	return t.index >= len(t.source)
}

func (t *tokenizer) next() rune {
	char := t.source[t.index]

	// ensure we don't go past EOF
	if t.index < len(t.source) {
		t.index++
	}

	if char == '\n' {
		t.line++
		t.col = 1
	} else {
		t.col++
	}

	return char
}

func (t *tokenizer) peek() rune {
	return t.source[t.index]
}

func (t *tokenizer) peekAhead(n int) rune {
	if t.index+n >= len(t.source) {
		return ' '
	}

	return t.source[t.index+n]
}

func (t *tokenizer) readUntil(ch rune) string {
	read := []rune{}
	for !t.isEOF() && t.peek() != ch {
		read = append(read, t.next())
	}

	return string(read)
}

func (t *tokenizer) readIdentifier() string {
	ident := []rune{}
	for !t.isEOF() {
		ch := t.peek()
		if unicode.IsLetter(ch) || unicode.IsDigit(ch) || ch == '_' || ch == '?' {
			ident = append(ident, t.next())
		} else {
			break
		}
	}

	return string(ident)
}

func (t *tokenizer) readNumber() string {
	literal := []rune{}
	dot := false

	for !t.isEOF() {
		ch := t.peek()
		if unicode.IsDigit(ch) {
			literal = append(literal, t.next())
		} else if ch == '.' && !dot {
			if t.peekAhead(1) == '.' {
				break
			}
			dot = true
			literal = append(literal, t.next())
		} else {
			break
		}
	}

	return string(literal)
}

func (t *tokenizer) pos() position {
	return position{
		line:     t.line,
		col:      t.col,
		filename: t.filename,
	}
}

func (t *tokenizer) nextToken() token {
	ch := t.next()

	switch ch {
	case ',':
		return token{kind: comma, pos: t.pos()}
	case '.':
		if !t.isEOF() && t.peek() == '.' {
			pos := t.pos()
			_ = t.next()

			return token{kind: dotdot, pos: pos}
		}
		return token{kind: dot, pos: t.pos()}
	case '(':
		return token{kind: leftParen, pos: t.pos()}
	case ')':
		return token{kind: rightParen, pos: t.pos()}
	case '[':
		return token{kind: leftBracket, pos: t.pos()}
	case ']':
		return token{kind: rightBracket, pos: t.pos()}
	case '{':
		return token{kind: leftBrace, pos: t.pos()}
	case '}':
		return token{kind: rightBrace, pos: t.pos()}
	case ':':
		if !t.isEOF() && t.peek() == '=' {
			pos := t.pos()
			t.next()
			return token{kind: assign, pos: pos}
		}
		return token{kind: colon, pos: t.pos()}

	case '=':
		if !t.isEOF() && t.peek() == '>' {
			pos := t.pos()
			t.next()
			return token{kind: doubleArrow, pos: pos}
		} else if !t.isEOF() && t.peek() == '=' {
			pos := t.pos()
			t.next()
			return token{kind: eq, pos: pos}
		}
		return token{kind: set, pos: t.pos()}
	case '>':
		if !t.isEOF() && t.peek() == '=' {
			pos := t.pos()
			t.next()
			return token{kind: geq, pos: pos}
		}
		return token{kind: greater, pos: t.pos()}
	case '<':
		if !t.isEOF() && t.peek() == '=' {
			pos := t.pos()
			t.next()
			return token{kind: leq, pos: pos}
		}
		return token{kind: less, pos: t.pos()}
	case '!':
		if !t.isEOF() && t.peek() == '=' {
			pos := t.pos()
			t.next()
			return token{kind: neq, pos: pos}
		}
		return token{kind: not, pos: t.pos()}

	case '+':
		return token{kind: plus, pos: t.pos()}
	case '-':
		if !t.isEOF() && t.peek() == '>' {
			pos := t.pos()
			t.next()
			return token{kind: singleArrow, pos: pos}
		}
		return token{kind: minus, pos: t.pos()}
	case '*':
		return token{kind: times, pos: t.pos()}
	case '/':
		if !t.isEOF() && t.peek() == '/' {
			pos := t.pos()
			t.next()
			commentString := strings.TrimSpace(t.readUntil('\n'))
			return token{kind: comment, pos: pos, payload: commentString}
		}
		return token{kind: divide, pos: t.pos()}
	case '%':
		return token{kind: modulus, pos: t.pos()}
	case '"':
		pos := t.pos()
		builder := strings.Builder{}
		for !t.isEOF() && t.peek() != '"' {
			ch := t.next()
			if ch == '\\' {
				builder.WriteRune(ch)
				if t.isEOF() {
					break
				} else {
					ch = t.next()
				}
			}
			builder.WriteRune(ch)
		}

		if t.isEOF() {
			return token{
				kind:    stringLiteral,
				pos:     pos,
				payload: builder.String(),
			}
		}

		t.next()
		return token{
			kind:    stringLiteral,
			pos:     pos,
			payload: builder.String(),
		}
	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		pos := t.pos()
		payload := string(ch) + t.readNumber()
		return token{kind: numberLiteral, pos: pos, payload: payload}
	default:
		pos := t.pos()
		payload := string(ch) + t.readIdentifier()
		switch payload {
		case "true":
			return token{kind: trueLiteral, pos: pos}
		case "false":
			return token{kind: falseLiteral, pos: pos}
		case "null":
			return token{kind: nullLiteral, pos: pos}
		case "if":
			return token{kind: ifKeyword, pos: pos}
		case "else":
			return token{kind: elseKeyword, pos: pos}
		case "for":
			return token{kind: forKeyword, pos: pos}
		case "in":
			return token{kind: inKeyword, pos: pos}
		case "break":
			return token{kind: breakKeyword, pos: pos}
		case "return":
			return token{kind: returnKeyword, pos: pos}
		default:
			return token{kind: identifier, pos: pos, payload: payload}
		}
	}
}

func (t *tokenizer) Tokenize() []token {
	tokens := []token{}

	// check for shebang and skip
	if !t.isEOF() && t.peek() == '#' && t.peekAhead(1) == '!' {
		t.readUntil('\n')
		if !t.isEOF() {
			t.next()
		}
	}

	// skip whitespace
	for !t.isEOF() && unicode.IsSpace(t.peek()) {
		t.next()
	}

	last := token{kind: comma}

	for !t.isEOF() {
		next := t.nextToken()

		// separate expressions
		if (last.kind != leftParen && last.kind != leftBracket &&
			last.kind != leftBrace && last.kind != comma) &&
			(next.kind == rightParen || next.kind == rightBracket ||
				next.kind == rightBrace) {
			tokens = append(tokens, token{
				kind: comma,
				pos:  t.pos(),
			})
		}

		if next.kind == comment {
			next = last
		} else {
			tokens = append(tokens, next)
		}

		for !t.isEOF() && unicode.IsSpace(t.peek()) {
			if t.peek() == '\n' {
				// if we hit a token that can end a statement, insert a comma
				switch next.kind {
				case rightParen, rightBrace, rightBracket, identifier, numberLiteral, stringLiteral, trueLiteral, falseLiteral, nullLiteral:
					next = token{
						kind: comma,
						pos:  t.pos(),
					}
					tokens = append(tokens, next)
				}
			}
			t.next()
		}

		if next.kind != comment {
			last = next
		}
	}

	if last.kind != comma {
		tokens = append(tokens, token{
			kind: comma,
			pos:  t.pos(),
		})
	}

	return tokens
}
