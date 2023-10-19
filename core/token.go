package core

import (
	"fmt"
	"strings"
	"unicode"
)

type tokenKind int

const (
	UNKNOWN tokenKind = iota
	COMMENT

	// language tokens
	COMMA
	DOT
	DOTDOT
	COLON
	LEFT_PAREN
	RIGHT_PAREN
	LEFT_BRACKET
	RIGHT_BRACKET
	LEFT_BRACE
	RIGHT_BRACE
	ASSIGN // :=
	SET    // =
	SINGLE_ARROW
	DOUBLE_ARROW

	// binary operators
	PLUS
	MINUS
	TIMES
	DIVIDE
	MODULUS
	AND
	OR
	GREATER
	LESS
	EQ
	GEQ
	LEQ
	NEQ

	// unary operators
	NOT
	NEGATE

	// keywords
	BREAK_KEYWORD
	CONTINUE_KEYWORD
	IF_KEYWORD
	ELSE_KEYWORD
	FOR_KEYWORD
	IN_KEYWORD
	RETURN_KEYWORD

	// literals
	IDENTIFIER
	TRUE_LITERAL
	FALSE_LITERAL
	STRING_LITERAL
	NUMBER_LITERAL
	NULL_LITERAL
)

type position struct {
	line     int
	col      int
	filename string
	Offset   int
}

func (p position) String() string {
	return fmt.Sprintf("[%d:%d]", p.line, p.col)
}

type token struct {
	Kind    tokenKind
	Pos     position
	Payload string
	Length  uint
}

func (t token) String() string {
	switch t.Kind {
	case COMMA:
		return ","
	case DOT:
		return "."
	case DOTDOT:
		return ".."
	case COLON:
		return ":"
	case LEFT_PAREN:
		return "("
	case RIGHT_PAREN:
		return ")"
	case LEFT_BRACKET:
		return "["
	case RIGHT_BRACKET:
		return "]"
	case LEFT_BRACE:
		return "{"
	case RIGHT_BRACE:
		return "}"
	case ASSIGN:
		return ":="
	case SET:
		return "="
	case SINGLE_ARROW:
		return "->"
	case DOUBLE_ARROW:
		return "=>"

	case PLUS:
		return "+"
	case MINUS:
		return "-"
	case TIMES:
		return "*"
	case DIVIDE:
		return "/"
	case MODULUS:
		return "%"
	case AND:
		return "&&"
	case OR:
		return "||"
	case GREATER:
		return ">"
	case LESS:
		return "<"
	case EQ:
		return "=="
	case GEQ:
		return ">="
	case LEQ:
		return "<="
	case NEQ:
		return "!="

	case NOT:
		return "!"
	case NEGATE:
		return "-"

	case BREAK_KEYWORD:
		return "break"
	case CONTINUE_KEYWORD:
		return "continue"
	case IF_KEYWORD:
		return "if"
	case ELSE_KEYWORD:
		return "else"
	case FOR_KEYWORD:
		return "for"
	case IN_KEYWORD:
		return "in"
	case RETURN_KEYWORD:
		return "return"

	case IDENTIFIER:
		return fmt.Sprintf("var(%s)", t.Payload)
	case TRUE_LITERAL:
		return "true"
	case FALSE_LITERAL:
		return "false"
	case NULL_LITERAL:
		return "null"
	case STRING_LITERAL:
		return fmt.Sprintf("string(%s)", t.Payload)
	case NUMBER_LITERAL:
		return fmt.Sprintf("number(%s)", t.Payload)

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

func NewTokenizer(source string) tokenizer {
	return tokenizer{
		source:   []rune(source),
		index:    0,
		filename: "<input>",
		line:     1,
		col:      0,
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
		t.col = 0
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
	offset := t.index
	if offset > 0 {
		offset -= 1
	}
	return position{
		line:     t.line,
		col:      t.col,
		Offset:   offset,
		filename: t.filename,
	}
}

func (t *tokenizer) nextToken() token {
	ch := t.next()

	switch ch {
	case ',':
		return token{Kind: COMMA, Pos: t.pos(), Length: 1}
	case '.':
		if !t.isEOF() && t.peek() == '.' {
			pos := t.pos()
			_ = t.next()

			return token{Kind: DOTDOT, Pos: pos, Length: 1}
		}
		return token{Kind: DOT, Pos: t.pos(), Length: 1}
	case '(':
		return token{Kind: LEFT_PAREN, Pos: t.pos(), Length: 1}
	case ')':
		return token{Kind: RIGHT_PAREN, Pos: t.pos(), Length: 1}
	case '[':
		return token{Kind: LEFT_BRACKET, Pos: t.pos(), Length: 1}
	case ']':
		return token{Kind: RIGHT_BRACKET, Pos: t.pos(), Length: 1}
	case '{':
		return token{Kind: LEFT_BRACE, Pos: t.pos(), Length: 1}
	case '}':
		return token{Kind: RIGHT_BRACE, Pos: t.pos()}
	case ':':
		if !t.isEOF() && t.peek() == '=' {
			pos := t.pos()
			t.next()
			return token{Kind: ASSIGN, Pos: pos, Length: 2}
		}
		return token{Kind: COLON, Pos: t.pos(), Length: 1}

	case '=':
		if !t.isEOF() && t.peek() == '>' {
			pos := t.pos()
			t.next()
			return token{Kind: DOUBLE_ARROW, Pos: pos, Length: 2}
		} else if !t.isEOF() && t.peek() == '=' {
			pos := t.pos()
			t.next()
			return token{Kind: EQ, Pos: pos, Length: 1}
		}
		return token{Kind: SET, Pos: t.pos(), Length: 1}
	case '>':
		if !t.isEOF() && t.peek() == '=' {
			pos := t.pos()
			t.next()
			return token{Kind: GEQ, Pos: pos, Length: 2}
		}
		return token{Kind: GREATER, Pos: t.pos(), Length: 1}
	case '<':
		if !t.isEOF() && t.peek() == '=' {
			pos := t.pos()
			t.next()
			return token{Kind: LEQ, Pos: pos, Length: 2}
		}
		return token{Kind: LESS, Pos: t.pos(), Length: 1}
	case '!':
		if !t.isEOF() && t.peek() == '=' {
			pos := t.pos()
			t.next()
			return token{Kind: NEQ, Pos: pos, Length: 2}
		}
		return token{Kind: NOT, Pos: t.pos(), Length: 1}

	case '+':
		return token{Kind: PLUS, Pos: t.pos(), Length: 1}
	case '-':
		if !t.isEOF() && t.peek() == '>' {
			pos := t.pos()
			t.next()
			return token{Kind: SINGLE_ARROW, Pos: pos, Length: 2}
		}
		return token{Kind: MINUS, Pos: t.pos(), Length: 1}
	case '*':
		return token{Kind: TIMES, Pos: t.pos(), Length: 1}
	case '/':
		if !t.isEOF() && t.peek() == '/' {
			pos := t.pos()
			t.next()
			commentString := t.readUntil('\n')
			return token{Kind: COMMENT, Pos: pos, Payload: commentString, Length: 2 + uint(len(commentString))}
		}
		return token{Kind: DIVIDE, Pos: t.pos(), Length: 1}
	case '%':
		return token{Kind: MODULUS, Pos: t.pos()}
	case '"':
		length := uint(0)
		pos := t.pos()
		builder := strings.Builder{}
		for !t.isEOF() && t.peek() != '"' {
			ch := t.next()
			length += 1
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
				Kind:    STRING_LITERAL,
				Pos:     pos,
				Payload: builder.String(),
				Length:  length,
			}
		}

		t.next()
		return token{
			Kind:    STRING_LITERAL,
			Pos:     pos,
			Payload: builder.String(),
			Length:  length + 1,
		}
	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		pos := t.pos()
		payload := string(ch) + t.readNumber()
		return token{Kind: NUMBER_LITERAL, Pos: pos, Payload: payload, Length: uint(len(payload))}
	default:
		pos := t.pos()
		payload := string(ch) + t.readIdentifier()
		switch payload {
		case "true":
			return token{Kind: TRUE_LITERAL, Pos: pos, Length: 4}
		case "false":
			return token{Kind: FALSE_LITERAL, Pos: pos, Length: 5}
		case "null":
			return token{Kind: NULL_LITERAL, Pos: pos, Length: 4}
		case "if":
			return token{Kind: IF_KEYWORD, Pos: pos, Length: 2}
		case "else":
			return token{Kind: ELSE_KEYWORD, Pos: pos, Length: 4}
		case "for":
			return token{Kind: FOR_KEYWORD, Pos: pos, Length: 3}
		case "in":
			return token{Kind: IN_KEYWORD, Pos: pos, Length: 2}
		case "break":
			return token{Kind: BREAK_KEYWORD, Pos: pos, Length: 5}
		case "continue":
			return token{Kind: CONTINUE_KEYWORD, Pos: pos, Length: 8}
		case "return":
			return token{Kind: RETURN_KEYWORD, Pos: pos, Length: 8}
		default:
			return token{Kind: IDENTIFIER, Pos: pos, Payload: payload, Length: uint(len(payload))}
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

	last := token{Kind: COMMA}

	for !t.isEOF() {
		next := t.nextToken()

		// separate expressions
		if (last.Kind != LEFT_PAREN && last.Kind != LEFT_BRACKET &&
			last.Kind != LEFT_BRACE && last.Kind != COMMA) &&
			(next.Kind == RIGHT_PAREN || next.Kind == RIGHT_BRACKET ||
				next.Kind == RIGHT_BRACE) {
			tokens = append(tokens, token{
				Kind: COMMA,
				Pos:  t.pos(),
			})
		}

		if next.Kind == COMMENT {
			next = last
		} else {
			tokens = append(tokens, next)
		}

		for !t.isEOF() && unicode.IsSpace(t.peek()) {
			if t.peek() == '\n' {
				// if we hit a token that can end a statement, insert a comma
				switch next.Kind {
				case RIGHT_PAREN, RIGHT_BRACE, RIGHT_BRACKET, IDENTIFIER, NUMBER_LITERAL, STRING_LITERAL, TRUE_LITERAL, FALSE_LITERAL, NULL_LITERAL:
					next = token{
						Kind: COMMA,
						Pos:  t.pos(),
					}
					tokens = append(tokens, next)
				}
			}
			t.next()
		}

		if next.Kind != COMMENT {
			last = next
		}
	}

	if last.Kind != COMMA {
		tokens = append(tokens, token{
			Kind: COMMA,
			Pos:  t.pos(),
		})
	}

	return tokens
}
