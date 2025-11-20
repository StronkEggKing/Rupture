########################
# LIBRARIES AND MODULES
########################

from strings_with_arrows import *

import string


############
# CONSTANTS
############

DIGITS = '0123456789'
LETTERS = string.ascii_letters
LETTERS_DIGITS = LETTERS + DIGITS


############
# ERROR
############

class Error :
    def __init__(self, PosStart, PosEnd, error_name, detail) :
        self.PosStart = PosStart
        self.PosEnd = PosEnd
        self.error_name = error_name
        self.detail = detail

    def AsString(self) :
        result = f'{self.error_name}: {self.detail}, '
        result += f'File {self.PosStart.filename}, line {self.PosStart.line + 1}'
        result += f'\n\n' + string_with_arrows(self.PosStart.filetext, self.PosStart, self.PosEnd)
        return result

class WrongChar(Error) :
    def __init__(self, PosStart, PosEnd, detail) :
        super().__init__(PosStart, PosEnd, 'Wrong Character, bitch', detail)

class InvalidSyntax(Error) :
    def __init__(self, PosStart, PosEnd, detail) :
        super().__init__(PosStart, PosEnd, 'Invaild Syntax, bitch', detail)

class ExpectedChar(Error) :
    def __init__(self, PosStart, PosEnd, detail) :
        super().__init__(PosStart, PosEnd, "I expected a character, bitch!", detail)

class Runtime(Error) :
    def __init__(self, PosStart, PosEnd, detail, context=None) :
        super().__init__(PosStart, PosEnd, "YOU FUCKED ME UP, MAN!", detail)
        self.context = context

    def AsString(self) :
        result = self.generate_traceback()
        result = f'{self.error_name}: {self.detail}, '
        result += f'\n\n' + string_with_arrows(self.PosStart.filetext, self.PosStart, self.PosEnd)
        return result

    def generate_traceback(self):
        result = ''
        pos = self.PosStart
        context = self.context

        while context :
            result = f'    File {pos.filename}, line {pos.line + 1}, in {context.DisplayName}\n' + result
            pos = context.ParentEntryPos
            context = context.parent

        return 'Traceback (most recent call last):\n' + result



############
# POSITION
############

class Position :
    def __init__(self, idx, line, column, filename, filetext) :
        self.idx = idx
        self.line = line
        self.column = column
        self.filename = filename
        self.filetext = filetext

    def advance(self, CurrentChar=None) :
        self.idx += 1
        self.column += 1

        if CurrentChar == '\n' :
            self.line += 1
            self.column = 0
    
        return self

    def copy(self) :
        return Position(self.idx, self.line, self.column, self.filename, self.filetext)


############
# TOKENS
############

TT_INT = 'INT'
TT_FLOAT = 'FLOAT'
TT_STRING = 'STRING'
TT_IDENTIFIER = 'IDENTIFIER'
TT_KEYWORD = 'KEYWORD'
TT_PLUS = 'PLUS'
TT_MINUS = 'MINUS'
TT_MULTIPLY = 'MULTIPLY'
TT_DIVIDE = 'DIVIDE'
TT_POWER = 'POWER'
TT_EQUAL = 'EQUAL'
TT_NE = 'NE'
TT_DE = 'DE'
TT_LT = 'LT'
TT_GT = 'GT'
TT_LTE = 'LTE'
TT_GTE = 'GTE'
TT_LeftParen = 'LeftParen'
TT_RightParen = 'RightParen'
TT_COMMA = 'COMMA'
TT_ARROW = 'ARROW'
TT_EOF = 'EOF'

KEYWORDS = [
    'spasm', # variable maker
    'plus', # "and"
    'slash', # "or"
    'op', # "not"
    'panic?', # "if"
    ':', # "then"
    'nopanic?', # "elif" / "elseif"
    'nope', # "else"
    'during', # "For"
    'towards', # "to"
    'shadow', # "step"
    'loopdammit', # "While"
    'rupt', # "function"
]

class Token :
    def __init__(self, type_, value=None, PosStart=None, PosEnd=None) :
        self.type = type_
        self.value = value
        self.PosStart = None
        self.PosEnd = None

        if PosStart is not None :
            self.PosStart = PosStart.copy()
            self.PosEnd = PosStart.copy()
            self.PosEnd.advance()

        if PosEnd is not None :
            self.PosEnd = PosEnd.copy()
    
    def matches(self, type_, value) :
            return self.type == type_ and self.value == value

    def __repr__(self) :
        if self.value is not None: 
            return f'{self.type}:{self.value}'
        return f'{self.type}'


############
# LEXER
############

class Lexer :
    def __init__(self, filename, text) :
        self.text = text
        self.filename = filename
        self.pos = Position(-1, 0, -1, filename, text)
        self.CurrentChar = None
        self.advance()

    def advance(self) :
        self.pos.advance(self.CurrentChar)
        self.CurrentChar = self.text[self.pos.idx] if self.pos.idx < len(self.text) else None

    def MakeNumber(self) :
        NumString = ''
        DotCount = 0
        PosStart = self.pos.copy()

        while self.CurrentChar != None and self.CurrentChar in DIGITS + '.' :
            if self.CurrentChar == '.' :
                if DotCount == 1 : break
                DotCount += 1
                NumString += '.'
            else :
                NumString += self.CurrentChar
            self.advance()

        if NumString == '' or NumString == '.' :
            return Token(TT_INT, 0, PosStart, self.pos)

        if DotCount == 0 :
            return Token(TT_INT, int(NumString), PosStart, self.pos)
        else :
            return Token(TT_FLOAT, float(NumString), PosStart, self.pos)

    def MakeIdentifier(self) :
        idString = ''
        PosStart = self.pos.copy()

        while self.CurrentChar != None and self.CurrentChar in LETTERS_DIGITS + '_?':
            idString += self.CurrentChar
            self.advance()

        tokenType = TT_KEYWORD if idString in KEYWORDS else TT_IDENTIFIER
        return Token(tokenType, idString, PosStart, self.pos)

    def MakeNotEqual(self) :
        PosStart = self.pos.copy()
        self.advance()

        if self.CurrentChar == '=' :
            self.advance()
            return Token(TT_NE, PosStart=PosStart, PosEnd=self.pos), None

        self.advance()
        return None, ExpectedChar(PosStart, self.pos, "I expected an '=' after '!' or '~', you fucking dumbass!")

    def MakeEqual(self) :
        token_type = TT_EQUAL
        PosStart = self.pos.copy()
        self.advance()

        if self.CurrentChar == '=' :
            self.advance()
            token_type = TT_DE

        return Token(token_type, PosStart=PosStart, PosEnd=self.pos)

    def MakeLT(self) :
        token_type = TT_LT
        PosStart = self.pos.copy()
        self.advance()

        if self.CurrentChar == '=' :
            self.advance()
            token_type = TT_LTE

        return Token(token_type, PosStart=PosStart, PosEnd=self.pos)

    def MakeGT(self) :
        token_type = TT_GT
        PosStart = self.pos.copy()
        self.advance()

        if self.CurrentChar == '=' :
            self.advance()
            token_type = TT_GTE

        return Token(token_type, PosStart=PosStart, PosEnd=self.pos)

    def MakeMorA(self) :
        token_type = TT_MINUS
        PosStart = self.pos.copy()
        self.advance()

        if self.CurrentChar == ">" :
            self.advance()
            token_type = TT_ARROW

        return Token(token_type, PosStart=PosStart, PosEnd=self.pos)

    def MakeString(self) :
        string = ''
        PosStart = self.pos.copy()
        EscapeChar = False
        self.advance()

        EscapeChars = {
            'n' : '\n',
            't' : '\t'
        }

        while self.CurrentChar != None and (self.CurrentChar != '"' or EscapeChar == True) :
            if EscapeChar == True :
                string += EscapeChars.get(self.CurrentChar, self.CurrentChar)
            else :
                if self.CurrentChar == '\\' :
                    EscapeChar = True
                else :
                    string += self.CurrentChar
            self.advance()
            EscapeChar = False

        self.advance()

        return Token(TT_STRING, string, PosStart, self.pos)
    
    def MakeToken(self) :
        tokens = []

        while self.CurrentChar is not None :
            if self.CurrentChar in ' \t' :
                self.advance()
            elif self.CurrentChar in DIGITS :
                tokens.append(self.MakeNumber())
            elif self.CurrentChar == '+' :
                tokens.append(Token(TT_PLUS, PosStart=self.pos))
                self.advance()
            elif self.CurrentChar == '-' :
                tokens.append(self.MakeMorA())
            elif self.CurrentChar == '*' :
                tokens.append(Token(TT_MULTIPLY, PosStart=self.pos))
                self.advance()
            elif self.CurrentChar == '/' :
                tokens.append(Token(TT_DIVIDE, PosStart=self.pos))
                self.advance()
            elif self.CurrentChar == '^' :
                tokens.append(Token(TT_POWER, PosStart=self.pos))
                self.advance()
            elif self.CurrentChar == '(' :
                tokens.append(Token(TT_LeftParen, PosStart=self.pos))
                self.advance()
            elif self.CurrentChar == ')' :
                tokens.append(Token(TT_RightParen, PosStart=self.pos))
                self.advance()
            elif self.CurrentChar == '!' or self.CurrentChar == '~' :
                token, error = self.MakeNotEqual()
                if error : return [], error
                tokens.append(token)
            elif self.CurrentChar == '=' :
                tokens.append(self.MakeEqual())
            elif self.CurrentChar == '>' :
                tokens.append(self.MakeGT())
            elif self.CurrentChar == '<' :
                tokens.append(self.MakeLT())
            elif self.CurrentChar == ':' :
                tokens.append(Token(TT_KEYWORD, ':', PosStart=self.pos))
                self.advance()
            elif self.CurrentChar == ',' :
                tokens.append(Token(TT_COMMA, PosStart=self.pos))
                self.advance()
            elif self.CurrentChar == '"' :
                tokens.append(self.MakeString())
            elif self.CurrentChar in LETTERS :
                tokens.append(self.MakeIdentifier())
            else :
                PosStart = self.pos.copy()
                char = self.CurrentChar
                self.advance()
                return [], WrongChar(PosStart, self.pos, "'" + char + "'")

        tokens.append(Token(TT_EOF, PosStart=self.pos))
        return tokens, None


############
# NODES
############

class NumberNode :
    def __init__(self, token) :
        self.token = token
        self.PosStart = self.token.PosStart
        self.PosEnd = self.token.PosEnd

    def __repr__(self) :
        return f'{self.token}'

class StringNode :
    def __init__(self, token) :
        self.token = token
        self.PosStart = self.token.PosStart
        self.PosEnd = self.token.PosEnd

    def __repr__(self) :
        return f'{self.token}'

class VarAccessNode :
    def __init__(self, VarNameToken) :
        self.VarNameToken = VarNameToken

        self.PosStart = VarNameToken.PosStart
        self.PosEnd = VarNameToken.PosEnd

class VarAssignNode :
    def __init__(self, VarNameToken, ValueNode) :
        self.VarNameToken = VarNameToken
        self.ValueNode = ValueNode

        self.PosStart = VarNameToken.PosStart
        self.PosEnd = VarNameToken.PosEnd

class BinOpNode :
    def __init__(self, LNode, RNode, OPToken) :
        self.LNode = LNode
        self.RNode = RNode
        self.OPToken = OPToken
        self.PosStart = self.LNode.PosStart
        self.PosEnd = self.RNode.PosEnd

    def __repr__(self) :
        return f'({self.LNode}, {self.OPToken}, {self.RNode})'

class UnaryOperationNode :
    def __init__(self, OPToken, node) :
        self.OPToken = OPToken
        self.node = node
        self.PosStart = self.OPToken.PosStart
        self.PosEnd = node.PosEnd

    def __repr__(self) :
        return f'({self.OPToken}, {self.node})'

class IfNode :
    def __init__(self, cases, else_case) :
        self.cases = cases
        self.else_case = else_case

        self.PosStart = self.cases[0][0].PosStart
        self.PosEnd = self.else_case or self.cases[len(self.cases) - 1][0].PosEnd

class ForNode :
    def __init__(self, VarNameToken, StartValueNode, EndValueNode, StepValueNode, BodyNode) :
        self.VarNameToken = VarNameToken
        self.StartValueNode = StartValueNode
        self.EndValueNode = EndValueNode
        self.StepValueNode = StepValueNode
        self.BodyNode = BodyNode

        self.PosStart = self.VarNameToken.PosStart
        self.PosEnd = self.BodyNode.PosEnd

class WhileNode :
    def __init__(self, ConditionNode, BodyNode) :
        self.ConditionNode = ConditionNode
        self.BodyNode = BodyNode

        self.PosStart = self.ConditionNode.PosStart
        self.PosEnd = self.BodyNode.PosEnd

class FuncDefNode :
    def __init__(self, VarNameToken, ArgNameTokens, BodyNode) :
        self.VarNameToken = VarNameToken
        self.ArgNameTokens = ArgNameTokens
        self.BodyNode = BodyNode

        if self.VarNameToken :
            self.PosStart = self.VarNameToken.PosStart
        elif len(self.ArgNameTokens) > 0 :
            self.PosStart = self.ArgNameTokens[0].PosStart
        else :
            self.PosStart = self.BodyNode.PosStart

        self.PosEnd = self.BodyNode.PosEnd

class CallNode :
    def __init__(self, NodeToCall, ArgNodes) :
        self.NodeToCall = NodeToCall
        self.ArgNodes = ArgNodes

        self.PosStart = self.NodeToCall.PosStart

        if len(self.ArgNodes) > 0 :
            self.PosEnd = self.ArgNodes[len(self.ArgNodes) - 1].PosEnd
        else :
            self.PosEnd = self.NodeToCall.PosEnd


############
# PARSE RESULT
############

class ParseResult :
    def __init__(self) :
        self.error = None
        self.node = None
        self.AdvanceCount = 0

    def RegisterAdvancement(self) :
        self.AdvanceCount += 1

    def register(self, res) :
        self.AdvanceCount += res.AdvanceCount
        if res.error : self.error = res.error
        return res.node

    def success(self, node) :
        self.node = node
        return self

    def failure(self, error) :
        if not self.error or self.AdvanceCount == 0 :
            self.error = error
        return self


############
# PARSER
############

class Parser :
    def __init__(self, tokens) :
        self.tokens = tokens
        self.tokenIndex = -1
        self.advance()

    def advance(self) :
        self.tokenIndex += 1
        if self.tokenIndex < len(self.tokens) :
            self.CurrentToken = self.tokens[self.tokenIndex]
        return self.CurrentToken

    def parse(self) :
        result = self.expr()
        if not result.error and self.CurrentToken.type != TT_EOF :
            return result.failure(InvalidSyntax(
                self.CurrentToken.PosStart, self.CurrentToken.PosEnd,
                "I expected one of these stinky operators, you dumbass!: '+', '-', '*', or '/'"
            ))
        return result

    def if_expr(self) :
        res = ParseResult()
        cases = []
        else_case = None

        if not self.CurrentToken.matches(TT_KEYWORD, 'panic?') :
            return res.failure(InvalidSyntax(
                self.CurrentToken.PosStart, self.CurrentToken.PosEnd,
                "I expected 'panic?' (if), you fucking dumbass!"
            ))

        res.RegisterAdvancement()
        self.advance()

        condition = res.register(self.expr())
        if res.error : return res

        if not self.CurrentToken.matches(TT_KEYWORD, ':') :
            return res.failure(InvalidSyntax(
                self.CurrentToken.PosStart, self.CurrentToken.PosEnd,
                f"I expected ':', you fucking dumbass!"
            ))

        res.RegisterAdvancement()
        self.advance()

        expr = res.register(self.expr())
        if res.error : return res
        cases.append((condition, expr))

        while self.CurrentToken.matches(TT_KEYWORD, 'nopanic?') :
            res.RegisterAdvancement()
            self.advance()

            condition = res.register(self.expr())
            if res.error : return res

            if not self.CurrentToken.matches(TT_KEYWORD, ':') :
                return res.failure(InvalidSyntax(
                    self.CurrentToken.PosStart, self.CurrentToken.PosEnd,
                    f"I expected ':', you fucking dumbass!"
                ))

            res.RegisterAdvancement()
            self.advance()

            expr = res.register(self.expr())
            if res.error : return res
            cases.append((condition, expr))

        if self.CurrentToken.matches(TT_KEYWORD, 'nope') :
            res.RegisterAdvancement()
            self.advance()

            else_case = res.register(self.expr())
            if res.error : return res
        
        return res.success(IfNode(cases, else_case))

    def for_expr(self) :
        res = ParseResult()

        if not self.CurrentToken.matches(TT_KEYWORD, 'during') :
            return res.failure(InvalidSyntax(
                self.CurrentToken.PosStart, self.CurrentToken.PosEnd,
                f"I expected 'during', you fucking dumbass!"
            ))

        res.RegisterAdvancement()
        self.advance()

        if self.CurrentToken.type != TT_IDENTIFIER :
            return res.failure(InvalidSyntax(
                self.CurrentToken.PosStart, self.CurrentToken.PosEnd,
                f"I expected an identifier, you dumbass!"
            ))

        VarName = self.CurrentToken
        res.RegisterAdvancement()
        self.advance()

        if self.CurrentToken.type != TT_EQUAL :
            return res.failure(InvalidSyntax(
                self.CurrentToken.PosStart, self.CurrentToken.PosEnd,
                f"I expected '=', you fucking dumbass!"
            ))

        res.RegisterAdvancement()
        self.advance()

        StartValue = res.register(self.expr())
        if res.error : return res

        if not self.CurrentToken.matches(TT_KEYWORD, 'towards') :
            return res.failure(InvalidSyntax(
                self.CurrentToken.PosStart, self.CurrentToken.PosEnd,
                f"I expected 'towards', you fucking dumbass!"
            ))

        res.RegisterAdvancement()
        self.advance()

        EndValue = res.register(self.expr())
        if res.error : return res

        if self.CurrentToken.matches(TT_KEYWORD, 'shadow') :
            res.RegisterAdvancement()
            self.advance()

            StepValue = res.register(self.expr())
            if res.error : return res
        else :
            StepValue = None

        if not self.CurrentToken.matches(TT_KEYWORD, ':') :
            return res.failure(InvalidSyntax(
                self.CurrentToken.PosStart, self.CurrentToken.PosEnd,
                f"I expected ':', you fucking dumbass!"
            ))

        res.RegisterAdvancement()
        self.advance()

        body = res.register(self.expr())
        if res.error : return res

        return res.success(ForNode(VarName, StartValue, EndValue, StepValue, body))

    def while_expr(self) :
        res = ParseResult()

        if not self.CurrentToken.matches(TT_KEYWORD, 'loopdammit') :
            return res.failure(InvalidSyntax(
                self.CurrentToken.PosStart, self.CurrentToken.PosEnd,
                f"I expected 'loopdammit', you fucking dumbass!"
            ))

        res.RegisterAdvancement()
        self.advance()

        condition = res.register(self.expr())
        if res.error : return res

        if not self.CurrentToken.matches(TT_KEYWORD, ':') :
            return res.failure(InvalidSyntax(
                self.CurrentToken.PosStart, self.CurrentToken.PosEnd,
                f"I expected ':', you fucking dumbass!"
            ))

        res.RegisterAdvancement()
        self.advance()

        body = res.register(self.expr())
        if res.error : return res

        return res.success(WhileNode(condition, body))

    def func_def(self) :
        res = ParseResult()

        if not self.CurrentToken.matches(TT_KEYWORD, 'rupt') :
            return res.failure(InvalidSyntax(
                self.CurrentToken.PosStart, self.CurrentToken.PosEnd,
                f"I expected 'rupt', you fucking dumbass!"
            ))

        res.RegisterAdvancement()
        self.advance()

        if self.CurrentToken.type == TT_IDENTIFIER :
            VarNameToken = self.CurrentToken
            res.RegisterAdvancement()
            self.advance()
            if self.CurrentToken.type != TT_LeftParen :
                return res.failure(InvalidSyntax(
                    self.CurrentToken.PosStart, self.CurrentToken.PosEnd,
                    f"I expected '(', you fucking dumbass"
                ))
        else :
            VarNameToken = None
            if self.CurrentToken.type != TT_LeftParen :
                return res.failure(InvalidSyntax(
                    self.CurrentToken.PosStart, self.CurrentToken.PosEnd,
                    f"I expected an identifier or '(', you fucking dumbass"
                ))

        res.RegisterAdvancement()
        self.advance()
        ArgNameTokens = []

        if self.CurrentToken.type == TT_IDENTIFIER :
            ArgNameTokens.append(self.CurrentToken)
            res.RegisterAdvancement()
            self.advance()
            while self.CurrentToken.type == TT_COMMA :
                res.RegisterAdvancement()
                self.advance()

                if self.CurrentToken.type !=TT_IDENTIFIER :
                    return res.failure(InvalidSyntax(
                        self.CurrentToken.PosStart, self.CurrentToken.PosEnd,
                        f"I expected an identifier, you fucking dumbass!"
                    ))

                ArgNameTokens.append(self.CurrentToken)
                res.RegisterAdvancement()
                self.advance()

            if self.CurrentToken.type != TT_RightParen :
                return res.failure(InvalidSyntax(
                    self.CurrentToken.PosStart, self.CurrentToken.PosEnd,
                    f"I expected ',' or ')', you fucking dumbass!"
                ))
        else :
            if self.CurrentToken.type != TT_RightParen :
                return res.failure(InvalidSyntax(
                    self.CurrentToken.PosStart, self.CurrentToken.PosEnd,
                    f"I expected an identifier or ')', you fucking dumbass!"
                ))

        res.RegisterAdvancement()
        self.advance()

        if self.CurrentToken.type != TT_ARROW :
            return res.failure(InvalidSyntax(
                self.CurrentToken.PosStart, self.CurrentToken.PosEnd,
                f"I expected '->', you fucking dumbass!"
            ))

        res.RegisterAdvancement()
        self.advance()
        NodeToReturn = res.register(self.expr())
        if res.error : return res

        return res.success(FuncDefNode(
            VarNameToken,
            ArgNameTokens,
            NodeToReturn
        ))

    def call(self) :
        res = ParseResult()
        atom = res.register(self.atom())
        if res.error : return res

        ArgNodes = []

        if self.CurrentToken.type == TT_LeftParen :
            res.RegisterAdvancement()
            self.advance()

            # immediate empty args: ( )
            if self.CurrentToken.type == TT_RightParen :
                res.RegisterAdvancement()
                self.advance()
                return res.success(CallNode(atom, ArgNodes))

            # otherwise parse first arg
            ArgNodes.append(res.register(self.expr()))
            if res.error :
                return res.failure(InvalidSyntax(
                    self.CurrentToken.PosStart, self.CurrentToken.PosEnd,
                    f"I expected ')', 'spasm', 'panic?', 'during', 'rupt', you fucking dumbass!"
                ))

            while self.CurrentToken.type == TT_COMMA :
                res.RegisterAdvancement()
                self.advance()

                ArgNodes.append(res.register(self.expr()))
                if res.error : return res

            if self.CurrentToken.type != TT_RightParen :
                return res.failure(InvalidSyntax(
                    self.CurrentToken.PosStart, self.CurrentToken.PosEnd,
                    f"I expected ',' or ')', you fucking dumbass!"
                ))

            res.RegisterAdvancement()
            self.advance()
            return res.success(CallNode(atom, ArgNodes))

        return res.success(atom)

    
    def atom(self) :
        res = ParseResult()
        token = self.CurrentToken

        if token.type in (TT_INT, TT_FLOAT) :
            res.RegisterAdvancement()
            self.advance()
            return res.success(NumberNode(token))
        elif token.type == TT_STRING :
            res.RegisterAdvancement()
            self.advance()
            return res.success(StringNode(token))
        elif token.type == TT_IDENTIFIER :
            res.RegisterAdvancement()
            self.advance()
            return res.success(VarAccessNode(token))
        elif token.type == TT_LeftParen :
            res.RegisterAdvancement()
            self.advance()
            expr = res.register(self.expr())
            if res.error : return res
            if self.CurrentToken.type == TT_RightParen :
                res.RegisterAdvancement()
                self.advance()
                return res.success(expr)
            else :
                return res.failure(InvalidSyntax(
                    self.CurrentToken.PosStart, self.CurrentToken.PosEnd,
                    "I expected the symbol ')'! Where is it? Are you dumb?"
                ))
        elif token.matches(TT_KEYWORD, 'panic?') :
            if_expr = res.register(self.if_expr())
            if res.error : return res
            return res.success(if_expr)
        elif token.matches(TT_KEYWORD, 'during') :
            for_expr = res.register(self.for_expr())
            if res.error : return res
            return res.success(for_expr)
        elif token.matches(TT_KEYWORD, 'loopdammit') :
            while_expr = res.register(self.while_expr())
            if res.error : return res
            return res.success(while_expr)
        elif token.matches(TT_KEYWORD, 'rupt') :
            func_def = res.register(self.func_def())
            if res.error : return res
            return res.success(func_def)
        
        return res.failure(InvalidSyntax(
            token.PosStart, token.PosEnd,
            "I expected an int, float, identifier, '+', '-', '*', '/', 'panic?', 'during', 'loopdammit', or 'rupt', you fucking dumbass!"
        ))

    def power(self) :
        return self.BinaryOperations(self.call, (TT_POWER,), self.factor)

    def factor(self) :
        res = ParseResult()
        token = self.CurrentToken

        if token.type in (TT_PLUS, TT_MINUS) :
            res.RegisterAdvancement()
            self.advance()
            node = res.register(self.factor())
            if res.error : return res
            return res.success(UnaryOperationNode(token, node))

        return self.power()

    def comp_expr(self) :
        res = ParseResult()
        token = self.CurrentToken 

        if self.CurrentToken.matches(TT_KEYWORD, "op") :
            operationToken = self.CurrentToken
            res.RegisterAdvancement()
            self.advance()

            node = res.register(self.comp_expr())
            if res.error : return res
            return res.success(UnaryOperationNode(operationToken, node))

        node = res.register(self.BinaryOperations(self.arith_expr, (TT_DE, TT_NE, TT_LT, TT_GT, TT_LTE, TT_GTE)))
        if res.error :
            return res.failure(InvalidSyntax(
                token.PosStart, token.PosEnd,
                "I expected an int, float, identifier, '+', '-', '*', '/', or 'op', you fucking dumbass!"
            ))

        return res.success(node)
    
    def term(self) :
        return self.BinaryOperations(self.factor, (TT_MULTIPLY, TT_DIVIDE))

    def arith_expr(self) :
        return self.BinaryOperations(self.term, (TT_PLUS, TT_MINUS))
    
    def expr(self) :
        res = ParseResult()

        if self.CurrentToken.matches(TT_KEYWORD, 'spasm') :
            res.RegisterAdvancement()
            self.advance()

            if self.CurrentToken.type != TT_IDENTIFIER :
                return res.failure(InvalidSyntax(
                    self.CurrentToken.PosStart, self.CurrentToken.PosEnd,
                    "I expected an identifier! Don't you know how name a variable?!"
                ))

            VarName = self.CurrentToken
            res.RegisterAdvancement()
            self.advance()

            if self.CurrentToken.type != TT_EQUAL :
                return res.failure(InvalidSyntax(
                    self.CurrentToken.PosStart, self.CurrentToken.PosEnd,
                    "I expected an equal sign! Where the FUCK is it?!"
                ))

            res.RegisterAdvancement()
            self.advance()
            expr = res.register(self.expr())
            if res.error : return res
            return res.success(VarAssignNode(VarName, expr))

        node = res.register(self.BinaryOperations(self.comp_expr, ((TT_KEYWORD, "plus"), (TT_KEYWORD, "slash"))))

        if res.error :
            return res.failure(InvalidSyntax(
                    self.CurrentToken.PosStart, self.CurrentToken.PosEnd,
                    "I expected an int, float, 'identifier', 'spasm', '+', '-', '*', '/', 'panic?', 'during', 'loopdammit', or 'rupt', you fucking dumbass!"
                ))

        return res.success(node)

    def BinaryOperations(self, functionA, operations, functionB=None) :
        if functionB == None :
            functionB = functionA
        res = ParseResult()
        left = res.register(functionA())
        if res.error : return res

        while self.CurrentToken.type in operations or (self.CurrentToken.type, self.CurrentToken.value,) in operations :
            OPToken = self.CurrentToken
            res.RegisterAdvancement()
            self.advance()
            right = res.register(functionB())
            if res.error : return res
            left = BinOpNode(left, right, OPToken)

        return res.success(left)


###################
# RUNTIME RESULTS
###################

class RTResults :
    def __init__(self) :
        self.value = None
        self.error = None

    def register(self, res) :
        if isinstance(res, RTResults):
            if res.error:
                self.error = res.error
            return res.value
        return res

    def success(self, value) :
        self.value = value
        return self

    def failure(self, error) :
        self.error = error
        return self


###############
# VALUES
###############

class Value :
    def __init__(self) :
        self.set_pos()
        self.set_context()

    def set_pos(self, PosStart=None, PosEnd=None) :
        self.PosStart = PosStart
        self.PosEnd = PosEnd
        return self

    def set_context(self, context=None) :
        self.context = context
        return self

    def addition(self, other) :
        return None, self.IllegalOperation(other)

    def subtraction(self, other) :
        return None, self.IllegalOperation(other)

    def multiplication(self, other) :
        return None, self.IllegalOperation(other)

    def division(self, other) :
        return None, self.IllegalOperation(other)

    def power(self, other) :
        return None, self.IllegalOperation(other)

    def get_comparison_eq(self, other) :
        return None, self.IllegalOperation(other)

    def get_comparison_ne(self, other) :
        return None, self.IllegalOperation(other)

    def get_comparison_lt(self, other) :
        return None, self.IllegalOperation(other)

    def get_comparison_gt(self, other) :
        return None, self.IllegalOperation(other)

    def get_comparison_lte(self, other) :
        return None, self.IllegalOperation(other)

    def get_comparison_gte(self, other) :
        return None, self.IllegalOperation(other)

    def anded(self, other) :
        return None, self.IllegalOperation(other)

    def ored(self, other) :
       return None, self.IllegalOperation(other)

    def Not(self) :
        return None, self.IllegalOperation(other)

    def execute(self, args) :
        return None, self.IllegalOperation(other)

    def copy(self) :
        raise Exception('No copy method defined')

    def is_true(self) :
        return False

    def IllegalOperation(self, other=None) :
        if not other : other = self
        return Runtime(
            self.PosStart, other.PosEnd,
            'THAT;S ILLEGAL!',
            self.context
        )

class Number(Value) :
    def __init__(self, value) :
        super().__init__()
        self.value = value

    def addition(self, other) :
        if isinstance(other, Number) :
            return Number(self.value + other.value).set_context(self.context), None
        else :
            return None, Value.IllegalOperation(self.PosStart, other.PosEnd)

    def subtraction(self, other) :
        if isinstance(other, Number) :
            return Number(self.value - other.value).set_context(self.context), None
        else :
            return None, Value.IllegalOperation(self.PosStart, other.PosEnd)

    def multiplication(self, other) :
        if isinstance(other, Number) :
            return Number(self.value * other.value).set_context(self.context), None
        else :
            return None, Value.IllegalOperation(self.PosStart, other.PosEnd)

    def division(self, other) :
        if not isinstance(other, Number):
            return None, self.IllegalOperation(other)
        if other.value == 0 :
            return None, Runtime(
                other.PosStart, other.PosEnd,
                "You can't divide by zero, you dumb shit!",
                self.context
            )
        return Number(self.value / other.value).set_context(self.context), None

    def power(self, other) :
        if isinstance(other, Number) :
            return Number(self.value ** other.value).set_context(self.context), None
        else :
            return None, Value.IllegalOperation(self.PosStart, other.PosEnd)

    def get_comparison_eq(self, other) :
        if isinstance(other, Number) :
            return Number(int(self.value == other.value)).set_context(self.context), None
        else :
            return None, Value.IllegalOperation(self.PosStart, other.PosEnd)

    def get_comparison_ne(self, other) :
        if isinstance(other, Number) :
            return Number(int(self.value != other.value)).set_context(self.context), None
        else :
            return None, Value.IllegalOperation(self.PosStart, other.PosEnd)

    def get_comparison_lt(self, other) :
        if isinstance(other, Number) :
            return Number(int(self.value < other.value)).set_context(self.context), None
        else :
            return None, Value.IllegalOperation(self.PosStart, other.PosEnd)

    def get_comparison_gt(self, other) :
        if isinstance(other, Number) :
            return Number(int(self.value > other.value)).set_context(self.context), None
        else :
            return None, Value.IllegalOperation(self.PosStart, other.PosEnd)

    def get_comparison_lte(self, other) :
        if isinstance(other, Number) :
            return Number(int(self.value <= other.value)).set_context(self.context), None
        else :
            return None, Value.IllegalOperation(self.PosStart, other.PosEnd)

    def get_comparison_gte(self, other) :
        if isinstance(other, Number) :
            return Number(int(self.value >= other.value)).set_context(self.context), None
        else :
            return None, Value.IllegalOperation(self.PosStart, other.PosEnd)

    def anded(self, other) :
        if isinstance(other, Number) :
            return Number(int(self.value and other.value)).set_context(self.context), None
        else :
            return None, Value.IllegalOperation(self.PosStart, other.PosEnd)

    def ored(self, other) :
        if isinstance(other, Number) :
            return Number(int(self.value or other.value)).set_context(self.context), None
        else :
            return None, Value.IllegalOperation(self.PosStart, other.PosEnd)

    def Not(self) :
        if isinstance(other, Number) :
            return Number(1 if self.value == 0 else 0).set_context(self.context), None
        else :
            return None, Value.IllegalOperation(self.PosStart, other.PosEnd)

    def copy(self) :
        copy = Number(self.value)
        copy.set_pos(self.PosStart, self.PosEnd)
        copy.set_context(self.context)
        return copy

    def is_true(self) :
        return self.value != 0

    def __repr__(self) :
        return str(self.value)

class String(Value) :
    def __init__(self, value) :
        super().__init__()
        self.value = value

    # match the Value API (lowercase method names)
    def addition(self, other) :
        if isinstance(other, String) :
            return String(self.value + other.value).set_context(self.context), None
        return None, self.IllegalOperation(other)

    def multiplication(self, other) :
        # allow "a" * 3
        if isinstance(other, Number) :
            return String(self.value * int(other.value)).set_context(self.context), None
        return None, self.IllegalOperation(other)

    def is_true(self) :
        return len(self.value) > 0

    def copy(self) :
        copy = String(self.value)
        copy.set_pos(self.PosStart, self.PosEnd)
        copy.set_context(self.context)
        return copy

    def __repr__(self) :
        return f'"{self.value}"'

class Function(Value) :
    def __init__(self, name, BodyNode, ArgNames) :
        super().__init__()
        self.name = name or '<REDACTED>'
        self.BodyNode = BodyNode
        self.ArgNames = ArgNames

    def execute(self, args) :
        res = RTResults()
        interpreter = Interpreter()
        NewContext = Context(self.name, self.context, self.PosStart)
        NewContext.SymbolTable = SymbolTable(NewContext.parent.SymbolTable)

        if len(args) > len(self.ArgNames) :
            extra = len(args) - len(self.ArgNames)
            return res.failure(Runtime(
                self.PosStart, self.PosEnd,
                f"{extra} THERE ARE TOO MANY ARGUMENTS PASSED INTO {self.name}, YOU MOTHERFUCKER!",
                self.context
            ))

        if len(args) < len(self.ArgNames) :
            missing = len(self.ArgNames) - len(args)
            return res.failure(Runtime(
                self.PosStart, self.PosEnd,
                f"{missing} THERE ARE TOO FEW ARGUMENTS PASSED INTO {self.name}, YOU MOTHERFUCKER!",
                self.context
            ))

        for i in range(len(args)) :
            ArgName = self.ArgNames[i]
            ArgValue = args[i]
            ArgValue.set_context(NewContext)
            NewContext.SymbolTable.set(ArgName.value if isinstance(ArgName, Token) else ArgName, ArgValue)

        value = res.register(interpreter.visit(self.BodyNode, NewContext))
        if res.error : return res

        return res.success(value)

    def copy(self) :
        copy = Function(self.name, self.BodyNode,self.ArgNames)
        copy.set_pos(self.PosStart, self.PosEnd)
        copy.set_context(self.context)
        return copy

    def __repr__(self) :
        return f"<function {self.name}>"


###############
# CONTEXT
###############

class Context :
    def __init__(self, DisplayName, parent=None, ParentEntryPos=None) :
        self.DisplayName = DisplayName
        self.parent = parent
        self.ParentEntryPos = ParentEntryPos
        self.SymbolTable = None


###############
# SYMBOL TABLE
###############

class SymbolTable :
    def __init__(self, parent=None) :
        self.symbols = {}
        self.parent = parent

    def get(self, VarName) :
        value = self.symbols.get(VarName, None)
        if value == None and self.parent :
            return self.parent.get(VarName)
        return value

    def set(self, VarName, value) :
        self.symbols[VarName] = value

    def remove(self, name) :
        del self.symbols[name]


###############
# INTERPRETER
###############

class Interpreter :
    def visit(self, node, context) :
        MethodName = f'visit_{type(node).__name__}'
        Method = getattr(self, MethodName, self.no_visit_method)
        return Method(node, context)

    def no_visit_method(self, node, context) :
        raise Exception(f'No visit_{type(node).__name__} method defined in this stupid interpreter!')

    def visit_NumberNode(self, node, context) :
        number = Number(node.token.value).set_context(context).set_pos(node.PosStart, node.PosEnd)
        return RTResults().success(number)

    def visit_StringNode(self, node, context) :
        return RTResults().success(String(node.token.value).set_context(context).set_pos(node.PosStart, node.PosEnd))

    def visit_VarAccessNode(self, node, context) :
        res=RTResults()
        VarName = node.VarNameToken.value
        value = context.SymbolTable.get(VarName)

        if not value :
            return res.failure(Runtime(
                node.PosStart, node.PosEnd,
                f"'{VarName}' is not defined! What are ya? An idiot?",
                context
            ))

        value = value.copy().set_pos(node.PosStart, node.PosEnd)
        return res.success(value)

    def visit_VarAssignNode(self, node, context) :
        res = RTResults()
        VarName = node.VarNameToken.value
        value = res.register(self.visit(node.ValueNode, context))
        if res.error : return res

        context.SymbolTable.set(VarName, value)
        return res.success(value)

    def visit_BinOpNode(self, node, context) :
        res = RTResults()

        left = res.register(self.visit(node.LNode, context))
        if res.error : return res

        right = res.register(self.visit(node.RNode, context))
        if res.error : return res

        error = None
        result = None

        if node.OPToken.type == TT_PLUS :
            result, error = left.addition(right)
        elif node.OPToken.type == TT_MINUS :
            result, error = left.subtraction(right)
        elif node.OPToken.type == TT_MULTIPLY :
            result, error = left.multiplication(right)
        elif node.OPToken.type == TT_DIVIDE :
            result, error = left.division(right)
        elif node.OPToken.type == TT_POWER :
            result, error = left.power(right)
        elif node.OPToken.type == TT_DE :
            result, error = left.get_comparison_eq(right)
        elif node.OPToken.type == TT_NE :
            result, error = left.get_comparison_ne(right)
        elif node.OPToken.type == TT_LT :
            result, error = left.get_comparison_lt(right)
        elif node.OPToken.type == TT_GT :
            result, error = left.get_comparison_gt(right)
        elif node.OPToken.type == TT_LTE :
            result, error = left.get_comparison_lte(right)
        elif node.OPToken.type == TT_GTE :
            result, error = left.get_comparison_gte(right)
        elif node.OPToken.matches(TT_KEYWORD, "plus") :
            result, error = left.anded(right)
        elif node.OPToken.matches(TT_KEYWORD, "slash") :
            result, error = left.ored(right)
        else:
            return res.failure(Runtime(node.PosStart, node.PosEnd, "Unknown binary operator"))

        if error :
            return res.failure(error)
        return res.success(result.set_pos(node.PosStart, node.PosEnd))


    def visit_UnaryOperationNode(self, node, context) :
        res = RTResults()
        number = res.register(self.visit(node.node, context))
        if res.error : return res

        error = None

        if node.OPToken.type == TT_MINUS :
            number, error = number.multiplication(Number(-1))
        elif node.OPToken.matches(TT_KEYWORD, "op") :
            number, error = number.Not()

        if error :
            return res.failure(error)
        else :
            return res.success(number.set_pos(node.PosStart, node.PosEnd))

    def visit_IfNode(self, node, context) :
        res = RTResults()

        for condition, expr in node.cases :
            condition_value = res.register(self.visit(condition, context))
            if res.error : return res

            if condition_value.is_true() :
                expr_value = res.register(self.visit(expr, context))
                if res.error : return res
                return res.success(expr_value)

        if node.else_case :
            else_value = res.register(self.visit(node.else_case, context))
            if res.error : return res
            return res.success(else_value)

        return res.success(None)

    def visit_ForNode(self, node, context) :
        res = RTResults()

        StartValue = res.register(self.visit(node.StartValueNode, context))
        if res.error : return res

        EndValue = res.register(self.visit(node.EndValueNode, context))
        if res.error : return res

        if node.StepValueNode :
            StepValue = res.register(self.visit(node.StepValueNode, context))
            if res.error : return res
        else :
            StepValue = Number(1)

        i = StartValue.value

        if StepValue.value >= 0 :
            condition = lambda: i <= EndValue.value
        else :
            condition = lambda: i >= EndValue.value

        while condition():
            loop_num = Number(i).set_context(context).set_pos(node.PosStart, node.PosEnd)
            context.SymbolTable.set(node.VarNameToken.value, loop_num)

            res.register(self.visit(node.BodyNode, context))
            if res.error : return res

            i += StepValue.value

        return res.success(None)

    def visit_WhileNode(self, node, context) :
        res = RTResults()

        while True :
            condition = res.register(self.visit(node.ConditionNode, context))
            if res.error : return res

            if not condition.is_true() : break

            res.register(self.visit(node.BodyNode, context))
            if res.error : return res

        return res.success(None)

    def visit_FuncDefNode(self, node, context) :
        res = RTResults()

        FuncName = node.VarNameToken.value if node.VarNameToken else None
        BodyNode = node.BodyNode
        ArgNames = [ArgName.value for ArgName in node.ArgNameTokens]
        FuncValue = Function(FuncName, BodyNode, ArgNames).set_context(context).set_pos(node.PosStart, node.PosEnd)

        if node.VarNameToken :
            context.SymbolTable.set(FuncName, FuncValue)

        return res.success(FuncValue)

    def visit_CallNode(self, node, context) :
        res = RTResults()
        args = []

        ValueToCall = res.register(self.visit(node.NodeToCall, context))
        if res.error : return res
        ValueToCall = ValueToCall.copy().set_pos(node.PosStart, node.PosEnd)

        for ArgNode in node.ArgNodes :
            args.append(res.register(self.visit(ArgNode, context)))
            if res.error : return res

        ReturnVal = res.register(ValueToCall.execute(args))
        if res.error : return res
        return res.success(ReturnVal)


############
# RUN
############

GlobalSymbolTable = SymbolTable()
GlobalSymbolTable.set("void", Number(0))
GlobalSymbolTable.set("ting", Number(1))
GlobalSymbolTable.set("EEEHH", Number(0))

def run(filename, text) :
    # Generates tokens
    lexer = Lexer(filename, text)
    tokens, error = lexer.MakeToken()

    # Genrates AST
    if error : return None, error

    parser = Parser(tokens)
    ast = parser.parse()
    if ast.error : return None, ast.error

    # Runs the program
    interpreter = Interpreter()
    context = Context('<program>')
    context.SymbolTable = GlobalSymbolTable
    result = interpreter.visit(ast.node, context)

    return result.value, result.error
