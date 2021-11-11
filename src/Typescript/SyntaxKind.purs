module Typescript.SyntaxKind where

import Typescript.Utils.Enum (class EnumRep)
import Unsafe.Coerce (unsafeCoerce)

data SyntaxKindEnum

foreign import data Unknown :: SyntaxKindEnum
foreign import data EndOfFileToken :: SyntaxKindEnum
foreign import data SingleLineCommentTrivia :: SyntaxKindEnum
foreign import data MultiLineCommentTrivia :: SyntaxKindEnum
foreign import data NewLineTrivia :: SyntaxKindEnum
foreign import data WhitespaceTrivia :: SyntaxKindEnum
foreign import data ShebangTrivia :: SyntaxKindEnum
foreign import data ConflictMarkerTrivia :: SyntaxKindEnum
foreign import data NumericLiteral :: SyntaxKindEnum
foreign import data BigIntLiteral :: SyntaxKindEnum
foreign import data StringLiteral :: SyntaxKindEnum
foreign import data JsxText :: SyntaxKindEnum
foreign import data JsxTextAllWhiteSpaces :: SyntaxKindEnum
foreign import data RegularExpressionLiteral :: SyntaxKindEnum
foreign import data NoSubstitutionTemplateLiteral :: SyntaxKindEnum
foreign import data TemplateHead :: SyntaxKindEnum
foreign import data TemplateMiddle :: SyntaxKindEnum
foreign import data TemplateTail :: SyntaxKindEnum
foreign import data OpenBraceToken :: SyntaxKindEnum
foreign import data CloseBraceToken :: SyntaxKindEnum
foreign import data OpenParenToken :: SyntaxKindEnum
foreign import data CloseParenToken :: SyntaxKindEnum
foreign import data OpenBracketToken :: SyntaxKindEnum
foreign import data CloseBracketToken :: SyntaxKindEnum
foreign import data DotToken :: SyntaxKindEnum
foreign import data DotDotDotToken :: SyntaxKindEnum
foreign import data SemicolonToken :: SyntaxKindEnum
foreign import data CommaToken :: SyntaxKindEnum
foreign import data QuestionDotToken :: SyntaxKindEnum
foreign import data LessThanToken :: SyntaxKindEnum
foreign import data LessThanSlashToken :: SyntaxKindEnum
foreign import data GreaterThanToken :: SyntaxKindEnum
foreign import data LessThanEqualsToken :: SyntaxKindEnum
foreign import data GreaterThanEqualsToken :: SyntaxKindEnum
foreign import data EqualsEqualsToken :: SyntaxKindEnum
foreign import data ExclamationEqualsToken :: SyntaxKindEnum
foreign import data EqualsEqualsEqualsToken :: SyntaxKindEnum
foreign import data ExclamationEqualsEqualsToken :: SyntaxKindEnum
foreign import data EqualsGreaterThanToken :: SyntaxKindEnum
foreign import data PlusToken :: SyntaxKindEnum
foreign import data MinusToken :: SyntaxKindEnum
foreign import data AsteriskToken :: SyntaxKindEnum
foreign import data AsteriskAsteriskToken :: SyntaxKindEnum
foreign import data SlashToken :: SyntaxKindEnum
foreign import data PercentToken :: SyntaxKindEnum
foreign import data PlusPlusToken :: SyntaxKindEnum
foreign import data MinusMinusToken :: SyntaxKindEnum
foreign import data LessThanLessThanToken :: SyntaxKindEnum
foreign import data GreaterThanGreaterThanToken :: SyntaxKindEnum
foreign import data GreaterThanGreaterThanGreaterThanToken :: SyntaxKindEnum
foreign import data AmpersandToken :: SyntaxKindEnum
foreign import data BarToken :: SyntaxKindEnum
foreign import data CaretToken :: SyntaxKindEnum
foreign import data ExclamationToken :: SyntaxKindEnum
foreign import data TildeToken :: SyntaxKindEnum
foreign import data AmpersandAmpersandToken :: SyntaxKindEnum
foreign import data BarBarToken :: SyntaxKindEnum
foreign import data QuestionToken :: SyntaxKindEnum
foreign import data ColonToken :: SyntaxKindEnum
foreign import data AtToken :: SyntaxKindEnum
foreign import data QuestionQuestionToken :: SyntaxKindEnum
foreign import data BacktickToken :: SyntaxKindEnum
foreign import data HashToken :: SyntaxKindEnum
foreign import data EqualsToken :: SyntaxKindEnum
foreign import data PlusEqualsToken :: SyntaxKindEnum
foreign import data MinusEqualsToken :: SyntaxKindEnum
foreign import data AsteriskEqualsToken :: SyntaxKindEnum
foreign import data AsteriskAsteriskEqualsToken :: SyntaxKindEnum
foreign import data SlashEqualsToken :: SyntaxKindEnum
foreign import data PercentEqualsToken :: SyntaxKindEnum
foreign import data LessThanLessThanEqualsToken :: SyntaxKindEnum
foreign import data GreaterThanGreaterThanEqualsToken :: SyntaxKindEnum
foreign import data GreaterThanGreaterThanGreaterThanEqualsToken :: SyntaxKindEnum
foreign import data AmpersandEqualsToken :: SyntaxKindEnum
foreign import data BarEqualsToken :: SyntaxKindEnum
foreign import data BarBarEqualsToken :: SyntaxKindEnum
foreign import data AmpersandAmpersandEqualsToken :: SyntaxKindEnum
foreign import data QuestionQuestionEqualsToken :: SyntaxKindEnum
foreign import data CaretEqualsToken :: SyntaxKindEnum
foreign import data Identifier :: SyntaxKindEnum
foreign import data PrivateIdentifier :: SyntaxKindEnum
foreign import data BreakKeyword :: SyntaxKindEnum
foreign import data CaseKeyword :: SyntaxKindEnum
foreign import data CatchKeyword :: SyntaxKindEnum
foreign import data ClassKeyword :: SyntaxKindEnum
foreign import data ConstKeyword :: SyntaxKindEnum
foreign import data ContinueKeyword :: SyntaxKindEnum
foreign import data DebuggerKeyword :: SyntaxKindEnum
foreign import data DefaultKeyword :: SyntaxKindEnum
foreign import data DeleteKeyword :: SyntaxKindEnum
foreign import data DoKeyword :: SyntaxKindEnum
foreign import data ElseKeyword :: SyntaxKindEnum
foreign import data EnumKeyword :: SyntaxKindEnum
foreign import data ExportKeyword :: SyntaxKindEnum
foreign import data ExtendsKeyword :: SyntaxKindEnum
foreign import data FalseKeyword :: SyntaxKindEnum
foreign import data FinallyKeyword :: SyntaxKindEnum
foreign import data ForKeyword :: SyntaxKindEnum
foreign import data FunctionKeyword :: SyntaxKindEnum
foreign import data IfKeyword :: SyntaxKindEnum
foreign import data ImportKeyword :: SyntaxKindEnum
foreign import data InKeyword :: SyntaxKindEnum
foreign import data InstanceOfKeyword :: SyntaxKindEnum
foreign import data NewKeyword :: SyntaxKindEnum
foreign import data NullKeyword :: SyntaxKindEnum
foreign import data ReturnKeyword :: SyntaxKindEnum
foreign import data SuperKeyword :: SyntaxKindEnum
foreign import data SwitchKeyword :: SyntaxKindEnum
foreign import data ThisKeyword :: SyntaxKindEnum
foreign import data ThrowKeyword :: SyntaxKindEnum
foreign import data TrueKeyword :: SyntaxKindEnum
foreign import data TryKeyword :: SyntaxKindEnum
foreign import data TypeOfKeyword :: SyntaxKindEnum
foreign import data VarKeyword :: SyntaxKindEnum
foreign import data VoidKeyword :: SyntaxKindEnum
foreign import data WhileKeyword :: SyntaxKindEnum
foreign import data WithKeyword :: SyntaxKindEnum
foreign import data ImplementsKeyword :: SyntaxKindEnum
foreign import data InterfaceKeyword :: SyntaxKindEnum
foreign import data LetKeyword :: SyntaxKindEnum
foreign import data PackageKeyword :: SyntaxKindEnum
foreign import data PrivateKeyword :: SyntaxKindEnum
foreign import data ProtectedKeyword :: SyntaxKindEnum
foreign import data PublicKeyword :: SyntaxKindEnum
foreign import data StaticKeyword :: SyntaxKindEnum
foreign import data YieldKeyword :: SyntaxKindEnum
foreign import data AbstractKeyword :: SyntaxKindEnum
foreign import data AsKeyword :: SyntaxKindEnum
foreign import data AssertsKeyword :: SyntaxKindEnum
foreign import data AnyKeyword :: SyntaxKindEnum
foreign import data AsyncKeyword :: SyntaxKindEnum
foreign import data AwaitKeyword :: SyntaxKindEnum
foreign import data BooleanKeyword :: SyntaxKindEnum
foreign import data ConstructorKeyword :: SyntaxKindEnum
foreign import data DeclareKeyword :: SyntaxKindEnum
foreign import data GetKeyword :: SyntaxKindEnum
foreign import data InferKeyword :: SyntaxKindEnum
foreign import data IntrinsicKeyword :: SyntaxKindEnum
foreign import data IsKeyword :: SyntaxKindEnum
foreign import data KeyOfKeyword :: SyntaxKindEnum
foreign import data ModuleKeyword :: SyntaxKindEnum
foreign import data NamespaceKeyword :: SyntaxKindEnum
foreign import data NeverKeyword :: SyntaxKindEnum
foreign import data ReadonlyKeyword :: SyntaxKindEnum
foreign import data RequireKeyword :: SyntaxKindEnum
foreign import data NumberKeyword :: SyntaxKindEnum
foreign import data ObjectKeyword :: SyntaxKindEnum
foreign import data SetKeyword :: SyntaxKindEnum
foreign import data StringKeyword :: SyntaxKindEnum
foreign import data SymbolKeyword :: SyntaxKindEnum
foreign import data TypeKeyword :: SyntaxKindEnum
foreign import data UndefinedKeyword :: SyntaxKindEnum
foreign import data UniqueKeyword :: SyntaxKindEnum
foreign import data UnknownKeyword :: SyntaxKindEnum
foreign import data FromKeyword :: SyntaxKindEnum
foreign import data GlobalKeyword :: SyntaxKindEnum
foreign import data BigIntKeyword :: SyntaxKindEnum
foreign import data OverrideKeyword :: SyntaxKindEnum
foreign import data OfKeyword :: SyntaxKindEnum
foreign import data QualifiedName :: SyntaxKindEnum
foreign import data ComputedPropertyName :: SyntaxKindEnum
foreign import data TypeParameter :: SyntaxKindEnum
foreign import data Parameter :: SyntaxKindEnum
foreign import data Decorator :: SyntaxKindEnum
foreign import data PropertySignature :: SyntaxKindEnum
foreign import data PropertyDeclaration :: SyntaxKindEnum
foreign import data MethodSignature :: SyntaxKindEnum
foreign import data MethodDeclaration :: SyntaxKindEnum
foreign import data ClassStaticBlockDeclaration :: SyntaxKindEnum
foreign import data Constructor :: SyntaxKindEnum
foreign import data GetAccessor :: SyntaxKindEnum
foreign import data SetAccessor :: SyntaxKindEnum
foreign import data CallSignature :: SyntaxKindEnum
foreign import data ConstructSignature :: SyntaxKindEnum
foreign import data IndexSignature :: SyntaxKindEnum
foreign import data TypePredicate :: SyntaxKindEnum
foreign import data TypeReference :: SyntaxKindEnum
foreign import data FunctionType :: SyntaxKindEnum
foreign import data ConstructorType :: SyntaxKindEnum
foreign import data TypeQuery :: SyntaxKindEnum
foreign import data TypeLiteral :: SyntaxKindEnum
foreign import data ArrayType :: SyntaxKindEnum
foreign import data TupleType :: SyntaxKindEnum
foreign import data OptionalType :: SyntaxKindEnum
foreign import data RestType :: SyntaxKindEnum
foreign import data UnionType :: SyntaxKindEnum
foreign import data IntersectionType :: SyntaxKindEnum
foreign import data ConditionalType :: SyntaxKindEnum
foreign import data InferType :: SyntaxKindEnum
foreign import data ParenthesizedType :: SyntaxKindEnum
foreign import data ThisType :: SyntaxKindEnum
foreign import data TypeOperator :: SyntaxKindEnum
foreign import data IndexedAccessType :: SyntaxKindEnum
foreign import data MappedType :: SyntaxKindEnum
foreign import data LiteralType :: SyntaxKindEnum
foreign import data NamedTupleMember :: SyntaxKindEnum
foreign import data TemplateLiteralType :: SyntaxKindEnum
foreign import data TemplateLiteralTypeSpan :: SyntaxKindEnum
foreign import data ImportType :: SyntaxKindEnum
foreign import data ObjectBindingPattern :: SyntaxKindEnum
foreign import data ArrayBindingPattern :: SyntaxKindEnum
foreign import data BindingElement :: SyntaxKindEnum
foreign import data ArrayLiteralExpression :: SyntaxKindEnum
foreign import data ObjectLiteralExpression :: SyntaxKindEnum
foreign import data PropertyAccessExpression :: SyntaxKindEnum
foreign import data ElementAccessExpression :: SyntaxKindEnum
foreign import data CallExpression :: SyntaxKindEnum
foreign import data NewExpression :: SyntaxKindEnum
foreign import data TaggedTemplateExpression :: SyntaxKindEnum
foreign import data TypeAssertionExpression :: SyntaxKindEnum
foreign import data ParenthesizedExpression :: SyntaxKindEnum
foreign import data FunctionExpression :: SyntaxKindEnum
foreign import data ArrowFunction :: SyntaxKindEnum
foreign import data DeleteExpression :: SyntaxKindEnum
foreign import data TypeOfExpression :: SyntaxKindEnum
foreign import data VoidExpression :: SyntaxKindEnum
foreign import data AwaitExpression :: SyntaxKindEnum
foreign import data PrefixUnaryExpression :: SyntaxKindEnum
foreign import data PostfixUnaryExpression :: SyntaxKindEnum
foreign import data BinaryExpression :: SyntaxKindEnum
foreign import data ConditionalExpression :: SyntaxKindEnum
foreign import data TemplateExpression :: SyntaxKindEnum
foreign import data YieldExpression :: SyntaxKindEnum
foreign import data SpreadElement :: SyntaxKindEnum
foreign import data ClassExpression :: SyntaxKindEnum
foreign import data OmittedExpression :: SyntaxKindEnum
foreign import data ExpressionWithTypeArguments :: SyntaxKindEnum
foreign import data AsExpression :: SyntaxKindEnum
foreign import data NonNullExpression :: SyntaxKindEnum
foreign import data MetaProperty :: SyntaxKindEnum
foreign import data SyntheticExpression :: SyntaxKindEnum
foreign import data TemplateSpan :: SyntaxKindEnum
foreign import data SemicolonClassElement :: SyntaxKindEnum
foreign import data Block :: SyntaxKindEnum
foreign import data EmptyStatement :: SyntaxKindEnum
foreign import data VariableStatement :: SyntaxKindEnum
foreign import data ExpressionStatement :: SyntaxKindEnum
foreign import data IfStatement :: SyntaxKindEnum
foreign import data DoStatement :: SyntaxKindEnum
foreign import data WhileStatement :: SyntaxKindEnum
foreign import data ForStatement :: SyntaxKindEnum
foreign import data ForInStatement :: SyntaxKindEnum
foreign import data ForOfStatement :: SyntaxKindEnum
foreign import data ContinueStatement :: SyntaxKindEnum
foreign import data BreakStatement :: SyntaxKindEnum
foreign import data ReturnStatement :: SyntaxKindEnum
foreign import data WithStatement :: SyntaxKindEnum
foreign import data SwitchStatement :: SyntaxKindEnum
foreign import data LabeledStatement :: SyntaxKindEnum
foreign import data ThrowStatement :: SyntaxKindEnum
foreign import data TryStatement :: SyntaxKindEnum
foreign import data DebuggerStatement :: SyntaxKindEnum
foreign import data VariableDeclaration :: SyntaxKindEnum
foreign import data VariableDeclarationList :: SyntaxKindEnum
foreign import data FunctionDeclaration :: SyntaxKindEnum
foreign import data ClassDeclaration :: SyntaxKindEnum
foreign import data InterfaceDeclaration :: SyntaxKindEnum
foreign import data TypeAliasDeclaration :: SyntaxKindEnum
foreign import data EnumDeclaration :: SyntaxKindEnum
foreign import data ModuleDeclaration :: SyntaxKindEnum
foreign import data ModuleBlock :: SyntaxKindEnum
foreign import data CaseBlock :: SyntaxKindEnum
foreign import data NamespaceExportDeclaration :: SyntaxKindEnum
foreign import data ImportEqualsDeclaration :: SyntaxKindEnum
foreign import data ImportDeclaration :: SyntaxKindEnum
foreign import data ImportClause :: SyntaxKindEnum
foreign import data NamespaceImport :: SyntaxKindEnum
foreign import data NamedImports :: SyntaxKindEnum
foreign import data ImportSpecifier :: SyntaxKindEnum
foreign import data ExportAssignment :: SyntaxKindEnum
foreign import data ExportDeclaration :: SyntaxKindEnum
foreign import data NamedExports :: SyntaxKindEnum
foreign import data NamespaceExport :: SyntaxKindEnum
foreign import data ExportSpecifier :: SyntaxKindEnum
foreign import data MissingDeclaration :: SyntaxKindEnum
foreign import data ExternalModuleReference :: SyntaxKindEnum
foreign import data JsxElement :: SyntaxKindEnum
foreign import data JsxSelfClosingElement :: SyntaxKindEnum
foreign import data JsxOpeningElement :: SyntaxKindEnum
foreign import data JsxClosingElement :: SyntaxKindEnum
foreign import data JsxFragment :: SyntaxKindEnum
foreign import data JsxOpeningFragment :: SyntaxKindEnum
foreign import data JsxClosingFragment :: SyntaxKindEnum
foreign import data JsxAttribute :: SyntaxKindEnum
foreign import data JsxAttributes :: SyntaxKindEnum
foreign import data JsxSpreadAttribute :: SyntaxKindEnum
foreign import data JsxExpression :: SyntaxKindEnum
foreign import data CaseClause :: SyntaxKindEnum
foreign import data DefaultClause :: SyntaxKindEnum
foreign import data HeritageClause :: SyntaxKindEnum
foreign import data CatchClause :: SyntaxKindEnum
foreign import data PropertyAssignment :: SyntaxKindEnum
foreign import data ShorthandPropertyAssignment :: SyntaxKindEnum
foreign import data SpreadAssignment :: SyntaxKindEnum
foreign import data EnumMember :: SyntaxKindEnum
foreign import data UnparsedPrologue :: SyntaxKindEnum
foreign import data UnparsedPrepend :: SyntaxKindEnum
foreign import data UnparsedText :: SyntaxKindEnum
foreign import data UnparsedInternalText :: SyntaxKindEnum
foreign import data UnparsedSyntheticReference :: SyntaxKindEnum
foreign import data SourceFile :: SyntaxKindEnum
foreign import data Bundle :: SyntaxKindEnum
foreign import data UnparsedSource :: SyntaxKindEnum
foreign import data InputFiles :: SyntaxKindEnum
foreign import data JSDocTypeExpression :: SyntaxKindEnum
foreign import data JSDocNameReference :: SyntaxKindEnum
foreign import data JSDocMemberName :: SyntaxKindEnum
foreign import data JSDocAllType :: SyntaxKindEnum
foreign import data JSDocUnknownType :: SyntaxKindEnum
foreign import data JSDocNullableType :: SyntaxKindEnum
foreign import data JSDocNonNullableType :: SyntaxKindEnum
foreign import data JSDocOptionalType :: SyntaxKindEnum
foreign import data JSDocFunctionType :: SyntaxKindEnum
foreign import data JSDocVariadicType :: SyntaxKindEnum
foreign import data JSDocNamepathType :: SyntaxKindEnum
foreign import data JSDocComment :: SyntaxKindEnum
foreign import data JSDocText :: SyntaxKindEnum
foreign import data JSDocTypeLiteral :: SyntaxKindEnum
foreign import data JSDocSignature :: SyntaxKindEnum
foreign import data JSDocLink :: SyntaxKindEnum
foreign import data JSDocLinkCode :: SyntaxKindEnum
foreign import data JSDocLinkPlain :: SyntaxKindEnum
foreign import data JSDocTag :: SyntaxKindEnum
foreign import data JSDocAugmentsTag :: SyntaxKindEnum
foreign import data JSDocImplementsTag :: SyntaxKindEnum
foreign import data JSDocAuthorTag :: SyntaxKindEnum
foreign import data JSDocDeprecatedTag :: SyntaxKindEnum
foreign import data JSDocClassTag :: SyntaxKindEnum
foreign import data JSDocPublicTag :: SyntaxKindEnum
foreign import data JSDocPrivateTag :: SyntaxKindEnum
foreign import data JSDocProtectedTag :: SyntaxKindEnum
foreign import data JSDocReadonlyTag :: SyntaxKindEnum
foreign import data JSDocOverrideTag :: SyntaxKindEnum
foreign import data JSDocCallbackTag :: SyntaxKindEnum
foreign import data JSDocEnumTag :: SyntaxKindEnum
foreign import data JSDocParameterTag :: SyntaxKindEnum
foreign import data JSDocReturnTag :: SyntaxKindEnum
foreign import data JSDocThisTag :: SyntaxKindEnum
foreign import data JSDocTypeTag :: SyntaxKindEnum
foreign import data JSDocTemplateTag :: SyntaxKindEnum
foreign import data JSDocTypedefTag :: SyntaxKindEnum
foreign import data JSDocSeeTag :: SyntaxKindEnum
foreign import data JSDocPropertyTag :: SyntaxKindEnum
foreign import data SyntaxList :: SyntaxKindEnum
foreign import data NotEmittedStatement :: SyntaxKindEnum
foreign import data PartiallyEmittedExpression :: SyntaxKindEnum
foreign import data CommaListExpression :: SyntaxKindEnum
foreign import data MergeDeclarationMarker :: SyntaxKindEnum
foreign import data EndOfDeclarationMarker :: SyntaxKindEnum
foreign import data SyntheticReferenceExpression :: SyntaxKindEnum
foreign import data Count :: SyntaxKindEnum
foreign import data FirstAssignment :: SyntaxKindEnum
foreign import data LastAssignment :: SyntaxKindEnum
foreign import data FirstCompoundAssignment :: SyntaxKindEnum
foreign import data LastCompoundAssignment :: SyntaxKindEnum
foreign import data FirstReservedWord :: SyntaxKindEnum
foreign import data LastReservedWord :: SyntaxKindEnum
foreign import data FirstKeyword :: SyntaxKindEnum
foreign import data LastKeyword :: SyntaxKindEnum
foreign import data FirstFutureReservedWord :: SyntaxKindEnum
foreign import data LastFutureReservedWord :: SyntaxKindEnum
foreign import data FirstTypeNode :: SyntaxKindEnum
foreign import data LastTypeNode :: SyntaxKindEnum
foreign import data FirstPunctuation :: SyntaxKindEnum
foreign import data LastPunctuation :: SyntaxKindEnum
foreign import data FirstToken :: SyntaxKindEnum
foreign import data LastToken :: SyntaxKindEnum
foreign import data FirstTriviaToken :: SyntaxKindEnum
foreign import data LastTriviaToken :: SyntaxKindEnum
foreign import data FirstLiteralToken :: SyntaxKindEnum
foreign import data LastLiteralToken :: SyntaxKindEnum
foreign import data FirstTemplateToken :: SyntaxKindEnum
foreign import data LastTemplateToken :: SyntaxKindEnum
foreign import data FirstBinaryOperator :: SyntaxKindEnum
foreign import data LastBinaryOperator :: SyntaxKindEnum
foreign import data FirstStatement :: SyntaxKindEnum
foreign import data LastStatement :: SyntaxKindEnum
foreign import data FirstNode :: SyntaxKindEnum
foreign import data FirstJSDocNode :: SyntaxKindEnum
foreign import data LastJSDocNode :: SyntaxKindEnum
foreign import data FirstJSDocTagNode :: SyntaxKindEnum
foreign import data LastJSDocTagNode :: SyntaxKindEnum

data SyntaxKind :: SyntaxKindEnum -> Type
data SyntaxKind k

foreign import unknown :: SyntaxKind Unknown
foreign import endOfFileToken :: SyntaxKind EndOfFileToken
foreign import singleLineCommentTrivia :: SyntaxKind SingleLineCommentTrivia
foreign import multiLineCommentTrivia :: SyntaxKind MultiLineCommentTrivia
foreign import newLineTrivia :: SyntaxKind NewLineTrivia
foreign import whitespaceTrivia :: SyntaxKind WhitespaceTrivia
foreign import shebangTrivia :: SyntaxKind ShebangTrivia
foreign import conflictMarkerTrivia :: SyntaxKind ConflictMarkerTrivia
foreign import numericLiteral :: SyntaxKind NumericLiteral
foreign import bigIntLiteral :: SyntaxKind BigIntLiteral
foreign import stringLiteral :: SyntaxKind StringLiteral
foreign import jsxText :: SyntaxKind JsxText
foreign import jsxTextAllWhiteSpaces :: SyntaxKind JsxTextAllWhiteSpaces
foreign import regularExpressionLiteral :: SyntaxKind RegularExpressionLiteral
foreign import noSubstitutionTemplateLiteral :: SyntaxKind NoSubstitutionTemplateLiteral
foreign import templateHead :: SyntaxKind TemplateHead
foreign import templateMiddle :: SyntaxKind TemplateMiddle
foreign import templateTail :: SyntaxKind TemplateTail
foreign import openBraceToken :: SyntaxKind OpenBraceToken
foreign import closeBraceToken :: SyntaxKind CloseBraceToken
foreign import openParenToken :: SyntaxKind OpenParenToken
foreign import closeParenToken :: SyntaxKind CloseParenToken
foreign import openBracketToken :: SyntaxKind OpenBracketToken
foreign import closeBracketToken :: SyntaxKind CloseBracketToken
foreign import dotToken :: SyntaxKind DotToken
foreign import dotDotDotToken :: SyntaxKind DotDotDotToken
foreign import semicolonToken :: SyntaxKind SemicolonToken
foreign import commaToken :: SyntaxKind CommaToken
foreign import questionDotToken :: SyntaxKind QuestionDotToken
foreign import lessThanToken :: SyntaxKind LessThanToken
foreign import lessThanSlashToken :: SyntaxKind LessThanSlashToken
foreign import greaterThanToken :: SyntaxKind GreaterThanToken
foreign import lessThanEqualsToken :: SyntaxKind LessThanEqualsToken
foreign import greaterThanEqualsToken :: SyntaxKind GreaterThanEqualsToken
foreign import equalsEqualsToken :: SyntaxKind EqualsEqualsToken
foreign import exclamationEqualsToken :: SyntaxKind ExclamationEqualsToken
foreign import equalsEqualsEqualsToken :: SyntaxKind EqualsEqualsEqualsToken
foreign import exclamationEqualsEqualsToken :: SyntaxKind ExclamationEqualsEqualsToken
foreign import equalsGreaterThanToken :: SyntaxKind EqualsGreaterThanToken
foreign import plusToken :: SyntaxKind PlusToken
foreign import minusToken :: SyntaxKind MinusToken
foreign import asteriskToken :: SyntaxKind AsteriskToken
foreign import asteriskAsteriskToken :: SyntaxKind AsteriskAsteriskToken
foreign import slashToken :: SyntaxKind SlashToken
foreign import percentToken :: SyntaxKind PercentToken
foreign import plusPlusToken :: SyntaxKind PlusPlusToken
foreign import minusMinusToken :: SyntaxKind MinusMinusToken
foreign import lessThanLessThanToken :: SyntaxKind LessThanLessThanToken
foreign import greaterThanGreaterThanToken :: SyntaxKind GreaterThanGreaterThanToken
foreign import greaterThanGreaterThanGreaterThanToken :: SyntaxKind GreaterThanGreaterThanGreaterThanToken
foreign import ampersandToken :: SyntaxKind AmpersandToken
foreign import barToken :: SyntaxKind BarToken
foreign import caretToken :: SyntaxKind CaretToken
foreign import exclamationToken :: SyntaxKind ExclamationToken
foreign import tildeToken :: SyntaxKind TildeToken
foreign import ampersandAmpersandToken :: SyntaxKind AmpersandAmpersandToken
foreign import barBarToken :: SyntaxKind BarBarToken
foreign import questionToken :: SyntaxKind QuestionToken
foreign import colonToken :: SyntaxKind ColonToken
foreign import atToken :: SyntaxKind AtToken
foreign import questionQuestionToken :: SyntaxKind QuestionQuestionToken
foreign import backtickToken :: SyntaxKind BacktickToken
foreign import hashToken :: SyntaxKind HashToken
foreign import equalsToken :: SyntaxKind EqualsToken
foreign import plusEqualsToken :: SyntaxKind PlusEqualsToken
foreign import minusEqualsToken :: SyntaxKind MinusEqualsToken
foreign import asteriskEqualsToken :: SyntaxKind AsteriskEqualsToken
foreign import asteriskAsteriskEqualsToken :: SyntaxKind AsteriskAsteriskEqualsToken
foreign import slashEqualsToken :: SyntaxKind SlashEqualsToken
foreign import percentEqualsToken :: SyntaxKind PercentEqualsToken
foreign import lessThanLessThanEqualsToken :: SyntaxKind LessThanLessThanEqualsToken
foreign import greaterThanGreaterThanEqualsToken :: SyntaxKind GreaterThanGreaterThanEqualsToken
foreign import greaterThanGreaterThanGreaterThanEqualsToken :: SyntaxKind GreaterThanGreaterThanGreaterThanEqualsToken
foreign import ampersandEqualsToken :: SyntaxKind AmpersandEqualsToken
foreign import barEqualsToken :: SyntaxKind BarEqualsToken
foreign import barBarEqualsToken :: SyntaxKind BarBarEqualsToken
foreign import ampersandAmpersandEqualsToken :: SyntaxKind AmpersandAmpersandEqualsToken
foreign import questionQuestionEqualsToken :: SyntaxKind QuestionQuestionEqualsToken
foreign import caretEqualsToken :: SyntaxKind CaretEqualsToken
foreign import identifier :: SyntaxKind Identifier
foreign import privateIdentifier :: SyntaxKind PrivateIdentifier
foreign import breakKeyword :: SyntaxKind BreakKeyword
foreign import caseKeyword :: SyntaxKind CaseKeyword
foreign import catchKeyword :: SyntaxKind CatchKeyword
foreign import classKeyword :: SyntaxKind ClassKeyword
foreign import constKeyword :: SyntaxKind ConstKeyword
foreign import continueKeyword :: SyntaxKind ContinueKeyword
foreign import debuggerKeyword :: SyntaxKind DebuggerKeyword
foreign import defaultKeyword :: SyntaxKind DefaultKeyword
foreign import deleteKeyword :: SyntaxKind DeleteKeyword
foreign import doKeyword :: SyntaxKind DoKeyword
foreign import elseKeyword :: SyntaxKind ElseKeyword
foreign import enumKeyword :: SyntaxKind EnumKeyword
foreign import exportKeyword :: SyntaxKind ExportKeyword
foreign import extendsKeyword :: SyntaxKind ExtendsKeyword
foreign import falseKeyword :: SyntaxKind FalseKeyword
foreign import finallyKeyword :: SyntaxKind FinallyKeyword
foreign import forKeyword :: SyntaxKind ForKeyword
foreign import functionKeyword :: SyntaxKind FunctionKeyword
foreign import ifKeyword :: SyntaxKind IfKeyword
foreign import importKeyword :: SyntaxKind ImportKeyword
foreign import inKeyword :: SyntaxKind InKeyword
foreign import instanceOfKeyword :: SyntaxKind InstanceOfKeyword
foreign import newKeyword :: SyntaxKind NewKeyword
foreign import nullKeyword :: SyntaxKind NullKeyword
foreign import returnKeyword :: SyntaxKind ReturnKeyword
foreign import superKeyword :: SyntaxKind SuperKeyword
foreign import switchKeyword :: SyntaxKind SwitchKeyword
foreign import thisKeyword :: SyntaxKind ThisKeyword
foreign import throwKeyword :: SyntaxKind ThrowKeyword
foreign import trueKeyword :: SyntaxKind TrueKeyword
foreign import tryKeyword :: SyntaxKind TryKeyword
foreign import typeOfKeyword :: SyntaxKind TypeOfKeyword
foreign import varKeyword :: SyntaxKind VarKeyword
foreign import voidKeyword :: SyntaxKind VoidKeyword
foreign import whileKeyword :: SyntaxKind WhileKeyword
foreign import withKeyword :: SyntaxKind WithKeyword
foreign import implementsKeyword :: SyntaxKind ImplementsKeyword
foreign import interfaceKeyword :: SyntaxKind InterfaceKeyword
foreign import letKeyword :: SyntaxKind LetKeyword
foreign import packageKeyword :: SyntaxKind PackageKeyword
foreign import privateKeyword :: SyntaxKind PrivateKeyword
foreign import protectedKeyword :: SyntaxKind ProtectedKeyword
foreign import publicKeyword :: SyntaxKind PublicKeyword
foreign import staticKeyword :: SyntaxKind StaticKeyword
foreign import yieldKeyword :: SyntaxKind YieldKeyword
foreign import abstractKeyword :: SyntaxKind AbstractKeyword
foreign import asKeyword :: SyntaxKind AsKeyword
foreign import assertsKeyword :: SyntaxKind AssertsKeyword
foreign import anyKeyword :: SyntaxKind AnyKeyword
foreign import asyncKeyword :: SyntaxKind AsyncKeyword
foreign import awaitKeyword :: SyntaxKind AwaitKeyword
foreign import booleanKeyword :: SyntaxKind BooleanKeyword
foreign import constructorKeyword :: SyntaxKind ConstructorKeyword
foreign import declareKeyword :: SyntaxKind DeclareKeyword
foreign import getKeyword :: SyntaxKind GetKeyword
foreign import inferKeyword :: SyntaxKind InferKeyword
foreign import intrinsicKeyword :: SyntaxKind IntrinsicKeyword
foreign import isKeyword :: SyntaxKind IsKeyword
foreign import keyOfKeyword :: SyntaxKind KeyOfKeyword
foreign import moduleKeyword :: SyntaxKind ModuleKeyword
foreign import namespaceKeyword :: SyntaxKind NamespaceKeyword
foreign import neverKeyword :: SyntaxKind NeverKeyword
foreign import readonlyKeyword :: SyntaxKind ReadonlyKeyword
foreign import requireKeyword :: SyntaxKind RequireKeyword
foreign import numberKeyword :: SyntaxKind NumberKeyword
foreign import objectKeyword :: SyntaxKind ObjectKeyword
foreign import setKeyword :: SyntaxKind SetKeyword
foreign import stringKeyword :: SyntaxKind StringKeyword
foreign import symbolKeyword :: SyntaxKind SymbolKeyword
foreign import typeKeyword :: SyntaxKind TypeKeyword
foreign import undefinedKeyword :: SyntaxKind UndefinedKeyword
foreign import uniqueKeyword :: SyntaxKind UniqueKeyword
foreign import unknownKeyword :: SyntaxKind UnknownKeyword
foreign import fromKeyword :: SyntaxKind FromKeyword
foreign import globalKeyword :: SyntaxKind GlobalKeyword
foreign import bigIntKeyword :: SyntaxKind BigIntKeyword
foreign import overrideKeyword :: SyntaxKind OverrideKeyword
foreign import ofKeyword :: SyntaxKind OfKeyword
foreign import qualifiedName :: SyntaxKind QualifiedName
foreign import computedPropertyName :: SyntaxKind ComputedPropertyName
foreign import typeParameter :: SyntaxKind TypeParameter
foreign import parameter :: SyntaxKind Parameter
foreign import decorator :: SyntaxKind Decorator
foreign import propertySignature :: SyntaxKind PropertySignature
foreign import propertyDeclaration :: SyntaxKind PropertyDeclaration
foreign import methodSignature :: SyntaxKind MethodSignature
foreign import methodDeclaration :: SyntaxKind MethodDeclaration
foreign import classStaticBlockDeclaration :: SyntaxKind ClassStaticBlockDeclaration
foreign import constructor :: SyntaxKind Constructor
foreign import getAccessor :: SyntaxKind GetAccessor
foreign import setAccessor :: SyntaxKind SetAccessor
foreign import callSignature :: SyntaxKind CallSignature
foreign import constructSignature :: SyntaxKind ConstructSignature
foreign import indexSignature :: SyntaxKind IndexSignature
foreign import typePredicate :: SyntaxKind TypePredicate
foreign import typeReference :: SyntaxKind TypeReference
foreign import functionType :: SyntaxKind FunctionType
foreign import constructorType :: SyntaxKind ConstructorType
foreign import typeQuery :: SyntaxKind TypeQuery
foreign import typeLiteral :: SyntaxKind TypeLiteral
foreign import arrayType :: SyntaxKind ArrayType
foreign import tupleType :: SyntaxKind TupleType
foreign import optionalType :: SyntaxKind OptionalType
foreign import restType :: SyntaxKind RestType
foreign import unionType :: SyntaxKind UnionType
foreign import intersectionType :: SyntaxKind IntersectionType
foreign import conditionalType :: SyntaxKind ConditionalType
foreign import inferType :: SyntaxKind InferType
foreign import parenthesizedType :: SyntaxKind ParenthesizedType
foreign import thisType :: SyntaxKind ThisType
foreign import typeOperator :: SyntaxKind TypeOperator
foreign import indexedAccessType :: SyntaxKind IndexedAccessType
foreign import mappedType :: SyntaxKind MappedType
foreign import literalType :: SyntaxKind LiteralType
foreign import namedTupleMember :: SyntaxKind NamedTupleMember
foreign import templateLiteralType :: SyntaxKind TemplateLiteralType
foreign import templateLiteralTypeSpan :: SyntaxKind TemplateLiteralTypeSpan
foreign import importType :: SyntaxKind ImportType
foreign import objectBindingPattern :: SyntaxKind ObjectBindingPattern
foreign import arrayBindingPattern :: SyntaxKind ArrayBindingPattern
foreign import bindingElement :: SyntaxKind BindingElement
foreign import arrayLiteralExpression :: SyntaxKind ArrayLiteralExpression
foreign import objectLiteralExpression :: SyntaxKind ObjectLiteralExpression
foreign import propertyAccessExpression :: SyntaxKind PropertyAccessExpression
foreign import elementAccessExpression :: SyntaxKind ElementAccessExpression
foreign import callExpression :: SyntaxKind CallExpression
foreign import newExpression :: SyntaxKind NewExpression
foreign import taggedTemplateExpression :: SyntaxKind TaggedTemplateExpression
foreign import typeAssertionExpression :: SyntaxKind TypeAssertionExpression
foreign import parenthesizedExpression :: SyntaxKind ParenthesizedExpression
foreign import functionExpression :: SyntaxKind FunctionExpression
foreign import arrowFunction :: SyntaxKind ArrowFunction
foreign import deleteExpression :: SyntaxKind DeleteExpression
foreign import typeOfExpression :: SyntaxKind TypeOfExpression
foreign import voidExpression :: SyntaxKind VoidExpression
foreign import awaitExpression :: SyntaxKind AwaitExpression
foreign import prefixUnaryExpression :: SyntaxKind PrefixUnaryExpression
foreign import postfixUnaryExpression :: SyntaxKind PostfixUnaryExpression
foreign import binaryExpression :: SyntaxKind BinaryExpression
foreign import conditionalExpression :: SyntaxKind ConditionalExpression
foreign import templateExpression :: SyntaxKind TemplateExpression
foreign import yieldExpression :: SyntaxKind YieldExpression
foreign import spreadElement :: SyntaxKind SpreadElement
foreign import classExpression :: SyntaxKind ClassExpression
foreign import omittedExpression :: SyntaxKind OmittedExpression
foreign import expressionWithTypeArguments :: SyntaxKind ExpressionWithTypeArguments
foreign import asExpression :: SyntaxKind AsExpression
foreign import nonNullExpression :: SyntaxKind NonNullExpression
foreign import metaProperty :: SyntaxKind MetaProperty
foreign import syntheticExpression :: SyntaxKind SyntheticExpression
foreign import templateSpan :: SyntaxKind TemplateSpan
foreign import semicolonClassElement :: SyntaxKind SemicolonClassElement
foreign import block :: SyntaxKind Block
foreign import emptyStatement :: SyntaxKind EmptyStatement
foreign import variableStatement :: SyntaxKind VariableStatement
foreign import expressionStatement :: SyntaxKind ExpressionStatement
foreign import ifStatement :: SyntaxKind IfStatement
foreign import doStatement :: SyntaxKind DoStatement
foreign import whileStatement :: SyntaxKind WhileStatement
foreign import forStatement :: SyntaxKind ForStatement
foreign import forInStatement :: SyntaxKind ForInStatement
foreign import forOfStatement :: SyntaxKind ForOfStatement
foreign import continueStatement :: SyntaxKind ContinueStatement
foreign import breakStatement :: SyntaxKind BreakStatement
foreign import returnStatement :: SyntaxKind ReturnStatement
foreign import withStatement :: SyntaxKind WithStatement
foreign import switchStatement :: SyntaxKind SwitchStatement
foreign import labeledStatement :: SyntaxKind LabeledStatement
foreign import throwStatement :: SyntaxKind ThrowStatement
foreign import tryStatement :: SyntaxKind TryStatement
foreign import debuggerStatement :: SyntaxKind DebuggerStatement
foreign import variableDeclaration :: SyntaxKind VariableDeclaration
foreign import variableDeclarationList :: SyntaxKind VariableDeclarationList
foreign import functionDeclaration :: SyntaxKind FunctionDeclaration
foreign import classDeclaration :: SyntaxKind ClassDeclaration
foreign import interfaceDeclaration :: SyntaxKind InterfaceDeclaration
foreign import typeAliasDeclaration :: SyntaxKind TypeAliasDeclaration
foreign import enumDeclaration :: SyntaxKind EnumDeclaration
foreign import moduleDeclaration :: SyntaxKind ModuleDeclaration
foreign import moduleBlock :: SyntaxKind ModuleBlock
foreign import caseBlock :: SyntaxKind CaseBlock
foreign import namespaceExportDeclaration :: SyntaxKind NamespaceExportDeclaration
foreign import importEqualsDeclaration :: SyntaxKind ImportEqualsDeclaration
foreign import importDeclaration :: SyntaxKind ImportDeclaration
foreign import importClause :: SyntaxKind ImportClause
foreign import namespaceImport :: SyntaxKind NamespaceImport
foreign import namedImports :: SyntaxKind NamedImports
foreign import importSpecifier :: SyntaxKind ImportSpecifier
foreign import exportAssignment :: SyntaxKind ExportAssignment
foreign import exportDeclaration :: SyntaxKind ExportDeclaration
foreign import namedExports :: SyntaxKind NamedExports
foreign import namespaceExport :: SyntaxKind NamespaceExport
foreign import exportSpecifier :: SyntaxKind ExportSpecifier
foreign import missingDeclaration :: SyntaxKind MissingDeclaration
foreign import externalModuleReference :: SyntaxKind ExternalModuleReference
foreign import jsxElement :: SyntaxKind JsxElement
foreign import jsxSelfClosingElement :: SyntaxKind JsxSelfClosingElement
foreign import jsxOpeningElement :: SyntaxKind JsxOpeningElement
foreign import jsxClosingElement :: SyntaxKind JsxClosingElement
foreign import jsxFragment :: SyntaxKind JsxFragment
foreign import jsxOpeningFragment :: SyntaxKind JsxOpeningFragment
foreign import jsxClosingFragment :: SyntaxKind JsxClosingFragment
foreign import jsxAttribute :: SyntaxKind JsxAttribute
foreign import jsxAttributes :: SyntaxKind JsxAttributes
foreign import jsxSpreadAttribute :: SyntaxKind JsxSpreadAttribute
foreign import jsxExpression :: SyntaxKind JsxExpression
foreign import caseClause :: SyntaxKind CaseClause
foreign import defaultClause :: SyntaxKind DefaultClause
foreign import heritageClause :: SyntaxKind HeritageClause
foreign import catchClause :: SyntaxKind CatchClause
foreign import propertyAssignment :: SyntaxKind PropertyAssignment
foreign import shorthandPropertyAssignment :: SyntaxKind ShorthandPropertyAssignment
foreign import spreadAssignment :: SyntaxKind SpreadAssignment
foreign import enumMember :: SyntaxKind EnumMember
foreign import unparsedPrologue :: SyntaxKind UnparsedPrologue
foreign import unparsedPrepend :: SyntaxKind UnparsedPrepend
foreign import unparsedText :: SyntaxKind UnparsedText
foreign import unparsedInternalText :: SyntaxKind UnparsedInternalText
foreign import unparsedSyntheticReference :: SyntaxKind UnparsedSyntheticReference
foreign import sourceFile :: SyntaxKind SourceFile
foreign import bundle :: SyntaxKind Bundle
foreign import unparsedSource :: SyntaxKind UnparsedSource
foreign import inputFiles :: SyntaxKind InputFiles
foreign import jSDocTypeExpression :: SyntaxKind JSDocTypeExpression
foreign import jSDocNameReference :: SyntaxKind JSDocNameReference
foreign import jSDocMemberName :: SyntaxKind JSDocMemberName
foreign import jSDocAllType :: SyntaxKind JSDocAllType
foreign import jSDocUnknownType :: SyntaxKind JSDocUnknownType
foreign import jSDocNullableType :: SyntaxKind JSDocNullableType
foreign import jSDocNonNullableType :: SyntaxKind JSDocNonNullableType
foreign import jSDocOptionalType :: SyntaxKind JSDocOptionalType
foreign import jSDocFunctionType :: SyntaxKind JSDocFunctionType
foreign import jSDocVariadicType :: SyntaxKind JSDocVariadicType
foreign import jSDocNamepathType :: SyntaxKind JSDocNamepathType
foreign import jSDocComment :: SyntaxKind JSDocComment
foreign import jSDocText :: SyntaxKind JSDocText
foreign import jSDocTypeLiteral :: SyntaxKind JSDocTypeLiteral
foreign import jSDocSignature :: SyntaxKind JSDocSignature
foreign import jSDocLink :: SyntaxKind JSDocLink
foreign import jSDocLinkCode :: SyntaxKind JSDocLinkCode
foreign import jSDocLinkPlain :: SyntaxKind JSDocLinkPlain
foreign import jSDocTag :: SyntaxKind JSDocTag
foreign import jSDocAugmentsTag :: SyntaxKind JSDocAugmentsTag
foreign import jSDocImplementsTag :: SyntaxKind JSDocImplementsTag
foreign import jSDocAuthorTag :: SyntaxKind JSDocAuthorTag
foreign import jSDocDeprecatedTag :: SyntaxKind JSDocDeprecatedTag
foreign import jSDocClassTag :: SyntaxKind JSDocClassTag
foreign import jSDocPublicTag :: SyntaxKind JSDocPublicTag
foreign import jSDocPrivateTag :: SyntaxKind JSDocPrivateTag
foreign import jSDocProtectedTag :: SyntaxKind JSDocProtectedTag
foreign import jSDocReadonlyTag :: SyntaxKind JSDocReadonlyTag
foreign import jSDocOverrideTag :: SyntaxKind JSDocOverrideTag
foreign import jSDocCallbackTag :: SyntaxKind JSDocCallbackTag
foreign import jSDocEnumTag :: SyntaxKind JSDocEnumTag
foreign import jSDocParameterTag :: SyntaxKind JSDocParameterTag
foreign import jSDocReturnTag :: SyntaxKind JSDocReturnTag
foreign import jSDocThisTag :: SyntaxKind JSDocThisTag
foreign import jSDocTypeTag :: SyntaxKind JSDocTypeTag
foreign import jSDocTemplateTag :: SyntaxKind JSDocTemplateTag
foreign import jSDocTypedefTag :: SyntaxKind JSDocTypedefTag
foreign import jSDocSeeTag :: SyntaxKind JSDocSeeTag
foreign import jSDocPropertyTag :: SyntaxKind JSDocPropertyTag
foreign import syntaxList :: SyntaxKind SyntaxList
foreign import notEmittedStatement :: SyntaxKind NotEmittedStatement
foreign import partiallyEmittedExpression :: SyntaxKind PartiallyEmittedExpression
foreign import commaListExpression :: SyntaxKind CommaListExpression
foreign import mergeDeclarationMarker :: SyntaxKind MergeDeclarationMarker
foreign import endOfDeclarationMarker :: SyntaxKind EndOfDeclarationMarker
foreign import syntheticReferenceExpression :: SyntaxKind SyntheticReferenceExpression
foreign import count :: SyntaxKind Count
foreign import firstAssignment :: SyntaxKind FirstAssignment
foreign import lastAssignment :: SyntaxKind LastAssignment
foreign import firstCompoundAssignment :: SyntaxKind FirstCompoundAssignment
foreign import lastCompoundAssignment :: SyntaxKind LastCompoundAssignment
foreign import firstReservedWord :: SyntaxKind FirstReservedWord
foreign import lastReservedWord :: SyntaxKind LastReservedWord
foreign import firstKeyword :: SyntaxKind FirstKeyword
foreign import lastKeyword :: SyntaxKind LastKeyword
foreign import firstFutureReservedWord :: SyntaxKind FirstFutureReservedWord
foreign import lastFutureReservedWord :: SyntaxKind LastFutureReservedWord
foreign import firstTypeNode :: SyntaxKind FirstTypeNode
foreign import lastTypeNode :: SyntaxKind LastTypeNode
foreign import firstPunctuation :: SyntaxKind FirstPunctuation
foreign import lastPunctuation :: SyntaxKind LastPunctuation
foreign import firstToken :: SyntaxKind FirstToken
foreign import lastToken :: SyntaxKind LastToken
foreign import firstTriviaToken :: SyntaxKind FirstTriviaToken
foreign import lastTriviaToken :: SyntaxKind LastTriviaToken
foreign import firstLiteralToken :: SyntaxKind FirstLiteralToken
foreign import lastLiteralToken :: SyntaxKind LastLiteralToken
foreign import firstTemplateToken :: SyntaxKind FirstTemplateToken
foreign import lastTemplateToken :: SyntaxKind LastTemplateToken
foreign import firstBinaryOperator :: SyntaxKind FirstBinaryOperator
foreign import lastBinaryOperator :: SyntaxKind LastBinaryOperator
foreign import firstStatement :: SyntaxKind FirstStatement
foreign import lastStatement :: SyntaxKind LastStatement
foreign import firstNode :: SyntaxKind FirstNode
foreign import firstJSDocNode :: SyntaxKind FirstJSDocNode
foreign import lastJSDocNode :: SyntaxKind LastJSDocNode
foreign import firstJSDocTagNode :: SyntaxKind FirstJSDocTagNode
foreign import lastJSDocTagNode :: SyntaxKind LastJSDocTagNode

instance EnumRep SyntaxKind Unknown Int where
  runtimeRep = unsafeCoerce unknown
  enumRep = unknown

instance EnumRep SyntaxKind EndOfFileToken Int where
  runtimeRep = unsafeCoerce endOfFileToken
  enumRep = endOfFileToken

instance EnumRep SyntaxKind SingleLineCommentTrivia Int where
  runtimeRep = unsafeCoerce singleLineCommentTrivia
  enumRep = singleLineCommentTrivia

instance EnumRep SyntaxKind MultiLineCommentTrivia Int where
  runtimeRep = unsafeCoerce multiLineCommentTrivia
  enumRep = multiLineCommentTrivia

instance EnumRep SyntaxKind NewLineTrivia Int where
  runtimeRep = unsafeCoerce newLineTrivia
  enumRep = newLineTrivia

instance EnumRep SyntaxKind WhitespaceTrivia Int where
  runtimeRep = unsafeCoerce whitespaceTrivia
  enumRep = whitespaceTrivia

instance EnumRep SyntaxKind ShebangTrivia Int where
  runtimeRep = unsafeCoerce shebangTrivia
  enumRep = shebangTrivia

instance EnumRep SyntaxKind ConflictMarkerTrivia Int where
  runtimeRep = unsafeCoerce conflictMarkerTrivia
  enumRep = conflictMarkerTrivia

instance EnumRep SyntaxKind NumericLiteral Int where
  runtimeRep = unsafeCoerce numericLiteral
  enumRep = numericLiteral

instance EnumRep SyntaxKind BigIntLiteral Int where
  runtimeRep = unsafeCoerce bigIntLiteral
  enumRep = bigIntLiteral

instance EnumRep SyntaxKind StringLiteral Int where
  runtimeRep = unsafeCoerce stringLiteral
  enumRep = stringLiteral

instance EnumRep SyntaxKind JsxText Int where
  runtimeRep = unsafeCoerce jsxText
  enumRep = jsxText

instance EnumRep SyntaxKind JsxTextAllWhiteSpaces Int where
  runtimeRep = unsafeCoerce jsxTextAllWhiteSpaces
  enumRep = jsxTextAllWhiteSpaces

instance EnumRep SyntaxKind RegularExpressionLiteral Int where
  runtimeRep = unsafeCoerce regularExpressionLiteral
  enumRep = regularExpressionLiteral

instance EnumRep SyntaxKind NoSubstitutionTemplateLiteral Int where
  runtimeRep = unsafeCoerce noSubstitutionTemplateLiteral
  enumRep = noSubstitutionTemplateLiteral

instance EnumRep SyntaxKind TemplateHead Int where
  runtimeRep = unsafeCoerce templateHead
  enumRep = templateHead

instance EnumRep SyntaxKind TemplateMiddle Int where
  runtimeRep = unsafeCoerce templateMiddle
  enumRep = templateMiddle

instance EnumRep SyntaxKind TemplateTail Int where
  runtimeRep = unsafeCoerce templateTail
  enumRep = templateTail

instance EnumRep SyntaxKind OpenBraceToken Int where
  runtimeRep = unsafeCoerce openBraceToken
  enumRep = openBraceToken

instance EnumRep SyntaxKind CloseBraceToken Int where
  runtimeRep = unsafeCoerce closeBraceToken
  enumRep = closeBraceToken

instance EnumRep SyntaxKind OpenParenToken Int where
  runtimeRep = unsafeCoerce openParenToken
  enumRep = openParenToken

instance EnumRep SyntaxKind CloseParenToken Int where
  runtimeRep = unsafeCoerce closeParenToken
  enumRep = closeParenToken

instance EnumRep SyntaxKind OpenBracketToken Int where
  runtimeRep = unsafeCoerce openBracketToken
  enumRep = openBracketToken

instance EnumRep SyntaxKind CloseBracketToken Int where
  runtimeRep = unsafeCoerce closeBracketToken
  enumRep = closeBracketToken

instance EnumRep SyntaxKind DotToken Int where
  runtimeRep = unsafeCoerce dotToken
  enumRep = dotToken

instance EnumRep SyntaxKind DotDotDotToken Int where
  runtimeRep = unsafeCoerce dotDotDotToken
  enumRep = dotDotDotToken

instance EnumRep SyntaxKind SemicolonToken Int where
  runtimeRep = unsafeCoerce semicolonToken
  enumRep = semicolonToken

instance EnumRep SyntaxKind CommaToken Int where
  runtimeRep = unsafeCoerce commaToken
  enumRep = commaToken

instance EnumRep SyntaxKind QuestionDotToken Int where
  runtimeRep = unsafeCoerce questionDotToken
  enumRep = questionDotToken

instance EnumRep SyntaxKind LessThanToken Int where
  runtimeRep = unsafeCoerce lessThanToken
  enumRep = lessThanToken

instance EnumRep SyntaxKind LessThanSlashToken Int where
  runtimeRep = unsafeCoerce lessThanSlashToken
  enumRep = lessThanSlashToken

instance EnumRep SyntaxKind GreaterThanToken Int where
  runtimeRep = unsafeCoerce greaterThanToken
  enumRep = greaterThanToken

instance EnumRep SyntaxKind LessThanEqualsToken Int where
  runtimeRep = unsafeCoerce lessThanEqualsToken
  enumRep = lessThanEqualsToken

instance EnumRep SyntaxKind GreaterThanEqualsToken Int where
  runtimeRep = unsafeCoerce greaterThanEqualsToken
  enumRep = greaterThanEqualsToken

instance EnumRep SyntaxKind EqualsEqualsToken Int where
  runtimeRep = unsafeCoerce equalsEqualsToken
  enumRep = equalsEqualsToken

instance EnumRep SyntaxKind ExclamationEqualsToken Int where
  runtimeRep = unsafeCoerce exclamationEqualsToken
  enumRep = exclamationEqualsToken

instance EnumRep SyntaxKind EqualsEqualsEqualsToken Int where
  runtimeRep = unsafeCoerce equalsEqualsEqualsToken
  enumRep = equalsEqualsEqualsToken

instance EnumRep SyntaxKind ExclamationEqualsEqualsToken Int where
  runtimeRep = unsafeCoerce exclamationEqualsEqualsToken
  enumRep = exclamationEqualsEqualsToken

instance EnumRep SyntaxKind EqualsGreaterThanToken Int where
  runtimeRep = unsafeCoerce equalsGreaterThanToken
  enumRep = equalsGreaterThanToken

instance EnumRep SyntaxKind PlusToken Int where
  runtimeRep = unsafeCoerce plusToken
  enumRep = plusToken

instance EnumRep SyntaxKind MinusToken Int where
  runtimeRep = unsafeCoerce minusToken
  enumRep = minusToken

instance EnumRep SyntaxKind AsteriskToken Int where
  runtimeRep = unsafeCoerce asteriskToken
  enumRep = asteriskToken

instance EnumRep SyntaxKind AsteriskAsteriskToken Int where
  runtimeRep = unsafeCoerce asteriskAsteriskToken
  enumRep = asteriskAsteriskToken

instance EnumRep SyntaxKind SlashToken Int where
  runtimeRep = unsafeCoerce slashToken
  enumRep = slashToken

instance EnumRep SyntaxKind PercentToken Int where
  runtimeRep = unsafeCoerce percentToken
  enumRep = percentToken

instance EnumRep SyntaxKind PlusPlusToken Int where
  runtimeRep = unsafeCoerce plusPlusToken
  enumRep = plusPlusToken

instance EnumRep SyntaxKind MinusMinusToken Int where
  runtimeRep = unsafeCoerce minusMinusToken
  enumRep = minusMinusToken

instance EnumRep SyntaxKind LessThanLessThanToken Int where
  runtimeRep = unsafeCoerce lessThanLessThanToken
  enumRep = lessThanLessThanToken

instance EnumRep SyntaxKind GreaterThanGreaterThanToken Int where
  runtimeRep = unsafeCoerce greaterThanGreaterThanToken
  enumRep = greaterThanGreaterThanToken

instance EnumRep SyntaxKind GreaterThanGreaterThanGreaterThanToken Int where
  runtimeRep = unsafeCoerce greaterThanGreaterThanGreaterThanToken
  enumRep = greaterThanGreaterThanGreaterThanToken

instance EnumRep SyntaxKind AmpersandToken Int where
  runtimeRep = unsafeCoerce ampersandToken
  enumRep = ampersandToken

instance EnumRep SyntaxKind BarToken Int where
  runtimeRep = unsafeCoerce barToken
  enumRep = barToken

instance EnumRep SyntaxKind CaretToken Int where
  runtimeRep = unsafeCoerce caretToken
  enumRep = caretToken

instance EnumRep SyntaxKind ExclamationToken Int where
  runtimeRep = unsafeCoerce exclamationToken
  enumRep = exclamationToken

instance EnumRep SyntaxKind TildeToken Int where
  runtimeRep = unsafeCoerce tildeToken
  enumRep = tildeToken

instance EnumRep SyntaxKind AmpersandAmpersandToken Int where
  runtimeRep = unsafeCoerce ampersandAmpersandToken
  enumRep = ampersandAmpersandToken

instance EnumRep SyntaxKind BarBarToken Int where
  runtimeRep = unsafeCoerce barBarToken
  enumRep = barBarToken

instance EnumRep SyntaxKind QuestionToken Int where
  runtimeRep = unsafeCoerce questionToken
  enumRep = questionToken

instance EnumRep SyntaxKind ColonToken Int where
  runtimeRep = unsafeCoerce colonToken
  enumRep = colonToken

instance EnumRep SyntaxKind AtToken Int where
  runtimeRep = unsafeCoerce atToken
  enumRep = atToken

instance EnumRep SyntaxKind QuestionQuestionToken Int where
  runtimeRep = unsafeCoerce questionQuestionToken
  enumRep = questionQuestionToken

instance EnumRep SyntaxKind BacktickToken Int where
  runtimeRep = unsafeCoerce backtickToken
  enumRep = backtickToken

instance EnumRep SyntaxKind HashToken Int where
  runtimeRep = unsafeCoerce hashToken
  enumRep = hashToken

instance EnumRep SyntaxKind EqualsToken Int where
  runtimeRep = unsafeCoerce equalsToken
  enumRep = equalsToken

instance EnumRep SyntaxKind PlusEqualsToken Int where
  runtimeRep = unsafeCoerce plusEqualsToken
  enumRep = plusEqualsToken

instance EnumRep SyntaxKind MinusEqualsToken Int where
  runtimeRep = unsafeCoerce minusEqualsToken
  enumRep = minusEqualsToken

instance EnumRep SyntaxKind AsteriskEqualsToken Int where
  runtimeRep = unsafeCoerce asteriskEqualsToken
  enumRep = asteriskEqualsToken

instance EnumRep SyntaxKind AsteriskAsteriskEqualsToken Int where
  runtimeRep = unsafeCoerce asteriskAsteriskEqualsToken
  enumRep = asteriskAsteriskEqualsToken

instance EnumRep SyntaxKind SlashEqualsToken Int where
  runtimeRep = unsafeCoerce slashEqualsToken
  enumRep = slashEqualsToken

instance EnumRep SyntaxKind PercentEqualsToken Int where
  runtimeRep = unsafeCoerce percentEqualsToken
  enumRep = percentEqualsToken

instance EnumRep SyntaxKind LessThanLessThanEqualsToken Int where
  runtimeRep = unsafeCoerce lessThanLessThanEqualsToken
  enumRep = lessThanLessThanEqualsToken

instance EnumRep SyntaxKind GreaterThanGreaterThanEqualsToken Int where
  runtimeRep = unsafeCoerce greaterThanGreaterThanEqualsToken
  enumRep = greaterThanGreaterThanEqualsToken

instance EnumRep SyntaxKind GreaterThanGreaterThanGreaterThanEqualsToken Int where
  runtimeRep = unsafeCoerce greaterThanGreaterThanGreaterThanEqualsToken
  enumRep = greaterThanGreaterThanGreaterThanEqualsToken

instance EnumRep SyntaxKind AmpersandEqualsToken Int where
  runtimeRep = unsafeCoerce ampersandEqualsToken
  enumRep = ampersandEqualsToken

instance EnumRep SyntaxKind BarEqualsToken Int where
  runtimeRep = unsafeCoerce barEqualsToken
  enumRep = barEqualsToken

instance EnumRep SyntaxKind BarBarEqualsToken Int where
  runtimeRep = unsafeCoerce barBarEqualsToken
  enumRep = barBarEqualsToken

instance EnumRep SyntaxKind AmpersandAmpersandEqualsToken Int where
  runtimeRep = unsafeCoerce ampersandAmpersandEqualsToken
  enumRep = ampersandAmpersandEqualsToken

instance EnumRep SyntaxKind QuestionQuestionEqualsToken Int where
  runtimeRep = unsafeCoerce questionQuestionEqualsToken
  enumRep = questionQuestionEqualsToken

instance EnumRep SyntaxKind CaretEqualsToken Int where
  runtimeRep = unsafeCoerce caretEqualsToken
  enumRep = caretEqualsToken

instance EnumRep SyntaxKind Identifier Int where
  runtimeRep = unsafeCoerce identifier
  enumRep = identifier

instance EnumRep SyntaxKind PrivateIdentifier Int where
  runtimeRep = unsafeCoerce privateIdentifier
  enumRep = privateIdentifier

instance EnumRep SyntaxKind BreakKeyword Int where
  runtimeRep = unsafeCoerce breakKeyword
  enumRep = breakKeyword

instance EnumRep SyntaxKind CaseKeyword Int where
  runtimeRep = unsafeCoerce caseKeyword
  enumRep = caseKeyword

instance EnumRep SyntaxKind CatchKeyword Int where
  runtimeRep = unsafeCoerce catchKeyword
  enumRep = catchKeyword

instance EnumRep SyntaxKind ClassKeyword Int where
  runtimeRep = unsafeCoerce classKeyword
  enumRep = classKeyword

instance EnumRep SyntaxKind ConstKeyword Int where
  runtimeRep = unsafeCoerce constKeyword
  enumRep = constKeyword

instance EnumRep SyntaxKind ContinueKeyword Int where
  runtimeRep = unsafeCoerce continueKeyword
  enumRep = continueKeyword

instance EnumRep SyntaxKind DebuggerKeyword Int where
  runtimeRep = unsafeCoerce debuggerKeyword
  enumRep = debuggerKeyword

instance EnumRep SyntaxKind DefaultKeyword Int where
  runtimeRep = unsafeCoerce defaultKeyword
  enumRep = defaultKeyword

instance EnumRep SyntaxKind DeleteKeyword Int where
  runtimeRep = unsafeCoerce deleteKeyword
  enumRep = deleteKeyword

instance EnumRep SyntaxKind DoKeyword Int where
  runtimeRep = unsafeCoerce doKeyword
  enumRep = doKeyword

instance EnumRep SyntaxKind ElseKeyword Int where
  runtimeRep = unsafeCoerce elseKeyword
  enumRep = elseKeyword

instance EnumRep SyntaxKind EnumKeyword Int where
  runtimeRep = unsafeCoerce enumKeyword
  enumRep = enumKeyword

instance EnumRep SyntaxKind ExportKeyword Int where
  runtimeRep = unsafeCoerce exportKeyword
  enumRep = exportKeyword

instance EnumRep SyntaxKind ExtendsKeyword Int where
  runtimeRep = unsafeCoerce extendsKeyword
  enumRep = extendsKeyword

instance EnumRep SyntaxKind FalseKeyword Int where
  runtimeRep = unsafeCoerce falseKeyword
  enumRep = falseKeyword

instance EnumRep SyntaxKind FinallyKeyword Int where
  runtimeRep = unsafeCoerce finallyKeyword
  enumRep = finallyKeyword

instance EnumRep SyntaxKind ForKeyword Int where
  runtimeRep = unsafeCoerce forKeyword
  enumRep = forKeyword

instance EnumRep SyntaxKind FunctionKeyword Int where
  runtimeRep = unsafeCoerce functionKeyword
  enumRep = functionKeyword

instance EnumRep SyntaxKind IfKeyword Int where
  runtimeRep = unsafeCoerce ifKeyword
  enumRep = ifKeyword

instance EnumRep SyntaxKind ImportKeyword Int where
  runtimeRep = unsafeCoerce importKeyword
  enumRep = importKeyword

instance EnumRep SyntaxKind InKeyword Int where
  runtimeRep = unsafeCoerce inKeyword
  enumRep = inKeyword

instance EnumRep SyntaxKind InstanceOfKeyword Int where
  runtimeRep = unsafeCoerce instanceOfKeyword
  enumRep = instanceOfKeyword

instance EnumRep SyntaxKind NewKeyword Int where
  runtimeRep = unsafeCoerce newKeyword
  enumRep = newKeyword

instance EnumRep SyntaxKind NullKeyword Int where
  runtimeRep = unsafeCoerce nullKeyword
  enumRep = nullKeyword

instance EnumRep SyntaxKind ReturnKeyword Int where
  runtimeRep = unsafeCoerce returnKeyword
  enumRep = returnKeyword

instance EnumRep SyntaxKind SuperKeyword Int where
  runtimeRep = unsafeCoerce superKeyword
  enumRep = superKeyword

instance EnumRep SyntaxKind SwitchKeyword Int where
  runtimeRep = unsafeCoerce switchKeyword
  enumRep = switchKeyword

instance EnumRep SyntaxKind ThisKeyword Int where
  runtimeRep = unsafeCoerce thisKeyword
  enumRep = thisKeyword

instance EnumRep SyntaxKind ThrowKeyword Int where
  runtimeRep = unsafeCoerce throwKeyword
  enumRep = throwKeyword

instance EnumRep SyntaxKind TrueKeyword Int where
  runtimeRep = unsafeCoerce trueKeyword
  enumRep = trueKeyword

instance EnumRep SyntaxKind TryKeyword Int where
  runtimeRep = unsafeCoerce tryKeyword
  enumRep = tryKeyword

instance EnumRep SyntaxKind TypeOfKeyword Int where
  runtimeRep = unsafeCoerce typeOfKeyword
  enumRep = typeOfKeyword

instance EnumRep SyntaxKind VarKeyword Int where
  runtimeRep = unsafeCoerce varKeyword
  enumRep = varKeyword

instance EnumRep SyntaxKind VoidKeyword Int where
  runtimeRep = unsafeCoerce voidKeyword
  enumRep = voidKeyword

instance EnumRep SyntaxKind WhileKeyword Int where
  runtimeRep = unsafeCoerce whileKeyword
  enumRep = whileKeyword

instance EnumRep SyntaxKind WithKeyword Int where
  runtimeRep = unsafeCoerce withKeyword
  enumRep = withKeyword

instance EnumRep SyntaxKind ImplementsKeyword Int where
  runtimeRep = unsafeCoerce implementsKeyword
  enumRep = implementsKeyword

instance EnumRep SyntaxKind InterfaceKeyword Int where
  runtimeRep = unsafeCoerce interfaceKeyword
  enumRep = interfaceKeyword

instance EnumRep SyntaxKind LetKeyword Int where
  runtimeRep = unsafeCoerce letKeyword
  enumRep = letKeyword

instance EnumRep SyntaxKind PackageKeyword Int where
  runtimeRep = unsafeCoerce packageKeyword
  enumRep = packageKeyword

instance EnumRep SyntaxKind PrivateKeyword Int where
  runtimeRep = unsafeCoerce privateKeyword
  enumRep = privateKeyword

instance EnumRep SyntaxKind ProtectedKeyword Int where
  runtimeRep = unsafeCoerce protectedKeyword
  enumRep = protectedKeyword

instance EnumRep SyntaxKind PublicKeyword Int where
  runtimeRep = unsafeCoerce publicKeyword
  enumRep = publicKeyword

instance EnumRep SyntaxKind StaticKeyword Int where
  runtimeRep = unsafeCoerce staticKeyword
  enumRep = staticKeyword

instance EnumRep SyntaxKind YieldKeyword Int where
  runtimeRep = unsafeCoerce yieldKeyword
  enumRep = yieldKeyword

instance EnumRep SyntaxKind AbstractKeyword Int where
  runtimeRep = unsafeCoerce abstractKeyword
  enumRep = abstractKeyword

instance EnumRep SyntaxKind AsKeyword Int where
  runtimeRep = unsafeCoerce asKeyword
  enumRep = asKeyword

instance EnumRep SyntaxKind AssertsKeyword Int where
  runtimeRep = unsafeCoerce assertsKeyword
  enumRep = assertsKeyword

instance EnumRep SyntaxKind AnyKeyword Int where
  runtimeRep = unsafeCoerce anyKeyword
  enumRep = anyKeyword

instance EnumRep SyntaxKind AsyncKeyword Int where
  runtimeRep = unsafeCoerce asyncKeyword
  enumRep = asyncKeyword

instance EnumRep SyntaxKind AwaitKeyword Int where
  runtimeRep = unsafeCoerce awaitKeyword
  enumRep = awaitKeyword

instance EnumRep SyntaxKind BooleanKeyword Int where
  runtimeRep = unsafeCoerce booleanKeyword
  enumRep = booleanKeyword

instance EnumRep SyntaxKind ConstructorKeyword Int where
  runtimeRep = unsafeCoerce constructorKeyword
  enumRep = constructorKeyword

instance EnumRep SyntaxKind DeclareKeyword Int where
  runtimeRep = unsafeCoerce declareKeyword
  enumRep = declareKeyword

instance EnumRep SyntaxKind GetKeyword Int where
  runtimeRep = unsafeCoerce getKeyword
  enumRep = getKeyword

instance EnumRep SyntaxKind InferKeyword Int where
  runtimeRep = unsafeCoerce inferKeyword
  enumRep = inferKeyword

instance EnumRep SyntaxKind IntrinsicKeyword Int where
  runtimeRep = unsafeCoerce intrinsicKeyword
  enumRep = intrinsicKeyword

instance EnumRep SyntaxKind IsKeyword Int where
  runtimeRep = unsafeCoerce isKeyword
  enumRep = isKeyword

instance EnumRep SyntaxKind KeyOfKeyword Int where
  runtimeRep = unsafeCoerce keyOfKeyword
  enumRep = keyOfKeyword

instance EnumRep SyntaxKind ModuleKeyword Int where
  runtimeRep = unsafeCoerce moduleKeyword
  enumRep = moduleKeyword

instance EnumRep SyntaxKind NamespaceKeyword Int where
  runtimeRep = unsafeCoerce namespaceKeyword
  enumRep = namespaceKeyword

instance EnumRep SyntaxKind NeverKeyword Int where
  runtimeRep = unsafeCoerce neverKeyword
  enumRep = neverKeyword

instance EnumRep SyntaxKind ReadonlyKeyword Int where
  runtimeRep = unsafeCoerce readonlyKeyword
  enumRep = readonlyKeyword

instance EnumRep SyntaxKind RequireKeyword Int where
  runtimeRep = unsafeCoerce requireKeyword
  enumRep = requireKeyword

instance EnumRep SyntaxKind NumberKeyword Int where
  runtimeRep = unsafeCoerce numberKeyword
  enumRep = numberKeyword

instance EnumRep SyntaxKind ObjectKeyword Int where
  runtimeRep = unsafeCoerce objectKeyword
  enumRep = objectKeyword

instance EnumRep SyntaxKind SetKeyword Int where
  runtimeRep = unsafeCoerce setKeyword
  enumRep = setKeyword

instance EnumRep SyntaxKind StringKeyword Int where
  runtimeRep = unsafeCoerce stringKeyword
  enumRep = stringKeyword

instance EnumRep SyntaxKind SymbolKeyword Int where
  runtimeRep = unsafeCoerce symbolKeyword
  enumRep = symbolKeyword

instance EnumRep SyntaxKind TypeKeyword Int where
  runtimeRep = unsafeCoerce typeKeyword
  enumRep = typeKeyword

instance EnumRep SyntaxKind UndefinedKeyword Int where
  runtimeRep = unsafeCoerce undefinedKeyword
  enumRep = undefinedKeyword

instance EnumRep SyntaxKind UniqueKeyword Int where
  runtimeRep = unsafeCoerce uniqueKeyword
  enumRep = uniqueKeyword

instance EnumRep SyntaxKind UnknownKeyword Int where
  runtimeRep = unsafeCoerce unknownKeyword
  enumRep = unknownKeyword

instance EnumRep SyntaxKind FromKeyword Int where
  runtimeRep = unsafeCoerce fromKeyword
  enumRep = fromKeyword

instance EnumRep SyntaxKind GlobalKeyword Int where
  runtimeRep = unsafeCoerce globalKeyword
  enumRep = globalKeyword

instance EnumRep SyntaxKind BigIntKeyword Int where
  runtimeRep = unsafeCoerce bigIntKeyword
  enumRep = bigIntKeyword

instance EnumRep SyntaxKind OverrideKeyword Int where
  runtimeRep = unsafeCoerce overrideKeyword
  enumRep = overrideKeyword

instance EnumRep SyntaxKind OfKeyword Int where
  runtimeRep = unsafeCoerce ofKeyword
  enumRep = ofKeyword

instance EnumRep SyntaxKind QualifiedName Int where
  runtimeRep = unsafeCoerce qualifiedName
  enumRep = qualifiedName

instance EnumRep SyntaxKind ComputedPropertyName Int where
  runtimeRep = unsafeCoerce computedPropertyName
  enumRep = computedPropertyName

instance EnumRep SyntaxKind TypeParameter Int where
  runtimeRep = unsafeCoerce typeParameter
  enumRep = typeParameter

instance EnumRep SyntaxKind Parameter Int where
  runtimeRep = unsafeCoerce parameter
  enumRep = parameter

instance EnumRep SyntaxKind Decorator Int where
  runtimeRep = unsafeCoerce decorator
  enumRep = decorator

instance EnumRep SyntaxKind PropertySignature Int where
  runtimeRep = unsafeCoerce propertySignature
  enumRep = propertySignature

instance EnumRep SyntaxKind PropertyDeclaration Int where
  runtimeRep = unsafeCoerce propertyDeclaration
  enumRep = propertyDeclaration

instance EnumRep SyntaxKind MethodSignature Int where
  runtimeRep = unsafeCoerce methodSignature
  enumRep = methodSignature

instance EnumRep SyntaxKind MethodDeclaration Int where
  runtimeRep = unsafeCoerce methodDeclaration
  enumRep = methodDeclaration

instance EnumRep SyntaxKind ClassStaticBlockDeclaration Int where
  runtimeRep = unsafeCoerce classStaticBlockDeclaration
  enumRep = classStaticBlockDeclaration

instance EnumRep SyntaxKind Constructor Int where
  runtimeRep = unsafeCoerce constructor
  enumRep = constructor

instance EnumRep SyntaxKind GetAccessor Int where
  runtimeRep = unsafeCoerce getAccessor
  enumRep = getAccessor

instance EnumRep SyntaxKind SetAccessor Int where
  runtimeRep = unsafeCoerce setAccessor
  enumRep = setAccessor

instance EnumRep SyntaxKind CallSignature Int where
  runtimeRep = unsafeCoerce callSignature
  enumRep = callSignature

instance EnumRep SyntaxKind ConstructSignature Int where
  runtimeRep = unsafeCoerce constructSignature
  enumRep = constructSignature

instance EnumRep SyntaxKind IndexSignature Int where
  runtimeRep = unsafeCoerce indexSignature
  enumRep = indexSignature

instance EnumRep SyntaxKind TypePredicate Int where
  runtimeRep = unsafeCoerce typePredicate
  enumRep = typePredicate

instance EnumRep SyntaxKind TypeReference Int where
  runtimeRep = unsafeCoerce typeReference
  enumRep = typeReference

instance EnumRep SyntaxKind FunctionType Int where
  runtimeRep = unsafeCoerce functionType
  enumRep = functionType

instance EnumRep SyntaxKind ConstructorType Int where
  runtimeRep = unsafeCoerce constructorType
  enumRep = constructorType

instance EnumRep SyntaxKind TypeQuery Int where
  runtimeRep = unsafeCoerce typeQuery
  enumRep = typeQuery

instance EnumRep SyntaxKind TypeLiteral Int where
  runtimeRep = unsafeCoerce typeLiteral
  enumRep = typeLiteral

instance EnumRep SyntaxKind ArrayType Int where
  runtimeRep = unsafeCoerce arrayType
  enumRep = arrayType

instance EnumRep SyntaxKind TupleType Int where
  runtimeRep = unsafeCoerce tupleType
  enumRep = tupleType

instance EnumRep SyntaxKind OptionalType Int where
  runtimeRep = unsafeCoerce optionalType
  enumRep = optionalType

instance EnumRep SyntaxKind RestType Int where
  runtimeRep = unsafeCoerce restType
  enumRep = restType

instance EnumRep SyntaxKind UnionType Int where
  runtimeRep = unsafeCoerce unionType
  enumRep = unionType

instance EnumRep SyntaxKind IntersectionType Int where
  runtimeRep = unsafeCoerce intersectionType
  enumRep = intersectionType

instance EnumRep SyntaxKind ConditionalType Int where
  runtimeRep = unsafeCoerce conditionalType
  enumRep = conditionalType

instance EnumRep SyntaxKind InferType Int where
  runtimeRep = unsafeCoerce inferType
  enumRep = inferType

instance EnumRep SyntaxKind ParenthesizedType Int where
  runtimeRep = unsafeCoerce parenthesizedType
  enumRep = parenthesizedType

instance EnumRep SyntaxKind ThisType Int where
  runtimeRep = unsafeCoerce thisType
  enumRep = thisType

instance EnumRep SyntaxKind TypeOperator Int where
  runtimeRep = unsafeCoerce typeOperator
  enumRep = typeOperator

instance EnumRep SyntaxKind IndexedAccessType Int where
  runtimeRep = unsafeCoerce indexedAccessType
  enumRep = indexedAccessType

instance EnumRep SyntaxKind MappedType Int where
  runtimeRep = unsafeCoerce mappedType
  enumRep = mappedType

instance EnumRep SyntaxKind LiteralType Int where
  runtimeRep = unsafeCoerce literalType
  enumRep = literalType

instance EnumRep SyntaxKind NamedTupleMember Int where
  runtimeRep = unsafeCoerce namedTupleMember
  enumRep = namedTupleMember

instance EnumRep SyntaxKind TemplateLiteralType Int where
  runtimeRep = unsafeCoerce templateLiteralType
  enumRep = templateLiteralType

instance EnumRep SyntaxKind TemplateLiteralTypeSpan Int where
  runtimeRep = unsafeCoerce templateLiteralTypeSpan
  enumRep = templateLiteralTypeSpan

instance EnumRep SyntaxKind ImportType Int where
  runtimeRep = unsafeCoerce importType
  enumRep = importType

instance EnumRep SyntaxKind ObjectBindingPattern Int where
  runtimeRep = unsafeCoerce objectBindingPattern
  enumRep = objectBindingPattern

instance EnumRep SyntaxKind ArrayBindingPattern Int where
  runtimeRep = unsafeCoerce arrayBindingPattern
  enumRep = arrayBindingPattern

instance EnumRep SyntaxKind BindingElement Int where
  runtimeRep = unsafeCoerce bindingElement
  enumRep = bindingElement

instance EnumRep SyntaxKind ArrayLiteralExpression Int where
  runtimeRep = unsafeCoerce arrayLiteralExpression
  enumRep = arrayLiteralExpression

instance EnumRep SyntaxKind ObjectLiteralExpression Int where
  runtimeRep = unsafeCoerce objectLiteralExpression
  enumRep = objectLiteralExpression

instance EnumRep SyntaxKind PropertyAccessExpression Int where
  runtimeRep = unsafeCoerce propertyAccessExpression
  enumRep = propertyAccessExpression

instance EnumRep SyntaxKind ElementAccessExpression Int where
  runtimeRep = unsafeCoerce elementAccessExpression
  enumRep = elementAccessExpression

instance EnumRep SyntaxKind CallExpression Int where
  runtimeRep = unsafeCoerce callExpression
  enumRep = callExpression

instance EnumRep SyntaxKind NewExpression Int where
  runtimeRep = unsafeCoerce newExpression
  enumRep = newExpression

instance EnumRep SyntaxKind TaggedTemplateExpression Int where
  runtimeRep = unsafeCoerce taggedTemplateExpression
  enumRep = taggedTemplateExpression

instance EnumRep SyntaxKind TypeAssertionExpression Int where
  runtimeRep = unsafeCoerce typeAssertionExpression
  enumRep = typeAssertionExpression

instance EnumRep SyntaxKind ParenthesizedExpression Int where
  runtimeRep = unsafeCoerce parenthesizedExpression
  enumRep = parenthesizedExpression

instance EnumRep SyntaxKind FunctionExpression Int where
  runtimeRep = unsafeCoerce functionExpression
  enumRep = functionExpression

instance EnumRep SyntaxKind ArrowFunction Int where
  runtimeRep = unsafeCoerce arrowFunction
  enumRep = arrowFunction

instance EnumRep SyntaxKind DeleteExpression Int where
  runtimeRep = unsafeCoerce deleteExpression
  enumRep = deleteExpression

instance EnumRep SyntaxKind TypeOfExpression Int where
  runtimeRep = unsafeCoerce typeOfExpression
  enumRep = typeOfExpression

instance EnumRep SyntaxKind VoidExpression Int where
  runtimeRep = unsafeCoerce voidExpression
  enumRep = voidExpression

instance EnumRep SyntaxKind AwaitExpression Int where
  runtimeRep = unsafeCoerce awaitExpression
  enumRep = awaitExpression

instance EnumRep SyntaxKind PrefixUnaryExpression Int where
  runtimeRep = unsafeCoerce prefixUnaryExpression
  enumRep = prefixUnaryExpression

instance EnumRep SyntaxKind PostfixUnaryExpression Int where
  runtimeRep = unsafeCoerce postfixUnaryExpression
  enumRep = postfixUnaryExpression

instance EnumRep SyntaxKind BinaryExpression Int where
  runtimeRep = unsafeCoerce binaryExpression
  enumRep = binaryExpression

instance EnumRep SyntaxKind ConditionalExpression Int where
  runtimeRep = unsafeCoerce conditionalExpression
  enumRep = conditionalExpression

instance EnumRep SyntaxKind TemplateExpression Int where
  runtimeRep = unsafeCoerce templateExpression
  enumRep = templateExpression

instance EnumRep SyntaxKind YieldExpression Int where
  runtimeRep = unsafeCoerce yieldExpression
  enumRep = yieldExpression

instance EnumRep SyntaxKind SpreadElement Int where
  runtimeRep = unsafeCoerce spreadElement
  enumRep = spreadElement

instance EnumRep SyntaxKind ClassExpression Int where
  runtimeRep = unsafeCoerce classExpression
  enumRep = classExpression

instance EnumRep SyntaxKind OmittedExpression Int where
  runtimeRep = unsafeCoerce omittedExpression
  enumRep = omittedExpression

instance EnumRep SyntaxKind ExpressionWithTypeArguments Int where
  runtimeRep = unsafeCoerce expressionWithTypeArguments
  enumRep = expressionWithTypeArguments

instance EnumRep SyntaxKind AsExpression Int where
  runtimeRep = unsafeCoerce asExpression
  enumRep = asExpression

instance EnumRep SyntaxKind NonNullExpression Int where
  runtimeRep = unsafeCoerce nonNullExpression
  enumRep = nonNullExpression

instance EnumRep SyntaxKind MetaProperty Int where
  runtimeRep = unsafeCoerce metaProperty
  enumRep = metaProperty

instance EnumRep SyntaxKind SyntheticExpression Int where
  runtimeRep = unsafeCoerce syntheticExpression
  enumRep = syntheticExpression

instance EnumRep SyntaxKind TemplateSpan Int where
  runtimeRep = unsafeCoerce templateSpan
  enumRep = templateSpan

instance EnumRep SyntaxKind SemicolonClassElement Int where
  runtimeRep = unsafeCoerce semicolonClassElement
  enumRep = semicolonClassElement

instance EnumRep SyntaxKind Block Int where
  runtimeRep = unsafeCoerce block
  enumRep = block

instance EnumRep SyntaxKind EmptyStatement Int where
  runtimeRep = unsafeCoerce emptyStatement
  enumRep = emptyStatement

instance EnumRep SyntaxKind VariableStatement Int where
  runtimeRep = unsafeCoerce variableStatement
  enumRep = variableStatement

instance EnumRep SyntaxKind ExpressionStatement Int where
  runtimeRep = unsafeCoerce expressionStatement
  enumRep = expressionStatement

instance EnumRep SyntaxKind IfStatement Int where
  runtimeRep = unsafeCoerce ifStatement
  enumRep = ifStatement

instance EnumRep SyntaxKind DoStatement Int where
  runtimeRep = unsafeCoerce doStatement
  enumRep = doStatement

instance EnumRep SyntaxKind WhileStatement Int where
  runtimeRep = unsafeCoerce whileStatement
  enumRep = whileStatement

instance EnumRep SyntaxKind ForStatement Int where
  runtimeRep = unsafeCoerce forStatement
  enumRep = forStatement

instance EnumRep SyntaxKind ForInStatement Int where
  runtimeRep = unsafeCoerce forInStatement
  enumRep = forInStatement

instance EnumRep SyntaxKind ForOfStatement Int where
  runtimeRep = unsafeCoerce forOfStatement
  enumRep = forOfStatement

instance EnumRep SyntaxKind ContinueStatement Int where
  runtimeRep = unsafeCoerce continueStatement
  enumRep = continueStatement

instance EnumRep SyntaxKind BreakStatement Int where
  runtimeRep = unsafeCoerce breakStatement
  enumRep = breakStatement

instance EnumRep SyntaxKind ReturnStatement Int where
  runtimeRep = unsafeCoerce returnStatement
  enumRep = returnStatement

instance EnumRep SyntaxKind WithStatement Int where
  runtimeRep = unsafeCoerce withStatement
  enumRep = withStatement

instance EnumRep SyntaxKind SwitchStatement Int where
  runtimeRep = unsafeCoerce switchStatement
  enumRep = switchStatement

instance EnumRep SyntaxKind LabeledStatement Int where
  runtimeRep = unsafeCoerce labeledStatement
  enumRep = labeledStatement

instance EnumRep SyntaxKind ThrowStatement Int where
  runtimeRep = unsafeCoerce throwStatement
  enumRep = throwStatement

instance EnumRep SyntaxKind TryStatement Int where
  runtimeRep = unsafeCoerce tryStatement
  enumRep = tryStatement

instance EnumRep SyntaxKind DebuggerStatement Int where
  runtimeRep = unsafeCoerce debuggerStatement
  enumRep = debuggerStatement

instance EnumRep SyntaxKind VariableDeclaration Int where
  runtimeRep = unsafeCoerce variableDeclaration
  enumRep = variableDeclaration

instance EnumRep SyntaxKind VariableDeclarationList Int where
  runtimeRep = unsafeCoerce variableDeclarationList
  enumRep = variableDeclarationList

instance EnumRep SyntaxKind FunctionDeclaration Int where
  runtimeRep = unsafeCoerce functionDeclaration
  enumRep = functionDeclaration

instance EnumRep SyntaxKind ClassDeclaration Int where
  runtimeRep = unsafeCoerce classDeclaration
  enumRep = classDeclaration

instance EnumRep SyntaxKind InterfaceDeclaration Int where
  runtimeRep = unsafeCoerce interfaceDeclaration
  enumRep = interfaceDeclaration

instance EnumRep SyntaxKind TypeAliasDeclaration Int where
  runtimeRep = unsafeCoerce typeAliasDeclaration
  enumRep = typeAliasDeclaration

instance EnumRep SyntaxKind EnumDeclaration Int where
  runtimeRep = unsafeCoerce enumDeclaration
  enumRep = enumDeclaration

instance EnumRep SyntaxKind ModuleDeclaration Int where
  runtimeRep = unsafeCoerce moduleDeclaration
  enumRep = moduleDeclaration

instance EnumRep SyntaxKind ModuleBlock Int where
  runtimeRep = unsafeCoerce moduleBlock
  enumRep = moduleBlock

instance EnumRep SyntaxKind CaseBlock Int where
  runtimeRep = unsafeCoerce caseBlock
  enumRep = caseBlock

instance EnumRep SyntaxKind NamespaceExportDeclaration Int where
  runtimeRep = unsafeCoerce namespaceExportDeclaration
  enumRep = namespaceExportDeclaration

instance EnumRep SyntaxKind ImportEqualsDeclaration Int where
  runtimeRep = unsafeCoerce importEqualsDeclaration
  enumRep = importEqualsDeclaration

instance EnumRep SyntaxKind ImportDeclaration Int where
  runtimeRep = unsafeCoerce importDeclaration
  enumRep = importDeclaration

instance EnumRep SyntaxKind ImportClause Int where
  runtimeRep = unsafeCoerce importClause
  enumRep = importClause

instance EnumRep SyntaxKind NamespaceImport Int where
  runtimeRep = unsafeCoerce namespaceImport
  enumRep = namespaceImport

instance EnumRep SyntaxKind NamedImports Int where
  runtimeRep = unsafeCoerce namedImports
  enumRep = namedImports

instance EnumRep SyntaxKind ImportSpecifier Int where
  runtimeRep = unsafeCoerce importSpecifier
  enumRep = importSpecifier

instance EnumRep SyntaxKind ExportAssignment Int where
  runtimeRep = unsafeCoerce exportAssignment
  enumRep = exportAssignment

instance EnumRep SyntaxKind ExportDeclaration Int where
  runtimeRep = unsafeCoerce exportDeclaration
  enumRep = exportDeclaration

instance EnumRep SyntaxKind NamedExports Int where
  runtimeRep = unsafeCoerce namedExports
  enumRep = namedExports

instance EnumRep SyntaxKind NamespaceExport Int where
  runtimeRep = unsafeCoerce namespaceExport
  enumRep = namespaceExport

instance EnumRep SyntaxKind ExportSpecifier Int where
  runtimeRep = unsafeCoerce exportSpecifier
  enumRep = exportSpecifier

instance EnumRep SyntaxKind MissingDeclaration Int where
  runtimeRep = unsafeCoerce missingDeclaration
  enumRep = missingDeclaration

instance EnumRep SyntaxKind ExternalModuleReference Int where
  runtimeRep = unsafeCoerce externalModuleReference
  enumRep = externalModuleReference

instance EnumRep SyntaxKind JsxElement Int where
  runtimeRep = unsafeCoerce jsxElement
  enumRep = jsxElement

instance EnumRep SyntaxKind JsxSelfClosingElement Int where
  runtimeRep = unsafeCoerce jsxSelfClosingElement
  enumRep = jsxSelfClosingElement

instance EnumRep SyntaxKind JsxOpeningElement Int where
  runtimeRep = unsafeCoerce jsxOpeningElement
  enumRep = jsxOpeningElement

instance EnumRep SyntaxKind JsxClosingElement Int where
  runtimeRep = unsafeCoerce jsxClosingElement
  enumRep = jsxClosingElement

instance EnumRep SyntaxKind JsxFragment Int where
  runtimeRep = unsafeCoerce jsxFragment
  enumRep = jsxFragment

instance EnumRep SyntaxKind JsxOpeningFragment Int where
  runtimeRep = unsafeCoerce jsxOpeningFragment
  enumRep = jsxOpeningFragment

instance EnumRep SyntaxKind JsxClosingFragment Int where
  runtimeRep = unsafeCoerce jsxClosingFragment
  enumRep = jsxClosingFragment

instance EnumRep SyntaxKind JsxAttribute Int where
  runtimeRep = unsafeCoerce jsxAttribute
  enumRep = jsxAttribute

instance EnumRep SyntaxKind JsxAttributes Int where
  runtimeRep = unsafeCoerce jsxAttributes
  enumRep = jsxAttributes

instance EnumRep SyntaxKind JsxSpreadAttribute Int where
  runtimeRep = unsafeCoerce jsxSpreadAttribute
  enumRep = jsxSpreadAttribute

instance EnumRep SyntaxKind JsxExpression Int where
  runtimeRep = unsafeCoerce jsxExpression
  enumRep = jsxExpression

instance EnumRep SyntaxKind CaseClause Int where
  runtimeRep = unsafeCoerce caseClause
  enumRep = caseClause

instance EnumRep SyntaxKind DefaultClause Int where
  runtimeRep = unsafeCoerce defaultClause
  enumRep = defaultClause

instance EnumRep SyntaxKind HeritageClause Int where
  runtimeRep = unsafeCoerce heritageClause
  enumRep = heritageClause

instance EnumRep SyntaxKind CatchClause Int where
  runtimeRep = unsafeCoerce catchClause
  enumRep = catchClause

instance EnumRep SyntaxKind PropertyAssignment Int where
  runtimeRep = unsafeCoerce propertyAssignment
  enumRep = propertyAssignment

instance EnumRep SyntaxKind ShorthandPropertyAssignment Int where
  runtimeRep = unsafeCoerce shorthandPropertyAssignment
  enumRep = shorthandPropertyAssignment

instance EnumRep SyntaxKind SpreadAssignment Int where
  runtimeRep = unsafeCoerce spreadAssignment
  enumRep = spreadAssignment

instance EnumRep SyntaxKind EnumMember Int where
  runtimeRep = unsafeCoerce enumMember
  enumRep = enumMember

instance EnumRep SyntaxKind UnparsedPrologue Int where
  runtimeRep = unsafeCoerce unparsedPrologue
  enumRep = unparsedPrologue

instance EnumRep SyntaxKind UnparsedPrepend Int where
  runtimeRep = unsafeCoerce unparsedPrepend
  enumRep = unparsedPrepend

instance EnumRep SyntaxKind UnparsedText Int where
  runtimeRep = unsafeCoerce unparsedText
  enumRep = unparsedText

instance EnumRep SyntaxKind UnparsedInternalText Int where
  runtimeRep = unsafeCoerce unparsedInternalText
  enumRep = unparsedInternalText

instance EnumRep SyntaxKind UnparsedSyntheticReference Int where
  runtimeRep = unsafeCoerce unparsedSyntheticReference
  enumRep = unparsedSyntheticReference

instance EnumRep SyntaxKind SourceFile Int where
  runtimeRep = unsafeCoerce sourceFile
  enumRep = sourceFile

instance EnumRep SyntaxKind Bundle Int where
  runtimeRep = unsafeCoerce bundle
  enumRep = bundle

instance EnumRep SyntaxKind UnparsedSource Int where
  runtimeRep = unsafeCoerce unparsedSource
  enumRep = unparsedSource

instance EnumRep SyntaxKind InputFiles Int where
  runtimeRep = unsafeCoerce inputFiles
  enumRep = inputFiles

instance EnumRep SyntaxKind JSDocTypeExpression Int where
  runtimeRep = unsafeCoerce jSDocTypeExpression
  enumRep = jSDocTypeExpression

instance EnumRep SyntaxKind JSDocNameReference Int where
  runtimeRep = unsafeCoerce jSDocNameReference
  enumRep = jSDocNameReference

instance EnumRep SyntaxKind JSDocMemberName Int where
  runtimeRep = unsafeCoerce jSDocMemberName
  enumRep = jSDocMemberName

instance EnumRep SyntaxKind JSDocAllType Int where
  runtimeRep = unsafeCoerce jSDocAllType
  enumRep = jSDocAllType

instance EnumRep SyntaxKind JSDocUnknownType Int where
  runtimeRep = unsafeCoerce jSDocUnknownType
  enumRep = jSDocUnknownType

instance EnumRep SyntaxKind JSDocNullableType Int where
  runtimeRep = unsafeCoerce jSDocNullableType
  enumRep = jSDocNullableType

instance EnumRep SyntaxKind JSDocNonNullableType Int where
  runtimeRep = unsafeCoerce jSDocNonNullableType
  enumRep = jSDocNonNullableType

instance EnumRep SyntaxKind JSDocOptionalType Int where
  runtimeRep = unsafeCoerce jSDocOptionalType
  enumRep = jSDocOptionalType

instance EnumRep SyntaxKind JSDocFunctionType Int where
  runtimeRep = unsafeCoerce jSDocFunctionType
  enumRep = jSDocFunctionType

instance EnumRep SyntaxKind JSDocVariadicType Int where
  runtimeRep = unsafeCoerce jSDocVariadicType
  enumRep = jSDocVariadicType

instance EnumRep SyntaxKind JSDocNamepathType Int where
  runtimeRep = unsafeCoerce jSDocNamepathType
  enumRep = jSDocNamepathType

instance EnumRep SyntaxKind JSDocComment Int where
  runtimeRep = unsafeCoerce jSDocComment
  enumRep = jSDocComment

instance EnumRep SyntaxKind JSDocText Int where
  runtimeRep = unsafeCoerce jSDocText
  enumRep = jSDocText

instance EnumRep SyntaxKind JSDocTypeLiteral Int where
  runtimeRep = unsafeCoerce jSDocTypeLiteral
  enumRep = jSDocTypeLiteral

instance EnumRep SyntaxKind JSDocSignature Int where
  runtimeRep = unsafeCoerce jSDocSignature
  enumRep = jSDocSignature

instance EnumRep SyntaxKind JSDocLink Int where
  runtimeRep = unsafeCoerce jSDocLink
  enumRep = jSDocLink

instance EnumRep SyntaxKind JSDocLinkCode Int where
  runtimeRep = unsafeCoerce jSDocLinkCode
  enumRep = jSDocLinkCode

instance EnumRep SyntaxKind JSDocLinkPlain Int where
  runtimeRep = unsafeCoerce jSDocLinkPlain
  enumRep = jSDocLinkPlain

instance EnumRep SyntaxKind JSDocTag Int where
  runtimeRep = unsafeCoerce jSDocTag
  enumRep = jSDocTag

instance EnumRep SyntaxKind JSDocAugmentsTag Int where
  runtimeRep = unsafeCoerce jSDocAugmentsTag
  enumRep = jSDocAugmentsTag

instance EnumRep SyntaxKind JSDocImplementsTag Int where
  runtimeRep = unsafeCoerce jSDocImplementsTag
  enumRep = jSDocImplementsTag

instance EnumRep SyntaxKind JSDocAuthorTag Int where
  runtimeRep = unsafeCoerce jSDocAuthorTag
  enumRep = jSDocAuthorTag

instance EnumRep SyntaxKind JSDocDeprecatedTag Int where
  runtimeRep = unsafeCoerce jSDocDeprecatedTag
  enumRep = jSDocDeprecatedTag

instance EnumRep SyntaxKind JSDocClassTag Int where
  runtimeRep = unsafeCoerce jSDocClassTag
  enumRep = jSDocClassTag

instance EnumRep SyntaxKind JSDocPublicTag Int where
  runtimeRep = unsafeCoerce jSDocPublicTag
  enumRep = jSDocPublicTag

instance EnumRep SyntaxKind JSDocPrivateTag Int where
  runtimeRep = unsafeCoerce jSDocPrivateTag
  enumRep = jSDocPrivateTag

instance EnumRep SyntaxKind JSDocProtectedTag Int where
  runtimeRep = unsafeCoerce jSDocProtectedTag
  enumRep = jSDocProtectedTag

instance EnumRep SyntaxKind JSDocReadonlyTag Int where
  runtimeRep = unsafeCoerce jSDocReadonlyTag
  enumRep = jSDocReadonlyTag

instance EnumRep SyntaxKind JSDocOverrideTag Int where
  runtimeRep = unsafeCoerce jSDocOverrideTag
  enumRep = jSDocOverrideTag

instance EnumRep SyntaxKind JSDocCallbackTag Int where
  runtimeRep = unsafeCoerce jSDocCallbackTag
  enumRep = jSDocCallbackTag

instance EnumRep SyntaxKind JSDocEnumTag Int where
  runtimeRep = unsafeCoerce jSDocEnumTag
  enumRep = jSDocEnumTag

instance EnumRep SyntaxKind JSDocParameterTag Int where
  runtimeRep = unsafeCoerce jSDocParameterTag
  enumRep = jSDocParameterTag

instance EnumRep SyntaxKind JSDocReturnTag Int where
  runtimeRep = unsafeCoerce jSDocReturnTag
  enumRep = jSDocReturnTag

instance EnumRep SyntaxKind JSDocThisTag Int where
  runtimeRep = unsafeCoerce jSDocThisTag
  enumRep = jSDocThisTag

instance EnumRep SyntaxKind JSDocTypeTag Int where
  runtimeRep = unsafeCoerce jSDocTypeTag
  enumRep = jSDocTypeTag

instance EnumRep SyntaxKind JSDocTemplateTag Int where
  runtimeRep = unsafeCoerce jSDocTemplateTag
  enumRep = jSDocTemplateTag

instance EnumRep SyntaxKind JSDocTypedefTag Int where
  runtimeRep = unsafeCoerce jSDocTypedefTag
  enumRep = jSDocTypedefTag

instance EnumRep SyntaxKind JSDocSeeTag Int where
  runtimeRep = unsafeCoerce jSDocSeeTag
  enumRep = jSDocSeeTag

instance EnumRep SyntaxKind JSDocPropertyTag Int where
  runtimeRep = unsafeCoerce jSDocPropertyTag
  enumRep = jSDocPropertyTag

instance EnumRep SyntaxKind SyntaxList Int where
  runtimeRep = unsafeCoerce syntaxList
  enumRep = syntaxList

instance EnumRep SyntaxKind NotEmittedStatement Int where
  runtimeRep = unsafeCoerce notEmittedStatement
  enumRep = notEmittedStatement

instance EnumRep SyntaxKind PartiallyEmittedExpression Int where
  runtimeRep = unsafeCoerce partiallyEmittedExpression
  enumRep = partiallyEmittedExpression

instance EnumRep SyntaxKind CommaListExpression Int where
  runtimeRep = unsafeCoerce commaListExpression
  enumRep = commaListExpression

instance EnumRep SyntaxKind MergeDeclarationMarker Int where
  runtimeRep = unsafeCoerce mergeDeclarationMarker
  enumRep = mergeDeclarationMarker

instance EnumRep SyntaxKind EndOfDeclarationMarker Int where
  runtimeRep = unsafeCoerce endOfDeclarationMarker
  enumRep = endOfDeclarationMarker

instance EnumRep SyntaxKind SyntheticReferenceExpression Int where
  runtimeRep = unsafeCoerce syntheticReferenceExpression
  enumRep = syntheticReferenceExpression

instance EnumRep SyntaxKind Count Int where
  runtimeRep = unsafeCoerce count
  enumRep = count

instance EnumRep SyntaxKind FirstAssignment Int where
  runtimeRep = unsafeCoerce firstAssignment
  enumRep = firstAssignment

instance EnumRep SyntaxKind LastAssignment Int where
  runtimeRep = unsafeCoerce lastAssignment
  enumRep = lastAssignment

instance EnumRep SyntaxKind FirstCompoundAssignment Int where
  runtimeRep = unsafeCoerce firstCompoundAssignment
  enumRep = firstCompoundAssignment

instance EnumRep SyntaxKind LastCompoundAssignment Int where
  runtimeRep = unsafeCoerce lastCompoundAssignment
  enumRep = lastCompoundAssignment

instance EnumRep SyntaxKind FirstReservedWord Int where
  runtimeRep = unsafeCoerce firstReservedWord
  enumRep = firstReservedWord

instance EnumRep SyntaxKind LastReservedWord Int where
  runtimeRep = unsafeCoerce lastReservedWord
  enumRep = lastReservedWord

instance EnumRep SyntaxKind FirstKeyword Int where
  runtimeRep = unsafeCoerce firstKeyword
  enumRep = firstKeyword

instance EnumRep SyntaxKind LastKeyword Int where
  runtimeRep = unsafeCoerce lastKeyword
  enumRep = lastKeyword

instance EnumRep SyntaxKind FirstFutureReservedWord Int where
  runtimeRep = unsafeCoerce firstFutureReservedWord
  enumRep = firstFutureReservedWord

instance EnumRep SyntaxKind LastFutureReservedWord Int where
  runtimeRep = unsafeCoerce lastFutureReservedWord
  enumRep = lastFutureReservedWord

instance EnumRep SyntaxKind FirstTypeNode Int where
  runtimeRep = unsafeCoerce firstTypeNode
  enumRep = firstTypeNode

instance EnumRep SyntaxKind LastTypeNode Int where
  runtimeRep = unsafeCoerce lastTypeNode
  enumRep = lastTypeNode

instance EnumRep SyntaxKind FirstPunctuation Int where
  runtimeRep = unsafeCoerce firstPunctuation
  enumRep = firstPunctuation

instance EnumRep SyntaxKind LastPunctuation Int where
  runtimeRep = unsafeCoerce lastPunctuation
  enumRep = lastPunctuation

instance EnumRep SyntaxKind FirstToken Int where
  runtimeRep = unsafeCoerce firstToken
  enumRep = firstToken

instance EnumRep SyntaxKind LastToken Int where
  runtimeRep = unsafeCoerce lastToken
  enumRep = lastToken

instance EnumRep SyntaxKind FirstTriviaToken Int where
  runtimeRep = unsafeCoerce firstTriviaToken
  enumRep = firstTriviaToken

instance EnumRep SyntaxKind LastTriviaToken Int where
  runtimeRep = unsafeCoerce lastTriviaToken
  enumRep = lastTriviaToken

instance EnumRep SyntaxKind FirstLiteralToken Int where
  runtimeRep = unsafeCoerce firstLiteralToken
  enumRep = firstLiteralToken

instance EnumRep SyntaxKind LastLiteralToken Int where
  runtimeRep = unsafeCoerce lastLiteralToken
  enumRep = lastLiteralToken

instance EnumRep SyntaxKind FirstTemplateToken Int where
  runtimeRep = unsafeCoerce firstTemplateToken
  enumRep = firstTemplateToken

instance EnumRep SyntaxKind LastTemplateToken Int where
  runtimeRep = unsafeCoerce lastTemplateToken
  enumRep = lastTemplateToken

instance EnumRep SyntaxKind FirstBinaryOperator Int where
  runtimeRep = unsafeCoerce firstBinaryOperator
  enumRep = firstBinaryOperator

instance EnumRep SyntaxKind LastBinaryOperator Int where
  runtimeRep = unsafeCoerce lastBinaryOperator
  enumRep = lastBinaryOperator

instance EnumRep SyntaxKind FirstStatement Int where
  runtimeRep = unsafeCoerce firstStatement
  enumRep = firstStatement

instance EnumRep SyntaxKind LastStatement Int where
  runtimeRep = unsafeCoerce lastStatement
  enumRep = lastStatement

instance EnumRep SyntaxKind FirstNode Int where
  runtimeRep = unsafeCoerce firstNode
  enumRep = firstNode

instance EnumRep SyntaxKind FirstJSDocNode Int where
  runtimeRep = unsafeCoerce firstJSDocNode
  enumRep = firstJSDocNode

instance EnumRep SyntaxKind LastJSDocNode Int where
  runtimeRep = unsafeCoerce lastJSDocNode
  enumRep = lastJSDocNode

instance EnumRep SyntaxKind FirstJSDocTagNode Int where
  runtimeRep = unsafeCoerce firstJSDocTagNode
  enumRep = firstJSDocTagNode

instance EnumRep SyntaxKind LastJSDocTagNode Int where
  runtimeRep = unsafeCoerce lastJSDocTagNode
  enumRep = lastJSDocTagNode
