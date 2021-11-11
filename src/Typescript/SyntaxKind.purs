module Typescript.SyntaxKind where

import Type.Data.List (type (:>), Nil')
import Typescript.Utils.Enum (class EnumRep)

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

type SyntaxKindTList =
  Unknown
    :> EndOfFileToken
    :> SingleLineCommentTrivia
    :> MultiLineCommentTrivia
    :> NewLineTrivia
    :> WhitespaceTrivia
    :> ShebangTrivia
    :> ConflictMarkerTrivia
    :> NumericLiteral
    :> BigIntLiteral
    :> StringLiteral
    :> JsxText
    :> JsxTextAllWhiteSpaces
    :> RegularExpressionLiteral
    :> NoSubstitutionTemplateLiteral
    :> TemplateHead
    :> TemplateMiddle
    :> TemplateTail
    :> OpenBraceToken
    :> CloseBraceToken
    :> OpenParenToken
    :> CloseParenToken
    :> OpenBracketToken
    :> CloseBracketToken
    :> DotToken
    :> DotDotDotToken
    :> SemicolonToken
    :> CommaToken
    :> QuestionDotToken
    :> LessThanToken
    :> LessThanSlashToken
    :> GreaterThanToken
    :> LessThanEqualsToken
    :> GreaterThanEqualsToken
    :> EqualsEqualsToken
    :> ExclamationEqualsToken
    :> EqualsEqualsEqualsToken
    :> ExclamationEqualsEqualsToken
    :> EqualsGreaterThanToken
    :> PlusToken
    :> MinusToken
    :> AsteriskToken
    :> AsteriskAsteriskToken
    :> SlashToken
    :> PercentToken
    :> PlusPlusToken
    :> MinusMinusToken
    :> LessThanLessThanToken
    :> GreaterThanGreaterThanToken
    :> GreaterThanGreaterThanGreaterThanToken
    :> AmpersandToken
    :> BarToken
    :> CaretToken
    :> ExclamationToken
    :> TildeToken
    :> AmpersandAmpersandToken
    :> BarBarToken
    :> QuestionToken
    :> ColonToken
    :> AtToken
    :> QuestionQuestionToken
    :> BacktickToken
    :> HashToken
    :> EqualsToken
    :> PlusEqualsToken
    :> MinusEqualsToken
    :> AsteriskEqualsToken
    :> AsteriskAsteriskEqualsToken
    :> SlashEqualsToken
    :> PercentEqualsToken
    :> LessThanLessThanEqualsToken
    :> GreaterThanGreaterThanEqualsToken
    :> GreaterThanGreaterThanGreaterThanEqualsToken
    :> AmpersandEqualsToken
    :> BarEqualsToken
    :> BarBarEqualsToken
    :> AmpersandAmpersandEqualsToken
    :> QuestionQuestionEqualsToken
    :> CaretEqualsToken
    :> Identifier
    :> PrivateIdentifier
    :> BreakKeyword
    :> CaseKeyword
    :> CatchKeyword
    :> ClassKeyword
    :> ConstKeyword
    :> ContinueKeyword
    :> DebuggerKeyword
    :> DefaultKeyword
    :> DeleteKeyword
    :> DoKeyword
    :> ElseKeyword
    :> EnumKeyword
    :> ExportKeyword
    :> ExtendsKeyword
    :> FalseKeyword
    :> FinallyKeyword
    :> ForKeyword
    :> FunctionKeyword
    :> IfKeyword
    :> ImportKeyword
    :> InKeyword
    :> InstanceOfKeyword
    :> NewKeyword
    :> NullKeyword
    :> ReturnKeyword
    :> SuperKeyword
    :> SwitchKeyword
    :> ThisKeyword
    :> ThrowKeyword
    :> TrueKeyword
    :> TryKeyword
    :> TypeOfKeyword
    :> VarKeyword
    :> VoidKeyword
    :> WhileKeyword
    :> WithKeyword
    :> ImplementsKeyword
    :> InterfaceKeyword
    :> LetKeyword
    :> PackageKeyword
    :> PrivateKeyword
    :> ProtectedKeyword
    :> PublicKeyword
    :> StaticKeyword
    :> YieldKeyword
    :> AbstractKeyword
    :> AsKeyword
    :> AssertsKeyword
    :> AnyKeyword
    :> AsyncKeyword
    :> AwaitKeyword
    :> BooleanKeyword
    :> ConstructorKeyword
    :> DeclareKeyword
    :> GetKeyword
    :> InferKeyword
    :> IntrinsicKeyword
    :> IsKeyword
    :> KeyOfKeyword
    :> ModuleKeyword
    :> NamespaceKeyword
    :> NeverKeyword
    :> ReadonlyKeyword
    :> RequireKeyword
    :> NumberKeyword
    :> ObjectKeyword
    :> SetKeyword
    :> StringKeyword
    :> SymbolKeyword
    :> TypeKeyword
    :> UndefinedKeyword
    :> UniqueKeyword
    :> UnknownKeyword
    :> FromKeyword
    :> GlobalKeyword
    :> BigIntKeyword
    :> OverrideKeyword
    :> OfKeyword
    :> QualifiedName
    :> ComputedPropertyName
    :> TypeParameter
    :> Parameter
    :> Decorator
    :> PropertySignature
    :> PropertyDeclaration
    :> MethodSignature
    :> MethodDeclaration
    :> ClassStaticBlockDeclaration
    :> Constructor
    :> GetAccessor
    :> SetAccessor
    :> CallSignature
    :> ConstructSignature
    :> IndexSignature
    :> TypePredicate
    :> TypeReference
    :> FunctionType
    :> ConstructorType
    :> TypeQuery
    :> TypeLiteral
    :> ArrayType
    :> TupleType
    :> OptionalType
    :> RestType
    :> UnionType
    :> IntersectionType
    :> ConditionalType
    :> InferType
    :> ParenthesizedType
    :> ThisType
    :> TypeOperator
    :> IndexedAccessType
    :> MappedType
    :> LiteralType
    :> NamedTupleMember
    :> TemplateLiteralType
    :> TemplateLiteralTypeSpan
    :> ImportType
    :> ObjectBindingPattern
    :> ArrayBindingPattern
    :> BindingElement
    :> ArrayLiteralExpression
    :> ObjectLiteralExpression
    :> PropertyAccessExpression
    :> ElementAccessExpression
    :> CallExpression
    :> NewExpression
    :> TaggedTemplateExpression
    :> TypeAssertionExpression
    :> ParenthesizedExpression
    :> FunctionExpression
    :> ArrowFunction
    :> DeleteExpression
    :> TypeOfExpression
    :> VoidExpression
    :> AwaitExpression
    :> PrefixUnaryExpression
    :> PostfixUnaryExpression
    :> BinaryExpression
    :> ConditionalExpression
    :> TemplateExpression
    :> YieldExpression
    :> SpreadElement
    :> ClassExpression
    :> OmittedExpression
    :> ExpressionWithTypeArguments
    :> AsExpression
    :> NonNullExpression
    :> MetaProperty
    :> SyntheticExpression
    :> TemplateSpan
    :> SemicolonClassElement
    :> Block
    :> EmptyStatement
    :> VariableStatement
    :> ExpressionStatement
    :> IfStatement
    :> DoStatement
    :> WhileStatement
    :> ForStatement
    :> ForInStatement
    :> ForOfStatement
    :> ContinueStatement
    :> BreakStatement
    :> ReturnStatement
    :> WithStatement
    :> SwitchStatement
    :> LabeledStatement
    :> ThrowStatement
    :> TryStatement
    :> DebuggerStatement
    :> VariableDeclaration
    :> VariableDeclarationList
    :> FunctionDeclaration
    :> ClassDeclaration
    :> InterfaceDeclaration
    :> TypeAliasDeclaration
    :> EnumDeclaration
    :> ModuleDeclaration
    :> ModuleBlock
    :> CaseBlock
    :> NamespaceExportDeclaration
    :> ImportEqualsDeclaration
    :> ImportDeclaration
    :> ImportClause
    :> NamespaceImport
    :> NamedImports
    :> ImportSpecifier
    :> ExportAssignment
    :> ExportDeclaration
    :> NamedExports
    :> NamespaceExport
    :> ExportSpecifier
    :> MissingDeclaration
    :> ExternalModuleReference
    :> JsxElement
    :> JsxSelfClosingElement
    :> JsxOpeningElement
    :> JsxClosingElement
    :> JsxFragment
    :> JsxOpeningFragment
    :> JsxClosingFragment
    :> JsxAttribute
    :> JsxAttributes
    :> JsxSpreadAttribute
    :> JsxExpression
    :> CaseClause
    :> DefaultClause
    :> HeritageClause
    :> CatchClause
    :> PropertyAssignment
    :> ShorthandPropertyAssignment
    :> SpreadAssignment
    :> EnumMember
    :> UnparsedPrologue
    :> UnparsedPrepend
    :> UnparsedText
    :> UnparsedInternalText
    :> UnparsedSyntheticReference
    :> SourceFile
    :> Bundle
    :> UnparsedSource
    :> InputFiles
    :> JSDocTypeExpression
    :> JSDocNameReference
    :> JSDocMemberName
    :> JSDocAllType
    :> JSDocUnknownType
    :> JSDocNullableType
    :> JSDocNonNullableType
    :> JSDocOptionalType
    :> JSDocFunctionType
    :> JSDocVariadicType
    :> JSDocNamepathType
    :> JSDocComment
    :> JSDocText
    :> JSDocTypeLiteral
    :> JSDocSignature
    :> JSDocLink
    :> JSDocLinkCode
    :> JSDocLinkPlain
    :> JSDocTag
    :> JSDocAugmentsTag
    :> JSDocImplementsTag
    :> JSDocAuthorTag
    :> JSDocDeprecatedTag
    :> JSDocClassTag
    :> JSDocPublicTag
    :> JSDocPrivateTag
    :> JSDocProtectedTag
    :> JSDocReadonlyTag
    :> JSDocOverrideTag
    :> JSDocCallbackTag
    :> JSDocEnumTag
    :> JSDocParameterTag
    :> JSDocReturnTag
    :> JSDocThisTag
    :> JSDocTypeTag
    :> JSDocTemplateTag
    :> JSDocTypedefTag
    :> JSDocSeeTag
    :> JSDocPropertyTag
    :> SyntaxList
    :> NotEmittedStatement
    :> PartiallyEmittedExpression
    :> CommaListExpression
    :> MergeDeclarationMarker
    :> EndOfDeclarationMarker
    :> SyntheticReferenceExpression
    :> Count
    :> FirstAssignment
    :> LastAssignment
    :> FirstCompoundAssignment
    :> LastCompoundAssignment
    :> FirstReservedWord
    :> LastReservedWord
    :> FirstKeyword
    :> LastKeyword
    :> FirstFutureReservedWord
    :> LastFutureReservedWord
    :> FirstTypeNode
    :> LastTypeNode
    :> FirstPunctuation
    :> LastPunctuation
    :> FirstToken
    :> LastToken
    :> FirstTriviaToken
    :> LastTriviaToken
    :> FirstLiteralToken
    :> LastLiteralToken
    :> FirstTemplateToken
    :> LastTemplateToken
    :> FirstBinaryOperator
    :> LastBinaryOperator
    :> FirstStatement
    :> LastStatement
    :> FirstNode
    :> FirstJSDocNode
    :> LastJSDocNode
    :> FirstJSDocTagNode
    :> LastJSDocTagNode
    :>
      Nil'

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

instance EnumRep SyntaxKind Unknown where
  enumRep = unknown

instance EnumRep SyntaxKind EndOfFileToken where
  enumRep = endOfFileToken

instance EnumRep SyntaxKind SingleLineCommentTrivia where
  enumRep = singleLineCommentTrivia

instance EnumRep SyntaxKind MultiLineCommentTrivia where
  enumRep = multiLineCommentTrivia

instance EnumRep SyntaxKind NewLineTrivia where
  enumRep = newLineTrivia

instance EnumRep SyntaxKind WhitespaceTrivia where
  enumRep = whitespaceTrivia

instance EnumRep SyntaxKind ShebangTrivia where
  enumRep = shebangTrivia

instance EnumRep SyntaxKind ConflictMarkerTrivia where
  enumRep = conflictMarkerTrivia

instance EnumRep SyntaxKind NumericLiteral where
  enumRep = numericLiteral

instance EnumRep SyntaxKind BigIntLiteral where
  enumRep = bigIntLiteral

instance EnumRep SyntaxKind StringLiteral where
  enumRep = stringLiteral

instance EnumRep SyntaxKind JsxText where
  enumRep = jsxText

instance EnumRep SyntaxKind JsxTextAllWhiteSpaces where
  enumRep = jsxTextAllWhiteSpaces

instance EnumRep SyntaxKind RegularExpressionLiteral where
  enumRep = regularExpressionLiteral

instance EnumRep SyntaxKind NoSubstitutionTemplateLiteral where
  enumRep = noSubstitutionTemplateLiteral

instance EnumRep SyntaxKind TemplateHead where
  enumRep = templateHead

instance EnumRep SyntaxKind TemplateMiddle where
  enumRep = templateMiddle

instance EnumRep SyntaxKind TemplateTail where
  enumRep = templateTail

instance EnumRep SyntaxKind OpenBraceToken where
  enumRep = openBraceToken

instance EnumRep SyntaxKind CloseBraceToken where
  enumRep = closeBraceToken

instance EnumRep SyntaxKind OpenParenToken where
  enumRep = openParenToken

instance EnumRep SyntaxKind CloseParenToken where
  enumRep = closeParenToken

instance EnumRep SyntaxKind OpenBracketToken where
  enumRep = openBracketToken

instance EnumRep SyntaxKind CloseBracketToken where
  enumRep = closeBracketToken

instance EnumRep SyntaxKind DotToken where
  enumRep = dotToken

instance EnumRep SyntaxKind DotDotDotToken where
  enumRep = dotDotDotToken

instance EnumRep SyntaxKind SemicolonToken where
  enumRep = semicolonToken

instance EnumRep SyntaxKind CommaToken where
  enumRep = commaToken

instance EnumRep SyntaxKind QuestionDotToken where
  enumRep = questionDotToken

instance EnumRep SyntaxKind LessThanToken where
  enumRep = lessThanToken

instance EnumRep SyntaxKind LessThanSlashToken where
  enumRep = lessThanSlashToken

instance EnumRep SyntaxKind GreaterThanToken where
  enumRep = greaterThanToken

instance EnumRep SyntaxKind LessThanEqualsToken where
  enumRep = lessThanEqualsToken

instance EnumRep SyntaxKind GreaterThanEqualsToken where
  enumRep = greaterThanEqualsToken

instance EnumRep SyntaxKind EqualsEqualsToken where
  enumRep = equalsEqualsToken

instance EnumRep SyntaxKind ExclamationEqualsToken where
  enumRep = exclamationEqualsToken

instance EnumRep SyntaxKind EqualsEqualsEqualsToken where
  enumRep = equalsEqualsEqualsToken

instance EnumRep SyntaxKind ExclamationEqualsEqualsToken where
  enumRep = exclamationEqualsEqualsToken

instance EnumRep SyntaxKind EqualsGreaterThanToken where
  enumRep = equalsGreaterThanToken

instance EnumRep SyntaxKind PlusToken where
  enumRep = plusToken

instance EnumRep SyntaxKind MinusToken where
  enumRep = minusToken

instance EnumRep SyntaxKind AsteriskToken where
  enumRep = asteriskToken

instance EnumRep SyntaxKind AsteriskAsteriskToken where
  enumRep = asteriskAsteriskToken

instance EnumRep SyntaxKind SlashToken where
  enumRep = slashToken

instance EnumRep SyntaxKind PercentToken where
  enumRep = percentToken

instance EnumRep SyntaxKind PlusPlusToken where
  enumRep = plusPlusToken

instance EnumRep SyntaxKind MinusMinusToken where
  enumRep = minusMinusToken

instance EnumRep SyntaxKind LessThanLessThanToken where
  enumRep = lessThanLessThanToken

instance EnumRep SyntaxKind GreaterThanGreaterThanToken where
  enumRep = greaterThanGreaterThanToken

instance EnumRep SyntaxKind GreaterThanGreaterThanGreaterThanToken where
  enumRep = greaterThanGreaterThanGreaterThanToken

instance EnumRep SyntaxKind AmpersandToken where
  enumRep = ampersandToken

instance EnumRep SyntaxKind BarToken where
  enumRep = barToken

instance EnumRep SyntaxKind CaretToken where
  enumRep = caretToken

instance EnumRep SyntaxKind ExclamationToken where
  enumRep = exclamationToken

instance EnumRep SyntaxKind TildeToken where
  enumRep = tildeToken

instance EnumRep SyntaxKind AmpersandAmpersandToken where
  enumRep = ampersandAmpersandToken

instance EnumRep SyntaxKind BarBarToken where
  enumRep = barBarToken

instance EnumRep SyntaxKind QuestionToken where
  enumRep = questionToken

instance EnumRep SyntaxKind ColonToken where
  enumRep = colonToken

instance EnumRep SyntaxKind AtToken where
  enumRep = atToken

instance EnumRep SyntaxKind QuestionQuestionToken where
  enumRep = questionQuestionToken

instance EnumRep SyntaxKind BacktickToken where
  enumRep = backtickToken

instance EnumRep SyntaxKind HashToken where
  enumRep = hashToken

instance EnumRep SyntaxKind EqualsToken where
  enumRep = equalsToken

instance EnumRep SyntaxKind PlusEqualsToken where
  enumRep = plusEqualsToken

instance EnumRep SyntaxKind MinusEqualsToken where
  enumRep = minusEqualsToken

instance EnumRep SyntaxKind AsteriskEqualsToken where
  enumRep = asteriskEqualsToken

instance EnumRep SyntaxKind AsteriskAsteriskEqualsToken where
  enumRep = asteriskAsteriskEqualsToken

instance EnumRep SyntaxKind SlashEqualsToken where
  enumRep = slashEqualsToken

instance EnumRep SyntaxKind PercentEqualsToken where
  enumRep = percentEqualsToken

instance EnumRep SyntaxKind LessThanLessThanEqualsToken where
  enumRep = lessThanLessThanEqualsToken

instance EnumRep SyntaxKind GreaterThanGreaterThanEqualsToken where
  enumRep = greaterThanGreaterThanEqualsToken

instance EnumRep SyntaxKind GreaterThanGreaterThanGreaterThanEqualsToken where
  enumRep = greaterThanGreaterThanGreaterThanEqualsToken

instance EnumRep SyntaxKind AmpersandEqualsToken where
  enumRep = ampersandEqualsToken

instance EnumRep SyntaxKind BarEqualsToken where
  enumRep = barEqualsToken

instance EnumRep SyntaxKind BarBarEqualsToken where
  enumRep = barBarEqualsToken

instance EnumRep SyntaxKind AmpersandAmpersandEqualsToken where
  enumRep = ampersandAmpersandEqualsToken

instance EnumRep SyntaxKind QuestionQuestionEqualsToken where
  enumRep = questionQuestionEqualsToken

instance EnumRep SyntaxKind CaretEqualsToken where
  enumRep = caretEqualsToken

instance EnumRep SyntaxKind Identifier where
  enumRep = identifier

instance EnumRep SyntaxKind PrivateIdentifier where
  enumRep = privateIdentifier

instance EnumRep SyntaxKind BreakKeyword where
  enumRep = breakKeyword

instance EnumRep SyntaxKind CaseKeyword where
  enumRep = caseKeyword

instance EnumRep SyntaxKind CatchKeyword where
  enumRep = catchKeyword

instance EnumRep SyntaxKind ClassKeyword where
  enumRep = classKeyword

instance EnumRep SyntaxKind ConstKeyword where
  enumRep = constKeyword

instance EnumRep SyntaxKind ContinueKeyword where
  enumRep = continueKeyword

instance EnumRep SyntaxKind DebuggerKeyword where
  enumRep = debuggerKeyword

instance EnumRep SyntaxKind DefaultKeyword where
  enumRep = defaultKeyword

instance EnumRep SyntaxKind DeleteKeyword where
  enumRep = deleteKeyword

instance EnumRep SyntaxKind DoKeyword where
  enumRep = doKeyword

instance EnumRep SyntaxKind ElseKeyword where
  enumRep = elseKeyword

instance EnumRep SyntaxKind EnumKeyword where
  enumRep = enumKeyword

instance EnumRep SyntaxKind ExportKeyword where
  enumRep = exportKeyword

instance EnumRep SyntaxKind ExtendsKeyword where
  enumRep = extendsKeyword

instance EnumRep SyntaxKind FalseKeyword where
  enumRep = falseKeyword

instance EnumRep SyntaxKind FinallyKeyword where
  enumRep = finallyKeyword

instance EnumRep SyntaxKind ForKeyword where
  enumRep = forKeyword

instance EnumRep SyntaxKind FunctionKeyword where
  enumRep = functionKeyword

instance EnumRep SyntaxKind IfKeyword where
  enumRep = ifKeyword

instance EnumRep SyntaxKind ImportKeyword where
  enumRep = importKeyword

instance EnumRep SyntaxKind InKeyword where
  enumRep = inKeyword

instance EnumRep SyntaxKind InstanceOfKeyword where
  enumRep = instanceOfKeyword

instance EnumRep SyntaxKind NewKeyword where
  enumRep = newKeyword

instance EnumRep SyntaxKind NullKeyword where
  enumRep = nullKeyword

instance EnumRep SyntaxKind ReturnKeyword where
  enumRep = returnKeyword

instance EnumRep SyntaxKind SuperKeyword where
  enumRep = superKeyword

instance EnumRep SyntaxKind SwitchKeyword where
  enumRep = switchKeyword

instance EnumRep SyntaxKind ThisKeyword where
  enumRep = thisKeyword

instance EnumRep SyntaxKind ThrowKeyword where
  enumRep = throwKeyword

instance EnumRep SyntaxKind TrueKeyword where
  enumRep = trueKeyword

instance EnumRep SyntaxKind TryKeyword where
  enumRep = tryKeyword

instance EnumRep SyntaxKind TypeOfKeyword where
  enumRep = typeOfKeyword

instance EnumRep SyntaxKind VarKeyword where
  enumRep = varKeyword

instance EnumRep SyntaxKind VoidKeyword where
  enumRep = voidKeyword

instance EnumRep SyntaxKind WhileKeyword where
  enumRep = whileKeyword

instance EnumRep SyntaxKind WithKeyword where
  enumRep = withKeyword

instance EnumRep SyntaxKind ImplementsKeyword where
  enumRep = implementsKeyword

instance EnumRep SyntaxKind InterfaceKeyword where
  enumRep = interfaceKeyword

instance EnumRep SyntaxKind LetKeyword where
  enumRep = letKeyword

instance EnumRep SyntaxKind PackageKeyword where
  enumRep = packageKeyword

instance EnumRep SyntaxKind PrivateKeyword where
  enumRep = privateKeyword

instance EnumRep SyntaxKind ProtectedKeyword where
  enumRep = protectedKeyword

instance EnumRep SyntaxKind PublicKeyword where
  enumRep = publicKeyword

instance EnumRep SyntaxKind StaticKeyword where
  enumRep = staticKeyword

instance EnumRep SyntaxKind YieldKeyword where
  enumRep = yieldKeyword

instance EnumRep SyntaxKind AbstractKeyword where
  enumRep = abstractKeyword

instance EnumRep SyntaxKind AsKeyword where
  enumRep = asKeyword

instance EnumRep SyntaxKind AssertsKeyword where
  enumRep = assertsKeyword

instance EnumRep SyntaxKind AnyKeyword where
  enumRep = anyKeyword

instance EnumRep SyntaxKind AsyncKeyword where
  enumRep = asyncKeyword

instance EnumRep SyntaxKind AwaitKeyword where
  enumRep = awaitKeyword

instance EnumRep SyntaxKind BooleanKeyword where
  enumRep = booleanKeyword

instance EnumRep SyntaxKind ConstructorKeyword where
  enumRep = constructorKeyword

instance EnumRep SyntaxKind DeclareKeyword where
  enumRep = declareKeyword

instance EnumRep SyntaxKind GetKeyword where
  enumRep = getKeyword

instance EnumRep SyntaxKind InferKeyword where
  enumRep = inferKeyword

instance EnumRep SyntaxKind IntrinsicKeyword where
  enumRep = intrinsicKeyword

instance EnumRep SyntaxKind IsKeyword where
  enumRep = isKeyword

instance EnumRep SyntaxKind KeyOfKeyword where
  enumRep = keyOfKeyword

instance EnumRep SyntaxKind ModuleKeyword where
  enumRep = moduleKeyword

instance EnumRep SyntaxKind NamespaceKeyword where
  enumRep = namespaceKeyword

instance EnumRep SyntaxKind NeverKeyword where
  enumRep = neverKeyword

instance EnumRep SyntaxKind ReadonlyKeyword where
  enumRep = readonlyKeyword

instance EnumRep SyntaxKind RequireKeyword where
  enumRep = requireKeyword

instance EnumRep SyntaxKind NumberKeyword where
  enumRep = numberKeyword

instance EnumRep SyntaxKind ObjectKeyword where
  enumRep = objectKeyword

instance EnumRep SyntaxKind SetKeyword where
  enumRep = setKeyword

instance EnumRep SyntaxKind StringKeyword where
  enumRep = stringKeyword

instance EnumRep SyntaxKind SymbolKeyword where
  enumRep = symbolKeyword

instance EnumRep SyntaxKind TypeKeyword where
  enumRep = typeKeyword

instance EnumRep SyntaxKind UndefinedKeyword where
  enumRep = undefinedKeyword

instance EnumRep SyntaxKind UniqueKeyword where
  enumRep = uniqueKeyword

instance EnumRep SyntaxKind UnknownKeyword where
  enumRep = unknownKeyword

instance EnumRep SyntaxKind FromKeyword where
  enumRep = fromKeyword

instance EnumRep SyntaxKind GlobalKeyword where
  enumRep = globalKeyword

instance EnumRep SyntaxKind BigIntKeyword where
  enumRep = bigIntKeyword

instance EnumRep SyntaxKind OverrideKeyword where
  enumRep = overrideKeyword

instance EnumRep SyntaxKind OfKeyword where
  enumRep = ofKeyword

instance EnumRep SyntaxKind QualifiedName where
  enumRep = qualifiedName

instance EnumRep SyntaxKind ComputedPropertyName where
  enumRep = computedPropertyName

instance EnumRep SyntaxKind TypeParameter where
  enumRep = typeParameter

instance EnumRep SyntaxKind Parameter where
  enumRep = parameter

instance EnumRep SyntaxKind Decorator where
  enumRep = decorator

instance EnumRep SyntaxKind PropertySignature where
  enumRep = propertySignature

instance EnumRep SyntaxKind PropertyDeclaration where
  enumRep = propertyDeclaration

instance EnumRep SyntaxKind MethodSignature where
  enumRep = methodSignature

instance EnumRep SyntaxKind MethodDeclaration where
  enumRep = methodDeclaration

instance EnumRep SyntaxKind ClassStaticBlockDeclaration where
  enumRep = classStaticBlockDeclaration

instance EnumRep SyntaxKind Constructor where
  enumRep = constructor

instance EnumRep SyntaxKind GetAccessor where
  enumRep = getAccessor

instance EnumRep SyntaxKind SetAccessor where
  enumRep = setAccessor

instance EnumRep SyntaxKind CallSignature where
  enumRep = callSignature

instance EnumRep SyntaxKind ConstructSignature where
  enumRep = constructSignature

instance EnumRep SyntaxKind IndexSignature where
  enumRep = indexSignature

instance EnumRep SyntaxKind TypePredicate where
  enumRep = typePredicate

instance EnumRep SyntaxKind TypeReference where
  enumRep = typeReference

instance EnumRep SyntaxKind FunctionType where
  enumRep = functionType

instance EnumRep SyntaxKind ConstructorType where
  enumRep = constructorType

instance EnumRep SyntaxKind TypeQuery where
  enumRep = typeQuery

instance EnumRep SyntaxKind TypeLiteral where
  enumRep = typeLiteral

instance EnumRep SyntaxKind ArrayType where
  enumRep = arrayType

instance EnumRep SyntaxKind TupleType where
  enumRep = tupleType

instance EnumRep SyntaxKind OptionalType where
  enumRep = optionalType

instance EnumRep SyntaxKind RestType where
  enumRep = restType

instance EnumRep SyntaxKind UnionType where
  enumRep = unionType

instance EnumRep SyntaxKind IntersectionType where
  enumRep = intersectionType

instance EnumRep SyntaxKind ConditionalType where
  enumRep = conditionalType

instance EnumRep SyntaxKind InferType where
  enumRep = inferType

instance EnumRep SyntaxKind ParenthesizedType where
  enumRep = parenthesizedType

instance EnumRep SyntaxKind ThisType where
  enumRep = thisType

instance EnumRep SyntaxKind TypeOperator where
  enumRep = typeOperator

instance EnumRep SyntaxKind IndexedAccessType where
  enumRep = indexedAccessType

instance EnumRep SyntaxKind MappedType where
  enumRep = mappedType

instance EnumRep SyntaxKind LiteralType where
  enumRep = literalType

instance EnumRep SyntaxKind NamedTupleMember where
  enumRep = namedTupleMember

instance EnumRep SyntaxKind TemplateLiteralType where
  enumRep = templateLiteralType

instance EnumRep SyntaxKind TemplateLiteralTypeSpan where
  enumRep = templateLiteralTypeSpan

instance EnumRep SyntaxKind ImportType where
  enumRep = importType

instance EnumRep SyntaxKind ObjectBindingPattern where
  enumRep = objectBindingPattern

instance EnumRep SyntaxKind ArrayBindingPattern where
  enumRep = arrayBindingPattern

instance EnumRep SyntaxKind BindingElement where
  enumRep = bindingElement

instance EnumRep SyntaxKind ArrayLiteralExpression where
  enumRep = arrayLiteralExpression

instance EnumRep SyntaxKind ObjectLiteralExpression where
  enumRep = objectLiteralExpression

instance EnumRep SyntaxKind PropertyAccessExpression where
  enumRep = propertyAccessExpression

instance EnumRep SyntaxKind ElementAccessExpression where
  enumRep = elementAccessExpression

instance EnumRep SyntaxKind CallExpression where
  enumRep = callExpression

instance EnumRep SyntaxKind NewExpression where
  enumRep = newExpression

instance EnumRep SyntaxKind TaggedTemplateExpression where
  enumRep = taggedTemplateExpression

instance EnumRep SyntaxKind TypeAssertionExpression where
  enumRep = typeAssertionExpression

instance EnumRep SyntaxKind ParenthesizedExpression where
  enumRep = parenthesizedExpression

instance EnumRep SyntaxKind FunctionExpression where
  enumRep = functionExpression

instance EnumRep SyntaxKind ArrowFunction where
  enumRep = arrowFunction

instance EnumRep SyntaxKind DeleteExpression where
  enumRep = deleteExpression

instance EnumRep SyntaxKind TypeOfExpression where
  enumRep = typeOfExpression

instance EnumRep SyntaxKind VoidExpression where
  enumRep = voidExpression

instance EnumRep SyntaxKind AwaitExpression where
  enumRep = awaitExpression

instance EnumRep SyntaxKind PrefixUnaryExpression where
  enumRep = prefixUnaryExpression

instance EnumRep SyntaxKind PostfixUnaryExpression where
  enumRep = postfixUnaryExpression

instance EnumRep SyntaxKind BinaryExpression where
  enumRep = binaryExpression

instance EnumRep SyntaxKind ConditionalExpression where
  enumRep = conditionalExpression

instance EnumRep SyntaxKind TemplateExpression where
  enumRep = templateExpression

instance EnumRep SyntaxKind YieldExpression where
  enumRep = yieldExpression

instance EnumRep SyntaxKind SpreadElement where
  enumRep = spreadElement

instance EnumRep SyntaxKind ClassExpression where
  enumRep = classExpression

instance EnumRep SyntaxKind OmittedExpression where
  enumRep = omittedExpression

instance EnumRep SyntaxKind ExpressionWithTypeArguments where
  enumRep = expressionWithTypeArguments

instance EnumRep SyntaxKind AsExpression where
  enumRep = asExpression

instance EnumRep SyntaxKind NonNullExpression where
  enumRep = nonNullExpression

instance EnumRep SyntaxKind MetaProperty where
  enumRep = metaProperty

instance EnumRep SyntaxKind SyntheticExpression where
  enumRep = syntheticExpression

instance EnumRep SyntaxKind TemplateSpan where
  enumRep = templateSpan

instance EnumRep SyntaxKind SemicolonClassElement where
  enumRep = semicolonClassElement

instance EnumRep SyntaxKind Block where
  enumRep = block

instance EnumRep SyntaxKind EmptyStatement where
  enumRep = emptyStatement

instance EnumRep SyntaxKind VariableStatement where
  enumRep = variableStatement

instance EnumRep SyntaxKind ExpressionStatement where
  enumRep = expressionStatement

instance EnumRep SyntaxKind IfStatement where
  enumRep = ifStatement

instance EnumRep SyntaxKind DoStatement where
  enumRep = doStatement

instance EnumRep SyntaxKind WhileStatement where
  enumRep = whileStatement

instance EnumRep SyntaxKind ForStatement where
  enumRep = forStatement

instance EnumRep SyntaxKind ForInStatement where
  enumRep = forInStatement

instance EnumRep SyntaxKind ForOfStatement where
  enumRep = forOfStatement

instance EnumRep SyntaxKind ContinueStatement where
  enumRep = continueStatement

instance EnumRep SyntaxKind BreakStatement where
  enumRep = breakStatement

instance EnumRep SyntaxKind ReturnStatement where
  enumRep = returnStatement

instance EnumRep SyntaxKind WithStatement where
  enumRep = withStatement

instance EnumRep SyntaxKind SwitchStatement where
  enumRep = switchStatement

instance EnumRep SyntaxKind LabeledStatement where
  enumRep = labeledStatement

instance EnumRep SyntaxKind ThrowStatement where
  enumRep = throwStatement

instance EnumRep SyntaxKind TryStatement where
  enumRep = tryStatement

instance EnumRep SyntaxKind DebuggerStatement where
  enumRep = debuggerStatement

instance EnumRep SyntaxKind VariableDeclaration where
  enumRep = variableDeclaration

instance EnumRep SyntaxKind VariableDeclarationList where
  enumRep = variableDeclarationList

instance EnumRep SyntaxKind FunctionDeclaration where
  enumRep = functionDeclaration

instance EnumRep SyntaxKind ClassDeclaration where
  enumRep = classDeclaration

instance EnumRep SyntaxKind InterfaceDeclaration where
  enumRep = interfaceDeclaration

instance EnumRep SyntaxKind TypeAliasDeclaration where
  enumRep = typeAliasDeclaration

instance EnumRep SyntaxKind EnumDeclaration where
  enumRep = enumDeclaration

instance EnumRep SyntaxKind ModuleDeclaration where
  enumRep = moduleDeclaration

instance EnumRep SyntaxKind ModuleBlock where
  enumRep = moduleBlock

instance EnumRep SyntaxKind CaseBlock where
  enumRep = caseBlock

instance EnumRep SyntaxKind NamespaceExportDeclaration where
  enumRep = namespaceExportDeclaration

instance EnumRep SyntaxKind ImportEqualsDeclaration where
  enumRep = importEqualsDeclaration

instance EnumRep SyntaxKind ImportDeclaration where
  enumRep = importDeclaration

instance EnumRep SyntaxKind ImportClause where
  enumRep = importClause

instance EnumRep SyntaxKind NamespaceImport where
  enumRep = namespaceImport

instance EnumRep SyntaxKind NamedImports where
  enumRep = namedImports

instance EnumRep SyntaxKind ImportSpecifier where
  enumRep = importSpecifier

instance EnumRep SyntaxKind ExportAssignment where
  enumRep = exportAssignment

instance EnumRep SyntaxKind ExportDeclaration where
  enumRep = exportDeclaration

instance EnumRep SyntaxKind NamedExports where
  enumRep = namedExports

instance EnumRep SyntaxKind NamespaceExport where
  enumRep = namespaceExport

instance EnumRep SyntaxKind ExportSpecifier where
  enumRep = exportSpecifier

instance EnumRep SyntaxKind MissingDeclaration where
  enumRep = missingDeclaration

instance EnumRep SyntaxKind ExternalModuleReference where
  enumRep = externalModuleReference

instance EnumRep SyntaxKind JsxElement where
  enumRep = jsxElement

instance EnumRep SyntaxKind JsxSelfClosingElement where
  enumRep = jsxSelfClosingElement

instance EnumRep SyntaxKind JsxOpeningElement where
  enumRep = jsxOpeningElement

instance EnumRep SyntaxKind JsxClosingElement where
  enumRep = jsxClosingElement

instance EnumRep SyntaxKind JsxFragment where
  enumRep = jsxFragment

instance EnumRep SyntaxKind JsxOpeningFragment where
  enumRep = jsxOpeningFragment

instance EnumRep SyntaxKind JsxClosingFragment where
  enumRep = jsxClosingFragment

instance EnumRep SyntaxKind JsxAttribute where
  enumRep = jsxAttribute

instance EnumRep SyntaxKind JsxAttributes where
  enumRep = jsxAttributes

instance EnumRep SyntaxKind JsxSpreadAttribute where
  enumRep = jsxSpreadAttribute

instance EnumRep SyntaxKind JsxExpression where
  enumRep = jsxExpression

instance EnumRep SyntaxKind CaseClause where
  enumRep = caseClause

instance EnumRep SyntaxKind DefaultClause where
  enumRep = defaultClause

instance EnumRep SyntaxKind HeritageClause where
  enumRep = heritageClause

instance EnumRep SyntaxKind CatchClause where
  enumRep = catchClause

instance EnumRep SyntaxKind PropertyAssignment where
  enumRep = propertyAssignment

instance EnumRep SyntaxKind ShorthandPropertyAssignment where
  enumRep = shorthandPropertyAssignment

instance EnumRep SyntaxKind SpreadAssignment where
  enumRep = spreadAssignment

instance EnumRep SyntaxKind EnumMember where
  enumRep = enumMember

instance EnumRep SyntaxKind UnparsedPrologue where
  enumRep = unparsedPrologue

instance EnumRep SyntaxKind UnparsedPrepend where
  enumRep = unparsedPrepend

instance EnumRep SyntaxKind UnparsedText where
  enumRep = unparsedText

instance EnumRep SyntaxKind UnparsedInternalText where
  enumRep = unparsedInternalText

instance EnumRep SyntaxKind UnparsedSyntheticReference where
  enumRep = unparsedSyntheticReference

instance EnumRep SyntaxKind SourceFile where
  enumRep = sourceFile

instance EnumRep SyntaxKind Bundle where
  enumRep = bundle

instance EnumRep SyntaxKind UnparsedSource where
  enumRep = unparsedSource

instance EnumRep SyntaxKind InputFiles where
  enumRep = inputFiles

instance EnumRep SyntaxKind JSDocTypeExpression where
  enumRep = jSDocTypeExpression

instance EnumRep SyntaxKind JSDocNameReference where
  enumRep = jSDocNameReference

instance EnumRep SyntaxKind JSDocMemberName where
  enumRep = jSDocMemberName

instance EnumRep SyntaxKind JSDocAllType where
  enumRep = jSDocAllType

instance EnumRep SyntaxKind JSDocUnknownType where
  enumRep = jSDocUnknownType

instance EnumRep SyntaxKind JSDocNullableType where
  enumRep = jSDocNullableType

instance EnumRep SyntaxKind JSDocNonNullableType where
  enumRep = jSDocNonNullableType

instance EnumRep SyntaxKind JSDocOptionalType where
  enumRep = jSDocOptionalType

instance EnumRep SyntaxKind JSDocFunctionType where
  enumRep = jSDocFunctionType

instance EnumRep SyntaxKind JSDocVariadicType where
  enumRep = jSDocVariadicType

instance EnumRep SyntaxKind JSDocNamepathType where
  enumRep = jSDocNamepathType

instance EnumRep SyntaxKind JSDocComment where
  enumRep = jSDocComment

instance EnumRep SyntaxKind JSDocText where
  enumRep = jSDocText

instance EnumRep SyntaxKind JSDocTypeLiteral where
  enumRep = jSDocTypeLiteral

instance EnumRep SyntaxKind JSDocSignature where
  enumRep = jSDocSignature

instance EnumRep SyntaxKind JSDocLink where
  enumRep = jSDocLink

instance EnumRep SyntaxKind JSDocLinkCode where
  enumRep = jSDocLinkCode

instance EnumRep SyntaxKind JSDocLinkPlain where
  enumRep = jSDocLinkPlain

instance EnumRep SyntaxKind JSDocTag where
  enumRep = jSDocTag

instance EnumRep SyntaxKind JSDocAugmentsTag where
  enumRep = jSDocAugmentsTag

instance EnumRep SyntaxKind JSDocImplementsTag where
  enumRep = jSDocImplementsTag

instance EnumRep SyntaxKind JSDocAuthorTag where
  enumRep = jSDocAuthorTag

instance EnumRep SyntaxKind JSDocDeprecatedTag where
  enumRep = jSDocDeprecatedTag

instance EnumRep SyntaxKind JSDocClassTag where
  enumRep = jSDocClassTag

instance EnumRep SyntaxKind JSDocPublicTag where
  enumRep = jSDocPublicTag

instance EnumRep SyntaxKind JSDocPrivateTag where
  enumRep = jSDocPrivateTag

instance EnumRep SyntaxKind JSDocProtectedTag where
  enumRep = jSDocProtectedTag

instance EnumRep SyntaxKind JSDocReadonlyTag where
  enumRep = jSDocReadonlyTag

instance EnumRep SyntaxKind JSDocOverrideTag where
  enumRep = jSDocOverrideTag

instance EnumRep SyntaxKind JSDocCallbackTag where
  enumRep = jSDocCallbackTag

instance EnumRep SyntaxKind JSDocEnumTag where
  enumRep = jSDocEnumTag

instance EnumRep SyntaxKind JSDocParameterTag where
  enumRep = jSDocParameterTag

instance EnumRep SyntaxKind JSDocReturnTag where
  enumRep = jSDocReturnTag

instance EnumRep SyntaxKind JSDocThisTag where
  enumRep = jSDocThisTag

instance EnumRep SyntaxKind JSDocTypeTag where
  enumRep = jSDocTypeTag

instance EnumRep SyntaxKind JSDocTemplateTag where
  enumRep = jSDocTemplateTag

instance EnumRep SyntaxKind JSDocTypedefTag where
  enumRep = jSDocTypedefTag

instance EnumRep SyntaxKind JSDocSeeTag where
  enumRep = jSDocSeeTag

instance EnumRep SyntaxKind JSDocPropertyTag where
  enumRep = jSDocPropertyTag

instance EnumRep SyntaxKind SyntaxList where
  enumRep = syntaxList

instance EnumRep SyntaxKind NotEmittedStatement where
  enumRep = notEmittedStatement

instance EnumRep SyntaxKind PartiallyEmittedExpression where
  enumRep = partiallyEmittedExpression

instance EnumRep SyntaxKind CommaListExpression where
  enumRep = commaListExpression

instance EnumRep SyntaxKind MergeDeclarationMarker where
  enumRep = mergeDeclarationMarker

instance EnumRep SyntaxKind EndOfDeclarationMarker where
  enumRep = endOfDeclarationMarker

instance EnumRep SyntaxKind SyntheticReferenceExpression where
  enumRep = syntheticReferenceExpression

instance EnumRep SyntaxKind Count where
  enumRep = count

instance EnumRep SyntaxKind FirstAssignment where
  enumRep = firstAssignment

instance EnumRep SyntaxKind LastAssignment where
  enumRep = lastAssignment

instance EnumRep SyntaxKind FirstCompoundAssignment where
  enumRep = firstCompoundAssignment

instance EnumRep SyntaxKind LastCompoundAssignment where
  enumRep = lastCompoundAssignment

instance EnumRep SyntaxKind FirstReservedWord where
  enumRep = firstReservedWord

instance EnumRep SyntaxKind LastReservedWord where
  enumRep = lastReservedWord

instance EnumRep SyntaxKind FirstKeyword where
  enumRep = firstKeyword

instance EnumRep SyntaxKind LastKeyword where
  enumRep = lastKeyword

instance EnumRep SyntaxKind FirstFutureReservedWord where
  enumRep = firstFutureReservedWord

instance EnumRep SyntaxKind LastFutureReservedWord where
  enumRep = lastFutureReservedWord

instance EnumRep SyntaxKind FirstTypeNode where
  enumRep = firstTypeNode

instance EnumRep SyntaxKind LastTypeNode where
  enumRep = lastTypeNode

instance EnumRep SyntaxKind FirstPunctuation where
  enumRep = firstPunctuation

instance EnumRep SyntaxKind LastPunctuation where
  enumRep = lastPunctuation

instance EnumRep SyntaxKind FirstToken where
  enumRep = firstToken

instance EnumRep SyntaxKind LastToken where
  enumRep = lastToken

instance EnumRep SyntaxKind FirstTriviaToken where
  enumRep = firstTriviaToken

instance EnumRep SyntaxKind LastTriviaToken where
  enumRep = lastTriviaToken

instance EnumRep SyntaxKind FirstLiteralToken where
  enumRep = firstLiteralToken

instance EnumRep SyntaxKind LastLiteralToken where
  enumRep = lastLiteralToken

instance EnumRep SyntaxKind FirstTemplateToken where
  enumRep = firstTemplateToken

instance EnumRep SyntaxKind LastTemplateToken where
  enumRep = lastTemplateToken

instance EnumRep SyntaxKind FirstBinaryOperator where
  enumRep = firstBinaryOperator

instance EnumRep SyntaxKind LastBinaryOperator where
  enumRep = lastBinaryOperator

instance EnumRep SyntaxKind FirstStatement where
  enumRep = firstStatement

instance EnumRep SyntaxKind LastStatement where
  enumRep = lastStatement

instance EnumRep SyntaxKind FirstNode where
  enumRep = firstNode

instance EnumRep SyntaxKind FirstJSDocNode where
  enumRep = firstJSDocNode

instance EnumRep SyntaxKind LastJSDocNode where
  enumRep = lastJSDocNode

instance EnumRep SyntaxKind FirstJSDocTagNode where
  enumRep = firstJSDocTagNode

instance EnumRep SyntaxKind LastJSDocTagNode where
  enumRep = lastJSDocTagNode
