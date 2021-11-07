# Enums

Typescript enums are a bit tricky to accurately model in Purescript, since they are basically on the typelevel. Consider the following example:

```typescript
enum Syntax {
    NumberSyntax,
    StringSyntax 
}

interface NumberKeyword {
    kind: Syntax.NumberSyntax
}

const nk: NumberKeyword = { kind: Syntax.NumberSyntax }
const nk2: NumberKeyword = { kind: Syntax.StringSyntax }
```

In this example, the type of `kind` in `NumberKeyword` is `Syntax.NumberSyntax`, a value of the enum `Syntax`. So creating a `NumberKeyword` can be done by assigning `Syntax.NumberSyntax` to `kind`, but assigning `Syntax.StringSyntax` to `kind` results in the following type error:

```bash 
syntax.ts:12:30 - error TS2322: Type 'Syntax.StringSyntax' is not assignable to type 'Syntax.NumberSyntax'.

12 const nk2: NumberKeyword = { kind: Syntax.StringSyntax }
                                ~~~~

  syntax.ts:8:5
    8     kind: Syntax.NumberSyntax
          ~~~~
    The expected type comes from property 'kind' which is declared here on type 'NumberKeyword'


Found 1 error.
```

Note also, that the ts enum at runtime will typically have a runtime representation of `number` or `string`. 
Printing our `Syntax.NumberSyntax` will print out `0` (unless we change the runtime representation, which is also possible in TS):
```typescript
console.log(Syntax.NumberSyntax)
// 0
```

Currently we have two ways to model this behaviour. A strict approach which follows pretty much the typescript behaviour and a lax approach that follows a more purescripty approach but gives less guarantees.



## Strict approach

The strict approach is to model the enum as a typelevel adt.

```purescript
-- define the ts enum `Syntax` as a data type without values.
data SyntaxEnum
-- define the ts enum values as foreign data types of type `SyntaxEnum`
foreign import data NumberSyntax :: SyntaxEnum
foreign import data StringSyntax :: SyntaxEnum

-- define a typelevel proxy for `Syntax` in order to create types from the foreign data types.
data Syntax :: SyntaxEnum -> Type
data Syntax k

-- as we declared `NumberSyntax` and `StringSyntax` foreign, we have no way to construct them, so let's define foreign imports for them.
foreign import numberSyntax :: Syntax NumberSyntax
foreign import stringSyntax :: Syntax StringSyntax

-- now we can create the same ts record in Purescript with the same guarantee, i.e. that our `NumberKeyword` only accepts `NumberSyntax` enum values.
type NumberKeyword
  = { kind :: Syntax NumberSyntax
    }

nk :: NumberKeyword
nk =
  { kind: numberSyntax --  stringSyntax results in a type error
  }
```

**Advantages**
- Represents typescript enums pretty accurate and thus gives us the same type guarantees as the ts version.
- Works well with union types

**Disadvantages**
- Feels a bit clumsy/boilerplatey compared to the lax approach
- Some (background) machinery is necessary to make this work respectively user-friendly

## Lax approach

The lax approach is to use basic adts.

```purescript
data SyntaxEnum = NumberSyntax | StringSyntax

type NumberKeyword
  = { kind :: SyntaxEnum
    }

nk :: NumberKeyword
nk =
  { kind: NumberSyntax -- no guarantee here, could also assign StringSyntax
  }
```

**Advantages**
- Feels more natural in ps.

**Disadvantages**
- Does *not* give the same type guarantees as ts enums, which is a bit shameful.
- Does *not* work well with union types
