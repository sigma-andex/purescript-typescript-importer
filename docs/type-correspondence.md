# Typescript <=> Purescript type correspondence

This document defines the correspondence between the representations in Typescript and Purescript. This is a WIP document. Nothing written here is set in stone, may change in the future, and feedback is welcomed!

## Everyday types

As described in [everyday types](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html) in the TS documentation.

<table>
<thead>
<tr><td>Type</td><td>Typescript</td><td>Purescript</td><td>Comment</td><td>Open questions</td></tr>
</thead>
<tbody>
<tr>
<td>Strings</td>
<td>

```typescript
string 
```

</td>
<td>

```purescript
Data.String
```

</td>
  <td></td>
  <td></td>
</tr>
<tr>
<td>Numbers</td>
<td>

```typescript
number 
```
  
</td>
<td>

```purescript
Data.Number
```

</td>
  <td></td><td></td>
</tr>
<tr>
<td>Booleans</td>
<td>

```typescript
boolean 
```
</td>
<td>

```purescript
Data.Boolean
```
</td>
  <td></td><td></td>
</tr>
<tr>
<td>Arrays</td>
<td>

```typescript
Array<T> 
```
</td>
<td>

```purescript
Data.Array T 
```
</td>
  <td></td><td></td>
</tr>
<tr>
<td>Functions</td>
<td>

```typescript
const add : (a: number, b: number) => number = (a: number, b: string) => a + b
```
</td>
<td>

```purescript
add :: Number -> Number -> Number
```
</td>
  <td>A curried representation will be automatically generated. </td>
  <td></td>
</tr>



<tr>
<td>Object types aka records</td>
<td>

```typescript
type Point = { x: number; y: string }
const pt: Point = { x:5, y: "Bla"}
```
</td>
<td>

```purescript
type Point = { x:: Number, y:: String }
pt :: Point 
pt = { x:5, y: "Bla"}
```
</td>
<td></td><td></td>
</tr>
<tr>
<td>Optional properties</td>
<td>

```typescript
const person : { first: string; last?: string } = { first: "hans" }
```
</td>
<td>

```purescript
person :: { first:: String, last :: Maybe String } 
person = { first : "hans", last: Nothing}
```
</td>
<td>
Optional properties in Typescript are represented as `Maybe`s in Purescript. An automatic conversin between the `Nullable` type and `Maybe` is provided.
</td>
  <td></td>
</tr>
<tr>
<td>Union types</td>
<td>

```typescript
const id: number | string = "abc-123"
```
</td>
<td>

```purescript
id :: Number |+| String 
id = asOneOf "abc-123
```
</td>
<td>
Union types are represented using <a href="https://github.com/jvliwanag/purescript-untagged-union"><code>untagged-union</code></a>.
</td>
<td>Can we create the necessary typeclass machinery to support this?</td>
</tr>

</tbody>
</table>

