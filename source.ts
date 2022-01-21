

namespace person {
    // open
    type Person = { 
        name: string,
        age: number
    }
    interface PersonI{
        x : boolean
    }
    class PersonC { 
        name: string
        age: number
    }
    type Age = number 
    type Street = string
    type XS = string[]
    export function getName(p: Person, b: Age, c: Street, d: PersonC,e:PersonI, f: XS): number { return p.age }
    // getName :: forall r. Array (Array (PersonR r)) -> .... -> Number
    
}
