class Person {
  name: string
  age: Age
}

type Age = number

type PersonAlias = Person

export function getAge(person: PersonAlias): Age { return person.age }

export function createPerson(name: string, age: Age): PersonAlias {
    return { name, age }
}
