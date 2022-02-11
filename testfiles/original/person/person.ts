type Person = {
    name: string,
    age: Age
}

type Age = number

export function getAge(person: Person): Age { return person.age }

export function createPerson(name: string, age: Age): Person {
    return { name, age }
}

export const johnDoe: Person = { name: "John Doe", age: 30 }

export function createPersonOrDefault(name?: string, age?: Age): Person {
    return { name: name ? name : "John", age: age ? age : 30 }
}

export function createPersonWithName(name: string, age?: Age): Person {
    return { name: name, age: age ? age : 30 }
}

type NullablePerson = {
    name?: string,
    age?: Age
}
