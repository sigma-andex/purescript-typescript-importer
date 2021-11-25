type Person = {
    name: string,
    age: number
}

export function getAge(person: Person): number { return person.age }

export function createPerson(name: string, age: number): Person {
    return { name, age }
}

export const johnDoe: Person = { name: "John Doe", age: 30 }
