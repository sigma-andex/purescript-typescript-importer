type Person = {
    name: string,
    house: House
}

type House = {
    address: string
}

export function createPerson(name: string, house: House): Person {
    return { name, house  }
}

export const johnDoe: Person = { name: "John Doe", house: { address: "M" } }

export function getAddress(p: Person): string {
    return p.house.address
}
