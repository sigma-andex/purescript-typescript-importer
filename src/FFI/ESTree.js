"use strict"
const astring = require('astring')
const acorn = require('acorn')

// Only FFI the bare minimum me need for the ES code generation

const options = { ecmaVersion: 6 }

exports.parse =  (code) => acorn.parse(code, options)

exports.mkProgram = (body) => {
    return {
        type: 'Program',
        body: body
    }
}
exports.generate = (ast) => astring.generate(ast)
