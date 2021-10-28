import * as ts from "typescript"

const file = "person/person.ts"
let program = ts.createProgram([file], { allowJs: true })
const sourceFile = program.getSourceFile(file)
const printer = ts.createPrinter({ newLine: ts.NewLineKind.LineFeed })


ts.forEachChild(sourceFile!, node => {
    // if (ts.isFunctionDeclaration(node)) {
    //     console.log(`Got a function: ${node}`)
    // } else if (ts.isVariableStatement(node)) {
    //     console.log(`Got a variable: ${node}`)
    // } else if (ts.isInterfaceDeclaration(node)) {
    //     console.log(`Got a interface: ${node}`)
    // } else {
        //console.log(`Got unknown: ${printer.printNode(ts.EmitHint.Unspecified,node,sourceFile!)}`, node)
    //}

    switch (node.kind) {
      case ts.SyntaxKind.TypeAliasDeclaration:
       const foo = node as ts.TypeAliasDeclaration
       console.log("got typeAliasDeclaration with the name", foo.name)       
       break;

    }

    if(ts.isTypeAliasDeclaration(node)) {
        console.log("Name", node.name.escapedText)
        node.forEachChild(n => {
            if(ts.isTypeLiteralNode(n)){
                console.log("Got literal")
                n.members.map(member => {
                    if(ts.isPropertySignature(member)) {
                        console.log((member.name as ts.Identifier).escapedText)
                        switch(member.type!.kind){
                            case ts.SyntaxKind.StringKeyword: 
                                console.log("String")
                                break
                            case ts.SyntaxKind.NumberKeyword:
                                console.log("Number")
                                break
                        }
                    }
                })
            }
        })
    }
})